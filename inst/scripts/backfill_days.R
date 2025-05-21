# usage: backfill_days.R [--] [--help] [--config CONFIG]
# 
# Backfill copernicus data
# 
# flags:
#   -h, --help    show this help message and exit
# 
# optional arguments:
#   -c, --config  configuration file [default:
#      /mnt/s1/projects/ecocast/coredata/copernicus/config/fetch-day-GLOBAL_MULTIYEAR_PHY_001_030.yaml]

suppressPackageStartupMessages({
  library(copernicus)
  library(andreas)
  library(stars)
  library(dplyr)
  library(charlier)
  library(argparser)
  library(cofbb)
  library(yaml)
})



backfill_dataset = function(tbl, key, path = ".", DB = NULL, cfg = NULL, verbose = interactive()){
  
  if (verbose) cat("backfill_dataset: ", tbl$dataset_id[1], "at", tbl$depth[1], "\n")
  # these are what the catalog offers for this dataset
  # we assume for a given dataset all start/end dates are shared in
  # common
  available_dates = seq(from = min(tbl$start_time),
                        to = max(tbl$end_time),
                        by = copernicus::dataset_period(tbl$dataset_id[1]))
  # here we compute the missing dates
  missing_dates = if(nrow(DB) > 0) {
      have = DB |> 
        dplyr::filter(.data$id == tbl$dataset_id[1]) |>
        dplyr::arrange(date) |>
        dplyr::pull(date)
      available_dates[!(available_dates %in% have)]
    } else {
      available_dates
    }
  
  db = tbl |>
    #dplyr::group_by(depth) |>
    dplyr::group_map(
      function(tab, quay){
        lapply(seq_along(missing_dates),
          function(idate){
            if (verbose){
              cat("  backfill_dataset: ", format(missing_dates[idate]), "\n")
            }
            time = c(missing_dates[idate], missing_dates[idate])
            depth = c(tab$mindepth[1], tab$maxdepth[2])
            x = andreas::fetch_andreas(tab,
                                       bb = cfg$bb,
                                       time = time)[[1]]
            dimx = stars::st_dimensions(x)
            andreas = attr(x, "andreas")
            names(x) <- tab$name
            period = copernicus::dataset_period(tab$dataset_id[1])
            treatment = "raw"
            d = stars::st_dimensions(x)
            time = andreas$time |> format("%Y-%m-%dT00000")
            db = tab |> 
              rowwise()|>
              group_map(
                function(p, k){
                  nm = p$short_name
                  fname = sprintf("%s__%s_%s_%s_%s_%s.tif", 
                                  p$dataset_id, 
                                  time, 
                                  p$depth, 
                                  period, 
                                  nm, 
                                  treatment)
                  db = decompose_filename(fname)
                  ofiles = compose_filename(db, path)
                  
                  for (i in seq_along(fname)){
                    ok = make_path(dirname(ofiles[i]))
                    s = if ("time" %in% names(dimx)){
                      stars::write_stars(dplyr::slice(x[nm], "time", i), ofiles[i]) 
                    } else {
                      stars::write_stars(x[nm], ofiles[i]) 
                    }
                  }
                  db
                } ) |>
              dplyr::bind_rows()
          }) |>
          dplyr::bind_rows()
      }, .keep = TRUE) |>
    dplyr::bind_rows()
  db
}


main = function(cfg = NULL){
 
  P = andreas::read_product_lut(cfg$product) |>
    dplyr::filter(fetch == "yes") |>
    dplyr::group_by(dataset_id, depth)
  
  path = copernicus::copernicus_path(cfg$region, cfg$product) |>
    copernicus::make_path()
  
  DB = andreas::read_database(path)
  
  # for each dataset_id
  # compare the stored dates with those served
  # retrieve just the missing ones
  newdb = P |>
    dplyr::group_map(backfill_dataset, path = path, DB = DB, cfg = cfg, .keep = TRUE) |>
    dplyr::bind_rows() |>
    andreas::append_database(product_path)
  
  
  return(0)
}

Args = argparser::arg_parser("Backfill copernicus data",
                             name = "backfill_days.R", 
                             hide.opts = TRUE) |>
  add_argument("--config",
               help = 'configuration file',
               default = copernicus_path("config", 
                                         "world-GLOBAL_MULTIYEAR_BGC_001_029.yaml")) |>
  parse_args()


cfg = yaml::read_yaml(Args$config)
cfg$bb = cofbb::get_bb(cfg$region)
charlier::start_logger(copernicus_path(cfg$reg, cfg$product, "log"))
charlier::info("backfill_days for %s", cfg$product)

MAX_MISSED_COUNT = 3

if (!interactive()){
  ok = main(cfg)
  charlier::info("backfill_days: done")
  quit(save = "no", status = ok)
} else {
  date = as.Date(Args$date)
}


