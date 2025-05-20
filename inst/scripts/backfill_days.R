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
  library(stars)
  library(dplyr)
  library(charlier)
  library(argparser)
  library(cofbb)
  library(yaml)
})

#' @param date a single Date
#' @param cfg the configuration
#' @param data_path the data directory
fetch_this_day = function(date, cfg, data_path, P){
  charlier::info("backfill_days: fetching %s", format(date, "%Y-%m-%d"))
  #daynum = format(date, "%d")
  #if (daynum == "01") charlier::info("backfill_days:fetching %s", format(date, "%Y-%b"))
  
  out_path <- copernicus::copernicus_path(cfg$region, 
                                          cfg$product, 
                                          format(date, "%Y"),
                                          format(date, "%m%d"))
  
  MISSED_COUNT = 0

  ss = try( P |>
              fetch_product_by_day(x = date, bb = cfg$bb))
  if (inherits(ss, "try-error")){
    MISSED_COUNT <<- MISSED_COUNT + 1
    charlier::warn("backfill_days: failed to fetch %s", format(date, "%Y-%m-%d"))
    return(NULL)
  }
  isnull = sapply(ss, is.null)
  ss = ss[!isnull]
  if (length(ss) == 0){
    MISSED_COUNT <<- MISSED_COUNT + 1
    charlier::warn("backfill_days: unable to fetch %s", format(date, "%Y-%m-%d"))
    return(NULL)
  }

  
  ff = lapply(names(ss),
              function(dataset){
                if (is.null(ss[[dataset]])) return(NULL)
                vars = names(ss[[dataset]])
                depth = filter(P, dataset_id == dataset) |>
                  dplyr::filter(short_name %in% vars) |>
                  dplyr::pull(dplyr::all_of("depth"))
                time = format(date, "%Y-%m-%dT000000")  
                per = dataset_period(dataset)
                treatment = "raw"
                # productid/region/yyyy/mmdd/datasetid__time_depth_period_var_treatment.ext
                f = file.path(out_path, 
                              sprintf("%s__%s_%s_%s_%s_%s.tif", dataset, time, depth, per, vars, treatment))
                ok = copernicus::make_path(dirname(f) |> unique())
                for (i in seq_along(names(ss[[dataset]]))) {
                  stars::write_stars(ss[[dataset]][i], f[i])
                }
                f
              })
  
  unlist(ff) %>%
    copernicus::decompose_filename() 
}

backfill_dataset = function(tbl, key, path = ".", DB = NULL){
  
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
    dplyr::group_by(depth) |>
    dplyr::group_map(
      function(tab, quay){
        lapply(seq_along(missing_dates),
          function(idate){
            time = c(missing_dates[i], missing_dates[i])
            depth = c(tb$mindepth, tbl$maxdepth)
            x = andreas::fetch_andreas(tab,
                                       depth = depth,
                                       time = time)
            
          }) |>
          dplyr::bind_rows()
      }, .keep = TRUE) |>
    dplyr::bind_rows()
  db
}


main = function(cfg = NULL){
 
  P = andreas::read_product_lut(cfg$product) |>
    dplyr::filter(fetch == "yes")
  
  path = copernicus::copernicus_path(cfg$region, cfg$product) |>
    copernicus::make_path()
  
  DB = andreas::read_database(path)
  
  # for each dataset_id
  # compare the stored dates with those served
  # retrieve just the missing ones
  newdb = P |>
    dplyr::group_by(.data$dataset_id) |>
    dplyr::group_map(backfill_dataset, path = path, DB = DB, .keep = TRUE) |>
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
                                         "fetch-day-GLOBAL_MULTIYEAR_BGC_001_029.yaml")) |>
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


