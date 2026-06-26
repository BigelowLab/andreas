# usage: backfill_days.R [--] [--help] [--config CONFIG] [--start START] [--end END]
# 
# Backfill copernicus data
# 
# flags:
#   -h, --help    show this help message and exit
# 
# optional arguments:
#   -c, --config  configuration file [default:
#       /mnt/s1/projects/ecocast/coredata/copernicus/config/world-GLOBAL_MULTIYEAR_PHY_001_030.yaml]
#   -s, --start   start date, default is 1993-01-01 [default: 1993-01-01]
#   -e, --end     end date, default is today + 3 [default: 2026-06-24]


# The 4 configs we should run on are in /mnt/s1/projects/ecocast/coredata/copernicus/config/
# world-GLOBAL_ANALYSISFORECAST_BGC_001_028.yaml
# world-GLOBAL_MULTIYEAR_BGC_001_029.yaml
# chfc-GLOBAL_ANALYSISFORECAST_PHY_001_024.yaml
# chfc-GLOBAL_MULTIYEAR_PHY_001_030.yaml
  
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


# p a single row of the product lut with .name (name_depth) column added
# path product path
# DB the COMPLETE database which we filter internally
# cfg config list
backfill_dataset = function(p, key, 
                            path = ".", 
                            DB = NULL, 
                            cfg = NULL){
  
  charlier::info("backfill_dataset: %s at %s", p$dataset_id[1], p$.name[1])
  # these are what the catalog offers for this dataset
  # we assume for a given dataset all start/end dates are shared in
  # common
  available_dates = seq(from = min(p$start_time),
                        to = max(p$end_time),
                        by = copernicus::dataset_period(p$dataset_id[1],
                                                        for_sequence = TRUE)) |>
    as.Date()
  # here we compute the missing dates
  missing_dates = if(nrow(DB) > 0) {
      have = DB |> 
        dplyr::filter(.data$id == p$dataset_id[1],
                      .data$.name == p$.name[1]) |>
        dplyr::arrange(date) |>
        dplyr::pull(date)
      available_dates[!(available_dates %in% have)]
    } else {
      available_dates
    }
  
  # if there are no missing dates then there are no records to add, return NULL
  n_missing = length(missing_dates)
  if (n_missing == 0){
    charlier::info("  no missing dates - returning")
    return(NULL)
  } else {
    charlier::info("  missing up to %i days", n_missing)
  }
  
  # TODO this is set up to download chunks of contiguous dates but it is implemented
  # on a per-date iteration.  Someday implement an improvement to grab contiguous dates
  # but for now this is fine.  It runs anywhere from 7s to 12s per day per variable per depth
  db = p |>
    dplyr::group_map(
      function(tab, quay){
        lapply(seq_along(missing_dates),
          function(idate){
            charlier::info("  backfill_dataset: %s for %s at %s", 
                           format(missing_dates[idate]),
                           paste(tab$name, collapse = ", "),
                           tab$depth)
            time = c(missing_dates[idate], missing_dates[idate])
            depth = c(tab$mindepth[1], tab$maxdepth[2])
            x = andreas::fetch_andreas(tab,
                                       bb = cfg$bb,
                                       time = time,
                                       form = "stars")[[1]]
            if (!is.null(x)){
              dimx = stars::st_dimensions(x)
              andreas = attr(x, "andreas")
              names(x) <- tab$name
              period = copernicus::dataset_period(tab$dataset_id[1])
              treatment = "raw"
              d = stars::st_dimensions(x)
              time = andreas$time |> format("%Y-%m-%dT000000")
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
            } else {
              # x is null
              db = NULL
            }
            db
          }) |>
          dplyr::bind_rows()
      }, .keep = TRUE) |>
    dplyr::bind_rows()
  db
}


main = function(cfg = NULL,
                dates = c(as.Date("1993-01-01"), Sys.Date() + 3)){
 
  P = andreas::read_product_lut(cfg$product) |>
    dplyr::filter(fetch == "yes") |>
    #dplyr::mutate(n_depth = ifelse(is.na(.data$n_depth), 1, .data$n_depth)) |>
    #dplyr::group_by(dataset_id, depth, n_depth) |>
    dplyr::mutate(.name = paste(.data$short_name, .data$depth, sep = "_"))
  
  path = copernicus::copernicus_path(cfg$region, cfg$product) |>
    copernicus::make_path()
  
  DB = andreas::read_database(path) |>
    dplyr::filter(dplyr::between(.data$date, dates[1], dates[2]))
  
  # for each dataset_id
  # compare the stored dates with those served
  # retrieve just the missing ones
  newdb = P |>
    dplyr::rowwise() |>
    dplyr::group_map(backfill_dataset, path = path, DB = DB, cfg = cfg, .keep = TRUE) |>
    dplyr::bind_rows() |>
    andreas::append_database(path)
  
  
  return(0)
}

Args = argparser::arg_parser("Backfill copernicus data",
                             name = "backfill_days.R", 
                             hide.opts = TRUE) |>
  add_argument("--config",
               help = 'configuration file',
               default = copernicus_path("config", 
                                         "world-GLOBAL_MULTIYEAR_BGC_001_029.yaml")) |>
  add_argument("--start",
               help = "start date, default is 1993-01-01",
               default = "1993-01-01") |>
  add_argument("--end",
               help = "end date, default is today + 3",
               default = format(Sys.Date() + 3, "%Y-%m-%d")) |>
  parse_args()


cfg = yaml::read_yaml(Args$config)
cfg$bb = cofbb::get_bb(cfg$region)
charlier::start_logger(copernicus_path(cfg$reg, cfg$product, "log"))
charlier::info("backfill_days for %s", cfg$product)
START_DATE = as.Date(Args$start, format = "%Y-%m-%d")
END_DATE = as.Date(Args$end, format = "%Y-%m-%d")
MAX_MISSED_COUNT = 3
dates = c(START_DATE, END_DATE)
if (!interactive()){
  ok = main(cfg, dates = dates)
  charlier::info("backfill_days: done")
  quit(save = "no", status = ok)
} 


