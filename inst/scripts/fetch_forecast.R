# usage: fetch_forecast.R [--] [--help] [--date DATE] [--config CONFIG]
# 
# Fetch a copernicus forecast
# 
# flags:
#   -h, --help    show this help message and exit
# 
# optional arguments:
#   -d, --date    the date of the forecast [default: 2025-03-10]
#   -c, --config  configuration file [default:
#         /usr/lib64/R/library/copernicus/config/nwa_daily_fetch.yaml]
# 

# Fetch 10 days of forecasts of Copernicus global-analysis-forecast-phy-001-024 data
# for the nwa region (today: (today + 9))
#
# fetch_copernicus
#   download file, open file, read as stars, close file, delete file
# save by slice var <reg>/<prod>/yyyy/mmdd/date_var_depth.tif
# append database and write

suppressPackageStartupMessages({
  library(copernicus)
  if (interactive()){
   devtools::load_all() 
  } else {
    library(andreas)
  }
  library(stars)
  library(dplyr)
  library(charlier)
  library(argparser)
  library(cofbb)
  library(yaml)
})


#' Fetch variable for one dataset
#' @param tbl one or more rows of product lut for one dataset
#' @param key likely empty tibble
#' @param dates the dates to retrieve
#' @param out_path the output path
#' @return a database table 
fetch_dataset = function(tbl, key, dates = NULL, out_path = NULL, cfg = NULL){
  
  if (DEVMODE) cat("fetch_dataset:", tbl$dataset_id[1],"\n")
  # here we need to check that the dates of the request (dates) fit within the 
  # bounds of the source (tbl$start_time, tbl$end_time).  Presumably for a given 
  # dataset the stars/stop dates are the same for all variables

  # here we get a list for each dataset-depth group
  x = fetch_andreas(tbl,
                    time = range(dates),
                    bb = cfg$bb, 
                    verbose = TRUE)[[1]]
  dimx = stars::st_dimensions(x)
  andreas = attr(x, "andreas")
  names(x) <- tbl$name
  period = copernicus::dataset_period(tbl$dataset_id[1])
  treatment = "raw"
  d = stars::st_dimensions(x)
  time = andreas$time |> format("%Y-%m-%dT00000")
  db = tbl |> 
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
        ofiles = compose_filename(db, out_path)
    
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
  db
}


main = function(date = Sys.Date(), cfg = NULL){
  
  if (!inherits(date, "Date")) date = as.Date(date)
  
  dates <- date + c(0,seq_len(9))
  
  P = andreas::read_product_lut(cfg$product) |>
    dplyr::filter(fetch == "yes") |>
    group_by(dataset_id, depth) 

  out_path <- copernicus::copernicus_path(cfg$region, cfg$product)
  
  if (FALSE) tbl = filter(P, dataset_id == "cmems_mod_glo_phy_anfc_0.083deg_P1D-m")
  
  
  db = P |>
    group_map(fetch_dataset, 
              out_path = out_path, 
              cfg = cfg, 
              dates = dates,
              .keep = TRUE) |>
    dplyr::bind_rows() |>
    andreas::append_database(out_path)
  
  
  return(0)
}

DEVMODE = interactive()

Args = argparser::arg_parser("Fetch a copernicus forecast",
                             name = "fetch_forecast.R", 
                             hide.opts = TRUE) |>
  add_argument("--date",
               help = "the date of the forecast",
               default = format(Sys.Date() - 1, "%Y-%m-%d"),
               type = "character") |>
  add_argument("--config",
               help = 'configuration file',
               default = copernicus_path("config","fetch-day-GLOBAL_ANALYSISFORECAST_PHY_001_024.yaml")) |>
  parse_args()


cfg = yaml::read_yaml(Args$config)
cfg$bb = cofbb::get_bb(cfg$region)
charlier::start_logger(copernicus::copernicus_path("log"))
date = as.Date(Args$date)
if (!interactive()){
  ok = main(date, cfg )
  charlier::info("fetch_forecast: done")
  quit(save = "no", status = ok)
} 
