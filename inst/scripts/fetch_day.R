# Fetch one day of Copernicus global-analysis-forecast-phy-001-024 data
# for the nwa region
#
# usage: fetch_day.R [--] [--help] [--date DATE] [--config CONFIG]
# 
# Fetch copernicus data for one day
# 
# flags:
#   -h, --help    show this help message and exit
# 
# optional arguments:
#   -d, --date    the date of the forecast [default: 2025-03-11]
#   -c, --config  configuration file [default:
#                    /mnt/s1/projects/ecocast/corecode/R/copernicus/inst/config/nwa_daily_fetch_surface.yaml]


suppressPackageStartupMessages({
  library(copernicus)
  library(stars)
  library(dplyr)
  library(charlier)
  library(argparser)
  library(cofbb)
  library(yaml)
})


main = function(date = Sys.Date()-1, cfg = NULL){
  
  if (!inherits(date, "Date")) date = as.Date(date)
  
  P = copernicus::product_lut(cfg$product) |>
    dplyr::filter(fetch == "yes") |>
    dplyr::group_by(product_id)
  
  data_path = out_path <- copernicus::copernicus_path(cfg$region, cfg$product)
  # reg/prod/yyyy/mmdd/datasetid__time_depth_period_var_treatment.ext
  out_path <- copernicus::copernicus_path(cfg$region, 
                                          cfg$product, 
                                          format(date, "%Y"),
                                          format(date, "%m%d"))
  ss = P |>
        fetch_product_by_day(x = date, bb = cfg$bb)

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
  
  
  charlier::info("fetch_forecast:updating database")
  db <- unlist(ff) |>
    copernicus::decompose_filename() |>
    copernicus::append_database(data_path)
  return(0)
}

Args = argparser::arg_parser("Fetch copernicus data for one day",
                             name = "fetch_day.R", 
                             hide.opts = TRUE) |>
  add_argument("--date",
               help = "the date of the forecast",
               default = format(Sys.Date(), "%Y-%m-%d"),
               type = "charcacter") |>
  add_argument("--config",
               help = 'configuration file',
               default = copernicus_path("config","fetch-day-GLOBAL_MULTIYEAR_PHY_001_030.yaml", package = "copernicus")) |>
  parse_args()


cfg = yaml::read_yaml(Args$config)
cfg$bb = cofbb::get_bb(cfg$region)
charlier::start_logger(copernicus::copernicus_path("log"))
date = as.Date(Args$date)
if (!interactive()){
  ok = main(as.Date(Args$date), cfg )
  charlier::info("fetch_day: done")
  quit(save = "no", status = ok)
} else {
  date = as.Date(Args$date)
}



