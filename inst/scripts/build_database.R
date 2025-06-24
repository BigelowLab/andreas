# usage: build_database.R [--] [--help] [--config CONFIG]
# 
# Build or augment a copernicus database
# 
# flags:
#   -h, --help    show this help message and exit
# 
# optional arguments:
#   -c, --config  configuration file [default:
#     /mnt/s1/projects/ecocast/coredata/copernicus/config/fetch-day-GLOBAL_MULTIYEAR_BGC_001_029.yaml]
#  This really for one-time usage to seed a database

suppressPackageStartupMessages({
  library(copernicus)
  library(stars)
  library(dplyr)
  library(charlier)
  library(argparser)
  library(cofbb)
  library(yaml)
})


# Given a subset of the product_lut (one dataset_id, one_depth)
# determines the date sequence of coverage, downloads the variables
# saves the images and builds a db
#
# returns a database
fetch_data = function(tbl, key, cfg = NULL, db = NULL){
  
  charlier::info("build_database for dataset %s (%s)", tbl$dataset_id[1], tbl$depth[1])
  template = dplyr::slice(db, 0)
  
  # tbl holds one group of dataset_id, depth
  # we assume each dataset covers one time period (variables share same temporal coverage)
  # so we find that sequence of dates and step through each of those
  period =  copernicus::dataset_period(tbl$dataset_id[1])
  dates = seq(from = min(tbl$start_time),
              to = max(tbl$end_time),
              by = period) |>
    as.Date()
  isnewyear = format(dates, "%m%d") == "0101"
  db = lapply(seq_along(dates),
              function(i){
                if (isnewyear[i]) charlier::info("starting %s", format(dates[i], "%Y"))
                # note we get a list back - but we are interested in just the first (and only)
                # element.
                x = andreas::fetch_andreas(tbl,
                                           bb = cfg$bb,
                                           time = c(dates[i], dates[i]))[[1]]
                # example database
                # id,date,time,depth,period,variable,treatment
                # cmems_mod_glo_phy_my_0.083deg_P1D-m,1993-01-01,000000,bot,day,bottomT,raw
                d = template
                d = dplyr::tibble(id = tbl$dataset_id[1],
                                  date = dates[i],
                                  time = "000000",
                                  depth = tbl$depth[1],
                                  period = period,
                                  variable = tbl$short_name, # all of the names
                                  treatment = "raw")
                for (j in seq_along(x)){
                  dummy = andreas::write_copernicus(x[j], dplyr::slice(d,j), cfg$path)
                }
                d
              }) |>
      dplyr::bind_rows()
  
  db
}

main = function(cfg){
  
  DB = andreas::read_database(cfg$path)
  P = andreas::read_product_lut(cfg$product) 
  db = P |>
    dplyr::filter(fetch == "yes") |>
    dplyr::group_by(dataset_id, depth) |>
    dplyr::group_map(fetch_data, cfg = cfg, db = DB, .keep = TRUE) |>
    dplyr::bind_rows() |>
    andreas::append_database(cfg$path)
  
  return(0)
}

Args = argparser::arg_parser("Build a copernicus database",
                             name = "build_database.R", 
                             hide.opts = TRUE) |>
  add_argument("--config",
               help = 'configuration file',
               default = copernicus_path("config", 
                                         "chfc-GLOBAL_MULTIYEAR_PHY_001_030.yaml")) |>
  parse_args()


cfg = yaml::read_yaml(Args$config)
cfg$bb = cofbb::get_bb(cfg$region)
cfg$path = copernicus::copernicus_path(cfg$reg, cfg$product) |>
  copernicus::make_path()
charlier::start_logger(copernicus_path(cfg$reg, cfg$product, "log"))
charlier::info("build_database for %s", cfg$product)

if (!interactive()){
  ok = main(cfg)
  charlier::info("backfill_days: done")
  quit(save = "no", status = ok)
} else {
  date = as.Date(Args$date)
}


