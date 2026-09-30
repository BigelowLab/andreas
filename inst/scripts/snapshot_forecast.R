# usage: snapshot_forecast.R [--] [--help] [--config CONFIG] [--date DATE] [--outpath OUTPATH]
# 
# Gather snapshots of a given region-product
# 
# flags:
#   -h, --help     show this help message and exit
# 
# optional arguments:
#   -c, --config   configuration file 
#   [default: /mnt/s1/projects/ecocast/coredata/copernicus/config/chfc-GLOBAL_ANALYSISFORECAST_PHY_001_024.yaml]
#   -d, --date     date to snapshot [default: 2026-09-30]
#   -o, --outpath  output data path [default: /mnt/s1/projects/ecocast/coredata/copernicus/snapshot]




# Use this script to take a snapshot of the forecast - storing it to a
# directory named for the forecast date.  For instance, taking a snapshot on
# Sep 25, 2026 will store forecast for the 25th to 25th + N days.  Repeating the
# snap shot for Sep 26, 2026 will store forecasts 26th to 26th + N day, etc.  N 
# set by the user.

suppressPackageStartupMessages({
  library(andreas)
  library(dplyr)
  library(rlang)
  library(stars)
  library(charlier)
  library(argparser)
})

Args = argparser::arg_parser("Gather snapshots of a given region-product",
                             name = "snapshot_forecast.R", 
                             hide.opts = TRUE) |>
  add_argument("--config",
               help = 'configuration file',
               default = copernicus_path("config", "chfc-GLOBAL_ANALYSISFORECAST_PHY_001_024.yaml")) |>
  add_argument("--date",
               help = "date to snapshot",
               default = Sys.Date() |> format("%Y-%m-%d")) |>
  add_argument("--outpath",
               help = "output data path",
               default = copernicus_path("snapshot")) |>
  parse_args()

cfg = yaml::read_yaml(Args$config)
charlier::start_logger(file.path(Args$outpath, "log"))
charlier::info("snapshot of %s-%s on %s", cfg$region, cfg$product, Args$date)

outpath = andreas::make_path(file.path(Args$outpath, cfg$region, cfg$product, Args$date))

inpath = andreas::copernicus_path(cfg$region, cfg$product)
DB = andreas::read_database(inpath) |>
  dplyr::filter(date >= as.Date(Args$date),
                treatment == "raw") |>
  andreas::write_database(outpath)

infiles = andreas::compose_filename(DB, inpath)
ok = file.copy(infiles, outpath, overwrite = TRUE)
charlier::info("  %i files copied", length(infiles))
charlier::info("  complete")
