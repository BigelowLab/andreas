# usage: copy_monthly_climatology.R [--] [--help] [--source SOURCE]
#        [--dest DEST] [--path PATH]
# 
# Copy (and crop) copernicus monthly climatology from world to a region
# 
# flags:
#   -h, --help    show this help message and exit
# 
# optional arguments:
#   -s, --source  configuration file for source [default:
#                 world-GLOBAL_MULTIYEAR_PHY_001_030.yaml]
#   -d, --dest    configuration file for destination [default:
#                 chfc-GLOBAL_MULTIYEAR_PHY_001_030.yaml]
#   -p, --path    copernicus data storage path [default:
#                 /mnt/s1/projects/ecocast/coredata/copernicus]

suppressPackageStartupMessages({
  library(andreas)
  library(charlier)
  library(dplyr)
  library(stars)
  library(sf)
  library(argparser)
})


main = function(scfg, dcfg){
  
  
  sDB = andreas::read_database(scfg$path) |>
    dplyr::filter(period == "month-clim") 
  
  dDB = andreas::read_database(dcfg$path)
  
  template = andreas::read_andreas(dDB |> dplyr::slice(1), dcfg$path)
  
  orig = sf_use_s2(FALSE)
  newDB = sDB |>
    dplyr::rowwise() |>
    dplyr::group_map(
      function(row, key){
        s = andreas::read_andreas(row, scfg$path) |>
          sf::st_crop(template, normalize = TRUE) |>
          andreas::write_andreas(row, dcfg$path)
        row
      }
    ) |>
    dplyr::bind_rows() |>
    andreas::append_database(dcfg$path)
  sf_use_s2(orig)
  newDB
}


DEVMODE = interactive()
Args = argparser::arg_parser("Copy (and crop) copernicus monthly climatology from world to a region",
                             name = "copy_monthly_climatology.R", 
                             hide.opts = TRUE) |>
  add_argument("--source",
               help = 'configuration file for source',
               default =  "world-GLOBAL_MULTIYEAR_PHY_001_030.yaml") |>
  add_argument("--dest",
               help = 'configuration file for destination',
               default = "chfc-GLOBAL_MULTIYEAR_PHY_001_030.yaml") |>
  add_argument("--path",
               help = "copernicus data storage path",
               default = andreas::copernicus_path()) |>
  parse_args()


cfg_path = file.path(Args$path, "config")
scfg = charlier::read_config(file.path(cfg_path, Args$source))
scfg$path = andreas::copernicus_path(scfg$region, scfg$product)
dcfg = charlier::read_config(file.path(cfg_path, Args$dest))
dcfg$path = andreas::copernicus_path(dcfg$region, dcfg$product)
if (!DEVMODE){
  ok = main(scfg, dcfg)
}