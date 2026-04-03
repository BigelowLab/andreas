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

backfill_missing = function(x, DB = NULL, path = NULL, cfg = NULL, LUT = NULL){
  
  # `x` has the .name and period we need to filter, 
  # it also has the missing dates
 
  
  
  db = DB |>
    dplyr::filter(.data$id == x$id,
                  .data$period == x$period,
                  .data$.name == x$.name)
  lut = LUT |>
    dplyr::filter(dataset_id == x$id,    # are these enough to 
                  depth == x$depth,      # match the catalog to the  
                  short_name == x$variable)  # internal database?
  
  db = lapply(seq_along(x$dates[[1]]),
    function(idate){
      date = x$dates[[1]][idate]
      
      s = fetch_andreas(lut,
                        time = date,
                        bb = cfg$bb)[[1]]
      andreas = attr(s, "andreas")
      period = copernicus::dataset_period(lut$dataset_id[1])
      treatment = "raw"
      d = stars::st_dimensions(s)
      time = andreas$time |> format("%Y-%m-%dT000000")
      nm = lut$short_name
      fname = sprintf("%s__%s_%s_%s_%s_%s.tif", 
                      lut$dataset_id, 
                      time, 
                      lut$depth, 
                      period, 
                      nm, 
                      treatment)
      db = andreas::decompose_filename(fname)
      ofile = andreas::compose_filename(db, path)
      filepath = andreas::make_path(dirname(ofile))
      charlier::info("saving %s", basename(ofile))
      s = stars::write_stars(s, ofile)
      db
    }) |>
  dplyr::bind_rows()
  
  db
}

main = function(DB, path, cfg, LUT){
  
  newdb = andreas::tabulate_missing(DB) |> 
    dplyr::filter(nmiss > 0) |>
    dplyr::rowwise() |>
    dplyr::group_map(
      function(row, key){
        backfill_missing(row, DB = DB,  path = path, cfg = cfg, LUT = LUT) 
      }) |>
    dplyr::bind_rows() |>
    andreas::append_database(path)
  newdb
}


Args = argparser::arg_parser("Backfill copernicus data",
                             name = "backfill_days.R", 
                             hide.opts = TRUE) |>
  add_argument("--config",
               help = 'configuration file',
               default = copernicus_path("config", 
                                         "chfc-GLOBAL_ANALYSISFORECAST_PHY_001_024.yaml")) |>
  parse_args()

cfg = yaml::read_yaml(Args$config)
cfg$bb = cofbb::get_bb(cfg$region)
charlier::start_logger(copernicus_path(cfg$reg, cfg$product, "log"))
charlier::info("backfill_missing for %s/%s", cfg$reg,cfg$product)

path = andreas::copernicus_path(cfg$reg, cfg$product)
DB = andreas::read_database(path)
LUT = andreas::read_product_lut(cfg$product)

newdb = main(DB, path, cfg, LUT )
