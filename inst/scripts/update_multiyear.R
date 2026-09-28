# usage: update_multiyear.R [--] [--help] [--config CONFIG]
# 
# Fetch copernicus multiyear data
# 
# flags:
#   -h, --help    show this help message and exit
# 
# optional arguments:
#   -c, --config  configuration file [default: /mnt/s1/projects/ecocast/coredata/copernicus/config/jordanbasin-GLOBAL_MULTIYEAR_PHY_001_030.yaml]

# Fetch "new" multiyear data per dataset-per-variable-depth
#
# read the catalog-LUT
# read the multiyear DB
# for each group (dataset, depth)
#   compare latest DB records to catalog-LUT availability
#   if (new available) fetch and add to DB


suppressPackageStartupMessages({
  library(andreas)
  library(stars)
  library(dplyr)
  library(charlier)
  library(argparser)
  library(cofbb)
})


#' Fetch variable for one dataset_id+depth group (multiple vars ok)
#' @param tbl one or more rows of product lut for one dataset
#' @param key likely empty tibble
#' @param out_path the output path
#' @return a database table 
fetch_dataset = function(p, key, out_path = NULL, cfg = NULL, DB = NULL){
  
  charlier::info("update_multiyear: %s %s at depth %s", 
                 p$dataset_id[1], 
                 paste(p$name, collapse = ", "),
                 p$depth[1])
  # first we get the range of times the caralog says are available
  # then we check to see when the last dates are for the existing DB
  last_available = max(as.Date(p$end_time))
  depth_range = c(min(p$mindepth), max(p$maxdepth))
  
  db = DB |>
    dplyr::filter(.data$id %in% p$dataset_id[1],
                  .data$variable %in% p$short_name)
  last_have = max(db$date)
  
  if (last_available <= last_have){
    charlier::info("  up to date, returning empty set")
    return(dplyr::slice(db, 0))
  }
  
  dates = seq(from = last_have + 1, 
              to = last_available, 
              by = "day")
  
  # here we get a list for each dataset-depth group
  x = fetch_andreas(p,
                    time = range(dates),
                    bb = cfg$bb, 
                    drop_depth = FALSE,
                    verbose = DEVMODE)[[1]]
  ndim = dim(x) |>
    dplyr::set_names(names(x))
  dimx = stars::st_dimensions(x)
  andreas = attr(x, "andreas")
  names(x) <- p$name
  period = copernicus::dataset_period(p$dataset_id[1])
  treatment = "raw"
  d = stars::st_dimensions(x)
  time = andreas[["time"]] |> as.Date()
  depths = if(p$depth[1] == "var"){
      andreas[['depth']] |>
        units::drop_units() |>
        andreas::format_depth_level()
    } else {
      p$depth[1]
    }
  
  # manufacture the output
  lapply(seq_along(time),
    function(i){
      if (p$depth[1] == "var"){
        ndepth = length(depths) * length(x)
        dplyr::tibble(id = rep(p$dataset_id[1], ndepth),
                  date = rep(time[i], ndepth),
                  time = rep("000000", ndepth),
                  depth = rep(depths, length.out = ndepth),
                  period = rep(period, ndepth),
                  variable = rep(p$short_name, each = length(depths)), # all of the names
                  treatment = rep("raw",ndepth),
                  .name = paste(variable, depth, sep = "_"),
                  .time = i)
        
      } else {
        dplyr::tibble(id = p$dataset_id[1],
                      date = time[i],
                      time = "000000",
                      depth = tbl$depth[1],
                      period = period,
                      variable = p$short_name, # all of the names
                      treatment = "raw",
                      .name = paste(variable, depth, sep = "_"),
                      .time = i)
      }
    }) |>
    dplyr::bind_rows() |>
    dplyr::mutate(.filename = andreas::compose_filename(.data, out_path),
                  .depth = match(.data$depth, depths)) |>
    dplyr::rowwise() |>
    dplyr::group_map(
      function(row, key){
        s = if (c("depth", "time") %in% names(dimx)) {
          dplyr::slice(x[row$variable[1]], "time", row$.time[1]) |>
          dplyr::slice("depth", row$.depth) |>
          stars::write_stars(row$.filename[1])
        } else if ("time" %in% names(dimx)){
          dplyr::slice(x[row$variable[1]], "time", row$.time[1]) |>
            stars::write_stars(row$.filename[1])
        } else if ("depth" %in% names(dimx)){
          dplyr::slice(x[row$variable[1]], "depth", row$.depth[1]) |>
            stars::write_stars(row$.filename[1])
        } else {
          x[row$variable[1]] |>
            stars::write_stars(row$.filename[1])
        }
        row
      }, .keep = TRUE) |>
    dplyr::bind_rows() |>
    dplyr::select(-dplyr::any_of(c(".time", ".filename", ".depth")))

}


# main iterates over each datset_id+depth group, accumulates
# a database of new data, and adds it to the existing.
main = function(cfg = NULL){
  
  
  P = andreas::read_product_lut(region = cfg$region,
                                product_id = cfg$product) |>
    dplyr::filter(fetch == "yes") |>
    group_by(dataset_id, depth) 
  
  out_path <- copernicus::copernicus_path(cfg$region, cfg$product)
  
  DB = andreas::read_database(out_path)
  
  if (FALSE){
    p = group_split(P)[[3]]
  }
  db = P |>
    group_map(fetch_dataset, 
              out_path = out_path, 
              cfg = cfg, 
              DB = DB,
              .keep = TRUE) |>
    dplyr::bind_rows() |>
    andreas::append_database(out_path)
  
  
  return(0)
}

DEVMODE = interactive()

Args = argparser::arg_parser("Fetch copernicus multiyear data",
                             name = "fetch_multiyear.R", 
                             hide.opts = TRUE) |>
  add_argument("--config",
               help = 'configuration file or all to update all active databases',
               default = copernicus_path("config", "jordanbasin-GLOBAL_MULTIYEAR_PHY_001_030.yaml")) |>
  parse_args()

charlier::start_logger(copernicus::copernicus_path("log-multiyear"))

if (Args$config == "all"){
  cfgs = active_databases(include = "path") |>
    dplyr::filter(period == "my") |>
    dplyr::rowwise() |>
    dplyr::group_map(
      function(row){
        filename = andreas::copernicus_path("config",
                                            sprintf("%s-%s.yaml", row$region, row$product_id))
        cfg = yaml::read_yaml(filename)
        cfg$bb = cofbb::get_bb(cfg$region)
        cfg$.name = basename(filename)
        cfg
      }
    )
} else {
  cfg = yaml::read_yaml(Args$config)
  cfg$bb = cofbb::get_bb(cfg$region)
  cfg$.name = basename(Args$config)
  cfgs = list(cfg)
}


if (!interactive()){
  for (cfg in cfgs){
    charlier::info("update_multiyear: %s %s", cfg$region, cfg$product)
    ok = main(cfg)
    charlier::info("update_multiyear: done for %s %s", cfg$region, cfg$product)
  }
  charlier::info("update_multiyear: completed")
  quit(save = "no", status = ok)
} 
