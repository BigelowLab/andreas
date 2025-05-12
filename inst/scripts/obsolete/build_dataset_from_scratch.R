suppressPackageStartupMessages({
  library(copernicus)
  library(stars)
  library(dplyr)
})


#' Build a dataset from scratch
#' 
#' @param 




#' Build the database from scratch - this is a one off developed spring 2023
#' as a result of data reorganization of the global_analysisforecast_phy_001_024
#' dataset at CMEMS
#' 
#' Use the regionLUT and serviceLUT to define the required sources 
#' For each source
#'    fetch, save layers (pay attention to 'sur' vs 'bot')
#'    return db
#' bind all db and save
#' 
#' @param service char, what service are we mining?
#' @param region char, for what region?
#' @param bb numeric, bounding box as [left, right, bottom, top]
rebuild_dataset_v0.1 <- function(service = 'global_analysisforecast_phy_001_024',
                            region = 'nwa',
                            bb = cofbb::get_bb("nwa2")){
  
  opath = copernicus_path(service)

  fetch_products = function(tbl, key, bb = c(-180, 180, -80, 90)){
    cat(key$product_id, "\n")
    nc <- open_nc(product_id = key$product_id)
    on.exit(ncdf4::nc_close(nc))
    
    times <- get_time(nc)
    depths <- get_depth(nc)
    
    db <- sapply(seq_along(times),
        function(i){         
          year = format(times[i], "%Y")
          mmdd = format(times[i], "%m%d")
          ymd = format(times[i], "%Y-%m-%d")
          if (mmdd == "0101") cat("Starting year:", year, "\n")
          this_depth = depths[1]
          S <- get_var(nc,
                       var = tbl$name,
                       bb = bb,
                       time = times[i],
                       depth = this_depth,
                       banded = FALSE,
                       form = c("array", "stars")[2])
          
          newpath = file.path(opath, year, mmdd)
          if (!dir.exists(newpath)) ok = dir.create(newpath, recursive = TRUE)
          filenames = file.path(newpath,
                               sprintf("%s_%s_%s.tif",
                                       ymd,
                                       tbl$name,
                                       tbl$depth))
          #print(S)
          for (j in seq_along(filenames)){
            #cat("write:", filenames[j], "\n")
            stars::write_stars(S[j], filenames[j])
          }
          
          filenames
        }) |>
    unname() |>
    decompose_filename()
  }  # fetch_products
  
  
  
  srv = read_service_lut(service) |>
    dplyr::select(dplyr::all_of(c("name", "product_id")))
  reg = read_region_lut(region)
  lut = dplyr::left_join(reg, srv, by = 'name')
  
  db <- lut |>
    dplyr::group_by(product_id) |>
    dplyr::group_map(fetch_products, bb = bb) |>
    dplyr::bind_rows() |>
    write_database(opath)
  
  cat("done\n")
} # rebuild_dataset
  

