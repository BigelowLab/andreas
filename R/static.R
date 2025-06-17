#' Read static variables, possibly subsetting with a bounding box
#' 
#' @export
#' @param name chr, the (names) of the static variables to read
#' @param path chr, the copernicus data path
#' @param bb bounding box as a numeric vector, st_bbox, or an object from which a 
#'   bounding box can be derived. NULL to skip.
#' @return stars object
read_static = function(name = c("deptho", "mask"),
                       path = copernicus::copernicus_path("nwa", "GLOBAL_ANALYSISFORECAST_PHY_001_024"),
                       bb = NULL){
  file = file.path(path, "static", paste0(name, ".tif"))
  ok = sapply(file, file.exists)
  if (!all(ok)){
    for (nm in names(ok)){
      if (!ok[[1]]) message("file not found: ", ok[nm])
    }
    stop("unable to read file(s)")
  }
  x = stars::read_stars(file) |>
    sf::st_set_crs(4326) |>
    rlang::set_names(name)
  if (!is.null(bb)){
    x = sf::st_crop(x, sf::st_bbox(bb))
  }
  x
}

#' Fetch static data for a product suite
#' 
#' @export
#' @param product_id chr the product id
#' @return a stars object
fetch_static = function(product_id = "GLOBAL_ANALYSISFORECAST_PHY_001_024"){
  
  p = copernicus::read_product_catalog(product_id[1]) |>
    dplyr::filter(grepl("static", .data$dataset_id, ignore.case = TRUE))
  
  x = copernicus::fetch_copernicus(use = "cli", 
                                   bind = TRUE,
                                   verbose = TRUE,
                                   dataset_id = p$dataset_id[1],
                                   vars = p$short_name)
  
}