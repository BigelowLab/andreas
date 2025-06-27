#' Read static variables for a regional dataset, possibly subsetting with a bounding box
#' 
#' *N.B.* When combining these with other Copernicus layers using `c()` you may 
#' encounter a message that says R doesn't know how to combine them.  Just increase
#' the `tolerance` value to `1e-6` and all will be well.  I can't explain why this
#' mismatch occurs, but this is the solution.
#' 
#' @export
#' @param name chr, the (names) of the static variables to read
#' @param path chr, the copernicus data path
#' @param bb bounding box as a numeric vector, st_bbox, or an object from which a 
#'   bounding box can be derived. NULL to skip.
#' @return stars object
read_static = function(name = c("deptho", "mask"),
                       path = copernicus::copernicus_path("gom", "GLOBAL_ANALYSISFORECAST_PHY_001_024"),
                       bb = NULL){
  
  file = file.path(path, "static", paste0(name, ".tif"))
  ok = sapply(file, file.exists)
  if (!all(ok)){
    for (nm in names(ok)){
      if (!ok[[1]]) message("file not found: ", nm)
    }
    stop("unable to read file(s)")
  }
  x = stars::read_stars(file) |>
    sf::st_set_crs(4326) |>
    rlang::set_names(name)
  if (!is.null(bb)){
    orig_s2 = sf::sf_use_s2(FALSE)
    x = sf::st_crop(x, sf::st_bbox(bb))
    dummy = sf::sf_use_s2(orig_s2)
  }
  x
}

#' Read global static variables, possibly subsetting with a bounding box
#' 
#' @export
#' @param name chr, the (names) of the static variables to read
#' @param product chr, the copernicus product_id
#' @param bb bounding box as a numeric vector, st_bbox, or an object from which a 
#'   bounding box can be derived. NULL to skip.
#' @return stars object
read_global_static = function(name = c("deptho", "mask"),
                       product = "GLOBAL_ANALYSISFORECAST_PHY_001_024",
                       bb = NULL){
  path = copernicus_path("static", product[1])
  file = file.path(path, paste0(name, ".tif"))
  ok = sapply(file, file.exists)
  if (!all(ok)){
    for (nm in names(ok)){
      if (!ok[[1]]) message("file not found: ", nm)
    }
    stop("unable to read file(s)")
  }
  x = stars::read_stars(file) |>
    sf::st_set_crs(4326) |>
    rlang::set_names(name)
  if (!is.null(bb)){
    orig_s2 = sf::sf_use_s2(FALSE)
    x = sf::st_crop(x, sf::st_bbox(bb))
    dummy = sf::sf_use_s2(orig_s2)
  }
  x
}


#' A convenience function that subsets (or copies) the primary static arrays
#' (in `copernicus_path("static")`) to region specified.
#' 
#' @export
#' @param name chr, one or more static variable names
#' @param region chr one or more destination region name
#' @param bb bounding box - something from which an st_bbox can be made.
#'   Ignored if region is "world", in which case we essentially copy to world. If 
#'   not provided and the `cofbb` package is available, then retrieve that
#'   dynamically
#' @return NULL invisibly
subset_static_by_region = function(name = c("deptho", "mask"),
                         region = c("world", "gom", "chfc"),
                         bb = NULL){

  db = list_databases(form = "table") |>
    dplyr::group_by(.data$region) |>
    dplyr::group_map(
      function(tbl, grp){
       
        if (tolower(tbl$region[1]) == "world"){
          # here we essentially copy
          for (product in tbl$product_id){
            cat("region:", tbl$region[1], "  product:", product, "\n")
            s = read_global_static(name, product)
            opath = copernicus_path(tbl$region[1], product, "static") |> make_path()
            for (nm in names(s)){
              ofile = file.path(opath, paste0(nm, ".tif"))
              dummy = stars::write_stars(s[nm], ofile)
            }
          }
        } else {
          # but here is a case-by-case
          if (is.null(bb)) {
            # try to get the bbox locally
            if (requireNamespace("cofbb")){
              bb = cofbb::get_bb(tbl$region, form = "sf") |>
                sf::st_set_crs(4326) |>
                sf::st_bbox()
            } else {
              stop("if region is not 'world' then bb must be provided")
            }
          }
          for (product in tbl$product_id){
            cat("region:", tbl$region[1], "  product:", product, "\n")
            s = read_global_static(name, product, bb = bb)
            opath = copernicus_path(tbl$region[1], product, "static") |> make_path()
            for (nm in names(s)){
              ofile = file.path(opath, paste0(nm, ".tif"))
              dummy = stars::write_stars(s[nm], ofile)
            } # nm loop
          } # product loop
          
        } # world?
      }, .keep = TRUE)
  return(invisible(NULL))
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