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
#' @param regions chr one or more destination region name
#' @param bb bounding box - something from which an st_bbox can be made.
#'   Ignored if region is "world", in which case we essentially copy to world. If 
#'   not provided and the `cofbb` package is available, then retrieve that
#'   dynamically
#' @return NULL invisibly
subset_static_by_region = function(name = c("deptho", "mask", "deptho_lev"),
                         regions = c("world", "nwa", "gom", "chfc"),
                         bb = NULL){

  db = list_databases(form = "table") |>
    dplyr::filter(.data$region %in% regions) |>
    dplyr::rowwise() |>
    dplyr::group_map(
      function(tbl, grp){
        cat("region: ", tbl$region, " product: ", tbl$product_id)
        if (tolower(tbl$region) == "world"){
          # here we essentially copy
          for (nm in name){
            opath = copernicus_path(tbl$region, tbl$product_id, "static") |> make_path()
            ofile = file.path(opath, paste0(nm, ".tif"))
            s = read_global_static(nm, tbl$product_id) |>
              stars::write_stars(ofile)
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
          for (nm in name){
            opath = copernicus_path(tbl$region, tbl$product_id, "static") |> make_path()
            ofile = file.path(opath, paste0(nm, ".tif"))
            cat("writing ", ofile, "\n")
            s = read_global_static(nm, tbl$product_id, bb = bb) |>
               stars::write_stars(ofile)
          } # nm loop

          
        } # world?
      })
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


#' Given a depth raster, compute a various [terrain metrics](https://rspatial.github.io/terra/reference/terrain.html)
#' 
#' The [terra package](https://CRAN.R-project.org/package=terra) is required.
#' 
#' @export
#' @param x a stars object that is a deptho or some bathymetry. Or, alternatively, 
#'   it is the path to a particular product suite, such as 
#'   `copernicus_path("chfc", "GLOBAL_MULTIYEAR_PHY_001_030")`.  In this case we 
#'   handle reading and writing the files for you
#' @param v character one of slope, aspect, TPI  (default), TRI, TRIriley, 
#'     TRIrmsd, roughness, flowdir
#' @param ... other arguments for [terra::terrain]
#' @return a stars object as a terrain metric
make_static_terrain = function(x = copernicus_path(list_databases())[[1]], 
                               v = "TPI",
                               ...){
  if (!requireNamespace("terra")) stop("please install the terra package first")
  require(terra)
  y = if (inherits(x, "stars")){
    as(x, "SpatRaster") |>
      terra::terrain(v = v, ...) |>
      stars::st_as_stars()
  } else {
    filename = file.path(x, "static", "deptho.tif")
    if (!file.exists(filename)) stop("deptho file not found:", filename)
    ofile = file.path(x, "static", paste0(v, ".tif"))
    terra::rast(filename) |>
      terra::terrain(v = v, ...) |>
      stars::st_as_stars() |>
      stars::write_stars(ofile)
  }
  y
}


#' Given a mask raster, compute a [raster-lut](https://github.com/BigelowLab/twinkle/blob/eae4a99bafbe5cc81ecfc8ddfb3d9f89c04f010e/R/stars.R#L160)
#' 
#' The [twinkle package](https://github.com/BigelowLab/twinkle) is required.
#' @export
#' @param x a stars object that is a mask (0/1) Or, alternatively, 
#'   it is the path to a particular product suite, such as 
#'   `copernicus_path("chfc", "GLOBAL_MULTIYEAR_PHY_001_030")`.  In this case we 
#'   handle reading and writing the files for you
#' @param ... other arguments for [twinkle::make_raster_lut].  Ignored is `x`
#'   is a path rather than stars object.
#' @return a stars object as a LUT
make_static_lut = function(x = copernicus_path(list_databases())[[1]], 
                           ...){
  if (!requireNamespace("twinkle")) stop("please install the twinkle package first")
  
  y = if (inherits(x, "stars")){
    twinkle::make_raster_lut(x, ...)
  } else {
    filename = file.path(x, "static", "mask.tif")
    if (!file.exists(filename)) stop("mask file not found:", filename)
    read_static("mask", x) |>
      twinkle::make_raster_lut(mask_value = 0) |>
      stars::write_stars(file.path(x, "static", "lut.tif"))
  }
  y
}

#' Map land-based points to water.
#' 
#' Given a set of points and a look up table, compute new locations over water
#' for any input points located over land.  Input points over water are unchanged.
#' 
#' @export
#' @param x sf object of points
#' @param lut stars object as per `read_static("lut", path)`
#' @param sf object with new geometry
#' @return sf object of points with new locations (where appropriate)
remap_to_water_pixel = function(x = read_buoys(), 
                               lut = read_static("lut")){
  
  old_index = stars::st_cells(lut, x)  # index into the LUT
  new_index = lut[[1]][old_index]      # the new index the LUT points to
  ix = new_index != old_index          # where they arew "new" (relocated)
  newpoints = twinkle::stars_index_to_loc(new_index[ix], lut, form = "sf")
  st_geometry(x[ix,]) <- st_geometry(newpoints)
  x
}



