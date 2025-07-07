#' Crop and warp a raster to match a template
#' 
#' @export
#' @param x stars object to be cropped and warped 
#' @param y stars object serves as template
#' @param ... other arguments for [stars::st_crop] and [stars::st_warp]
#' @return stars object
st_match = function(x, y, ...){
  x |>
    stars::st_crop(y, ...) |>
    stars::st_warp(y, ...)
}