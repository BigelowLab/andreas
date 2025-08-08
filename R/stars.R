#' Crop and warp a raster to match a template
#' 
#' @export
#' @param x stars object to be cropped and warped 
#' @param y stars object serves as template
#' @param ... other arguments for [stars::st_crop] and [stars::st_warp]
#' @return stars object
st_match = function(x, y, ...){
  x |>
    sf::st_crop(y, ...) |>
    stars::st_warp(y, ...)
}


#' Corrects stars objects with unexpected missing values. Intended for use with Copernicus.
#' Assumes that the stars object uses NA to both indicate masked values, i.e. land, and missing values.
#' @export
#' @param stars_obj stars, stars object up to 3 dimensions to correct
#' @param land_mask stars, the land mask associated with the stars object (same extent and res) where
#'   water pixels are non-zero and land is zero.  Must have the same footprint as `stars_obj`
#' @param replacement_values list, named list of attribute names and a value to sub in for any missing entries.
#' @return stars object with any incorrect missing values replaced w/ replacement value.
#' @examples
#' \dontrun{
#'  path = copernicus_path("chfc/GLOBAL_MULTIYEAR_PHY_001_030")
#'  DB = read_database(path) |>
#'    dplyr::filter(dplyr::between(.data$date, as.Date("2015-03-14"), as.Date("2015-03-16")))
#'  stars_obj =  read_andreas(DB, path)
#'  land_mask = read_static("mask", path)
#'  fixed = correct_andreas(stars_obj, land_mask)
#'  stars_obj
#'  fixed
#' }
correct_andreas <- function(stars_obj, land_mask, 
                            replacement_values = list("mlotst" = 700)) {
  if (!identical(dim(stars_obj)[1:2], dim(land_mask)[1:2])){
    stop("stars_obj and land_mask must have the same x/y dimensions")
  }
  water = land_mask[["mask"]] != 0       # where we "should" have data
  rep_names = names(replacement_values)  # we only consider names provided
  stars_names = names(stars_obj)   
  d = dim(stars_obj)                     # for reforming output of apply iteration
  for (nm in rep_names){
    if (nm %in% stars_names){            # found a match
      if (length(d) <= 2){               # no third dim
        fix_me = is.na(stars_obj[[nm]]) & water     # na over water
        stars_obj[[nm]][fix_me] <- replacement_values[[nm]]  
      } else {                             # yes third dim
        m = apply(stars_obj[[nm]], 3,      # iterate over third dimension
                  function(one){
                    fix_me = is.na(one) & water   # na over water
                    one[fix_me] <- replacement_values[[nm]]
                    one
                  })
        dim(m) <- d    # reform to correct 3d shape
        stars_obj[[nm]] <- m  # insert
      }
    }
  }
  
  # 2025-07-22 Omi Johnson original below sans land_mask
  # na_counts <- sapply(stars_obj, function(x) sum(is.na(x)))
  # # this method does assume that at least half of the attributes aren't missing any data
  # to_correct <- na_counts != median(na_counts)
  # flagged_columns <- names(na_counts[to_correct])
  # 
  # # Return if everything is in order
  # if(length(flagged_columns) == 0) {
  #   return(stars_obj)
  # }
  # 
  # # Check that all columns which are missing values have a specified replacement value
  # accounted <- flagged_columns %in% names(replacement_values)
  # if (!all(accounted)) {
  #   stop(paste("A column with missing values does not have a replacement value specified. Affected columns:",
  #              paste(flagged_columns[!accounted], collapse = ", ")))
  # }
  # 
  # # Which values are supposed to be NAs? i.e. are land mask
  # correct_NAs <- stars_obj |>
  #   pull(which(!to_correct)[[1]]) |> # attribute with no missing values
  #   is.na()
  # 
  # # Replacing missing values
  # for (flag_col in flagged_columns) {
  #   # Columns for target value with NA values which aren't supposed to be NA
  #   is_missing <- is.na(stars_obj[[flag_col]]) & !correct_NAs
  #   
  #   stars_obj[[flag_col]][is_missing] <- replacement_values[[flag_col]]
  # }
  
  return(stars_obj)
}