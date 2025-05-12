#' Read a product LUT used for searches and merge with a product description
#' 
#' @export
#' @param product_id chr, the product identifier.  The associated file must exist
#' @param description table, the table produced by [copernicus::read_product_description()] for
#'   the specified `product_id`
#' @return a table with the following
#' * product_id
#' * dataset_id
#' * dataset_name
#' * name chr short_name converted to camelCase (used for file View(xstorage)
#' * short_name
#' * standard_name
#' * units 
#' * depth really a depth name "sur", "bot", "mld" or NA but could be a number (as character())
#' * fetch   "yes", "no" or NA
#' * mindepth for `copernicusmarine` CLI request
#' * maxdepth for `copernicusmarine` CLI request
#' * start_time known data service availability
#' * end_time known data service availability
read_product_lut = function(product_id = 'GLOBAL_ANALYSISFORECAST_PHY_001_024',
                            description = copernicus::read_product_description(product_id)){
  
  filename = copernicus_path("lut", paste0(product_id[1], ".csv"))
  if(!file.exists(filename)) stop("product lut file not found: ", filename)
  lut = readr::read_csv(filename, show_col_types = FALSE) |> 
    dplyr::mutate(name = snakecase::to_lower_camel_case(.data$short_name),
                  .before = dplyr::all_of("short_name"))
  
  lut |>
    dplyr::select(-dplyr::all_of(c("product_id", "title"))) |>
    dplyr::full_join(description |>
                       dplyr::select(-dplyr::all_of(c("time_step", "min_depth", "max_depth", "units",
                                                      "standard_name"))) |>
                       dplyr::filter(.data$dataset_id %in% lut$dataset_id & .data$short_name %in% lut$short_name), 
                     by = c("dataset_id", "short_name")) |>
    dplyr::mutate(product_id = product_id[1], .before = 1)
  
}

#  Read a product LUT
#  
#  @export
#  @param product_id char, the product identifier
#  @param add_meta logical, if TRUE merge with `dataset_metadata`
#  @return tibble
# product_lut = function(product_id = 'GLOBAL_ANALYSISFORECAST_PHY_001_024',
#                        add_meta = TRUE){
#   
#   filename = copernicus_path("lut", paste0(product_id[1], ".csv"))
#   x = readr::read_csv(filename, show_col_types = FALSE) 
#   if ("short_name" %in% colnames(x)){
#     ix = x$short_name == "sea_surface_temperature_anomaly"
#     x$short_name[ix] = "sstanom"
#   }
#   
#   if (add_meta && ("short_name" %in% colnames(x))){
#     m = product_lut("dataset_metadata") |>
#       dplyr::select(-dplyr::all_of("product_id"))
#     x = x |>
#       dplyr::left_join(m, by = "dataset_id")
#   }
#   
#   x
# }

#  #' Create the table of dataset metadata
#  #' 
#  #' @export
#  #' @param x catalog (flattened or unflattened)
#  #' @return table narrowed and depduplictaed
#  create_dataset_metadata = function(x = read_product_catalog()){
#    x |> 
#      dplyr::select(dplyr::all_of(c("product_id", "dataset_id"))) |>
#      dplyr::distinct() |>
#      dplyr::mutate(start_date = NA,
#                    end_date = NA)
#  }

#' Generate a LUT suitable for the package.
#' 
#' If you chose to save (see `save_lut`) then be advised existing an existing
#' lut will be overwritten.
#' 
#' @export
#' @param x chr the name of the product
#' @param catalog table of products (unflattened)
#' @param save_lut log, if TRUE save to CSV format in `inst/lut`
#' @return a table of look up values.  You'll edit this file to decide which to fetch
#'  and what depths to fetch from.  Here's what is in the output table
#'  
#' * product_id
#' * title
#' * dataset_id
#' * dataset_name
#' * short_name
#' * standard_name
#' * units
#' * depth "sur"
#' * fetch  "no"
#' * mindepth 0
#' * maxdepth 1
create_lut <- function(x = "GLOBAL_ANALYSISFORECAST_BGC_001_028",
                       catalog = read_product_catalog(),
                       save_lut = FALSE){
  
  lut = catalog |>
    dplyr::filter(product_id == x[1]) |>
    flatten_product() |>
    dplyr::mutate(depth = "sur", 
                  fetch = "no",
                  mindepth = 0,
                  maxdepth = 1)
  if (save_lut) readr::write_csv(lut, copernicus_path("lut", paste0(x,".csv")))
  lut
}

# Read a regional LUT ala ("nwa_lut.csv")
# 
# @export
# @param region char the name of the region
# @param path char the path to the LUT
# @return tibble with "name", "longname" and "units"
# read_region_lut <- function(region = "nwa",
#                             path = copernicus_path("lut")){
#   
#   filename = file.path(path, paste0(region[1], "_lut.csv"))
#   if (!file.exists(filename)) stop("file not found:", basename(filename))
#   readr::read_csv(filename, col_types = "cccc")
# }



# Merge one or more LUTs together
# 
# @export
# @param service char the name of the service
# @param path char, the path to the LUTs
# @return tibble or combined product luts for this service
#   with "name", "longname" and "units"
# read_service_lut <- function(service = "global_analysisforecast_phy_001_024",
#                              path = copernicus_path("lut")){
#   fullpath = file.path(path, service[1])
#   ff <- list.files(fullpath, full.names = TRUE)
#   if (length(ff) == 0) stop("no files found in path:", fullpath )  
#   lapply(ff,
#     function(f){
#       readr::read_csv(f, col_types = "ccc") |>
#         dplyr::mutate(product_id = basename(f), .before = 1)
#       }) |>
#     dplyr::bind_rows()
# }
