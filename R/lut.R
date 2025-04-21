#' Read a product LUT
#' 
#' @export
#' @param product_id char, the product identifier
#' @param add_meta logical, if TRUE merge with `dataset_metadata`
#' @return tibble
product_lut = function(product_id = 'GLOBAL_ANALYSISFORECAST_PHY_001_024',
                       add_meta = TRUE){
  
  filename = copernicus_path("lut", paste0(product_id[1], ".csv"))
  x = readr::read_csv(filename, show_col_types = FALSE) 
  if ("short_name" %in% colnames(x)){
    ix = x$short_name == "sea_surface_temperature_anomaly"
    x$short_name[ix] = "sstanom"
  }
  
  if (add_meta && ("short_name" %in% colnames(x))){
    m = product_lut("dataset_metadata") |>
      dplyr::select(-dplyr::all_of("product_id"))
    x = x |>
      dplyr::left_join(m, by = "dataset_id")
  }
  
  x
}

#' Create the table of dataset metadata
#' 
#' @export
#' @param x catalog (flattened or unflattened)
#' @return table narrowed and depduplictaed
create_dataset_metadata = function(x = read_product_catalog()){
  x |> 
    dplyr::select(dplyr::all_of(c("product_id", "dataset_id"))) |>
    dplyr::distinct() |>
    dplyr::mutate(start_date = NA,
                  end_date = NA)
}

#' Generate a LUT suitable for the package
#' 
#' @export
#' @param x chr the name of the product
#' @param catalog table of products (unflattened)
#' @param save_lut log, if TRUE save to CSV format in `inst/lut`
#' @return a table of look up values.  You'll edit this file to decide whihc to fetch
#'  and what depths to fetch from.  
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
