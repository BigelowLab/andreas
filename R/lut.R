#' List the available LUTS
#' 
#' @export
#' @param path chr, the path to the LUT directory
#' @return character vector of one of more LUT names
list_luts = function(path = copernicus::copernicus_path("lut")){
  ff = list.files(path, pattern = "^.*\\.csv$")
  ff = sub(".csv", "", ff, fixed = TRUE)
  ix = grepl("[[:upper:]]", ff)
  ff[ix]
}

#' Read a product LUT used for searches and merge with a product description
#' 
#' @export
#' @param region chr, the regional name
#' @param product_id chr, the product identifier.  The associated file must exist
#' @param description table, the table produced by [nicolaus::read_catalog] for
#'   the specified `product_id`
#' @return a table with at least the following
#' * product_id
#' * dataset_id
#' * dataset_name
#' * name chr short_name converted to camelCase (used for file View(x))
#' * short_name
#' * standard_name
#' * units 
#' * depth really a depth name "sur", "bot", "mld", "zos" or NA but could be a number (as character())
#' * fetch   "yes", "no" or NA
#' * mindepth for `copernicusmarine` CLI request
#' * maxdepth for `copernicusmarine` CLI request
#' * start_time known data service availability
#' * end_time known data service availability
#' * time_step in seconds
#' * period "day", "month", etc
#' * min_depth minimum available depth
#' * max_depth maximum available depth
#' * n_depth number of available depth layers
read_product_lut = function(region = "chfc",
                            product_id = 'GLOBAL_ANALYSISFORECAST_PHY_001_024',
                            description = nicolaus::read_catalog()){
  
  if (FALSE){
    region = "chfc"
    product_id = 'GLOBAL_ANALYSISFORECAST_PHY_001_024'
    description = nicolaus::read_catalog()
  }
  filename = copernicus::copernicus_path("lut", 
            sprintf("%s-%s.csv", region[1], product_id[1]))
  if(!file.exists(filename)) stop("product lut file not found: ", filename)
  lut = readr::read_csv(filename, show_col_types = FALSE) |> 
    dplyr::mutate(name = snakecase::to_lower_camel_case(.data$short_name),
                  .before = dplyr::all_of("short_name")) |>
    dplyr::mutate(period = dataset_period(.data$dataset_id),
                  .after = dplyr::all_of("n_depth")) |>
    # here we update start_time and end_time with those from the description (catalog)
    # which is updated daily
    dplyr::select(-dplyr::all_of(c("start_time","end_time"))) |>
    dplyr::left_join(description |>
                       dplyr::select(dplyr::any_of(c("product_id",
                                                       "dataset_id",
                                                       "standard_name",
                                                       "start_time",
                                                       "end_time"))),
                     by = c(c("product_id",
                              "dataset_id",
                              "standard_name"))) |>
    dplyr::relocate(dplyr::all_of(c("start_time","end_time")),
                    .before = dplyr::all_of("time_step"))
  
  lut
}





#' Generate a LUT suitable for the package.
#' 
#' If you chose to save (see `save_lut`) then be advised existing an existing
#' lut will be overwritten.
#' 
#' @export
#' @param product chr the name of the product
#' @param region chr, the name of the region
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
create_lut <- function(product = "GLOBAL_ANALYSISFORECAST_BGC_001_028",
                       region = "nowhere",
                       catalog = nicolaus::read_catalog(),
                       save_lut = FALSE){
  
  lut = catalog |>
    dplyr::filter(product_id == product[1]) |>
    #flatten_product() |>
    dplyr::mutate(depth = "sur", 
                  fetch = "no",
                  mindepth = 0,
                  maxdepth = 1)
  if (save_lut) readr::write_csv(lut, 
                                 copernicus_path("lut", sprintf("%s-%s.csv",region,product)))
  lut
}



