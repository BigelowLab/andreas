# Coperncius coverage (1993 through forecast) is found within two different 
# product suites: anfc and my/myint.  The question arises, how can we access both seamlessly?  For
# example, say I wanted a timeseries for sea surface height (zos) from 1993 
# through next week?
#
# For a given region, 
#   read each database and add product column,
#   mark the table as "merged"  with attribute or class
#   select the most "X" record - that is on a given date 
#     prefer "MULTIYEAR" to "ANALYSISFORECST" products
#     prefer "my" to "myint" datasets
#   
# The user might select one or more of these records and then read the files.
# We need to catch at `compose_filename()` which will catch the presence of a `product`
# variable or the 'merged' class type.  Other than that there is no change.
#
# Writing from a merged database (class == "merged" or "product" in variables) is
# disallowed.  It would probably work, but why are you doing this?

#' Merge compatible databases for one region
#' 
#' @export
#' @param path chr the path to the regional data repository
#' @param ... other arguments for [read_database]
#' @param rm_dups logical, if TRUE remove duplicates preferentially 
#'   retaining MULTIYEAR over ANALAYSISFORECAST products and chosing
#'   "my" over "myint" datasets on a given date
#' @return a merged database
merge_database = function(path = copernicus_path("chfc"), 
                          rm_dups = TRUE, 
                          ...){
  if (length(path) > 1) stop("please specify only one region")
  if (!dir.exists(path)) stop("region directory not found: ", path)
  db = list_databases(form = "table") |>
    dplyr::filter(region == basename(path)) |>
    dplyr::rowwise() |>
    dplyr::group_map(
      function(tbl, grp){
       db = read_database(file.path(path, tbl$product_id), ...) |>
         dplyr::mutate(product = tbl$product_id, .before = 1)
      }) |>
    dplyr::bind_rows()
  
  if (rm_dups){
    db = dplyr::arrange(db, .data$date, .data$product, dplyr::desc(.data$id))  
                        # relies on ANALYSISFORECAST alphabetically 
                        # leading MULTIYEAR in the product variable
                        # and my preceding myint in the id (dataset) reversed variable
                        # it's just dumb luck that this works
    ix = dplyr::select(db, dplyr::all_of(c("date", "variable"))) |>
      duplicated(fromLast = TRUE)
    db = dplyr::filter(db, !ix)
  }
  class(db) <- c("merged", class(db))
  db
}
 
