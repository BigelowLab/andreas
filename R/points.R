#' Read Gulf of Maine buoy data as CSV or sf
#' 
#' @export
#' @param filename chr the name of the file to read
#' @param form chr one of "table" or "sf" (default)
#' @return eirther a data frame or spatial data frame
read_buoys = function(
    filename = system.file("exdata/buoy_listing.csv",
                            package = "andreas"),
    form = c("table", "sf")[2]){
  x = readr::read_csv(filename, col_types = "cccnn")
  if (tolower(form[1]) == "sf"){
    x = sf::st_as_sf(x, coords = c("lon", "lat"), crs = 4326)
  }
  x
}

#' Extract point data from a stars object
#' 
#' @export
#' @param x stars object (with or without time dimension)
#' @param y sf point data
#' @param form chr one of either "long" (the default) or "wide" to control 
#'   output column layout. 
#' @param ... other arguments passed to `stars::st_extract()`
#' @return table of variable data for each input point
extract_points = function(x, y, 
                          form = c("long", "wide")){
  
  n = nrow(y)
  N = floor(log10(n)+ 1)
  fmt = paste0("p%0.", N, "i")
  pnames = sprintf(fmt, seq_len(n))
  xy = sf::st_coordinates(y)
  if (length(dim(x)) == 2){
    
    m = stars::st_extract(x, xy)
    rownames(m) <- pnames
    p = dplyr::as_tibble(m, rownames = "point") |>
      tidyr::pivot_longer(-dplyr::all_of("point"),
                          values_to = "value",
                          names_to = "name")
    
  } else {
    nms = stars::st_get_dimension_values(x, 3)
    dtime = stars::st_dimensions(x)$time
    p = lapply(names(x),
               function(nm){
                 m = stars::st_extract(x[nm], xy)
                 colnames(m) <- nms
                 rownames(m) <- pnames
                 m |>
                   dplyr::as_tibble(rownames = "point") |>
                   dplyr::mutate(name = nm, .after = 1)
               }) |>
      dplyr::bind_rows() |>
      tidyr::pivot_longer(-dplyr::all_of(c("point", "name")),
                          names_to = "time",
                          values_to = "value")
 
    if (inherits(dtime$offset, "POSIXt")){
      timev = as.POSIXct(as.numeric(p$time), 
                         origin = as.POSIXct("1970-01-01", tz = "UTC"),
                         tz = "UTC")
      p = dplyr::mutate(p, time = timev)
    } else if (inherits(dtime$offset, "Date")){
      timev = as.Date(as.numeric(p$time), 
                         origin = as.Date("1970-01-01"))
      p = dplyr::mutate(p, time = timev)
    }
    
  }
  
  if (tolower(form[1]) == "wide"){
    p = tidyr::pivot_wider(p,
                           names_from = "name",
                           values_from = "value")
  }
  
  p
}