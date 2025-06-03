#' Read Gulf of Maine buoy data as CSV or sf
#' 
#' @export
#' @param filename chr the name of the file to read
#' @param form chr one of "table" or "sf" (default)
#' @return eirther a data frame or spatial data frame
read_buoys = function(
    filename = system.file("exdata/buoy_listing.com",
                            package = "andreas"),
    form = c("table", "sf")[2]){
  x = readr::read_csv(filename, col_types = "cccnn")
  if (tolower(form[1]) == "sf"){
    x = sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
  }
  x
}
