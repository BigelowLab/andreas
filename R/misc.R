#' Drop a degenerate dimension (usually depth or time)
#' 
#' This has only narrow utility when downloading form Copernicus having filtered
#' to a single depth (which is retained).  This function will drop that.
#' 
#' @export
#' @param x stars object
#' @return stars object possibly with fewer dimensions
drop_degenerate = function(x){
  #d = dim(x)
  #ix = which(d == 1)
  #if (length(ix) > 0){
  #  if (i %in% rev(ix)) {
  #    dropme = names(d[i])
  #    x = dplyr::slice(x, "depth", 1) 
  #  }
  #}
  
  x[drop=TRUE]
}

#' Perform grepl on multiple patterns; it's like  AND-ing or OR-ing successive grepl statements.
#' 
#' Adapted from https://stat.ethz.ch/pipermail/r-help/2012-June/316441.html
#'
#' @param pattern character vector of patterns
#' @param x the character vector to search
#' @param op logical vector operator back quoted, defaults to `|`
#' @param ... further arguments for \code{grepl} like \code{fixed} etc.
#' @return logical vector
mgrepl <- function(pattern, x, op = `|`, ... ){
  Reduce(op, lapply(pattern, grepl, x, ...))
}


#' Retrieve a look up table (lut) relating variable names
#' 
#' @export
#' @param table with "variable", "name", "longname" and "unit"
variable_lut = function(){
  dplyr::tribble(
    ~variable, ~name, ~longname, ~unit,
    "thetao", "temp", "potential temperature", "(\u00b0C)",
    "bottomT", "temp", "bottom temperature", "(\u00b0C)",
    "tob", "temp", "bottom temperature", "(\u00b0C)",
    "sst", "temp", "sea surface temperature", "(\u00b0C)",
    "so", "sal", "salinity", "(psu)",
    "sob", "sal", "bottom salinity", "(psu)",
    "sss", "sal", "sea surface salinity", "(psu)",
    "pbo", "pbo", "bottom pressure", "",
    "mlotst", "mlotst", "mixed layer depth", "(m)",
    "uo", "uo", "eastward current velocity", "(m/s)",
    "vo", "vo", "northward current velocity", "(m/s)",
    "wo", "wo", "upward current velocity", "(m/s)",
    "zos", "zos", "sea surface height", "(m)",
    "chl", "chl", "chlorophyll", "mg m-3",
    "no3", "no3", "nitrate", "mmol m-3",
    "nppv", "nppv", "net primary productionas carbon", "mg m-3 day-1",
    "o2", "o2", "oxygen", "mmol m-3",
    "po4", "po4", "phosphate", "mmol m-3",
    "si", "si", "silcate", "mmol m-3",
    "zooc", "zooc", "total zooplankton as carbon", "mmol m-3",
    "phyc", "phyc", "total phytoplankton as carbon", "mmol m-3")
}

#' Retrieve the common name of a given variable name
#' 
#' @export
#' @param x chr, vector of one or more variable names
#' @param lut table of look up values
#' @return charcater vector of short common names
common_name = function(x = c("bottomT", "pbo", "sob", "tob", "mlotst", 
                             "so", "thetao", "uo", "vo", "wo", "zos"),
                       lut = variable_lut()){
  
  common = lut$name |>
    rlang::set_names(lut$variable)
  common[x] |>
    unname()
}