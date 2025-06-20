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

