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