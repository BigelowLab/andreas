#' Decompose dataset_id into constituent parts
#' 
#' 
#' @seealso \href{https://help.marine.copernicus.eu/en/articles/6820094-how-is-the-nomenclature-of-copernicus-marine-data-defined}{Copernicus Docs}
#' @export
#' @param x chr one or more dataset ids
#' @param col_names chronicled, the names to use for the output table variables
#' @return table of constituent parts
#' \itemize{
#'  \item{origin}
#'  \item{group}
#'  \item{area}
#'  \item{theme}
#'  \item{type}
#'  \item{compinfo}
#'  \item{tempres}
#'  \item{pref_order, the prefered ordering determined by 'type'}
#'  }
decompose_dataset_id = function(x = c("cmems_mod_glo_phy_anfc_0.083deg_P1D-m", 
                                      "cmems_mod_glo_phy_my_0.083deg_P1D-m", 
                                      "cmems_mod_glo_phy_myint_0.083deg_P1D-m"),
                                col_names = c("origin", "group", "area", "theme",
                                              "type", "compinfo", "tempres")){
  # _anfc (analysis forecast), _nrt (near real time), _hcst (hindcast), _my (multiyear), _mynrt, _myint (interim)
  #  higher confidence -> lower confidence
  types = c("my", "myint", "hcst", "mynrt", "nrt", "anfc")
  torder = sprintf("%0.2i_%s",seq_along(types), types)
  names(torder) <- types
  ss = stringr::str_split(x, stringr::fixed("_"), simplify = TRUE) 
  colnames(ss) <- col_names
  ss = dplyr::as_tibble(ss) |>
    dplyr::mutate(pref_order = torder[.data$type] )
}