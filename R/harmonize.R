#' Harmonize two databases (multiyear MY and analysis-forecast ANFC)
#' 
#' If data are accruing for MY and ANFC at some point there may be overlap.  Newer 
#' MY files have the same content (or intent) as the older ANFC.  This function will
#' purge ANFC files where they overlap with MY.  Our assumption is the MY files
#' have more vetted and stable data than ANFC.
#' 
#' @export
#' @param my_path chr the path to the multiyear database
#' @param anfc_path chr, the path to the anfc database
#' @param dry_run logical, if TRUE just compute the overlap by don't actually
#'   purge data
#' @param verbose logical, if TRUE output messages about what is to be purged
#' @return database of anfc data that has been purged (unless dry_run is TRUE)
harmonize_databases = function(my_path = copernicus_path("jordanbasin/GLOBAL_MULTIYEAR_PHY_001_030"), 
                                anfc_path = copernicus_path("jordanbasin/GLOBAL_ANALYSISFORECAST_PHY_001_024"),
                                dry_run = TRUE,
                                verbose = !dry_run){
  
  if (FALSE){
    my_path = copernicus_path("jordanbasin/GLOBAL_MULTIYEAR_PHY_001_030")
    anfc_path = copernicus_path("jordanbasin/GLOBAL_ANALYSISFORECAST_PHY_001_024")
    dry_run = TRUE
  }
  my = read_database(my_path) |>
    dplyr::mutate(.key = sprintf("%s_%s_%s",
                                 format(.data$date, "%Y-%m-%d"),
                                 .data$period,
                                 .data$.name))
 
  anfc = read_database(anfc_path) |>
    dplyr::mutate(.key = sprintf("%s_%s_%s",
                                 format(.data$date, "%Y-%m-%d"),
                                 .data$period,
                                 .data$.name))
  
  purgeme = anfc |>
    dplyr::filter(.data$.key %in% my$.key) |>
    dplyr::select(-dplyr::all_of(".key"))
  keepme = anfc |>
    dplyr::filter(!.data$.key %in% my$.key) |>
    dplyr::select(-dplyr::all_of(".key"))
  
  if (verbose){
    message(sprintf("%i records to be purged from ANFC of %i original\nleaving %i records intact", 
                    nrow(purgeme), 
                    nrow(anfc),
                    nrow(keepme)))
  }
  
  if (!dry_run){
    purgefiles = compose_filename(purgeme, anfc_path)
    ok = file.remove(purgefiles)
    write_database(keepme, anfc_path)
  }
  
  purgeme
}