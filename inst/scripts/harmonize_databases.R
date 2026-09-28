#' Harmonizes databases by comparing ANFC to MY holdings, and removing duplicated
#' ANFC files.  If running interactively it runs as a DRY_RUN, but
#' if called via a script then it runs with the (intended) possibility 
#' of removing files.
#' 
#' The script relies upon the lut/activate-databases.csv file to determine
#' the regions/databases that get harmonized.

suppressPackageStartupMessages({
  library(andreas)
  library(dplyr)
  library(rlang)
})

VERBOSE = interactive()
DRY_RUN = interactive()

phy_products = c(my = "GLOBAL_ANALYSISFORECAST_PHY_001_024",
             anfc = "GLOBAL_MULTIYEAR_PHY_001_030")
bgc_products = c(my = "GLOBAL_ANALYSISFORECAST_BGC_001_028",
                 anfc = "GLOBAL_MULTIYEAR_BGC_001_029")

phy = active_databases(include = "path") |>
  dplyr::filter(product_id %in% unname(phy_products)) |>
  dplyr::group_by(region) |>
  dplyr::group_map(
    function(reg, key){
      cat(sprintf("product: %s region: %s", reg$group[1], key$region[1]), "\n")
      if (nrow(reg) < 2) return(NULL)
      purged = harmonize_databases(my_path = dplyr::filter(reg, period == 'my') |>
                                     dplyr::pull(dplyr::starts_with("path")),
                                   anfc_path = dplyr::filter(reg, period == 'anfc') |>
                                     dplyr::pull(dplyr::starts_with("path")),
                                   verbose = VERBOSE,
                                   dry_run = DRY_RUN)
    }
  )

bgc = active_databases(include = "path") |>
  dplyr::filter(product_id %in% unname(bgc_products)) |>
  dplyr::group_by(region) |>
  dplyr::group_map(
    function(reg, key){
      cat(sprintf("product: %s region: %s", reg$group[1], key$region[1]), "\n")
      if (nrow(reg) < 2) return(NULL)
      purged = harmonize_databases(my_path = dplyr::filter(reg, period == 'my') |>
                                     dplyr::pull(dplyr::starts_with("path")),
                                   anfc_path = dplyr::filter(reg, period == 'anfc') |>
                                     dplyr::pull(dplyr::starts_with("path")),
                                   verbose = VERBOSE,
                                   dry_run = DRY_RUN)
    }
  )
