#' Use this to update a select set of monthly means for the PHY databasii
#' Run it once a month, say on the second, to rebuild the previous (or any
#' missing monthly means)

suppressPackageStartupMessages({
  library(andreas)
  library(dplyr)
})

phy_names = c("sal_sur", "siconc_sur", "sithick_sur", "temp_bot", "temp_sur", 
              "uo_sur", "usi_sur", "vo_sur", "vsi_sur", "zos_zos")

dd = c(anfc = "chfc/GLOBAL_ANALYSISFORECAST_PHY_001_024", 
       my = "chfc/GLOBAL_MULTIYEAR_PHY_001_030")
path = sapply(dd, andreas::copernicus_path)
DBS = lapply(path, andreas::read_database)

DBS = lapply(names(DBS),
  function(nm){
    db = DBS[[nm]]
    db |> 
      dplyr::filter(period == "day", 
                    .name %in% phy_names) |>
      dplyr::mutate(ymd = format(date, "%Y-%m-01")) |>
      dplyr::group_by(.name, ymd) |>
      dplyr::group_map(
        function(grp, key){
          cat(grp$ymd[1], grp$.name[1], "\n")
          the_date = grp$ymd[1] |> as.Date()
          x = andreas::read_andreas(grp, path[[nm]]) |>
            stars::st_apply(c("x", "y"),
                            mean,
                            na.rm = TRUE)
          d = dplyr::slice(grp,1) |>
            dplyr::mutate(period = "month",
                          treatment = "mean",
                          date = the_date)
          stars::write_stars(x, andreas::compose_filename(d, path[[nm]]))
          grp
      }, .keep = TRUE) |>
      dplyr::bind_rows() |>
      andreas::append_database(path[[nm]])
    })

