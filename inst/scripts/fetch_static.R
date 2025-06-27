suppressPackageStartupMessages({
  library(copernicus)
  library(andreas)
  library(dplyr)
  library(sf)
  library(stars)
})

catalog = tibble(
  region = c("world", "world", "nwa", "nwa"),
  product_id = c('GLOBAL_ANALYSISFORECAST_BGC_001_028',
                 "GLOBAL_MULTIYEAR_BGC_001_029",
                 'GLOBAL_ANALYSISFORECAST_PHY_001_024',
                 "GLOBAL_MULTIYEAR_PHY_001_030"),
  dataset_id = c("cmems_mod_glo_bgc_anfc_0.25deg_static",
                 "cmems_mod_glo_bgc_my_0.25deg_static",
                 "cmems_mod_glo_phy_anfc_0.083deg_static",
                 "cmems_mod_glo_phy_my_0.083deg_static")
) |>
  filter(region == "world")




vars = c("deptho", "mask")
depth = c(0,1)

catalog |>
  rowwise() |>
  group_map(
    function(tbl, grp){
      bb = cofbb::get_bb(tbl$region)
      downfile = copernicus_path("tmp", paste0(tbl$dataset_id, ".nc"))
      ok = copernicus::download_copernicus_cli_subset(
        dataset_id = tbl$dataset_id,
        ofile = downfile,
        vars = vars,
        bb = bb,
        time = NULL,
        depth = depth,
        verbose = TRUE)
      
      for (v in vars){
        ofile = copernicus_path(tbl$region, tbl$product_id, "static", paste0(v, ".tif"))
        ok = make_path(dirname(ofile))
        x = stars::read_stars(downfile, v) |>
          sf::st_set_crs(4326) |>
          drop_degenerate() |>
          stars::write_stars(ofile)
      }
      unlink(downfile)
    }
  )





