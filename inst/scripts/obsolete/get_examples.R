suppressPackageStartupMessages({
  library(copernicus)
  library(stars)
  library(dplyr)
  library(charlier)
  library(argparser)
  library(cofbb)
  library(yaml)
  library(stringr)
})

DEST = "/mnt/s1/projects/ecocast/coredata/copernicus/examples"
bb = c(xmin = -70, ymin = 43, xmax = -69, ymax = 44)


download_raw = function(){

  xx = group_by(TAB, dataset_id) |>
    group_map(
      function(tbl, key){
        out_file = file.path(DEST, sprintf("%s__%s.nc", 
                                           tbl$product_id[1], 
                                           key$dataset_id))
        download_copernicus_cli_subset(
          dataset_id = key$dataset_id,
          vars = NULL,
          depth = NULL,
          bb = bb,
          time = c("2024-06-01", "2024-06-01"),
          ofile = out_file,
          verbose = TRUE)
      })
  
  ff = list.files(DEST, full.names = TRUE, pattern =  glob2rx("*.nc"))
  nms = basename(ff) |>
    str_replace(fixed(".nc"), "") |>
    str_split_i("__", 2)
  ss = lapply(ff, read_stars) |>
    setNames(nms)
  
  for (ds in names(ss)){
    if ("depth" %in% names(dim(ss[[ds]]))){
      cat(ds, "\n")
      cat(st_get_dimension_values(ss[[ds]], "depth") |> paste(collapse = ","), "\n")
    }
  }
  
}

dataset_id_max_depth = function(dataset) {
  switch(dataset,
         "cmems_mod_glo_phy_anfc_0.083deg_P1D-m" = 1, 
         "cmems_mod_glo_phy_anfc_0.083deg-sst-anomaly_P1D-m" = NULL, 
         "cmems_mod_glo_phy-cur_anfc_0.083deg_P1D-m" = NULL,
         "cmems_mod_glo_phy-so_anfc_0.083deg_P1D-m" = 1,
         "cmems_mod_glo_phy-thetao_anfc_0.083deg_P1D-m" = 1,
         "cmems_mod_glo_phy-wcur_anfc_0.083deg_P1D-m" = NULL
  )
}

fetch_examples = function(){
 
  # this example shows how to fetch based upon a product lut which has 
  # one or more datasets and each dataset has one or more variables
  date = "2024-12-31" 
  

  P = product_lut() |>
    dplyr::filter(fetch == "yes") |>
    dplyr::group_by(dataset_id)
  
  ss = P |>
    group_map(
      function(tbl, key){
        switch(key$dataset_id,
               "cmems_mod_glo_phy_anfc_0.083deg_P1D-m" = {
                 fetch_copernicus_cli_subset(dataset_id = key$dataset_id,
                                             product = tbl$product_id[1],
                                             vars = tbl$short_name,
                                             time = c(date, date),
                                             bb = bb,
                                             depth = NULL)
               },
               "cmems_mod_glo_phy_anfc_0.083deg-sst-anomaly_P1D-m" = {
                 fetch_copernicus_cli_subset(dataset_id = key$dataset_id,
                                             product = tbl$product_id[1],
                                             vars = tbl$short_name,
                                             time = c(date, date),
                                             bb = bb,
                                             depth = NULL) |>
                   rlang::set_names("sstanom")
               },
               "cmems_mod_glo_phy-cur_anfc_0.083deg_P1D-m" = {
                 fetch_copernicus_cli_subset(dataset_id = key$dataset_id,
                                             product = tbl$product_id[1],
                                             vars = tbl$short_name,
                                             time = c(date, date),
                                             bb = bb,
                                             depth = c(tbl$mindepth[1],tbl$maxdepth[1])) |>
                   dplyr::slice("depth", 1)
               },
               "cmems_mod_glo_phy-so_anfc_0.083deg_P1D-m" = {
                 fetch_copernicus_cli_subset(dataset_id = key$dataset_id,
                                             product = tbl$product_id[1],
                                             vars = tbl$short_name,
                                             time = c(date, date),
                                             bb = bb,
                                             depth = c(tbl$mindepth[1],tbl$maxdepth[1]))|>
                   dplyr::slice("depth", 1)
               },
               "cmems_mod_glo_phy-thetao_anfc_0.083deg_P1D-m" = {
                 fetch_copernicus_cli_subset(dataset_id = key$dataset_id,
                                             product = tbl$product_id[1],
                                             vars = tbl$short_name,
                                             time = c(date, date),
                                             bb = bb,
                                             depth = c(tbl$mindepth[1],tbl$maxdepth[1]))|>
                   dplyr::slice("depth", 1)
               },
               "cmems_mod_glo_phy-wcur_anfc_0.083deg_P1D-m" = {
                 fetch_copernicus_cli_subset(dataset_id = key$dataset_id,
                                             product = tbl$product_id[1],
                                             vars = tbl$short_name,
                                             time = c(date, date),
                                             bb = bb,
                                             depth = c(tbl$mindepth[1],tbl$maxdepth[1]))|>
                   dplyr::slice("depth", 1)
               })
      }) |>
    rlang::set_names(dplyr::group_data(P)$dataset_id)
  
    

}



