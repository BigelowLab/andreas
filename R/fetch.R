#' A wrapper around [copernicus::fetch_copernicus()] to fetch Copernicus data as
#' stars objects.
#' 
#' @export
#' @param x table - a look up table with one or more datasets
#' @param drop_depth logical, if TRUE then drop the depth dimension
#' @param ... other arguments passed to [copernicus::fetch_copernicus()]
#' @return stars object
fetch_andreas = function(x, 
                         drop_depth = TRUE,
                         ...){
  r = x |>
    dplyr::group_by(.data$dataset_id, .data$depth) |>
    dplyr::group_map(
      function(tbl, key, drop_depth = TRUE){
        filename = tempfile(fileext = ".nc")
        depth = if(is.na(tbl$mindepth[1])) {
            NULL
          } else {
            c(tbl$mindepth[1], tbl$maxdepth[1])
          }
        
        s = copernicus::fetch_copernicus(dataset_id = tbl$dataset_id[1], 
                                         vars = dplyr::pull(tbl, dplyr::all_of("short_name")),
                                         depth = depth,
                                         ofile = filename,
                                          ...) # time/bb
        if (is.null(s) || inherits(s, "try-error")){
          s = NULL
        } else {
          s = s |>
              rlang::set_names(dplyr::pull(tbl, dplyr::all_of("short_name")))
          if (drop_depth && ("depth" %in% names(stars::st_dimensions(s)))){
            s = dplyr::slice(s, "depth", 1)
          } 
          s
        }
      }, .keep = TRUE, drop_depth = drop_depth) 
  ix <- sapply(r, is.null)
  if (all(ix)) return(NULL)
  r = copernicus::bind_stars(r[!ix])
  r
}

#' A convenience tool for fetching a product suite on a given day.
#' 
#' This only works for known products/datasets.  Unknowns will generate a
#' warning and return NULL. For each dataset only a single depth layer is 
#' returned. 
#' 
#' @export
#' @param p table of product info to be grouped by `dataset_id`
#' @param x Date (or YYYY-mm-dd string)
#' @param bb bounding box or somehting from which a bounding box can be found.
#' @param ... other arguments for [fetch_copernicus_cli_subset]
#' @return list of stars objects (one for each dataset) possibly with NULLs for unknown datasets
fetch_product_by_day = function(p = product_lut() |>
                                  dplyr::filter(.data$fetch == "yes",
                                                .data$product_id == .data$product_id[1]),
                                x = Sys.Date()-7,
                                bb = c(xmin = -180, ymin = -90, 
                                       xmax = 180, ymax = 90) |>
                                  sf::st_bbox(crs = 4326),
                                ...){
  dates = c(x[1], x[length(x)])

  p = p |>
    dplyr::group_by(.data$dataset_id)
  
  p |>
    dplyr::group_map(
      function(tbl, key){
        switch(key$dataset_id,
               "cmems_mod_glo_phy_anfc_0.083deg_P1D-m" = {
                 s = try(fetch_copernicus_cli_subset(dataset_id = key$dataset_id,
                                             product = tbl$product_id[1],
                                             vars = tbl$short_name,
                                             time = dates,
                                             bb = bb,
                                             depth = NULL,
                                             ...))
                 if (inherits(s, "try-error")){
                   s = NULL
                 }
                 s
               },
               "cmems_mod_glo_phy_anfc_0.083deg-sst-anomaly_P1D-m" = {
                 s = try(fetch_copernicus_cli_subset(dataset_id = key$dataset_id,
                                             product = tbl$product_id[1],
                                             vars = tbl$short_name,
                                             time = dates,
                                             bb = bb,
                                             depth = NULL,
                                             ...))
                 if (inherits(s, "try-error")){
                   s = NULL
                 }
                 if (!is.null(s)) s = s |> rlang::set_names("sstanom")
                 s
               },
               "cmems_mod_glo_phy-cur_anfc_0.083deg_P1D-m" = {
                 s = try(fetch_copernicus_cli_subset(dataset_id = key$dataset_id,
                                             product = tbl$product_id[1],
                                             vars = tbl$short_name,
                                             time = dates,
                                             bb = bb,
                                             depth = c(tbl$mindepth[1],tbl$maxdepth[1]),
                                             ...))
                 if (inherits(s, "try-error")){
                   s = NULL
                 }
                 if (!is.null(s)) s = s |> dplyr::slice("depth", 1)
                 s
               },
               "cmems_mod_glo_phy-so_anfc_0.083deg_P1D-m" = {
                 s = try(fetch_copernicus_cli_subset(dataset_id = key$dataset_id,
                                             product = tbl$product_id[1],
                                             vars = tbl$short_name,
                                             time = dates,
                                             bb = bb,
                                             depth = c(tbl$mindepth[1],tbl$maxdepth[1]),
                                             ...))
                 if (inherits(s, "try-error")) s = NULL
                 
                 if (!is.null(s)) s = s |> dplyr::slice("depth", 1)
                 s
               },
               "cmems_mod_glo_phy-thetao_anfc_0.083deg_P1D-m" = {
                 s = try(fetch_copernicus_cli_subset(dataset_id = key$dataset_id,
                                             product = tbl$product_id[1],
                                             vars = tbl$short_name,
                                             time = dates,
                                             bb = bb,
                                             depth = c(tbl$mindepth[1],tbl$maxdepth[1]),
                                             ...))
                 if (inherits(s, "try-error")) s = NULL
                 if (!is.null(s)) s = s |> dplyr::slice("depth", 1)
                 s
               },
               "cmems_mod_glo_phy-wcur_anfc_0.083deg_P1D-m" = {
                 s = try(fetch_copernicus_cli_subset(dataset_id = key$dataset_id,
                                             product = tbl$product_id[1],
                                             vars = tbl$short_name,
                                             time = dates,
                                             bb = bb,
                                             depth = c(tbl$mindepth[1],tbl$maxdepth[1]),
                                             ...))
                 if (inherits(s, "try-error")) s = NULL
                 if (!is.null(s)) s = s |> dplyr::slice("depth", 1)
                 s
               },
               "cmems_mod_glo_phy_my_0.083deg_P1D-m" = {
                 s = try(fetch_copernicus_cli_subset(dataset_id = key$dataset_id,
                                             product = tbl$product_id[1],
                                             vars = tbl$short_name,
                                             time = dates,
                                             bb = bb,
                                             depth = c(tbl$mindepth[1],tbl$maxdepth[1]),
                                             ...))
                 if (inherits(s, "try-error")) s = NULL
                 s
               },
               {
                 warning("dataset_id not known, contact developer:", key$dataset_id)
                 NULL
               })
      }) |>
    rlang::set_names(dplyr::group_data(p) |> dplyr::pull(1))
}