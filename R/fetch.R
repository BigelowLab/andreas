#' Coerce user dates to fit within the catalog dates
#' 
#' @export
#' @param mydates vector of Dates
#' @param catalog a catalog table of one or more datasets
#' @param warn logcal, if TRUE thenissue a warning when the input is coerced
#' @return two element Date class object corced to fit within the catalog data range
coerce_dates = function(mydates = Sys.Date() + c(0,9), 
                        catalog = read_product_lut(),
                        warn = interactive()){
              
    mydates = as.Date(mydates)
    origdates = mydates
    min_y = as.Date(min(catalog$start_time, na.rm = TRUE))
    max_y = as.Date(max(catalog$end_time, na.rm = TRUE))
    
    ismall = mydates < min_y
    mydates[ismall] <- min_y
    
    ibig = mydates > max_y
    mydates[ibig] <- max_y
    
    if (warn && (!identical(mydates, origdates))){
      warning("dates have been coerced")
    }
    
    mydates
}


#' A wrapper around [copernicus::fetch_copernicus()] to fetch Copernicus data as
#' stars objects.
#' 
#' @export
#' @param x table - a look up table with one or more datasets
#' @param time NULL or a two element range of dates
#' @param bb NULL or a 4-element vector for subsetting
#' @param drop_depth logical, if TRUE then drop the depth dimension
#' @param coerce_time logical, if TRUE then coerce min and max time to fit within
#'   the catalog offering
#' @param must_have_time logical, if TRUE and the raster does not have a time 
#'   dimension (ala only one day) then add one
#' @param verbose logical, if TRUE be verbose
#' @param ... other arguments passed to [copernicus::fetch_copernicus()]
#' @return a list of stars objects groups by dataset_id and depth.  Each element, 
#' if not NULL, will have an "andreas" attribute that provides a list with 
#' dataset_id, depth and time.  We use this because a degenerate dimension (just one 
#' element) may be dropped, and so the time/depth context may be lost
fetch_andreas = function(x, 
                         drop_depth = TRUE,
                         time = c(Sys.Date(), as.Date(max(x$end_time))),
                         bb = c(xmin = -180, xmax = 180, ymin = -90, ymax = 90),
                         coerce_time = TRUE,
                         must_have_time = TRUE, 
                         verbose = FALSE,
                         ...){
  if (FALSE){
    drop_depth = TRUE
    time = c(Sys.Date(), as.Date(max(x$end_time)))
    bb = c(xmin = -77, xmax = -42.5, ymin = 36.5, ymax = 56.7)
    coerce_time = TRUE
    verbose = TRUE
  }
  r = x |>
    dplyr::group_by(.data$dataset_id, .data$depth) |>
    dplyr::group_map(
      function(tbl, key, drop_depth = TRUE){
        if (interactive() && verbose) cat("fetch_andreas: ", tbl$dataset_id[1], "\n")
        filename = tempfile(fileext = ".nc")
        depth = if(is.na(tbl$mindepth[1])) {
            NULL
          } else {
            c(tbl$mindepth[1], tbl$maxdepth[1])
          }
        
        if (!is.null(time)){
          if (coerce_time) {
            time = coerce_dates(time, tbl)
          } else {
            if (!all(time[1] >= as.Date(tbl$start_time))) {
              warning(sprintf("requested start time (%s) is before dataset begins (%s)", 
                              format(time[1], "%Y-%m-%d"), format(min(tbl$start_time)),"%Y-%m-%d"))
              return(NULL)
            } # check min time
            
            if (!all(time[length(time)] <= as.Date(tbl$end_time))) {
              warning(sprintf("requested start time (%s) is after dataset ends (%s)", 
                              format(time[1], "%Y-%m-%d"), format(max(tbl$end_time)),"%Y-%m-%d"))
              return(NULL)
            } # check max time
          } # corece_time?
        } # time is not NULL
        if (interactive() && verbose){
          
          # debugging notes
          cat("  fetch_dataset dataset_id: ", tbl$dataset_id[1], "\n")
          cat("  fetch_dataset vars: ", dplyr::pull(tbl, dplyr::all_of("short_name")) |>
                paste(collapse = ", "), "\n")
          cat("  fetch_dataset time: ", format(time, "%Y-%m-%d") |> paste(collapse = ", "), "\n")
          cat("  fetch_dataset depth: ", paste(depth, collapse = ", "), "\n")
          cat("  fetch_dataset bb: ", sprintf("%f, %f, %f, %f", bb[1], bb[2], bb[3], bb[4]), "\n")
          cat("  fetch_dataset filename: ", basename(filename), "\n")
          
          s = copernicus::fetch_copernicus(dataset_id = tbl$dataset_id[1],
                                           vars = dplyr::pull(tbl, dplyr::all_of("short_name")),
                                           time = time,
                                           depth = depth,
                                           ofile = filename,
                                           bb = bb)
        } else {
          s = copernicus::fetch_copernicus(dataset_id = tbl$dataset_id[1], 
                                           vars = dplyr::pull(tbl, dplyr::all_of("short_name")),
                                           time = time,
                                           depth = depth,
                                           ofile = filename,
                                           bb = bb,
                                           ...)
        }
        
        if (is.null(s) || inherits(s, "try-error")){
          s = NULL
        } else {
          # here we add an attribute to alert the user to the time and depth of 
          # values associated with this array.  They may be dropped (or may not have 
          # existed in the first place)
          d = stars::st_dimensions(s)
          andreas = list(dataset_id = tbl$dataset_id[1],
                         time = NULL, 
                         depth = NULL)
          andreas$depth = if ("depth" %in% names(d)){
            stars::st_get_dimension_values(s, "depth")
          } else {
            depth
          }
          andreas$time = if ("time" %in% names(d)){
            stars::st_get_dimension_values(s, "time")
          } else {
            unique(time)
          }
          
          s = s |>
            rlang::set_names(dplyr::pull(tbl, dplyr::all_of("name")))
          if (drop_depth && ("depth" %in% names(d))){
            s = dplyr::slice(s, "depth", 1, drop = TRUE)
          } 
          # slicing drops (user) attributes, so we add these after
          # slicing
          attr(s, "andreas") <- andreas
          s
        }
      }, .keep = TRUE, drop_depth = drop_depth) 

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