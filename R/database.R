#' Compose a file name from a database (possibly merged)
#'
#' @export
#' @param x database (tibble) possibly merged, with date, var, depth
#' @param path character, the root path for the file name
#' @param ext character, the file name extension to apply (including dot)
#' @return character vector of file names in form
#'         \code{<path>/YYYY/mmdd/id__datetime_depth_period_variable_treatment.ext}
compose_filename <- function(x, path = ".", ext = ".tif"){
  if (inherits(x, "merged") || "product" %in% colnames(x)){
    path = file.path(path, x$product)
  }
  # <path>/YYYY/mmdd/id__date_time_depth_period_variable_treatment.ext
  # cmems_mod_glo_phy-cur_anfc_0.083deg_P1D-m__2025-03-14T000000_0.494_day_vo_raw.tif
  file.path(path,
            format(x$date, "%Y/%m%d"),
            sprintf("%s__%s_%s_%s_%s_%s%s",
                    x$id,
                    sprintf("%sT%s", format(x$date, "%Y-%m-%d"), x$time),
                    x$depth, 
                    x$period,
                    x$variable,
                    x$treatment,
                    ext))
}


#' Decompose a filename into a database
#'
#' @export
#' @param x character, vector of one or more filenames
#' @param ext character, the extension to remove (including dot)
#' @return table (tibble) database
#' \itemize{
#'  \item{id chr, the dataset_id}
#'  \item{date Date}
#'  \item{time, chr, six-character HHMMSS}
#'  \item{depth chr, the depth in meters}
#'  \item{period chr, one of day, month, etc}
#'  \item{variable chr, the variable name}
#'  \item{treatment chr, treatment such as raw, mean, sum, etc}
#' }
decompose_filename = function(x = c("cmems_mod_glo_phy-cur_anfc_0.083deg_P1D-m__2025-03-18T000000_sur_day_uo_raw.tif", 
                                            "cmems_mod_glo_phy_anfc_0.083deg_P1D-m__2025-03-18T000000_sur_day_zos_raw.tif"),
                                     ext = ".tif"){
  
  datetime = function(x = c("2022-01-15T000000", "2022-01-16T123456")){
    list(date = as.Date(substring(x, 1,10), format = "%Y-%m-%d"),
         time = substring(x, 12))
  }
  # a tidy version of gsub
  global_sub <- function(x, pattern, replacement = ".tif", fixed = TRUE, ...){
    gsub(pattern, replacement, x, fixed = fixed, ...)
  }
  x <- basename(x) |>
    global_sub(pattern = ext, replacement = "") |>
    strsplit(split = "__", fixed = TRUE)
  y = sapply(x, '[[', 2) |>
    strsplit(split = "_", fixed = TRUE)
  
  dt = datetime(sapply(y, '[[', 1))
  dplyr::tibble(
    id = sapply(x, '[[', 1),
    date = dt$date,
    time = dt$time,
    depth = sapply(y, '[[', 2),
    period = sapply(y, '[[', 3),
    variable = sapply(y, '[[', 4),
    treatment = sapply(y, '[[', 5) )
}

#' Construct a database tibble give a data path
#'
#' @export
#' @param path character the directory to catalog
#' @param pattern character, the filename pattern (as glob) to search for
#' @param exclude chr, one or more character patterns to exclude
#' @param save_db logical, if TRUE save the database via [write_database]
#' @param ... other arguments for \code{\link{decompose_filename}}
#' @return tibble database
build_database <- function(path, pattern = "*.tif", 
                           save_db = FALSE, 
                           exclude = "static",
                           ...){
  if (missing(path)) stop("path is required")
  ff = list_files(path, pattern = pattern, exclude = exclude)
  db = decompose_filename(ff)
  if (save_db) db = write_database(db, path)
  return(db)
}

#' List files for a database
#'
#' @export
#' @param path character the directory to catalog
#' @param pattern character, the filename pattern (as glob) to search for
#' @param exclude chr, one or more character patterns to exclude
#' @return file list
list_files <- function(path,
                           pattern = "*.tif", 
                           exclude = "static"){
  if (missing(path)) stop("path is required")
  ff = list.files(path[1], pattern = utils::glob2rx(pattern),
                  recursive = TRUE, full.names = TRUE) 
  if (length(exclude) > 0){
    ff = ff[!mgrepl(exclude, ff, fixed = TRUE)]
  }
  return(ff)
}



#' Read a file-list database
#'
#' @export
#' @param path character the directory with the database
#' @param multiple logical, if TRUE then switch to a [merge_database] where
#'   the path points to a directory that contains multiple compatible 
#'   databases.
#' @param filename character, optional filename
#' @param ... optional arguments for [merge_database]
#' @return a tibble (possibly empty if the database doesn't exist)
read_database <- function(path,
                          multiple = FALSE,
                          filename = "database", 
                          ...){
  if (multiple){
    return(merge_database(path, filename = filename, ...))
  }
  if (missing(path)) stop("path is required")
  filepath <- file.path(path[1], filename[1])
  if(!file.exists(filepath)){
    db = dplyr::tibble(
            id = "",
            date = Sys.Date(),
            time = "",
            depth = "",
            period = "",
            variable = "",
            treatment = "") |>
      dplyr::slice(0)
  } else {
    # date var depth
    db = suppressMessages(readr::read_csv(filepath, col_types = 'cDccccc'))
  }
  db
}

#' Write the file-list database
#'
#' We save only date (YYYY-mm-dd), param, trt (treatment) and src (source). If you
#' have added other variables to the database they will be dropped in the saved
#' file.
#'
#' @export
#' @param x the tibble or data.frame database
#' @param path character the directory to where the database should reside
#' @param filename character, optional filename
#' @return the input tibble (even if saved version has columns dropped)
write_database <- function(x, path,
                           filename = "database"){
  if (missing(path)) stop("path is required")
  filepath <- file.path(path[1], filename[1])
  dummy <- x |>
    select_database() |>
    readr::write_csv(filepath)
  invisible(x)
}


#' Append to the file-list database
#'
#' @export
#' @param x the tibble or data.frame database
#' @param path character the directory to where the database should reside
#' @param filename character, the name of the database file
#' @return a tibble with appended data
append_database <- function(x, path, filename = "database"){
  x = select_database(x)
  if (!dir.exists(path[1])) stop("path not found:", path[1])
  origfilename <- file.path(path,filename[1])
  if(!file.exists(origfilename)){
    return(write_database(x, path, filename = filename))
  }
  orig = read_database(path, filename = filename)
  orig_info = colnames(orig)
  x_info = colnames(x)
  ident = identical(orig_info, x_info)
  if (!isTRUE(ident)){
    print(ident)
    stop("input database doesn't match one stored on disk")
  }
  dplyr::bind_rows(orig, x) |>
    dplyr::distinct() |>
    write_database(path, filename = filename)
}

#' Retrieve the database pre-defined variable names
#'
#' @export
#' @return charcater vector of variable names
database_variables = function(){
  c("id", "date", "time", "depth", "period", "variable", "treatment")
}

#' Select just the db columns
#' 
#' @export
#' @param x database table
#' @param cols chr, the column names to keep
#' @return a database table
select_database = function(x, cols = database_variables()){
  dplyr::select(x, dplyr::all_of(cols))
}



#' Given a database (days, 8DR or month), determine the times
#' that are missing between the first and last records
#' 
#' @export
#' @param x a database - typically filtered to just a single period
#' @param by chr, unit of time used for creating sequence to match against
#' @return Date dates that seem to be missing
missing_records = function(x, 
                           by = dplyr::slice(x,1) |> 
                             dplyr::pull(dplyr::all_of("period"))){
  dr = range(x$date)
  dd = seq(from = dr[1], to = dr[2], by = by)
  dd[!(dd %in% x$date)]
}


#' Retrieve a list of databases
#' 
#' @export
#' @param path chr, the root data directory
#' @param pattern chr, database filename regex pattern to search for
#' @param form one of "path" or "table"
#' @return database paths relative to the root path
list_databases = function(path = copernicus::copernicus_path(),
                          pattern = "^database$",
                          form = c("path", "table")[1]){
  
    ff = lapply(list.dirs(path, full.names = TRUE, recursive = FALSE),
                function(p) {
                  dd = list.dirs(p, full.names = TRUE, recursive = FALSE)
                  list.files(dd, pattern = pattern, full.names = TRUE, recursive = FALSE) |>
                    unlist()
                } ) |>
      unlist()
  ff = sub(paste0(path,.Platform$file.sep), "", dirname(ff))
  if (tolower(form[1]) == "table"){
    ss = strsplit(ff, .Platform$file.sep, fixed = TRUE)
    ff = dplyr::tibble(
      region = sapply(ss, "[[", 1),
      product_id = sapply(ss, "[[", 2))
  }
  ff
}