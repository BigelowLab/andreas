% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stars.R
\name{correct_andreas}
\alias{correct_andreas}
\title{Corrects stars objects with unexpected missing values. Intended for use with Copernicus.
Assumes that the stars object uses NA to both indicate masked values, i.e. land, and missing values.}
\usage{
correct_andreas(stars_obj, land_mask, replacement_values = list(mlotst = 700))
}
\arguments{
\item{stars_obj}{stars, stars object up to 3 dimensions to correct}

\item{land_mask}{stars, the land mask associated with the stars object (same extent and res) where
water pixels are non-zero and land is zero.  Must have the same footprint as `stars_obj`}

\item{replacement_values}{list, named list of attribute names and a value to sub in for any missing entries.}
}
\value{
stars object with any incorrect missing values replaced w/ replacement value.
}
\description{
Corrects stars objects with unexpected missing values. Intended for use with Copernicus.
Assumes that the stars object uses NA to both indicate masked values, i.e. land, and missing values.
}
\examples{
\dontrun{
 path = copernicus_path("chfc/GLOBAL_MULTIYEAR_PHY_001_030")
 DB = read_database(path) |>
   dplyr::filter(dplyr::between(.data$date, as.Date("2015-03-14"), as.Date("2015-03-16")))
 stars_obj =  read_andreas(DB, path)
 land_mask = read_static("mask", path)
 fixed = correct_andreas(stars_obj, land_mask)
 stars_obj
 fixed
}
}
