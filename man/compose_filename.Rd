% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/database.R
\name{compose_filename}
\alias{compose_filename}
\title{Compose a file name from a database (possibly merged)}
\usage{
compose_filename(x, path = ".", ext = ".tif")
}
\arguments{
\item{x}{database (tibble) possibly merged, with date, var, depth}

\item{path}{character, the root path for the file name}

\item{ext}{character, the file name extension to apply (including dot)}
}
\value{
character vector of file names in form
        \code{<path>/YYYY/mmdd/id__datetime_depth_period_variable_treatment.ext}
}
\description{
Compose a file name from a database (possibly merged)
}
