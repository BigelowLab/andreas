% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/static.R
\name{remap_to_water_pixel}
\alias{remap_to_water_pixel}
\title{Map land-based points to water.}
\usage{
remap_to_water_pixel(x = read_buoys(), lut = read_static("lut"))
}
\arguments{
\item{x}{sf object of points}

\item{lut}{stars object as per `read_static("lut", path)`}

\item{sf}{object with new geometry}
}
\value{
sf object of points with new locations (where appropriate)
}
\description{
Given a set of points and a look up table, compute new locations over water
for any input points located over land.  Input points over water are unchanged.
}
