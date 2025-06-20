% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lut.R
\name{read_product_lut}
\alias{read_product_lut}
\title{Read a product LUT used for searches and merge with a product description}
\usage{
read_product_lut(
  product_id = "GLOBAL_ANALYSISFORECAST_PHY_001_024",
  description = nicolaus::read_catalog()
)
}
\arguments{
\item{product_id}{chr, the product identifier.  The associated file must exist}

\item{description}{table, the table produced by [nicolaus::read_catalog] for
the specified `product_id`}
}
\value{
a table with the following
* product_id
* dataset_id
* dataset_name
* name chr short_name converted to camelCase (used for file View(xstorage)
* short_name
* standard_name
* units 
* depth really a depth name "sur", "bot", "mld", "zos" or NA but could be a number (as character())
* fetch   "yes", "no" or NA
* mindepth for `copernicusmarine` CLI request
* maxdepth for `copernicusmarine` CLI request
* start_time known data service availability
* end_time known data service availability
}
\description{
Read a product LUT used for searches and merge with a product description
}
