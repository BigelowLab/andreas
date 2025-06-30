andreas
================

Andreas (Andrew) Copernicus was the lesser known brother of [Nicolaus
Copernicus](https://en.wikipedia.org/wiki/Nicolaus_Copernicus). This
package serves as suite of functions and scripts to access and manage
data from the [Copernicus Marine Data
Store](https://data.marine.copernicus.eu/products). This package
leverages the tools provided by the [`copernicus` R
package](https://github.com/BigelowLab/copernicus).

This document is divided into two parts: “Fetching Remote Data” and
“Working with Local Archives”. You don’t need to fetch data is you
already have a local archive.

# Fetching Remote Data

You only need this part if you are creating or maintaining a local
archive of data. See this
[wiki](https://github.com/BigelowLab/andreas/wiki/Fetching-a-new-data)
page for an example of building a local dataset.

# Working with Local Archives

For most needs, you are interested in working with a previously
downloaded dataset.

## The local archive

The local archive is a collection of one or more products (and their
datasets) for a particular region of the world. For example, we have an
archive for the Northwest Atlantic that extends from Cape Hatteras to
Flemish Cap (“chfc”). The archive is a directory called “chfc” within
which there is one subdirectory for each product. Within each product
directory there may be series of subdirectories organized by year
(*e.g.* “2022”) and then by month-day (*e.g.* “0521”). Within the
month-day subdirectories there will be one or more GeoTIFF files (one
file for each variable).  
At the product level you will find a text-based file called “database”.
It is a comma delimited file that (in theory) keeps track of the files
that have been archived. We leverage this database to quickly find a
subset of the entire database.

### Using the database

The database is very light and easy to filter for just the records you
might need. Note that depth is a character data type; this provides you
with flexibility to define depth as ‘surface’ or ‘50-75’ or something
like that.

Let’s walk through reading the database, filtering it for a subset,
reading the files and finally displaying.

``` r
suppressPackageStartupMessages({
  library(andreas)
  library(copernicus)
  library(dplyr)
  library(stars)
})

path = copernicus::copernicus_path("chfc", "GLOBAL_ANALYSISFORECAST_PHY_001_024")
db <- andreas::read_database(path) |>
  dplyr::glimpse()
```

    ## Rows: 11,340
    ## Columns: 7
    ## $ id        <chr> "cmems_mod_glo_phy-cur_anfc_0.083deg_P1D-m", "cmems_mod_glo_…
    ## $ date      <date> 2025-06-23, 2025-06-24, 2025-06-25, 2025-06-26, 2025-06-27,…
    ## $ time      <chr> "000000", "000000", "000000", "000000", "000000", "000000", …
    ## $ depth     <chr> "sur", "sur", "sur", "sur", "sur", "sur", "sur", "sur", "sur…
    ## $ period    <chr> "day", "day", "day", "day", "day", "day", "day", "day", "day…
    ## $ variable  <chr> "uo", "uo", "uo", "uo", "uo", "uo", "uo", "uo", "uo", "uo", …
    ## $ treatment <chr> "raw", "raw", "raw", "raw", "raw", "raw", "raw", "raw", "raw…

Now we can read in the files.

``` r
s = db |>
  dplyr::slice(1:20) |>
  andreas::read_andreas(path)
s
```

    ## stars object with 3 dimensions and 2 attributes
    ## attribute(s), summary of first 1e+05 cells:
    ##          Min.     1st Qu.      Median        Mean    3rd Qu.     Max.  NA's
    ## uo  -1.498518 -0.02220678  0.04837541  0.09941523 0.15543639 1.905724 34262
    ## vo  -1.559447 -0.11044315 -0.02616734 -0.01419538 0.04699263 1.385352 34262
    ## dimension(s):
    ##      from  to         offset    delta  refsys x/y
    ## x       1 415         -77.04  0.08333  WGS 84 [x]
    ## y       1 261          56.71 -0.08333  WGS 84 [y]
    ## time    1  10 2025-06-23 UTC   1 days POSIXct

It may look complicated, but this permits the storage of multiple
datasets per product, and is a very flexible system.

``` r
db |> dplyr::count(id, depth, variable)
```

    ## # A tibble: 10 × 4
    ##    id                                           depth variable     n
    ##    <chr>                                        <chr> <chr>    <int>
    ##  1 cmems_mod_glo_phy-cur_anfc_0.083deg_P1D-m    sur   uo        1134
    ##  2 cmems_mod_glo_phy-cur_anfc_0.083deg_P1D-m    sur   vo        1134
    ##  3 cmems_mod_glo_phy-so_anfc_0.083deg_P1D-m     sur   so        1134
    ##  4 cmems_mod_glo_phy-thetao_anfc_0.083deg_P1D-m sur   thetao    1134
    ##  5 cmems_mod_glo_phy-wcur_anfc_0.083deg_P1D-m   sur   wo        1134
    ##  6 cmems_mod_glo_phy_anfc_0.083deg_P1D-m        bot   pbo       1134
    ##  7 cmems_mod_glo_phy_anfc_0.083deg_P1D-m        bot   sob       1134
    ##  8 cmems_mod_glo_phy_anfc_0.083deg_P1D-m        bot   tob       1134
    ##  9 cmems_mod_glo_phy_anfc_0.083deg_P1D-m        mld   mlotst    1134
    ## 10 cmems_mod_glo_phy_anfc_0.083deg_P1D-m        zos   zos       1134

### Static variables

Some products have static variables (model coordinates, depth, mask,
etc). We don’t retain any on a usual basis except for `deptho` and
`mask`.

You can read static variables easily by providing just the variable
names and the data path.

``` r
static = read_static(name = c("mask", "deptho"), path = path)
static
```

    ## stars object with 2 dimensions and 2 attributes
    ## attribute(s):
    ##            Min.  1st Qu.   Median         Mean  3rd Qu.     Max.  NA's
    ## mask    0.00000   0.0000    1.000    0.6816507    1.000    1.000     0
    ## deptho  7.92956 266.0403 3597.032 2992.1398081 4833.291 5727.917 34482
    ## dimension(s):
    ##   from  to offset    delta refsys point x/y
    ## x    1 415 -77.04  0.08333 WGS 84 FALSE [x]
    ## y    1 261  56.71 -0.08333 WGS 84 FALSE [y]
