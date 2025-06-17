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
archive of data. If you already have the data then skip ahead to
“Working with Local Archives”

## Look up tables

We use look up tables (`lut`), merged with Copernicus product
description tables, to allow users to define a subset of data sets.
These `luts` can be configured as needed, but for out use case we want
to know the `dataset_id`, `variable`, the `depth` and `time` to request.
The spatial bounding box coordinates are not maintained in our default
tables, but you could manage those elsewhere (we do using the [cofbb R
package](https://github.com/BigelowLab/cofbb)). Below is out example we
use for fetching data from the
[GLOBAL_ANALYSISFORECAST_PHY_001_024](https://data.marine.copernicus.eu/product/GLOBAL_ANALYSISFORECAST_PHY_001_024/description)
product suite.

``` r
suppressPackageStartupMessages({
  library(andreas)
  library(copernicus)
  library(dplyr)
  library(stars)
})

lut = read_product_lut(product_id = 'GLOBAL_ANALYSISFORECAST_PHY_001_024') |>
  glimpse()
```

    ## Rows: 20
    ## Columns: 14
    ## $ product_id    <chr> "GLOBAL_ANALYSISFORECAST_PHY_001_024", "GLOBAL_ANALYSISF…
    ## $ dataset_id    <chr> "cmems_mod_glo_phy-cur_anfc_0.083deg_P1D-m", "cmems_mod_…
    ## $ dataset_name  <chr> "daily mean fields from Global Ocean Physics Analysis an…
    ## $ name          <chr> "uo", "vo", "so", "thetao", "wo", "seaSurfaceTemperature…
    ## $ short_name    <chr> "uo", "vo", "so", "thetao", "wo", "sea_surface_temperatu…
    ## $ standard_name <chr> "eastward_sea_water_velocity", "northward_sea_water_velo…
    ## $ units         <chr> "m s-1", "m s-1", "1e-3", "degrees_C", "m s-1", "degrees…
    ## $ depth         <chr> "sur", "sur", "sur", "sur", "sur", "sur", "sur", "mld", …
    ## $ fetch         <chr> "yes", "yes", "yes", "yes", "yes", "no", "no", "yes", "y…
    ## $ mindepth      <dbl> 0, 0, 0, 0, 0, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ maxdepth      <dbl> 1, 1, 1, 1, 1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ start_time    <dttm> 2022-06-01, 2022-06-01, 2022-06-01, 2022-06-01, 2022-06…
    ## $ end_time      <dttm> 2025-06-25, 2025-06-25, 2025-06-25, 2025-06-25, 2025-06…
    ## $ n_depth       <dbl> 50, 50, 50, 50, 50, NA, NA, NA, NA, NA, NA, NA, NA, NA, …

Some of the variables are manually curated: `depth`, `fetch`, `mindepth`
and `maxdepth`. The balance are derived from the coperncius catalog
where they are described: `dataset_id`, `dataset_name`, `short_name`,
`standard_name`, `units`, `start_time` and `end_time`. The latter two
tell us the dates between which the products are known to be served.

The manually curated ones are ..

`depth` a descriptive variable with values including ‘sur’, ‘bot’ and
‘mld’, but you could store a numeric depth, too. The only thing to keep
in mind is the numeric depth would be cast to character class.

`fetch` is simple “yes” or “no” - this way you can turn them on and off
as needed to fetch data.

`mindepth` and `maxdepth` are passed to the copernicus package as
min/max filters for searching.

The `name` field is a automatically curated field, we convert
`short_name` to camelCase to purge the underscores (to save annoyance
later when archiving files.)

To be super clear, somebody has to maintain a look up table - it
requires some manual intervention. And you can designed the `LUT` to
serve your own needs. But once the table is set up, it can persist
between R sessions, as we save them in the
`copernicus::copernicus_path("lut")` directory.

## Fetch data

Requests for data from the [Copernicus Marine Data
Store](https://data.marine.copernicus.eu/products) are as simple as
filtering the above LUT for the desired records (rows). We use
`fetch_andreas()` for this, but keep in mind that it is not always
possible to bind attributes (variables in a stars object) if variables
are of varying dimensionality - in particular if they have a depth
dimension when retrieved in native form. To resolve this, the returned
value is a list of stars objects grouped by dataset_id and depth.

``` r
x = lut |>
  dplyr::filter(name %in% c("uo", "vo", "thetao", "mlotst")) |>
  fetch_andreas(time = Sys.Date() + c(0,1), 
                bb = cofbb::get_bb("gom"))
x
```

    ## [[1]]
    ## stars object with 3 dimensions and 2 attributes
    ## attribute(s):
    ##                Min.     1st Qu.     Median       Mean    3rd Qu.      Max. NA's
    ## uo [m/s] -0.3278423 -0.05054242 0.01493066 0.02080005 0.06699045 1.4672947 5360
    ## vo [m/s] -0.8702940 -0.00322425 0.03606658 0.04744518 0.07709160 0.7855048 5360
    ## dimension(s):
    ##      from  to         offset    delta  refsys x/y
    ## x       1 109         -72.04  0.08333      NA [x]
    ## y       1  85          46.04 -0.08333      NA [y]
    ## time    1   2 2025-06-17 UTC   1 days POSIXct    
    ## 
    ## [[2]]
    ## stars object with 3 dimensions and 1 attribute
    ## attribute(s):
    ##                 Min.  1st Qu.   Median     Mean  3rd Qu.    Max. NA's
    ## thetao [°C] 9.432176 13.37275 14.86423 16.12185 19.16676 26.1502 5360
    ## dimension(s):
    ##      from  to         offset    delta  refsys x/y
    ## x       1 109         -72.04  0.08333      NA [x]
    ## y       1  85          46.04 -0.08333      NA [y]
    ## time    1   2 2025-06-17 UTC   1 days POSIXct    
    ## 
    ## [[3]]
    ## stars object with 3 dimensions and 1 attribute
    ## attribute(s):
    ##                Min.  1st Qu.   Median     Mean  3rd Qu.     Max. NA's
    ## mlotst [m] 6.700655 10.46302 10.46302 12.18801 11.29948 34.88145 5360
    ## dimension(s):
    ##      from  to         offset    delta  refsys x/y
    ## x       1 109         -72.04  0.08333      NA [x]
    ## y       1  85          46.04 -0.08333      NA [y]
    ## time    1   2 2025-06-17 UTC   1 days POSIXct

## Archiving data

You can download and archive data using the database functionality
provided in this package. There are a number of ways to manage suites of
data, this is just one fairly light weight method.

Here, we store data in a directory tree that starts with `region` and
`product` at it’s root. Within the `product` we divide by `year`,
`monthday`. Within in each `monthday` directory there are one or more
files uniquely named to provide complete identification of datasetid,
time, depth, period, variable and treatment. Each file contains one
raster for one variable at one depth and one time.

Here is an example of a file name that follows this pattern
`datasetid__date_time_depth_period_variable_treatment.tif`.

    nwa/GLOBAL_ANALYSISFORECAST_PHY_001_024/2022/0601/cmems_mod_glo_phy-cur_anfc_0.083deg_P1D-m__2022-06-01T000000_sur_day_uo_raw.tif

Here you can see that data set id and the rest of the identifiers are
separated by a double underscore to aid in programmatic parsing. Time
includes the hour in case we ever want to download the 6-hour data sets.
Depth is currently only expressed as “sur”, “bot” and “mld”, but use of
measured values such as “1.493” *etc.* is allowed but not currently
used. The treatment, `raw`, in this case means the values are as
downloaded, however, if you ever wanted to roll your own running mean
(say 8-day rolling mean) or some other statistic this naming system
provides the flexibility you will need.

**NOTE** Don’t forget to [set your root data
path](https://github.com/BigelowLab/copernicus?tab=readme-ov-file#configure-data-path.).

First we define an output path for the Gulf of Maine data. The path
isn’t created until data is written to it. Then we simply call fetch and
write individual GeoTIFF files into a database structure. Note that we
provide an identifier that provides the provenance of the data - that’s
what the `attrs` is all about. We receive, in turn, a table that serves
as a database.

    path = copernicus::copernicus_path("gom", "GLOBAL_ANALYSISFORECAST_PHY_001_024")
    x = x[[1]]
    attrs = attr(x, "andreas")
    dataset_id = attrs$dataset_id
    dates = attrs$time

Now we step through each variable in the stars object.

    db = lapply(names(x),
      function(nm){
        db = sprintf("%s__%s_%s_%s_%s_%s%s",
                     dataset_id,
                     format(dates, "%Y-%m-%dT000000"),
                     "sur", 
                     "day",
                     nm,
                     "raw",
                     ".tif") |>
          decompose_filename()
        for(i in seq_along(dates)) write_copernicus(dplyr::slice(x[nm], "time", i), 
                                                    dplyr::slice(db, i),
                                                    path)
        db
      }) |>
      dplyr::bind_rows()

Since this is the first time you have downloaded and archived data, be
sure to save the database.

    write_database(db, path)

# Working with Local Archives

## The local archive

The local archive is a collection of one or more products (and their
datasets) for a particular region of the world. For example, we have an
archive for the Northwest Atlantic (“nwa”). The archive is a directory
called “nwa” withing which there is one subdirectory for each product.
Within that product directory there may be series of subdirectories
organized by year (*e.g.* “2022”) and then by month-day (*e.g.* “0521”).
Within the month-day subdirectories there will be one or more GeoTIFF
files (one file for each variable).

At the product level you will find a text-based file called “database”.
It is a comman delimited file that (in theory) keeps track of the files
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

path = copernicus::copernicus_path("nwa", "GLOBAL_ANALYSISFORECAST_PHY_001_024")
db <- andreas::read_database(path) |>
  dplyr::glimpse()
```

    ## Rows: 10,623
    ## Columns: 7
    ## $ id        <chr> "cmems_mod_glo_phy_anfc_0.083deg_P1D-m", "cmems_mod_glo_phy_…
    ## $ date      <date> 2022-06-01, 2022-06-01, 2022-06-01, 2022-06-01, 2022-06-01,…
    ## $ time      <chr> "000000", "000000", "000000", "000000", "000000", "000000", …
    ## $ depth     <chr> "bot", "bot", "bot", "mld", "zos", "sur", "sur", "sur", "sur…
    ## $ period    <chr> "day", "day", "day", "day", "day", "day", "day", "day", "day…
    ## $ variable  <chr> "pbo", "sob", "tob", "mlotst", "zos", "uo", "vo", "so", "the…
    ## $ treatment <chr> "raw", "raw", "raw", "raw", "raw", "raw", "raw", "raw", "raw…

Now we can read in the files.

``` r
s = db |>
  dplyr::slice(1:20) |>
  andreas::read_andreas(path)
s
```

    ## stars object with 3 dimensions and 10 attributes
    ## attribute(s):
    ##                  Min.       1st Qu.        Median          Mean       3rd Qu.
    ## mlotst   5.6761260033  1.506548e+01  2.346613e+01  2.554730e+01  3.346862e+01
    ## pbo      2.3081991673  2.094185e+02  3.313038e+03  2.678870e+03  4.736214e+03
    ## so       0.4448610246  3.234900e+01  3.439930e+01  3.363174e+01  3.608277e+01
    ## sob      0.4443303049  3.452236e+01  3.489942e+01  3.422937e+01  3.491039e+01
    ## thetao  -1.4067404270  4.698266e+00  9.474497e+00  1.160192e+01  1.924285e+01
    ## tob     -3.4031331539  1.820810e+00  1.959050e+00  3.044149e+00  3.256880e+00
    ## uo      -1.4517654181 -8.856426e-02  5.151139e-03  6.125728e-02  1.330495e-01
    ## vo      -1.8710198402 -1.409831e-01 -3.540789e-02 -2.850122e-02  7.419115e-02
    ## wo      -0.0000221812 -7.153756e-07 -8.779010e-08 -1.162482e-07  5.274567e-07
    ## zos     -1.1081553698 -7.113359e-01 -5.263894e-01 -4.529118e-01 -3.107776e-01
    ##                 Max.  NA's
    ## mlotst  1.331467e+02 68576
    ## pbo     5.861988e+03 68576
    ## so      3.697755e+01 68576
    ## sob     3.633598e+01 68576
    ## thetao  2.762478e+01 68576
    ## tob     2.607124e+01 68576
    ## uo      1.896001e+00 68576
    ## vo      1.524730e+00 68576
    ## wo      2.278500e-05 68576
    ## zos     7.834340e-01 68576
    ## dimension(s):
    ##      from  to         offset    delta  refsys point x/y
    ## x       1 415         -77.04  0.08333  WGS 84 FALSE [x]
    ## y       1 243          56.71 -0.08333  WGS 84 FALSE [y]
    ## time    1   2 2022-06-01 UTC   1 days POSIXct    NA

It may look complicated, but this permits the storage of multiple
datasets per product, and is a very flexible system.

``` r
db |> dplyr::count(id, depth, variable)
```

    ## # A tibble: 10 × 4
    ##    id                                           depth variable     n
    ##    <chr>                                        <chr> <chr>    <int>
    ##  1 cmems_mod_glo_phy-cur_anfc_0.083deg_P1D-m    sur   uo        1063
    ##  2 cmems_mod_glo_phy-cur_anfc_0.083deg_P1D-m    sur   vo        1063
    ##  3 cmems_mod_glo_phy-so_anfc_0.083deg_P1D-m     sur   so        1063
    ##  4 cmems_mod_glo_phy-thetao_anfc_0.083deg_P1D-m sur   thetao    1062
    ##  5 cmems_mod_glo_phy-wcur_anfc_0.083deg_P1D-m   sur   wo        1062
    ##  6 cmems_mod_glo_phy_anfc_0.083deg_P1D-m        bot   pbo       1062
    ##  7 cmems_mod_glo_phy_anfc_0.083deg_P1D-m        bot   sob       1062
    ##  8 cmems_mod_glo_phy_anfc_0.083deg_P1D-m        bot   tob       1062
    ##  9 cmems_mod_glo_phy_anfc_0.083deg_P1D-m        mld   mlotst    1062
    ## 10 cmems_mod_glo_phy_anfc_0.083deg_P1D-m        zos   zos       1062

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
    ## mask    0.00000   0.0000    1.000    0.6599931    1.000    1.000     0
    ## deptho  7.92956 222.4752 3597.032 2797.7241103 4833.291 5727.917 34288
    ## dimension(s):
    ##   from  to offset    delta refsys point x/y
    ## x    1 415 -77.04  0.08333 WGS 84 FALSE [x]
    ## y    1 243  56.71 -0.08333 WGS 84 FALSE [y]
