# here we assemble a lut to record the known start/stop dates for each product/dataset.  
# As it turns out, datasets within a product group have varying start and stop service dates.
# Sigh

stop("never never never source a file if you don't know what it does")
suppressPackageStartupMessages({
  library(copernicus)
  library(dplyr)
  library(readr)
})

path = "/mnt/s1/projects/ecocast/corecode/R/copernicus/inst/lut"
ff = list.files(path, pattern = glob2rx("GLOBAL_*.csv"), full.names = TRUE)
x = readr::read_csv(ff,
                    col_types = cols(
                      product_id = col_character(),
                      title = col_character(),
                      dataset_id = col_character(),
                      dataset_name = col_character(),
                      short_name = col_character(),
                      standard_name = col_character(),
                      units = col_character(),
                      depth = col_character(),
                      fetch = col_character(),
                      mindepth = col_double(),
                      maxdepth = col_double()
                    )) |>
  dplyr::select(product_id, dataset_id) |>
  dplyr::distinct() |>
  dplyr::mutate(start_date = NA_character_, end_date = NA_character_) |>
  readr::write_csv(file.path(path, "dataset_metadata.csv"))



X = read_product_catalog() |>
  select(product_id, dataset_id) |>
  filter(grepl("GLOBAL", .data$product_id), grepl("myint", .data$dataset_id)) |>
  distinct()
