suppressPackageStartupMessages({
  library(copernicus)
  library(dplyr)
})


# Somehow I missed "myint" interim datasets in the original production of the 
# "MUTLIYEAR" lut.  Well, technically, I omitted
# those datasets, but eitherway it was unwitting.  Here I try to scab on 
# the missing "myint" datasets without corrupting the existing dataset
product = "GLOBAL_MULTIYEAR_PHY_001_030"
catalog = read_product_catalog()
newlut = create_lut(product, catalog = catalog, save_lut = FALSE) |>
  filter(grepl("myint", .data$dataset_id))
oldlut = product_lut(product)

lut = bind_rows(oldlut, newlut)
