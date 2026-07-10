# transfer settings from PRODUCT.csv to region-PRODUCT.csv

library(dplyr)
library(andreas)
library(readr)

readr::write_csv(lut, 
                 copernicus_path("lut", sprintf("%s-%s.csv",region,x)))


x = tibble(
  new_lut = c("chfc-GLOBAL_ANALYSISFORECAST_PHY_001_024", 
               "chfc-GLOBAL_MULTIYEAR_PHY_001_030", 
               "world-GLOBAL_ANALYSISFORECAST_BGC_001_028", 
               #"world-GLOBAL_ANALYSISFORECAST_PHY_001_024", 
               "world-GLOBAL_MULTIYEAR_BGC_001_029"),
  product = strsplit(new_lut, "-", fixed = TRUE) |>
    sapply("[[",2),
  region = strsplit(new_lut, "-", fixed = TRUE) |>
    sapply("[[",1) )

lut = rowwise(x) |>
  group_map(
    function(row, key){
      new = create_lut(region = row$region, product = row$product, save_lut = FALSE)
      old = read_csv(copernicus_path("lut", sprintf("%s.csv",row$product)),
                     show_col_types = FALSE) |>
        filter(fetch == "yes")
      for (i in seq_len(nrow(old))){
        ix = new$dataset_id == old$dataset_id[i] & 
             new$standard_name == old$standard_name[i]
        if (any(ix)){
          ix = which(ix)
          new$depth[ix] <- old$depth[i]
          new$fetch[ix] <- old$fetch[i]
          new$mindepth[ix] <- old$mindepth[i]
          new$maxdepth[ix] <- old$maxdepth[i]
        }

      }
      
      readr::write_csv(new, 
         copernicus_path("lut", sprintf("%s-%s.csv",row$region,row$product)))
      new
    } )

