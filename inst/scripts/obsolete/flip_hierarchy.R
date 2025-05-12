# I think I made a mistake (perhaps quite a few!) organizing by
# copernicus_path(product, region) which yields things like 
# /mnt/s1/projects/ecocast/coredata/copernicus/GLOBAL_ANALYSISFORECAST_PHY_001_024/nwa
# which doesn't make sense on 2 fronts: (a) a user can simply copy a single 
# regional directory and (b) it doesn't match the organization of the cmems server
#
# So this script attempts to flip the hierarchy so the the above is 
# copernicus_path(region, product) ala 
# /mnt/s1/projects/ecocast/coredata/copernicus/nwa/GLOBAL_ANALYSISFORECAST_PHY_001_024

stop("do not run this script - trust me., it's a one time thing")
library(fs)
library(copernicus)
products = c("GLOBAL_ANALYSISFORECAST_PHY_001_024",
             "GLOBAL_MULTIYEAR_BGC_001_029",
             "GLOBAL_MULTIYEAR_PHY_001_030",
             "GLOBAL_ANALYSISFORECAST_BGC_001_028")

# 
# for each product
#  look inside for regional folders
#  for each region
#    newpath = make_path(copernicus_path(region, product))
#    copy_everything(from = copernicus_path(product, region),
#                    to = copernicus_path(region, product))

for (product in products){
  product_path = copernicus_path(product)
  regions = dir_ls(product_path) |> basename()
  for (region in regions){
    region_path = copernicus_path(region, product) |>
      make_path()
    ok = dir_copy(copernicus_path(product, region), region_path, overwrite = TRUE)
  } # regions
} # products
