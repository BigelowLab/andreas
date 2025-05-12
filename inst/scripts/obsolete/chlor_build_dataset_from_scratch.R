library(copernicus)
library(stars)
library(dplyr)

# https://my.cmems-du.eu/thredds/dodsC/c3s_obs-oc_glo_bgc-plankton_my_l4-multi-4km_P1M
base_uri = "https://my.cmems-du.eu/thredds/dodsC"
product_id = "c3s_obs-oc_glo_bgc-plankton_my_l4-multi-4km_P1M"
PATH = copernicus::copernicus_path(product_id,
                                   "world")
if(!(dir.exists(PATH))) ok <- dir.create(PATH, recursive = TRUE)

x <- copernicus::open_nc(product_id = product_id, base_uri = base_uri)
times <- as.Date(get_time(x))
res = get_res(x)
lon = get_lon(x)
xlim = range(lon) + c(-res[1],res[1])/2
nx = length(lon)
lat = get_lat(x)
ylim = range(lat) + c(-res[2],res[2])/2
ny = length(lat)
varid = 'CHL'

bb <- sf::st_bbox(c(xmin = xlim[1], ylim = ylim[1], xmax = xlim[2], ymax = ylim[2]),
                  crs = 4326)

for (i in seq_along(times)){
  path = file.path(PATH,
                   format(times[i], "%Y"))
  if (!dir.exists(path)) ok = dir.create(path, recursive = TRUE)
  filename = format(times[i], "%Y-%m-01.tif")
  ofile = file.path(path, filename)
  m <- ncdf4::ncvar_get(x, varid = varid,
                        start = c(1, 1, i),
                        count = c(nx, ny, 1))
  s <- stars::st_as_stars(bb,
                          values = m,
                          nx = nx,
                          ny = ny,
                          xlim = xlim,
                          ylim = ylim) |>
    stars::write_stars(ofile)
  cat("wrote", ofile, "\n")
}
cat("done\n")







