# one time renaming of the files into the pattern
# date_period_var_depth_treatment.tif

stop("do not run this unless you know what is going on")
suppressPackageStartupMessages({
  library(rlang)
  library(copernicus)
  library(stars)
  library(dplyr)
  library(charlier)
  library(argparser)
  library(cofbb)
  library(yaml)
})


product = "global-analysis-forecast-phy-001-024"
dataset_id = "cmems_mod_glo_phy-cur_anfc_0.083deg_P1D-m"
reg = "nwa"
path = copernicus_path(product, reg)
ff = list.files(path, recursive = TRUE, pattern = glob2rx("*.tif"),
                full.names = TRUE)

ss = sub(".tif", "", basename(ff), fixed = TRUE) |>
  strsplit("_", fixed = TRUE)
s = do.call(rbind, ss) |>
  as_tibble(.name_repair =  "minimal") |>
  set_names(c("date", "var", "depth")) |>
  mutate(period = "day", .after = 1) |>
  mutate(treatment = "none", .after = last_col())

ofile = sprintf("%s_%s_%s_%s_%s.tif",
                s$date, 
                s$period, 
                s$var,
                s$depth,
                s$treatment)

newfile = file.path(dirname(ff), ofile)

ok = file.rename(ff, newfile)
