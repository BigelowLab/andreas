# usage: anomlay_monthly.R [--] [--help] [--region REGION] [--start START] [--end END]
# 
# Compute monthly anomaly maps
# 
# flags:
#   -h, --help    show this help message and exit
# 
# optional arguments:
#   -r, --region  region name [default: chfc]
#   -s, --start   start date (default one year before today) [default: 2025-07-01]
#   -e, --end     end date (default, 28 days before today) [default: 2026-07-01]


suppressPackageStartupMessages({
  library(andreas)
  library(dplyr)
  library(ggplot2)
  library(stars)
  library(argparser)
  library(charlier)
})


first_of_month = function(x = Sys.Date(), form = c("Date", "string")[1]){
  x = as.Date(format(x, format = "%Y-%m-01"))
  if (tolower(form[1]) == "string") x = format(x, format = "%Y-%m-%d")
  x
}
today = Sys.Date()
end_month = first_of_month(today - 28, form = "string")
start_month = first_of_month(today - 365, form = "string")

args = argparser::arg_parser("Compute monthly anomaly maps",
                      name = "anomaly_monthly.R", 
                      hide.opts = TRUE) |>
  argparser::add_argument("--region",
               help = 'region name: "chfc" or "world"',
               default = 'chfc') |>
  argparser::add_argument("--start",
                help = "start date (default one year before today) but must be before 2022-06-01",
                default = start_month) |>
  argparser::add_argument("--end",
                help  = "end date (default, today)",
                default = end_month) |>
  parse_args()

paths = switch(tolower(args$region),
      "chfc" = c(anfc = "chfc/GLOBAL_ANALYSISFORECAST_PHY_001_024", 
                 my = "chfc/GLOBAL_MULTIYEAR_PHY_001_030"),
      "world" = c(anfc = "world/GLOBAL_ANALYSISFORECAST_PHY_001_024", 
                  my = "world/GLOBAL_MULTIYEAR_BGC_001_029"),
      stop("region not known:", args$region))

PATHS = sapply(paths, andreas::copernicus_path)
DBS = sapply(PATHS, andreas::read_database, simplify = FALSE)

baseline = andreas::read_andreas(DBS[['my']] |>
                                   dplyr::filter(period == "month-clim")|>
                                   dplyr::arrange(.data$date), 
                                 PATHS[['my']])  |>
  stars::st_set_dimensions(3, value = 1:12, names = "month")

dates = seq(from = first_of_month(args$start),
            to = first_of_month(args$end),
            by = "month")


# This is just for now so I can get things going
# the use cannot request a time before the beginning of the 2022-06-01 
# ANFC start date
if (min(dates) < as.Date("2022-06-01")){
  stop("this is only configured for analysis-forecast, not multi-year")
}

db = DBS[['anfc']] |>
  dplyr::filter(period == "month",
                treatment == "mean",
                date %in% dates,
                .name %in% names(baseline)) |>
  dplyr::mutate(mon = as.numeric(format(.data$date, '%m'))) 

db = db |>
  dplyr::rowwise() |>
  dplyr::group_map(
    function(row, key){
      #cat(format(row$date, format = "%Y-%m-%d"), row$.name, "\n")
      s = andreas::read_andreas(row, PATHS[['anfc']]) |>
        andreas::set_point()
      b = dplyr::slice(baseline[row$.name], "month", row$mon) |>
        stars::st_warp(s)
      row = row |>
        dplyr::mutate(treatment = "anom") 
      ofile = andreas::compose_filename(row, PATHS[['anfc']])
      #cat(ofile, "\n")
      anom = (s - b) |>
        stars::write_stars(ofile)
      row
    }, .keep = TRUE) |>
  dplyr::bind_rows() |>
  dplyr::select(-dplyr::all_of("mon")) |>
  andreas::append_database(PATHS[['anfc']])


