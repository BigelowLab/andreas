#! /bin/sh

SCRIPT=/mnt/ecocast/corecode/R/andreas/inst/scripts/fetch_forecast.R
LOG=/mnt/ecocast/coredata/copernicus/log
CONFIGPATH=/mnt/s1/projects/ecocast/coredata/copernicus/config/
configs=(fetch-day-GLOBAL_ANALYSISFORECAST_BGC_001_028.yaml fetch-day-GLOBAL_ANALYSISFORECAST_PHY_001_024.yaml)

# CMD="Rscript ${SCRIPT} --config ${CONFIGPATH}${configs[0]} >> ${LOG}"
# eval ${CMD}

CMD="Rscript ${SCRIPT} --config ${CONFIGPATH}${configs[1]} >> ${LOG}"
eval ${CMD}
