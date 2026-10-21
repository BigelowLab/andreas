#! /bin/sh

SCRIPT=/mnt/ecocast/corecode/R/andreas/inst/scripts/fetch_forecast.R
LOG=/mnt/ecocast/coredata/copernicus/log-forecast
CONFIGPATH=/mnt/s1/projects/ecocast/coredata/copernicus/config/
configs=(world-GLOBAL_ANALYSISFORECAST_BGC_001_028.yaml 
         chfc-GLOBAL_ANALYSISFORECAST_PHY_001_024.yaml
         world-GLOBAL_ANALYSISFORECAST_PHY_001_024.yaml
         sab-GLOBAL_ANALYSISFORECAST_PHY_001_024.yaml
         jordanbasin-GLOBAL_ANALYSISFORECAST_PHY_001_024.yaml)

now=`date`
echo "[${now}] fetch_forecast.sh" >> ${LOG}

for cfg in ${configs[@]}
do
  CMD="Rscript ${SCRIPT} --config ${CONFIGPATH}${cfg} --period day >> ${LOG}"
  echo "####"
  echo "##  ${cfg}" >> ${LOG}
  echo "####"
  echo "## ${CMD}" >> ${LOG}
  eval ${CMD}
done
now=`date`
echo "[${now}] fetch_forecast.sh complete" >> ${LOG}

