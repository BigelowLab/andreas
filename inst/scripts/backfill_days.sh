#!/bin/sh

# Backfill missing days in the copernicus repository.  Run this once a week (or there abouts)

# The 4 configs we should run on are in /mnt/s1/projects/ecocast/coredata/copernicus/config/
# world-GLOBAL_ANALYSISFORECAST_BGC_001_028.yaml
# world-GLOBAL_MULTIYEAR_BGC_001_029.yaml
# chfc-GLOBAL_ANALYSISFORECAST_PHY_001_024.yaml
# chfc-GLOBAL_MULTIYEAR_PHY_001_030.yaml
  
module load copernicus

SCRIPT="/mnt/ecocast/corecode/R/andreas/inst/scripts/backfill_days.R"
ROOT="/mnt/s1/projects/ecocast/coredata/copernicus/config/"
CFGS=("world-GLOBAL_ANALYSISFORECAST_BGC_001_028.yaml" 
      "world-GLOBAL_MULTIYEAR_BGC_001_029.yaml" 
      "chfc-GLOBAL_ANALYSISFORECAST_PHY_001_024.yaml"
      "chfc-GLOBAL_MULTIYEAR_PHY_001_030.yaml")

for cfg in "${CFGS[@]}"; do
    cmd="Rscript $SCRIPT --config $ROOT$cfg"
    #echo $cmd
    eval $cmd
done
