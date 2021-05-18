################################################################################
# Makefile for building and running docker containers
#
# PRODUCTION version

# NOTE:  2020-07-07
# NOTE:  
# NOTE:  monitoring-data-ingest-v4:1.3.6 uses mazamascience/pwfslsmoke:1.2.112
# NOTE:  which updates data processing and QC for ESAM_MULTI monitors from 
# NOTE:  usfs.airsis.com

production_build:
	docker build --no-cache -t mazamascience/mazamasatelliteutils:0.4.16 -t mazamascience/mazamasatelliteutils:latest -f Dockerfile .

