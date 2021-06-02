################################################################################
# Makefile for building and running docker containers
#
# PRODUCTION version

# NOTE:  2021-06-02

# NOTE:  You need to manually edit these version numbers and that in docker/Dockerfile to match
base_build:
	docker build --no-cache -t mazamascience/mazamasatelliteutils-base:1.0.0 -f Dockerfile-base .

base_publish:
	cd docker; docker login && docker push mazamascience/mazamasatelliteutils-base:1.0.0

production_build:
	docker build -t mazamascience/mazamasatelliteutils:0.4.18 -t mazamascience/mazamasatelliteutils:latest -f Dockerfile .

production_build_no-cache:
	docker build --no-cache -t mazamascience/mazamasatelliteutils:0.4.18 -t mazamascience/mazamasatelliteutils:latest -f Dockerfile .
