################################################################################
# Makefile for building and running docker containers
#
# PRODUCTION version

# NOTE:  2021-06-02

AirFirePlots_tarball:
	cd ..; R CMD build --resave-data .
	mv ../AirFirePlots_0.2.2.tar.gz .

production_build:
	docker build -t mazamascience/mazamasatelliteutils:0.4.18 -t mazamascience/mazamasatelliteutils:latest -f Dockerfile .

production_build_no-cache:
	docker build --no-cache -t mazamascience/mazamasatelliteutils:0.4.18 -t mazamascience/mazamasatelliteutils:latest -f Dockerfile .
