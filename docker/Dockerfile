FROM mazamascience/mazamasatelliteutils-base:1.0.0
MAINTAINER "Jonathan Callahan" jonathan@mazamascience.com

################################################################################

# Install AirFirePlots R package (while the Github repo is private)
WORKDIR /
COPY AirFirePlots_0.2.2.tar.gz /
RUN R CMD INSTALL AirFirePlots_0.2.2.tar.gz

################################################################################

# Install latest development versions of R packages
RUN installGithub.r \
    MazamaScience/MazamaSatelliteUtils
