FROM mazamascience/pwfslsmoke:1.2.112
MAINTAINER "Jonathan Callahan" jonathan@mazamascience.com

################################################################################
# Install packages required for ...
RUN install2.r --error \
    -r "https://cran.rstudio.com" \
    ncdf4

## Install latest development versions of packages
RUN installGithub.r \
    MazamaScience/MazamaCoreUtils \
    MazamaScience/MazamaSatelliteUtils \
    MazamaScience/PWFSLSmoke \

################################################################################
