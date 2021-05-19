FROM mazamascience/pwfslsmoke:1.2.112
MAINTAINER "Jonathan Callahan" jonathan@mazamascience.com

# Install Linux packages
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        libmagick++-dev \
        ffmpeg

################################################################################

# Install R packages required for ...
RUN install2.r --error \
    -r "https://cran.rstudio.com" \
    ggimage
#    ncdf4 \
#    magick
    
################################################################################

# Install AirFirePlots R package (while the Github repo is private)
WORKDIR /
COPY AirFirePlots_0.2.2.tar.gz /
RUN R CMD INSTALL AirFirePlots_0.2.2.tar.gz

################################################################################

# Install latest development versions of R packages
RUN installGithub.r \
    MazamaScience/MazamaCoreUtils \
    MazamaScience/MazamaSatelliteUtils \
    MazamaScience/PWFSLSmoke
