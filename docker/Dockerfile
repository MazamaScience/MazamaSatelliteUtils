FROM mazamascience/pwfslsmoke:1.2.112
MAINTAINER "Jonathan Callahan" jonathan@mazamascience.com

################################################################################
# Install packages required for ...
#RUN install2.r --error \
#    -r "https://cran.rstudio.com" \
#    ...

## Install latest development versions of packages
#RUN installGithub.r \
#    MazamaScience/MazamaCoreUtils \
#    MazamaScience/PWFSLSmoke

################################################################################
# Install python3 and log-status.py to send status to status-log.airfire.org

RUN apt-get update && apt-get install -y aptitude && aptitude install -y python3

COPY log-status.py /usr/local/bin

