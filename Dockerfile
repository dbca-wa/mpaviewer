FROM rocker/geospatial:latest
# system libraries of general use
## install debian packages
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libxml2-dev \
    libcairo2-dev \
    libsqlite3-dev \
    libmariadbd-dev \
    libpq-dev \
    libssh2-1-dev \
    unixodbc-dev \
    libcurl4-openssl-dev \
    libssl-dev

## update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

#RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN Rscript -e 'install.packages(c("remotes", "leaflet", "leaflet.extras","here", "config", "cowplot", "thinkr", "shinydashboard", "shinydashboardPlus", "shinycssloaders", "shinyalert", "shinyWidgets", "leafgl", "golem", "ckanr", "scales", "data.table", "ggbreak", "ggh4x", "lutz"))'
RUN Rscript -e 'remotes::install_github("UWAMEGFisheries/GlobalArchive@f3c4315600a0396c7f72382b81d9073d346d431d")'

# COPY /mpaviewer/renv.lock ./renv.lock
## app folder
COPY /mpaviewer ./app

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838)"]
