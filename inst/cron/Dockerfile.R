FROM rocker/verse:4.2.1
RUN apt-get update && apt-get install -y  rclone gdal-bin git-core libcairo2-dev libcurl4-openssl-dev libfribidi-dev libgdal-dev libgeos-dev libgeos++-dev libgit2-dev libharfbuzz-dev libicu-dev libpng-dev libproj-dev libssl-dev libtiff-dev libudunits2-dev libxml2-dev make pandoc pandoc-citeproc zlib1g-dev && rm -rf /var/lib/apt/lists/*
  RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
  RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("magrittr", upgrade="never", version = "2.0.3")'
RUN Rscript -e 'remotes::install_version("rlang", upgrade="never", version = "1.0.4")'
RUN Rscript -e 'remotes::install_version("glue", upgrade="never", version = "1.6.2")'
RUN Rscript -e 'remotes::install_version("purrr", upgrade="never", version = "0.3.4")'
RUN Rscript -e 'remotes::install_version("dplyr", upgrade="never", version = "1.0.9")'
RUN Rscript -e 'remotes::install_version("scales", upgrade="never", version = "1.2.0")'
RUN Rscript -e 'remotes::install_version("knitr", upgrade="never", version = "1.39")'
RUN Rscript -e 'remotes::install_version("stringr", upgrade="never", version = "1.4.0")'
RUN Rscript -e 'remotes::install_version("rmarkdown", upgrade="never", version = "2.14")'
RUN Rscript -e 'remotes::install_version("shiny", upgrade="never", version = "1.7.2")'
RUN Rscript -e 'remotes::install_version("testthat", upgrade="never", version = "3.1.4")'
RUN Rscript -e 'remotes::install_version("fs", upgrade="never", version = "1.5.2")'
RUN Rscript -e 'remotes::install_version("ggplot2", upgrade="never", version = "3.3.6")'
RUN Rscript -e 'remotes::install_version("tidyr", upgrade="never", version = "1.2.0")'
RUN Rscript -e 'remotes::install_version("devtools", upgrade="never", version = "2.4.3")'
RUN Rscript -e 'remotes::install_version("shinydashboard", upgrade="never", version = "0.7.2")'
RUN Rscript -e 'remotes::install_version("leaflet", upgrade="never", version = "2.1.1")'
RUN Rscript -e 'remotes::install_version("leaflet.extras", upgrade="never", version = "1.0.0")'
RUN Rscript -e 'remotes::install_version("here", upgrade="never", version = "1.0.1")'
RUN Rscript -e 'remotes::install_version("config", upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("thinkr", upgrade="never", version = "0.16")'
RUN Rscript -e 'remotes::install_version("data.table", upgrade="never", version = "1.14.2")'
RUN Rscript -e 'remotes::install_version("shinydashboardPlus", upgrade="never", version = "2.0.3")'
RUN Rscript -e 'remotes::install_version("shinycssloaders", upgrade="never", version = "1.0.0")'
RUN Rscript -e 'remotes::install_version("shinyalert", upgrade="never", version = "3.0.0")'
RUN Rscript -e 'remotes::install_version("shinyWidgets", upgrade="never", version = "0.7.2")'
RUN Rscript -e 'remotes::install_version("readr", upgrade="never", version = "2.1.2")'
RUN Rscript -e 'remotes::install_version("rgdal", upgrade="never", version = "1.5-32")'
RUN Rscript -e 'remotes::install_version("leafgl", upgrade="never", version = "0.1.1")'
RUN Rscript -e 'remotes::install_version("googledrive", upgrade="never", version = "2.0.0")'
RUN Rscript -e 'remotes::install_version("golem", upgrade="never", version = "0.3.3")'
RUN Rscript -e 'remotes::install_version("forcats", upgrade="never", version = "0.5.1")'
RUN Rscript -e 'remotes::install_version("ckanr", upgrade="never", version = "0.6.0")'
RUN Rscript -e 'remotes::install_version("httr", upgrade="never", version = "1.4.3")'
RUN Rscript -e 'remotes::install_github("r-spatial/sf@e0eeaa1e70522f4a7cdb439909e6151dbae5b029")'
RUN Rscript -e 'remotes::install_github("UWAMEGFisheries/GlobalArchive@f3c4315600a0396c7f72382b81d9073d346d431d")'
RUN mkdir /app
ADD . /app
WORKDIR /app
RUN R -e 'remotes::install_local(upgrade="never")'
EXPOSE 80
CMD R -e "options('shiny.port'=80,shiny.host='0.0.0.0');mpaviewer::run_app()"

# Mount a volume to /app/inst that is shared with turtleviewer2
# A second volume /app/config will hold an .Renviron file
RUN mkdir -p /app/inst/data && echo "WASTDR_API_URL=NOTSET" >> /app/inst/data/.Renviron

# Initiate cronjob
RUN crontab inst/cron/cronjob

# Run command on container startup
CMD echo "start cronjob" &&\
(cron) &&\
echo "init logs" &&\
touch /app/inst/cron.log &&\
tail -f /app/inst/cron.log
