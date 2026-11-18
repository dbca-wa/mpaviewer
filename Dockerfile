# Install a base R version - update here when we update the version used by renv
FROM rocker/r-ver:4.6.2

# Install debian packages
RUN apt-get update -qq && apt-get install -y --no-install-recommends \
    cmake \
    gdal-bin \
    git \
    libcurl4-openssl-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libfribidi-dev \
    libgdal-dev \
    libgeos-dev \
    libharfbuzz-dev \
    libicu-dev \
    libjpeg-dev \
    libpng-dev \
    libproj-dev \
    libsqlite3-dev \
    libssl-dev \
    libtiff-dev \
    libudunits2-dev \
    libuv1-dev \
    libwebp-dev \
    libx11-dev \
    libxml2-dev \
    make \
    pandoc \
    zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

# update system libraries
# Currently updates to the latest version of every package
# I need to work out if this is the best protocol
# The other option is so pin specific versions above
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

WORKDIR /app

RUN R -e "install.packages('renv', repos = c(CRAN = 'https://r-project.org'))"

COPY renv.lock renv.lock

RUN R -e "renv::restore(library = .libPaths()[1], prompt = FALSE)"

# Working directory has already been set, so changed the copy command
COPY . .

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/app.R', host = '0.0.0.0', port = 3838)"]
