FROM ghcr.io/dbca-wa/mpaviewer:latest
# Since this Dockerfile builds from the last image, it uses mpaviewer code from
# the previous tag.

# Install cron
RUN apt-get update && \
    apt-get install -y --no-install-recommends cron && \
    rm -rf /var/lib/apt/lists/*

# Mount a volume to /app/inst/data that is shared with mpaviewer
RUN mkdir -p /app/inst/data && echo "WASTDR_API_URL=NOTSET" >> /app/inst/data/.Renviron

# Initiate cronjob
RUN crontab /app/inst/cron/cronjob

# Run command on container startup
CMD echo "start cronjob" && (cron) && echo "init logs" && touch /app/inst/cron.log && tail -f /app/inst/cron.log
