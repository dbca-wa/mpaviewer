FROM ghcr.io/dbca-wa/mpaviewer:latest
# Since this Dockerfile builds from the last image, it uses mpaviewer code from
# the previous tag.

# Mount a volume to /app/inst/data that is shared with mpaviewer
RUN mkdir -p /app/inst/data && echo "WASTDR_API_URL=NOTSET" >> /app/inst/data/.Renviron

# Initiate cronjob
RUN crontab inst/cron/cronjob

# Run command on container startup
CMD echo "start cronjob" && (cron) && echo "init logs" && touch /app/inst/cron.log && tail -f /app/inst/cron.log
