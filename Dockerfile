FROM rocker/shiny:latest

RUN R -e "install.packages(c('shiny','DT','DBI','duckdb','ggplot2','data.table','readr','dplyr','tidyr','glue','jsonlite'), repos='https://cloud.r-project.org')"

COPY . /srv/shiny-server/qres-db
RUN chown -R shiny:shiny /srv/shiny-server/qres-db

EXPOSE 3838
CMD ["/usr/bin/shiny-server"]
