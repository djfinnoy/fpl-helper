# From official rocker/shiny-verse file
FROM rocker/shiny:3.5.3

RUN apt-get update -qq && apt-get -y --no-install-recommends install \
  libxml2-dev \
  libcairo2-dev \
  libsqlite3-dev \
  libmariadbd-dev \
  libmariadb-client-lgpl-dev \
  libpq-dev \
  libssl-dev \
  libcurl4-openssl-dev \
  libssh2-1-dev \
  unixodbc-dev \
  && R -e "source('https://bioconductor.org/biocLite.R')" \
  && install2.r --error \
    --deps TRUE \
    tidyverse \
    dplyr \
    devtools \
    formatR \
    remotes \
    selectr \
    caTools

# Install packages required by my shiny apps
RUN R -e "install.packages(c( \
    'plotly', \
    'shinydashboard', \
    'shinyWidgets', \
    'jsonlite', \
    'DT', \
    'ggthemes', \
    'RColorBrewer' \
  ), repos='http://cran.rstudio.com/')"


# Copy apps
RUN rm -r /srv/shiny-server && mkdir /srv/shiny-server
ADD shinyapps/fpl-helper /srv/shiny-server/

# Config
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
COPY shiny-server.sh /usr/bin/shiny-server.sh
RUN ["chmod", "+x", "/usr/bin/shiny-server.sh"]
CMD ["/usr/bin/shiny-server.sh"]
