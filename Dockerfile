FROM gcr.io/gcer-public/shiny-googleauthrdemo:latest
MAINTAINER Mark Edmondson (r@sunholo.com)

# install R package dependencies
RUN apt-get update && apt-get install -y \
    ##### ADD YOUR DEPENDENCIES
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2 \
    libxml2-dev \
    python3 \
    bedtools \
    gsutil && \
    apt-get clean;

RUN install2.r --error \
    -r 'http://cran.rstudio.com' \
    ##### ADD YOUR CRAN PACKAGES
    devtools \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

RUN Rscript -e 'devtools::install_github("wjawaid/enrichR")'
RUN Rscript -e 'devtools::install_github("PhanstielLab/bedtoolsr")'

## Install packages from CRAN
RUN install2.r --error \
    -r 'http://cran.rstudio.com' \
    ##### ADD YOUR CRAN PACKAGES
    ggplot2 \
    shiny \
    scales \
    readr \
    readxl \
    plotly \
    shinydashboard \
    stringr \
    gtools \
    RColorBrewer \
    shinycssloaders \
    shinyjs \
    tidyr \
    plyr \
    dplyr \
    enrichR \
    DT \
    shinyBS \
    shinythemes \
    shinyWidgets \
    shinydashboardPlus \
    shinyBS \
    writexl \
    tinytex \
    rmarkdown \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

## copy your shiny app folder below
COPY ./clincnvtemp1/ /srv/shiny-server/
