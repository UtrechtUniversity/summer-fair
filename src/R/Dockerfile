FROM rocker/tidyverse:4.0.3
COPY src/R /usr/local/src/myscripts
WORKDIR  /usr/local/src/myscripts
# Install R packages
RUN install2.r --error \
  --deps TRUE \
    SPARQL \
    lme4 \
    jsonlite \
    plumber \
    bbmle \
    magrittr \
    readxl \
    rje
CMD ["Rscript", "Untitled.R"]

