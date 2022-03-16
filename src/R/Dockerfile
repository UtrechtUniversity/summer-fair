FROM rocker/tidyverse:4.1.2
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
    rje \
    meta \
    metafor 
CMD ["Rscript", "TutorialAlgorithmDocker.R"]