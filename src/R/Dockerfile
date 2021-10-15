FROM rocker/tidyverse:4.1
COPY . /usr/local/src/myscripts
WORKDIR  /usr/local/src/myscripts
# Install R packages
RUN install2.r --error \
  --deps TRUE \
    SPARQL \
    lme4 \
    bbmle \
    magrittr \
    readxl \
    rje



CMD ["Rscript", "AlgorithmsTest.R"]

