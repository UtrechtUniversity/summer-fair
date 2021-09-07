FROM rocker/tidyverse:4.0.3
COPY . /usr/local/src/myscripts
WORKDIR  /usr/local/src/myscripts
# Install R packages
RUN install2.r --error \
  --deps TRUE \
    SPARQL \
    lme4
CMD ["Rscript", "SparqlQueries.R"]

