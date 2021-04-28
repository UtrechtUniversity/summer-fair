FROM rocker/rstudio:3.6.3
COPY . /usr/local/src/myscripts
WORKDIR  /usr/local/src/myscripts
# Install R packages
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
  libxml2-dev \
  && install2.r --error \
  --deps TRUE \
    SPARQL
CMD ["Rscript", "SparqlQueries.R"]

