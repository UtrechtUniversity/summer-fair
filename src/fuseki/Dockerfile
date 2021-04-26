FROM rocker/tidyverse:4.0.0
COPY . /usr/local/src/myscripts
WORKDIR  /usr/local/src/myscripts
# Install R packages
RUN install2.r --error \
    SPARQL 
CMD ["Rscript", "SparqlQueries.R"]

