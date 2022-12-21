FROM rocker/tidyverse:4.1.2
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

COPY docker-entrypoint /jena-fuseki

COPY --from=stain/jena-fuseki:4.0.0 /jena-fuseki /jena-fuseki

RUN apt-get update && apt-get install -y --no-install-recommends openjdk-11-jre python3-pip curl

COPY src/create_ontology/map_ontology .
COPY src/R/ src/R/

RUN pip3 install -r requirements.txt
ENTRYPOINT ["/jena-fuseki/docker-entrypoint.sh"]



