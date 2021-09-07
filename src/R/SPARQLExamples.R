#install.packages("devtools")
#devtools::install_github("ropensci/rdflib")
library(magrittr)
library(rdflib)

rdf <- rdf_parse("docs/ontology/trans_experiment_populated.owl", format = 'rdfxml')


#SPARQL query to get all experiment dates
sparql <- " PREFIX tr: <http://purl.org/NET/c4dm/trans_experiment.owl#>
            PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
            PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
            SELECT distinct ?date
            WHERE { ?z tr:experimentDay ?date.}
            ORDER BY (?date) "

dates <- rdf_query(rdf, sparql)

#SPARQL query to get the experiment dates of SWAB sample
sparql <- " PREFIX tr: <http://purl.org/NET/c4dm/trans_experiment.owl#>
            PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
            PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
            SELECT distinct ?date
            WHERE { ?z tr:experimentDay ?date;
                       rdf:type tr:Measurement;
                       rdf:type tr:Sample;
                       tr:hasType tr:Swab.}"

system.time(dates.swab <- rdf_query(rdf, sparql))


#SPARQL query to get all information about swab
sparql <- " PREFIX tr: <http://purl.org/NET/c4dm/trans_experiment.owl#>
            PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
            PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
            SELECT ?chicken ?date ?sample
            WHERE {
              ?z rdf:type tr:Measurement;
                 rdf:type tr:Sample;
                 tr:hasType tr:Swab;
                 tr:experimentDay ?date;
                 tr:involves ?chicken;
                 tr:sample ?sample}"

swab.data <- rdf_query(rdf, sparql)
