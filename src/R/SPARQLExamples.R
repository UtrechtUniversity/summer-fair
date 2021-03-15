#install.packages("devtools")
#devtools::install_github("ropensci/rdflib")
library(magrittr)
library(rdflib)

rdf <- rdf_parse("docs/ontology/trans_experiment_populated_sample.ttl", format = 'turtle')
#rdf <- rdf_parse("docs/ontology/trans_experiment_populated.ttl", format = 'turtle')

get.dates.swab <- function(){
    #SPARQL query to get the experiment dates of SWAB sample
    sparql <- " PREFIX tr: <http://purl.org/NET/c4dm/trans_experiment#>
              PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
              PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
              SELECT distinct ?date
              WHERE { ?z tr:experimentDay ?date;
                         rdf:type tr:Measurement;
                         tr:hasType tr:Swab.
                      }"

    return(rdf_query(rdf, sparql))
}

create.sample.value <- function(current.date, next.date){
    #SPARQL query to create final sample.
    #only two positive samples after each other are really positive
    sparql <- sprintf(" PREFIX tr: <http://purl.org/NET/c4dm/trans_experiment#>
              PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
              PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
              SELECT ?ex1 ?result
              WHERE { ?ex1 tr:experimentDay '%s';
                         rdf:type tr:Measurement;
                         tr:sample ?sample1;
                         tr:hasType tr:Swab;
                         tr:involves ?chicken.

                     ?ex2 tr:experimentDay '%s';
                         rdf:type tr:Measurement;
                         tr:sample ?sample2;
                         tr:hasType tr:Swab;
                         tr:involves ?chicken
                     BIND ( IF( ?sample1 != '0' && ?sample2 !='0', '1','0' ) AS ?result)

                    }", toString(current.date), toString(next.date))

    final.swab <- rdf_query(rdf, sparql)
    # loop over the result and assign final sample value
    for (i in 1 : nrow(final.swab)) {
        ex <- final.swab[i, 1]$ex1
        result <- final.swab[i, 2]$result

        rdf_add(rdf,
        subject = toString(ex),
        predicate = "http://purl.org/NET/c4dm/trans_experiment#finalSample",
        object = result)
    }
}

aggregate.data <- function(){
    #SPARQL query to get the aggregate the data
    #should only be run if we  created the triple
    #with final sample create.sample.value
    sparql1 <- " PREFIX tr: <http://purl.org/NET/c4dm/trans_experiment#>
              PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
              PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
              SELECT distinct ?date ?isolator (count(?chicken) as ?chickens)
              ?treatment (sum(?sample) as ?infected)
              WHERE { ?z tr:experimentDay ?date;
                         rdf:type tr:Measurement;
                         tr:hasType tr:Swab;
                         tr:finalSample ?sample;
                         tr:involves ?chicken.
                         ?chicken tr:group ?isolator;
                          tr:treatment ?treatment.}

                        group by ?isolator ?date
                        order by ?date ?isolator"
    data_to_aggergate1 <- rdf_query(rdf, sparql1)
    return(data_to_aggergate1)
}

swab.data <- function(){
    #SPARQL query to get the swab data
    sparql1 <- "  PREFIX tr: <http://purl.org/NET/c4dm/trans_experiment#>
              PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
              PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
              PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
              SELECT distinct ?date ?isolator ?chicken
              ?treatment  ?sample
              WHERE { ?z tr:experimentDay ?date;
                         rdf:type tr:Measurement;
                         tr:hasType tr:Swab;
                         tr:sample ?sample;
                         tr:involves ?chicken.
                         ?chicken tr:group ?isolator;
                         tr:treatment ?treatment.
              }
                        order by ?date ?isolator"
    swab.data <- rdf_query(rdf, sparql1)
    return(swab.data)
}


chicken.info <- function(){
    #SPARQL query to get the information about the chicken
    sparql <- " PREFIX tr: <http://purl.org/NET/c4dm/trans_experiment#>
              PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
              PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
              SELECT  ?chicken ?group ?sex ?treatment ?type
              WHERE { ?chicken  tr:group ?group ;
                                tr:sex ?sex ;
                                tr:treatment ?treatment ;
                                tr:type ?type.}"
    chicken.data <- rdf_query(rdf, sparql)
    return(chicken.data)
}