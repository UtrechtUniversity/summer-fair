# Title     : TODO
# Objective : TODO
# Created by: elena.slavco
# Created on: 2021-03-22
get.dates.swab <- function(){
    #SPARQL query to get the experiment dates of SWAB sample
    sparql <- " PREFIX tr: <http://purl.org/NET/c4dm/trans_experiment#>
              PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
              PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
              SELECT DISTINCT ?date
              WHERE { ?z tr:experimentDay ?date;
                         rdf:type tr:Measurement;
                         tr:hasType tr:Swab.
                      }"

    return( sparql)
}

create.sample.value <- function(current.date, next.date){
    # SPARQL query to create final sample.
    # takes as an argument 2 dates of the experiment and compares samples
    # only two positive samples after each other are really positive
    sparql <- sprintf(" PREFIX tr: <http://purl.org/NET/c4dm/trans_experiment#>
              PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
              PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
              INSERT INTO <http://trans.org> { ?ex1 tr:finalSample ?result}
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

    return(sparql)
}

aggregate.data <- function(){
    # SPARQL query to get the aggregate the data
    # should only be run if we  created the triple
    # with final sample create.sample.value
    sparql1 <- "PREFIX tr: <http://purl.org/NET/c4dm/trans_experiment#>
              PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
              PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
              PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
              SELECT   ?date ?isolator ?chickens ?treatment ?infected ((?chickens - ?infected) as ?susceptibles) {
              SELECT distinct ?date ?isolator (count(?chicken) as ?chickens)
              ?treatment (sum(xsd:integer(?sample)) as ?infected) 
              WHERE { ?z tr:experimentDay ?date;
                         rdf:type tr:Measurement;
                         tr:hasType tr:Swab;
                         tr:finalSample ?sample;
                         tr:involves ?chicken.
                         ?chicken tr:group ?isolator;
                          tr:treatment ?treatment.}

                        group by ?isolator ?date ?treatment
                        order by ?date ?isolator} "
    return(sparql1)
}

swab.data <- function(){
    # SPARQL query to get the swab data
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

    return(sparql1)
}


chicken.info <- function(){
    # SPARQL query to get the information about the chicken
    sparql <- " PREFIX tr: <http://purl.org/NET/c4dm/trans_experiment#>
              PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
              PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
              SELECT  ?chicken ?group ?sex ?treatment ?type
              WHERE { ?chicken  tr:group ?group ;
                                tr:sex ?sex ;
                                tr:treatment ?treatment ;
                                tr:type ?type.}"
    return(sparql)
}


endpoint <- "http://localhost:8890/sparql"
#dates
dates.swab <- sort(as.numeric(SPARQL(url = endpoint,query=get.dates.swab())$results))

swab.data <- SPARQL(url = endpoint,query=swab.data())$results

#Update data in dataset
for(index in c(1:(length(dates.swab)-1))){
    current.date <- dates.swab[index]
    next.date <- dates.swab[index+1]
    cat(paste("Create the triples for date %s"),current.date)
    SPARQL(url = endpoint,query=create.sample.value(current.date,next.date))
    
}

aggregated.data <- SPARQL(url = endpoint,query=aggregate.data())$results
