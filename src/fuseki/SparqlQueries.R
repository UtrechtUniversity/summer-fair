library(SPARQL)

endpoint <- "http://host.docker.internal:3030/dataset_c/query"

get.dates.swab <- function(){
  #SPARQL query to get the experiment dates of SWAB sample
  sparql <- " PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX tr: <http://www.semanticweb.org/trans_experiment#>
SELECT distinct ?date  
	WHERE { ?subject tr:experimentDay ?date;
 		          tr:hasSample ?sample.
?sample rdf:type tr:SwabSample;
tr:hasResult 'positive';
tr:involves ?chicken.

?chicken  rdf:type tr:Individual;
tr:type ?type.
 }
order by ?date"
  
  return( sparql)
}

swab.dates <- SPARQL(url = endpoint,query=get.dates.swab())$results
print(swab.dates)
write.csv(x=swab.dates,file='dates.csv')
