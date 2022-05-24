#########################################################
#                                                        
#                  Query for local algorithm                                 
#                                                        
#                  Author:      Egil Fischer                         
#                  Contact:     e.a.j.fischer@uu.nl                         
#                  Creation date          2-2-2022               
#########################################################
library(SPARQL)


get.data <- function(endpoint){
  sparql <- "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
            Prefix : <http://www.purl.org/trans_experiment#>
            Prefix om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
            PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
            PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
            SELECT ?round ?ex_day ?ex_hour ?hour_after_inoc  ?inoculationHour ?group ?level1 ?level2 ?level3 ?host_id ?treatment ?inoculationStatus ?sample_measure ?sample_result ?sample_type ?pathogen_name WHERE {
              ?experiment a :Experiment;
                          :experimentDay ?ex_day;
                          :hasMeasurement ?measurement.
              optional {?experiment :repetition ?round;}


              ?measurement a :Measurement;
                           :hasHost ?host;
                           :hasSample ?sample.
                optional {?measurement :experimentHour ?ex_hour.}
  optional {?measurement :hourAfterInoculation ?hour_after_inoc.}
              ?host :id ?host_id;
                    :treatment ?treatment;
                    :inoculationStatus ?inoculationStatus;
                    :locatedIn ?env.
     optional{?host :type ?host_type.}
              ?env  :groupNumber ?group.
 optional{?experiment :hasInoculation ?inoculation.
    ?inoculation :involves ?host.
 ?inoculation	 :experimentHour ?inoculationHour.}
              optional{?env :level1 ?level1.}
              optional{?env :level2 ?level2;}
              optional{?env :level3 ?level3;}
              optional{ ?measurement :experimentHour ?ex_hour. }
              optional{ ?measurement
                           :hasQuantity ?quantity.
                ?quantity om:hasValue ?measure.
                ?measure om:hasNumericalValue ?sample_measure.
              }
			   ?sample :hasType ?sample_type.
               optional{?sample  :hasPathogen ?pathogen.
                ?pathogen :name ?pathogen_name }
              optional {?sample :result ?sample_result.}
            }"
  return(SPARQL(url = endpoint,query=sparql)$results)
}

