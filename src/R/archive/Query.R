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
            Prefix : <http://www.purl.org/infection_trans#>
            Prefix om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
            PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
            PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
            SELECT ?round ?ex_day ?ex_hour ?hour_after_inoc  ?inoculationHour ?group ?level1 ?level2 ?level3 ?host_id ?type ?treatment ?inoculationStatus ?sample_measure ?unit_measure ?quantity_function ?sample_material ?detectionLimit ?sample_result ?sample_type ?pathogen_name ?gene_name  WHERE {
              ?experiment a :Experiment;
                          :experimentDay ?ex_day;
                          :hasMeasurement ?measurement.
              optional {?experiment :repetition ?round;}
              ?measurement a :Measurement;
                           :hasHost ?host.
                 ?measurement  :hasSample ?sample.
                optional {?measurement :experimentHour ?ex_hour.}
  optional {?measurement :hourAfterInoculation ?hour_after_inoc.}
              ?host :id ?host_id;
                    :locatedIn ?env.
      optional{?host :treatment ?treatment.}
      optional{?host   :inoculationStatus ?inoculationStatus.}
      optional{?host   :type ?type.}
                    
     optional{?host :type ?host_type.}
     optional{   ?env  :groupNumber ?group.}
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
    			
                ?measure om:hasNumericalValue ?sample_measure;
						 om:hasUnit  ?unit_measure.
              }
  optional { ?quantity om:hasAggregateFunction ?quantity_function.}
			optional{   ?sample :hasType ?sample_type.}
			optional{   ?sample :material ?sample_material.}
			optional{   ?sample :detectionLimit ?detectionLimit}
		  optional {?sample :result ?sample_result.}
            
               optional{?sample  :hasPathogen ?pathogen.
                ?pathogen :name ?pathogen_name }
              
              optional{?pathogen :hasGene ?gene.
                ?gene :name ?gene_name. }
      }
"
  return(SPARQL(url = endpoint,query=sparql)$results)
}

