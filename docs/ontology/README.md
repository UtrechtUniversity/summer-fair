# About 

The folder contains the ontology files: [ontology schema](trans_experiment.owl) and [populated ontology](trans_experiment_populated.owl).

TODO: describe the schema and add the picture

The example of one measurement (sample) of the ontology in turtle format is described as follows:

```
 PREFIX tr: <http://purl.org/NET/c4dm/trans_experiment.owl#>
 PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
 PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

# information about the sample
tr:101_Roswab48 a tr:Measurement,
        tr:Sample ;
    tr:experimentDay "8" ;
    tr:hasPathogen tr:Enterobacter_cloacae ;
    tr:hasType tr:Swab ;
    tr:involves tr:101_Ro ;
    tr:sample "+?" .

# information about the chicken   
tr:101_Ro a tr:Individual ;
    tr:group "H2_I9" ;
    tr:id "101_Ro" ;
    tr:locatedIn tr:101_Ro2_I9 ;
    tr:sex "M" ;
    tr:treatment "control" ;
    tr:type "S1" .

# information about the location    
tr:101_Ro2_I9 a tr:Location ;
    tr:house "2" ;
    tr:pen "I9" .
    

```


