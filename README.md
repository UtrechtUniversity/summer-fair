# SUMMER_FAIR
![GitHub](https://img.shields.io/github/license/UtrechtUniversity/summer-fair)

This software integrates existing data sets on transmission experiments by using semantic web technologies

Data sets on transmission experiments may vary in format, structure, syntax and semantics.
It is difficult to combine these heterogeneous data sets, for example, to do reanalysis and meta-analysis.
For this reason we developed an [ontology](/src/create_ontology/map_ontology/infection_trans.owl).
This shared vocabulary describes the main concepts and relations in the domain of transmission.
By mapping existing data sets to the concepts in the ontology, the data sets can be combined.
The mapped data sets are represented as linked data triples. 

## Ontology
The ontology developed in this project is called `Infection Transmission Ontology`; it describes the domain of antimicrobial resistance.
The ontology is registered in [BioPortal](https://bioportal.bioontology.org/ontologies/INFECTION_TRANS) where you can download the ontology in multiple formats.

In the [HTML documentation](https://htmlpreview.github.io/?https://github.com/UtrechtUniversity/summer-fair/blob/master/docs/index.html) you can find the definition of the concepts in Infection Transmission Ontology ontology.
See [here](../blob/master/docs/ontology/schema.png) for a graphic representation of the ontology schema.

The ontology has been published in a scientific paper at IEEE eScience 2022

## WIKI 
In the [wiki pages](https://github.com/UtrechtUniversity/summer-fair/wiki) you can find:
* a description of the Summer FAIR project
* background on semantic web technologies 
* a description and tutorials on the usage of the ontologies
* guidelines how to contribute to the ontology

## License
This project is licensed under the terms of the [MIT License](/LICENSE.md)

## Citation
Please [cite this project as described here](/CITATION.md).
