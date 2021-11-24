# summer-fair

This software integrates existing data sets on transmission experiments by using semantic web technologies

Data sets on transmission experiments may vary in format, structure, syntax and semantics.
It is difficult to combine these heterogeneous data sets, for example, to do reanalysis and meta-analysis.
For this reason we developed an [ontology](/src/create_ontology/map_ontology/trans_ont.owl).
This shared vocabulary describes the main concepts and relations in the domain of transmission.
By mapping existing data sets to the concepts in the ontology, the data sets can be combined.
The mapped data sets are represented as linked data triples. 

## Table of Contents
  - [Getting started](#getting-started)
    - [Prerequisites](#prerequisites)
    - [Installation](#installation)
  - [Usage](#usage)
    - [Mapping data](#mapping-data)
    - [Creating linked data](#creating-linked-data)
    - [Querying linked data](querying-linked-data)

## Getting started

### Prerequisites
Make sure you have installed 
Docker > 18.01
Python > 3.5

### Installation
Clone this repo 
```
$ git clone git@github.com:UtrechtUniversity/summer-fair.git
```

Install dependencies
```
# Go to project folder
$ cd summer-fair
$ pip install requirements.txt
```

### Manual
Get started with the summerfair software by trying it out in this [notebook](link) manual

Make sure you have [jupyter notebook](https://jupyter.org/install) installed


## Usage
Provide:
- data in table format (.csv or spreadsheet) 
- ontology schema (.owl)
- mapping file (.yml)

### Mapping data 
The ontology schema can be considered an empty data model. 
Map the columns in the csv file to the concepts in the domain ontology to populate the ontology.
Specify the mapping in a yaml file.
You can find examples of these mappings in the [example data](/data/examples)


### Creating linked data
The mapping file is used to automatically create instances and populate the ontology.
The instances are represented as RDF triples

```
# Go to mapping folder
$ cd src/create_ontology/map_ontology

# Run main script
$ python main.py

```

### Querying linked data
The created RDF triples are stored in a triple store and can be retrieved via the query language SPARQL.
The `docker.sh` shell script allows to create the local triple store and upload the data stored in `ontology` folder to  `mydataset` database in the triplestore.
Once the data is loaded to the triplestore, we run the local algorithm ([ExampleCode.R](ExampleCode.R))  that uses the SPARQL queries to retrieve the data. 

The R script [ExampleCode.R](ExampleCode.R) is the update version of [ExampleCode.R](../../src/R/ExampleCode.R), where we added 2 SPARQL queries (to retrieve dates and to retrieve the information about infected chickens per isolator) and made the data untidy.


In order to run the shell script, do the following in the terminal:

```buildoutcfg
cd src/fuseki # you need to run the script from fuseki directory
chmod +x ./docker.sh #granting permission to run the file
sh ./docker.sh # creating dockers for triplestore and R project
```
Once the script is run, you can see the model data estimates.
If you go to `http://localhost:3030` you can access and query the ontology using Apache Fuseki UI. 


## License

This project is licensed under the terms of the [MIT License](/LICENSE.md)

## Citation

Please [cite this project as described here](/CITATION.md).
