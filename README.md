# summer-fair

Intergrating existing data sets on transmission experiments by using semantic web technologies


## Getting started

  - [Prerequisites](#prerequisites)
  - [Installation](#installation)
  - [Creating linked data](#creating-linked-data)
  - [Querying linked data](querying-linked-data)

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

### Creating linked data
Provide:
- data in table format (.csv or spreadsheet) 
- ontology
- mapping file 

Populate the ontology and create a linked data set from the table data
```
# Go to mapping folder
$ cd src/create_ontology/map_ontology

# Run main script
$ python main.py

```

### Querying linked data

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



## Project organization

```
.
├── .gitignore
├── CITATION.md
├── LICENSE.md
├── README.md
├── requirements.txt
├── bin                <- Compiled and external code, ignored by git (PG)
│   └── external       <- Any external source code, ignored by git (RO)
├── config             <- Configuration files (HW)
├── data               <- All project data, ignored by git
│   ├── processed      <- The final, canonical data sets for modeling. (PG)
│   ├── raw            <- The original, immutable data dump. (RO)
│   └── temp           <- Intermediate data that has been transformed. (PG)
├── docs               <- Documentation notebook for users (HW)
│   ├── manuscript     <- Manuscript source, e.g., LaTeX, Markdown, etc. (HW)
│   └── reports        <- Other project reports and notebooks (e.g. Jupyter, .Rmd) (HW)
├── results
│   ├── figures        <- Figures for the manuscript or reports (PG)
│   └── output         <- Other output for the manuscript or reports (PG)
└── src                <- Source code for this project (HW)

```


## License

This project is licensed under the terms of the [MIT License](/LICENSE.md)

## Citation

Please [cite this project as described here](/CITATION.md).
