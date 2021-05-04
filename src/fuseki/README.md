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

