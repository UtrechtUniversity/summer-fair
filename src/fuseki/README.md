The `docker.sh` shell script allows to create the local triple store and upload the data stored in `ontology` folder to  `mydataset` database in the triplestore.

In order to run the shell script, do the following in the terminal:

```buildoutcfg
chmod +x ./docker.sh
sh ./docker.sh
```

If you go to `http://localhost:3030` you can access and query the ontology using Apache Fuseki UI. 
