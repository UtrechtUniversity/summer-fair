# Create 1st triple store with datasetA 
docker run --detach --name fuseki-data --volume /fuseki busybox
docker run --detach --name fuseki-app -p 3030:3030 \
  -e ADMIN_PASSWORD=admin \
  -e FUSEKI_DATASET_1=mydataset\
  --volumes-from fuseki-data stain/jena-fuseki:3.14.0

docker stop fuseki-app

docker run -d --name fuseki-loadsh-use-fuseki-data \
  --volumes-from fuseki-data \
  --volume "$(pwd)/src/create_ontology/map_ontology/data:/staging" \
  stain/jena-fuseki:3.14.0 \
  ./load.sh mydataset  populated_ont_A.ttl

docker stop fuseki-loadsh-use-fuseki-data

docker start fuseki-app

echo "Uploaded populated_ont_A.ttl to triplestore hosted at port 3030"

# Create 2nd triple store with datasetB

docker run --detach --name fuseki-data2 --volume /fuseki busybox
docker run --detach --name fuseki-app2 -p 3031:3030 \
  -e ADMIN_PASSWORD=admin \
  -e FUSEKI_DATASET_1=mydataset\
  --volumes-from fuseki-data2 stain/jena-fuseki:3.14.0

docker stop fuseki-app2

docker run -d --name fuseki-loadsh-use-fuseki-data2 \
  --volumes-from fuseki-data2 \
 --volume "$(pwd)/src/create_ontology/map_ontology/data:/staging" \
  stain/jena-fuseki:3.14.0 \
  ./load.sh mydataset  populated_ont.ttl

docker stop fuseki-loadsh-use-fuseki-data2

docker start fuseki-app2

echo "Uploaded populated_ont.ttl to triplestore hosted at port 3031"

# Run R script within r-studio container

# docker build -t rproject . -f src/R/Dockerfile
#
# docker run --name algorithm_combined -v  "$(pwd)/src/R:/usr/local/src/myscripts"  rproject

#-----------------#
#   THE END       #                  
#-----------------#


# Not used for tutorial 

# Run R script within r-studio container

# docker build -t rproject . -f src/R/Dockerfile
# 
# docker run -d --name algorithm --add-host host.docker.internal:host-gateway -p 8000:8000 rproject_plumber
# 
# docker run --name algorithm2  --add-host host.docker.internal:host-gateway -p 8001:8000 rproject_plumber




