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
  ./load.sh mydataset  populated_ont.ttl

docker stop fuseki-loadsh-use-fuseki-data

docker start fuseki-app

# Run R script within r-studio container

#docker build -t rproject .

#docker run --name sparql rproject