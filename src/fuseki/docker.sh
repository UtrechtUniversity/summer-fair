docker run --detach --name fuseki-data --volume /fuseki busybox
docker run --detach --name fuseki-app -p 3030:3030 \
  -e ADMIN_PASSWORD=admin \
  -e FUSEKI_DATASET_1=mydataset\
  --volumes-from fuseki-data stain/jena-fuseki:3.14.0

docker stop fuseki-app

docker run -d --name fuseki-loadsh-use-fuseki-data \
  --volumes-from fuseki-data \
  --volume "$(pwd)/ontology:/staging" \
  stain/jena-fuseki:3.14.0 \
  ./load.sh mydataset  trans_experiment_C.ttl

docker stop fuseki-loadsh-use-fuseki-data

docker start fuseki-app

