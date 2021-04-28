docker run --detach --name fuseki-data --volume /fuseki busybox
docker run --detach --name fuseki-app -p 3030:3030 \
  -e ADMIN_PASSWORD=admin \
  -e FUSEKI_DATASET_1=dataset_a\
  -e FUSEKI_DATASET_2=dataset_b\
  -e FUSEKI_DATASET_3=dataset_c\
  --volumes-from fuseki-data stain/jena-fuseki:3.14.0

docker stop fuseki-app

docker run -d --name fuseki-loadsh-use-fuseki-data \
  --volumes-from fuseki-data \
  --volume "$(pwd)/ontology:/staging" \
  stain/jena-fuseki:3.14.0 \
  ./load.sh dataset_c  trans_experiment_C.ttl \
  ./load.sh dataset_a  trans_experiment_A_OM.ttl

docker stop fuseki-loadsh-use-fuseki-data

docker start fuseki-app


# Run R script within r-studio container

docker build -t rproject .

docker run --name sparql rproject