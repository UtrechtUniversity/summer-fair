#!/bin/bash
#   Licensed to the Apache Software Foundation (ASF) under one or more
#   contributor license agreements.  See the NOTICE file distributed with
#   this work for additional information regarding copyright ownership.
#   The ASF licenses this file to You under the Apache License, Version 2.0
#   (the "License"); you may not use this file except in compliance with
#   the License.  You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.


if [ ! -f "$FUSEKI_BASE/shiro.ini" ] ; then
  # First time
  echo "###################################"
  echo "Initializing Apache Jena Fuseki"
  echo ""
  cp "$FUSEKI_HOME/shiro.ini" "$FUSEKI_BASE/shiro.ini"
  if [ -z "$ADMIN_PASSWORD" ] ; then
    ADMIN_PASSWORD=$(pwgen -s 15)
    echo "Randomly generated admin password:"
    echo ""
    echo "admin=$ADMIN_PASSWORD"
  fi
  echo ""
  echo "###################################"
fi

if [ -d "/fuseki-extra" ] && [ ! -d "$FUSEKI_BASE/extra" ] ; then
  ln -s "/fuseki-extra" "$FUSEKI_BASE/extra"
fi

# $ADMIN_PASSWORD only modifies if ${ADMIN_PASSWORD}
# is in shiro.ini
if [ -n "$ADMIN_PASSWORD" ] ; then
  export ADMIN_PASSWORD
  envsubst '${ADMIN_PASSWORD}' < "$FUSEKI_BASE/shiro.ini" > "$FUSEKI_BASE/shiro.ini.$$" && \
    mv "$FUSEKI_BASE/shiro.ini.$$" "$FUSEKI_BASE/shiro.ini"
  unset ADMIN_PASSWORD # Don't keep it in memory
  export ADMIN_PASSWORD
fi


# fork
exec "/jena-fuseki/fuseki-server" &
FUSEKI_PID=$!

TDB_VERSION=''
if [ ! -z ${TDB+x} ] && [ "${TDB}" = "2" ] ; then
  TDB_VERSION='tdb2'
else
  TDB_VERSION='tdb'
fi


# Wait until server is up
while [[ $(curl -I http://localhost:3030 2>/dev/null | head -n 1 | cut -d$' ' -f2) != '200' ]]; do
  sleep 1s
done

echo "Convert env to datasets"
# Convert env to datasets
# Convert env to datasets
curl -s 'http://localhost:3030/$/datasets'\
     -H "Authorization: Basic $(echo -n admin:${ADMIN_PASSWORD} | base64)" \
     -H 'Content-Type: application/x-www-form-urlencoded; charset=UTF-8'\
     --data "dbName=mydataset&dbType=${TDB_VERSION}"

# rejoin our exec


unzip -d /tmp/data /data/input/$1

python3 main.py --config /tmp/data/mapping.yml --filename /tmp/data/dataset.*
Rscript src/R/LocalAlgorithmDocker.R  src/R/summerfair_config.yaml

cp algorithmOutput.rds /data/output/"${1%.*}".rds

# Remove the input files
rm /data/input/$1


#kill $FUSEKI_PID

exit 0



