FROM python:3.9
WORKDIR /map
COPY  src/create_ontology/map_ontology /map
COPY  data/examples /map
RUN pip3 install -r requirements.txt
ENTRYPOINT ["python", "main.py"]