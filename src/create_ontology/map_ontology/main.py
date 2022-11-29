"""Create linked data set
Create instances and populate ontology schema.
The instances are represented as RDF triples (linked data).
Required arguments:
Data in tabular format
Ontology schema
Mapping file
"""

import click
from pathlib import Path

from dataset import Dataset
from mappings import Mappings
from ontology import Ontology

import requests

@click.command()
@click.option('--config', required=True,
              help='path to mapping file')
@click.option('--filename', required=True,
              help='path of the dataset')
@click.option('--worksheet', required=False,
              help='name of the worksheet to ')
def main(config, filename, worksheet):
    # File with ontology
    ont_file = 'infection_trans.owl'
    ontology_file = Path('data/populated_ont.ttl')
    print(config,filename)

    ontology = Ontology(ont_file)
    mappings = Mappings(config, ontology.data_properties)
    dataset = Dataset(filename, mappings)

    for _, row in dataset.tidy_dataset.iterrows():
        # check for the required field
        # if it doesn't exit then run it for each row
        if mappings.required_field is None or row[mappings.required_field]:
            ontology.populate_ontology(mappings, row)


    ontology.save_ontology(ontology_file)
    print('Populated ontology is created.' if ontology_file.is_file() else "Ontology file is not created." )

    ## Upload Data to the triplestore

    data = open(ontology_file).read()
    headers = {'Content-Type': 'text/turtle;charset=utf-8', 'Authorization': 'admin:admin'}
    r = requests.post('http://host.docker.internal:3032/mydataset/data?default', data=data.encode('utf-8'), headers=headers)
    info_message = 'Ontology uploaded to triplestore' if r.status_code == 200 else 'Check the triplestore logs'
    print(info_message)


if __name__ == '__main__':
    main()
