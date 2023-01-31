"""Create linked data set
Create instances and populate ontology schema.
The instances are represented as RDF triples (linked data).
Required arguments:
Data in tabular format
Ontology schema
Mapping file
"""

from pathlib import Path

import click

from dataset import Dataset
from mappings import Mappings
from ontology import Ontology
from utils import upload_to_triplestore


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
    print(config, filename)

    ontology = Ontology(ont_file)
    mappings = Mappings(config, ontology.data_properties)
    dataset = Dataset(filename, mappings)

    tidy_dataset = dataset.transform_dataset(mappings.ont_mappings).fillna('')

    for _, row in tidy_dataset.iterrows():
        # check for the required field
        # if it doesn't exit then run it for each row
        if mappings.required_field is None or row[mappings.required_field]:
            ontology.populate_ontology(mappings, row)

    ontology.save_ontology(ontology_file)
    print('Populated ontology is created.' if ontology_file.is_file() else "Ontology file is not created.")


    # Upload Data to the triplestore
    try:
        port = '3030'
        upload_to_triplestore(ontology_file, port)
    except Exception as e:
        print(f'Connection Error. Please make sure port {port} is available.')
        print(f"Error Message: {e}")


if __name__ == '__main__':
    main()
