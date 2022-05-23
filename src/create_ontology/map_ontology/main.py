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

@click.command()
@click.option('--config', required=True,
              help='path to mapping file')
@click.option('--filename', required=True,
              help='path of the dataset')
@click.option('--worksheet', required=False,
              help='name of the worksheet to ')
def main(config, filename, worksheet):
    # File with ontology
    ont_file = 'trans_ont.owl'

    mappings = Mappings(config)
    ontology = Ontology(ont_file)
    dataset = Dataset(filename, mappings)

    print('Starting populate ontology schema with data')
    for _, row in dataset.tidy_dataset.iterrows():
        # check for the required field
        # if it doesn't exit then run it for each row
        if mappings.required_field is None or row[mappings.required_field]:
            ontology.populate_ontology(mappings, row)

    ontology_file = Path('data/populated_ont.ttl')
    ontology.save_ontology(ontology_file)
    print('Populated ontology is created.' if ontology_file.is_file() else "Ontology file is not created." )


if __name__ == '__main__':
    main()
