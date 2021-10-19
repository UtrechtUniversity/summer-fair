import click
import pandas as pd

import utils
from mappings import Mappings
from ontology import Ontology
from dataset import Dataset


@click.command()
@click.option('--config', required=True,
			  help='path to mapping file')
@click.option('--filename', required=True,
			  help='path of the dataset')
def main(config, filename):
    # File with ontology
    ont_file = 'trans_ont.owl'

    mappings = Mappings(config)
    ontology = Ontology(ont_file)
    dataset = Dataset(filename, mappings)

    print(dataset.tidy_dataset)


    for _, row in dataset.tidy_dataset.iterrows():
        # check for the required field
        # if it doesn't exit then run it for each row
        if mappings.required_field is None or row[mappings.required_field]:
            ontology.populate_ontology(mappings, row, dataset.tidy_dataset.columns.values.tolist())

    ontology.save_ontology('data/populated_ont.owl')


if __name__ == '__main__':
    main()
