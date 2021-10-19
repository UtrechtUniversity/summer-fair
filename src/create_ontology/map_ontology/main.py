import pandas as pd
import click
from mappings import Mappings
from ontology import Ontology

@click.command()
@click.option('--config', required=True,
			  help='path to mapping file')
@click.option('--filename', required=True,
			  help='path of the dataset')
def main(config, filename):
    ont_file = 'trans_ont.owl'

    # parse dataset
    dataset = pd.read_excel(filename).fillna(method='ffill', axis=0).fillna('')
    columns = dataset.columns.values.tolist()

    # create mapping and ontology class
    mappings = Mappings(config, columns)
    ontology = Ontology(ont_file)

    for _, row in dataset.iterrows():
        # check for the required field
        # if it doesn't exit then run it for each row
        if mappings.required_field is None or row[mappings.required_field]:
            ontology.populate_ontology(mappings, row,columns)

    ontology.save_ontology('data/populated_ont.owl')

if __name__ == '__main__':
    main()
