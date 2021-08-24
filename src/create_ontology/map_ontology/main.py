import pandas as pd

from mappings import Mappings
from ontology import Ontology


def main():
    config = 'result_new.yml'
    filename = 'data_maldi_UU_2_updated.xlsx'
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
            ontology.populate_ontology(mappings, row)

    ontology.save_ontology('test.ttl')

if __name__ == '__main__':
    main()
