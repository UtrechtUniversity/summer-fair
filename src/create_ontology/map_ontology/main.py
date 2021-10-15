import pandas as pd

from mappings import Mappings
from ontology import Ontology
from collections import namedtuple
import utils


def main():
    config = 'result_new.yml'
    filename = 'data_maldi_UU_2_updated.xlsx'
    ont_file = 'trans_ont.owl'

    mappings = Mappings(config)
    ontology = Ontology(ont_file)

    # Check if the merged field is specified in config
    # If yes, merge the spreadsheet based on merged field
    merge_field = mappings.mappings.get('merge_spreadsheets_on', '')
    if merge_field:
        dataset = utils.merge_spreadsheets(filename, merge_field)
    else:
        dataset = pd.read_excel(filename)

    dataset = dataset.fillna('')

    columns = dataset.columns.values.tolist()
    reocur_columns_dict = utils.get_dataset_reocur(mappings.reocur_mappings, columns)
    # check if in the reocur_columns there are columns with meaningful data in column name
    # for example if (.*) is in column name. If yes, create new columns with meaningful data in df

    if reocur_columns_dict and utils.values_in_columnames(reocur_columns_dict):
        utils.create_new_columns_in_df(dataset, reocur_columns_dict)

    reocur_columns = [v for value in reocur_columns_dict.values() for v in value]
    unique_columns = list(set(columns).difference(reocur_columns))

    stacked_df = utils.transform_dataset(dataset, mappings, unique_columns, reocur_columns_dict).fillna('')

    # update values
    update_columns = mappings.update_values
    if update_columns:
        for column in update_columns:
            values = column['values']
            column_name = column['column_name']
            for new_value, old_values in values.items():
                stacked_df.loc[stacked_df[column_name].isin(old_values), [column_name]] = str(new_value)

        for _, row in dataset.iterrows():
            # check for the required field
            # if it doesn't exit then run it for each row
            if mappings.required_field is None or row[mappings.required_field]:
                ontology.populate_ontology(mappings, row, columns)

        ontology.save_ontology('populated_ont.owl')


if __name__ == '__main__':
    main()
