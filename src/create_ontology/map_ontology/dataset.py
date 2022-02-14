import re
from collections import defaultdict, Counter

import pandas as pd

import utils


class Dataset:
    def __init__(self, file, mappings):
        self.dataset = self.update_dataset(file, mappings).fillna('')

        self.columns = self.dataset.columns.values.tolist()
        self.reocur_columns_dict = self.get_dataset_reocur(mappings.reocur_mappings)
        self.multiple_columns = list(self.get_multiple_columns(mappings.ont_mappings))
        self.reusable_column = [column for column, count in Counter(self.multiple_columns).items() if count > 1]

        if self.reocur_columns_dict:
            self.create_new_columns_in_df()

        self.reocur_columns = [v for value in self.reocur_columns_dict.values() for v in value]

        self.tidy_dataset = self.transform_dataset(mappings.ont_mappings).fillna('')

        if mappings.update_values:
            self.update_dataset_values(mappings.update_values)

    def update_dataset_values(self, update_columns: list):
        for column in update_columns:
            values = column['values']
            column_name = column['column_name']
            for new_value, old_values in values.items():
                self.tidy_dataset.loc[self.tidy_dataset[column_name].isin(old_values), [column_name]] = str(new_value)

    def create_new_columns_in_df(self):

        for key, values in self.reocur_columns_dict.items():
            if '(.*)' in key:
                new_columns = []
                for column in values:
                    extracted_value = re.match(key, column)[1]
                    new_column_name = f'value_{column}'
                    new_columns.append(new_column_name)

                    self.dataset[new_column_name] = extracted_value

                self.reocur_columns_dict[key] = new_columns

    def get_dataset_reocur(self, reocur_columns):
        reocur_ds_columns = defaultdict(set)
        for pattern in reocur_columns:
            values = self.get_recoruring_values(pattern)
            if values:
                reocur_ds_columns[pattern] = values
        return reocur_ds_columns

    def get_recoruring_values(self, column_name_pattern):
        values = set()
        for column in self.columns:
            column_name_new = column_name_pattern.replace(".*", "\d{1,4}")
            if re.match(rf'{column_name_new}$', column):
                values.add(column)
        return values

    def update_dataset(self, file, mappings):
        # Check if the merged field is specified in config
        # If yes, merge the spreadsheet based on merged field
        merge_field = mappings.mappings.get('merge_spreadsheets_on', '')
        dataset = self.read_file(file, merge_field)
        if merge_field:
            dataset = self.merge_spreadsheets(file, merge_field)

        return dataset

    def merge_spreadsheets(self, workbook: str, merge_field: str) -> pd.DataFrame:
        # parse dataset
        df = pd.read_excel(workbook, sheet_name=None)
        worksheets = [*df]
        combined_dataset = df[worksheets[0]]
        for index in range(1, len(worksheets)):
            combined_dataset = combined_dataset.merge(df[worksheets[index]], on=merge_field, how="outer")
        return combined_dataset

    def read_file(self, file: str, merge_field=None):
        if file.endswith('.csv'):
            dataset = pd.read_csv(file)

        elif file.endswith('.xlsx') and merge_field:
            dataset = pd.read_excel(file, sheet_name=None)
        elif file.endswith('.xlsx') and not merge_field:
            dataset = pd.read_excel(file)
        else:
            print('We only support .csv and .xlsx files.')

        return dataset

    def transform_dataset(self, mappings):
        reshaped_combined = pd.DataFrame()
        for ont_class, properties_or_classes in mappings.items():
            if ont_class != 'Experiment':
                if not isinstance(properties_or_classes, list):
                    properties_or_classes = [properties_or_classes]  # put it in a list for simplification

                for property_or_class in properties_or_classes:

                    map_columns = utils.get_reocur_columns(property_or_class)
                    if map_columns:
                        # substitute the .* with columns, change column names
                        reshape_columns = self.reocur_columns_to_reshape(map_columns)
                        unique_columns = list(set(self.columns).difference(self.reocur_columns).difference(set(self.multiple_columns)))
                        uniq_reshaped = unique_columns + [v for value in reshape_columns.values() for v in value]
                        new_df = self.dataset[uniq_reshaped]
                    elif not map_columns and (len(properties_or_classes) > 1 or 'experimentDay' in property_or_class):
                        reshape_columns = {k: v for k, v in self.columns_to_reshape(property_or_class).items() if
                                           k not in self.reusable_column}
                        unique_columns = list(set(self.columns).difference(self.multiple_columns))
                        uniq_reshaped = unique_columns + [v for value in reshape_columns.values() for v in
                                                          value] + self.reusable_column

                    else:
                        continue

                    new_df = self.dataset[uniq_reshaped]
                    reshaped = pd.lreshape(new_df, reshape_columns)

                    reshaped['experimentDay'] = reshaped['experimentDay'].apply(lambda x: pd.to_numeric(x, errors = 'ignore'))
                    reshaped['experimentHour'] = reshaped['experimentHour'].apply(lambda x: pd.to_numeric(x, errors = 'ignore'))
                    # days and hours experiments
                    experiment_time = self.get_day_time(property_or_class)

                    if not reshaped_combined.empty:
                        reshaped_combined = reshaped_combined.merge(reshaped,
                                                                    on=unique_columns + experiment_time + self.reusable_column,
                                                                    how="outer")
                    else:
                        reshaped_combined = reshaped

                    if 'experimentHour' in property_or_class:
                        property_or_class['experimentHour'] = 'experimentHour'
                mappings['Experiment']['experimentDay'] = 'experimentDay'

        return reshaped_combined if not reshaped_combined.empty else self.dataset

    def reocur_columns_to_reshape(self, map_columns):
        return {(v if k != 'experimentDay' and k != 'experimentHour' else k): sorted(self.reocur_columns_dict[v]) for
                k, v in map_columns.items() if k not in self.reusable_column}

    def get_multiple_columns(self, mappings):
        update_columns= []
        if isinstance(mappings['Experiment']['experimentDay'], list):
            experiment_days = mappings['Experiment']['experimentDay']
            # for each experiment day we check for properties that belong to it
            # only if they are in dataset


            for properties_or_classes in mappings.values():
                if isinstance(properties_or_classes, list):  # put it in a list for simplification
                    for property in properties_or_classes:
                        update_columns = update_columns + self.check_experiment_group(experiment_days,
                                                                                      self.columns_to_reshape(property))
                else:
                    update_columns = update_columns + self.check_experiment_group(experiment_days,
                                                                                  self.columns_to_reshape(
                                                                                      properties_or_classes))

        return update_columns

    def check_experiment_group(self, experiment_days, group):
        update_columns = []
        for experiment_day in experiment_days:
            column_names = [v for v_list in group.values() for v in v_list]
            if experiment_day in column_names:
                update_columns = update_columns + column_names
        return update_columns

    def columns_to_reshape(self, properties_classes):
        # if columns in are in dataset, they need to be reshaped

        reshaped_columns = {}
        for property, column in properties_classes.items():
            if isinstance(column, dict):
                reshaped_columns.update(self.columns_to_reshape(column))
            if isinstance(column, str) and column in self.columns:
                if property in ['experimentDay', 'experimentHour']:
                    property = property
                else:
                    property = column
                reshaped_columns[property] = [column]

        return reshaped_columns

    def get_day_time(self, property_or_class):
        return [property for property in property_or_class.keys() if
                property == 'experimentDay' or property == 'experimentHour']
