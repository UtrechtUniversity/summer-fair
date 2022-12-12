import re
import utils
import pandas as pd
from sys import exit

from collections import defaultdict, Counter

#Colors for output
RED = '\x1b[1;31m' # ERROR
DEFAULT = '\x1b[0m' # normal output
YEL = '\x1b[1;33m' # Input request
BLUE = '\x1b[1;34m' # Logging, useful information to eyeball

class Dataset:
    def __init__(self, file, mappings):
        self.dataset = self.update_dataset(file, mappings).fillna('')

        self.column_pattern_values = defaultdict()
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

    def get_dataset_reocur(self, reocur_columns: set) -> defaultdict:
        """
        Method returns for each reocuring column name
        list of column headers from the dataset

        For example:
            reocur_columns = {'weight_d.*'}
            reocur_ds_columns = {'weight_d0', 'weight_d21'}
        """
        reocur_ds_columns = defaultdict(set)
        for pattern in reocur_columns:
            values = self.get_recoruring_values(pattern)
            if values:
                reocur_ds_columns[pattern] = list(values)
        return reocur_ds_columns

    def update_dataset_values(self, update_columns: list):
        """
        Method gets list of dictionaries, that specify
        for which columns to change the values and
        new and old values.

        For example:
        update_columns =[{ 'column_name': swab.*,
                           'values': {'1': ['+','+?'], '0': ['-','-*?','-*']}
                            }]

                Old dataframe                    Updated dataframe
                +----+--------+                  +----+--------+
                | ID | swab.* |                  | ID | swab.* |
                +----+--------+                  +----+--------+
                | 1  | +      |                  | 1  | 1      |
                +----+--------+                  +----+--------+
                | 2  | -*?    |       =>         | 2  | 0      |
                +----+--------+                  +----+--------+
                | 3  | +?     |                  | 3  | 1      |
                +----+--------+                  +----+--------+

        """
        for column in update_columns:
            values = column['values']
            column_name = column['column_name']
            for new_value, old_values in values.items():
                self.tidy_dataset.loc[self.tidy_dataset[column_name].isin(old_values), [column_name]] = str(new_value)

    def create_new_columns_in_df(self):
        """
        Methods creates new column in the dataframe for every
        reocuring column, where value of the column should be exctratced from the
        column name.
        New columns are created so we can easily reshape the dataset
        For example:
            key = 'weight_d(.*)'
            value = ['weight_d0', 'weight_d21']

            We create 2 columns one 'value_weight_d0' with value '0'
            and the other one 'value_weight_d0' with value '21'
        """

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
            column_headers = self.get_recoruring_values(pattern)
            if column_headers:
                reocur_ds_columns[pattern] = list(column_headers)
        return reocur_ds_columns

    def get_recoruring_values(self, column_name_pattern: str) -> set:
        """
        Method take a column patter and matches column headers from the dataset
        For example:
            column_name_patterm = 'BS.*_date'
            values =  {'BS0_date', 'BS1_date'}

        """
        column_headers = set()
        values = set()
        for column in self.columns:

            column_name_new = column_name_pattern.replace(".*", "(-?\d*\.{0,1}\d+)")
            matching = re.match(rf'{column_name_new}$', column)
            if matching:
                column_headers.add(column)
                values.add(matching[1])

        self.column_pattern_values[column_name_pattern] = values

        return column_headers

    def update_dataset(self, file, mappings):
        # Check if the merged field is specified in config
        # If yes, merge the spreadsheet based on merged field
        merge_field = mappings.mappings.get('merge_spreadsheets_on', '')
        dataset = self.read_file(file, merge_field)
        if merge_field:
            dataset = self.merge_spreadsheets(file, merge_field)

        return dataset

    def merge_spreadsheets(self, workbook: str, merge_field: str) -> pd.DataFrame:
        """
        Method merges spreahsheets based on merge_field
        specified in the config file
        """
        df = pd.read_excel(workbook, sheet_name=None)
        worksheets = [*df]
        combined_dataset = df[worksheets[0]]
        for index in range(1, len(worksheets)):
            combined_dataset = combined_dataset.merge(df[worksheets[index]], on=merge_field, how="outer")
        return combined_dataset

    def read_file(self, file: str, merge_field=None):
        """
        Input: path to a csv file (accepted seperators: ',', ';' or tab)
        Return: pandas DataFrame
        The function reads in a csv file and checks its dimensions
        """
        print(BLUE+"Reading: "+str(file))
        print(BLUE+"Merge field: "+str(merge_field))
        if file.endswith('.csv'):
            dataset = pd.read_csv(file, sep=None, engine = 'python',encoding='utf-8-sig')
        elif file.endswith('.xlsx') and merge_field:
            dataset = pd.read_excel(file, sheet_name=None)
        elif file.endswith('.xlsx') and not merge_field:
            dataset = pd.read_excel(file)
        else:
            print(RED+'ERROR read dataset: Please provide a comma sparated file or an Excelsheet'+DEFAULT)
            exit()
        print(BLUE+"Checking dataset shape ...")
        rows, cols = dataset.shape
        print("Rows ", rows, "Columns: ", cols, DEFAULT)
        if rows < 1:
            print(RED+'ERROR read dataset: File does not contain data or column names.'+DEFAULT)
            exit()
        if cols < 1:
            print(
                RED+'ERROR read dataset: File only contains one column. Check if values are separated by comma or semicolon'+DEFAULT)
            exit()

        return dataset

    def transform_dataset(self, mappings):
        """
        Methods transform dataset to a tidy dataset,
        returns a dataframe, where each row is per one experiment day or
        experiment day and hour
        """
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

                    # days and hours experiments
                    experiment_time = ['experimentDay']

                    if 'experimentHour' in reshaped:
                        reshaped['experimentHour'] = reshaped['experimentHour'].apply(lambda x: pd.to_numeric(x, errors = 'ignore'))
                        if 'experimentHour' in reshaped_combined:
                            experiment_time.append('experimentHour')


                    if not reshaped_combined.empty:
                        # check for already created columns


                        already_created  = set(reshaped_combined.columns).intersection(set(reshaped.columns))
                        # fix experiment hour
                        reshaped_combined = reshaped_combined.merge(reshaped,
                                                                    on= list(already_created) + self.reusable_column,
                                                                    how="outer")
                    else:
                        reshaped_combined = reshaped

                    if 'experimentHour' in property_or_class:
                        property_or_class['experimentHour'] = 'experimentHour'
                mappings['Experiment']['experimentDay'] = 'experimentDay'

        reshaped_combined.dropna(subset=['experimentDay'], inplace=True)
        return reshaped_combined if not reshaped_combined.empty else self.dataset

    def reocur_columns_to_reshape(self, map_columns:dict) -> dict:
        """
        Method returns which columns should be reshaped
        for a given slice of a mapping (mapping per concept)

        For example:
         map_columns = {'experimentDay': 'weight_d(.*)', 'hasNumericalValue': 'weight_d.*'}

         Method returns {'experimentDay': ['value_weight_d0', 'value_weight_d21'], 'weight_d.*': ['weight_d0', 'weight_d21']}

        """
        reshaped = {(v if k != 'experimentDay' and k != 'experimentHour' else k): self.reocur_columns_dict[v] for
                k, v in map_columns.items() if k not in self.reusable_column}

        # check if the values has the same lenght, otherwise create an empty columns for missing column headers
        if len(set([len(v) for v in list(reshaped.values())])) != 1:
            reshaped = self.create_missing_columns(map_columns, reshaped)


        reshaped = {k:sorted(v,key=utils.num_sort) for k,v in reshaped.items()}


        return reshaped

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

    def create_missing_columns(self,map_columns,reshape_columns):

        max_columns = sorted(reshape_columns, key=lambda k: len(reshape_columns[k]), reverse=True)[0]

        for property in reshape_columns.keys():


            if property!=max_columns:
                if max_columns not in self.column_pattern_values:
                    max_set = self.column_pattern_values[map_columns[max_columns]]

                else:
                    max_set = self.column_pattern_values[max_columns]


                column_differences = max_set.difference(self.column_pattern_values[property])
                if column_differences:
                    for diff in column_differences:
                        new_column_header = property.replace('.*',diff)
                        self.dataset[new_column_header] = ''

                        reshape_columns[property].append(new_column_header)


        return reshape_columns



















