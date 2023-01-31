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
    def __init__(self, filename, mappings):
        data = self.readData(filename)
        #If config indicates that tabs of Excel needs to be merged
        merge = mappings.mappings.get('merge_spreadsheets_on', '')

        worksheets = [*data]
        if merge:

            print("merge")
            if len(worksheets) > 1:
                data = self.merge_spreadsheets(data, merge)
        else:
            if isinstance(data,dict):
                data = data[worksheets[0]]

        if data.empty:
            print(RED+'Dataset is empty'+DEFAULT)
            exit(1)
        if len(data.columns) < len(mappings.mapping_columns):
            print(RED+'Missing columns'+DEFAULT)
            print("Expected are enough columns to map to")
            print(mappings.mapping_columns)
            exit(1)

        self.dataset = data.fillna('')
        self.column_pattern_values = defaultdict()
        self.columns = self.dataset.columns.values.tolist()
        self.series_dict = self.get_series_columns(mappings.reocur_mappings)
        self.multiple_columns = list(self.get_multiple_columns(mappings.ont_mappings))
        self.reusable_column = [column for column, count in Counter(self.multiple_columns).items() if count > 1]


        if self.series_dict:
            self.create_new_columns_in_df()
            #series_dict.update(self.__create_new_columns_in_df(series_dict))

        self.series_columns = [v for value in self.series_dict.values() for v in value]

       # self.tidy_dataset = self.transform_dataset(mappings.ont_mappings).fillna('')

        if mappings.update_values:
            self.update_dataset_values(mappings.update_values)

    def readData(self, filename):
        data = []
        if filename.endswith('.xlsx'):
            try:
                data = pd.read_excel(filename, sheet_name=None)
            except Exception as e:
                print(RED+"Reading Excel data failed"+DEFAULT)
                print(str(e))
        else:
            try:
                data = pd.read_csv(filename, sep=None, engine = 'python',encoding='utf-8-sig')
            except Exception as e:
                print(RED+"Reading tabular data failed"+DEFAULT)
                print(str(e))
        return data

    def get_series_columns(self, series_patterns: set) -> defaultdict:
        """
        The spreadsheet data can contain (time) series data.
        They are encoded by column names like 'weight_d0', 'weight_d21' (weight at day 0, ...).
        The mapping indicates which series data can be mapped to the ontolgy by providing patterns like 'weight_d.*'
        This function extracts all relevant column names per pattern as indicated by the mapping.

        For example:
            series_patterns = {'weight_d.*'}
            series_columns = {'weight_d0', 'weight_d21'}
        """
        series_columns = defaultdict(set)
        for pattern in series_patterns:
            column_headers = set()
            values = set()
            for column in self.columns:

                regEx = pattern.replace(".*", "(-?\d*\.{0,1}\d+)")
                matching = re.match(rf'{regEx}$', column)
                if matching:
                    column_headers.add(column)
                    values.add(matching[1])

            self.column_pattern_values[pattern] = values

            if values:
                series_columns[pattern] = list(column_headers)
            else:
                print(f'Mapped column {pattern} is not found in dataset. Please check the mapping file.')
        return series_columns



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

        for key, values in self.series_dict.items():
            if '(.*)' in key:
                new_columns = []
                for column in values:
                    extracted_value = re.match(key, column)[1]
                    new_column_name = f'value_{column}'
                    new_columns.append(new_column_name)

                    self.dataset[new_column_name] = extracted_value

                self.series_dict[key] = new_columns

    def merge_spreadsheets(self, workbook: pd.DataFrame, merge_field: str) -> pd.DataFrame:
        """
        Method merges spreahsheets based on merge_field
        specified in the config file
        """
        worksheets = [*workbook]
        combined_dataset = workbook[worksheets[0]]
        for index in range(1, len(worksheets)):
            combined_dataset = combined_dataset.merge(workbook[worksheets[index]], on=merge_field, how="outer")
        return combined_dataset

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
                        reshape_columns = self.__reocur_columns_to_reshape(map_columns, self.series_dict)
                        self.multiple_columns = []
                        unique_columns = list(
                            set(self.columns).difference(self.series_columns).difference(set(self.multiple_columns)))
                        uniq_reshaped = unique_columns + [v for value in reshape_columns.values() for v in value]




                    elif not map_columns and (len(properties_or_classes) > 1 or 'experimentDay' in property_or_class):

                        reshape_columns = {k: v for k, v in self.__columns_to_reshape(property_or_class).items() if
                                           v not in self.reusable_column}
                        unique_columns = list(set(self.columns).difference(self.multiple_columns))
                        uniq_reshaped =  list(set(unique_columns + [v for value in reshape_columns.values() for v in
                                                          value] + self.reusable_column))


                    else:

                        continue

                    new_df = self.dataset[uniq_reshaped]
                    reshaped = pd.lreshape(new_df, reshape_columns)

                    ## We might not have expeimentDay series.
                    ## But in case we have, it should be related to an experimentDay
                    ## In this case we rename the columns that is mapped to experiment to 'experimentDay'
                    if reshape_columns.keys() == self.series_dict.keys():
                        reshaped = reshaped.rename(columns={property_or_class['experimentDay']: 'experimentDay'})

                    reshaped['experimentDay'] = reshaped['experimentDay'].apply(
                        lambda x: pd.to_numeric(x, errors='ignore'))

                    # days and hours experiments
                    experiment_time = ['experimentDay']

                    if 'experimentHour' in reshaped:
                        reshaped['experimentHour'] = reshaped['experimentHour'].apply(
                            lambda x: pd.to_numeric(x, errors='ignore'))
                        if 'experimentHour' in reshaped_combined:
                            experiment_time.append('experimentHour')


                    if not reshaped_combined.empty:
                        # check for already created columns

                        already_created = set(reshaped_combined.columns).intersection(set(reshaped.columns))
                        # fix experiment hour
                        reshaped_combined = reshaped_combined.merge(reshaped,
                                                                    on=list(already_created),
                                                                    how="outer")
                    else:
                        reshaped_combined = reshaped

                    if 'experimentHour' in property_or_class:
                        property_or_class['experimentHour'] = 'experimentHour'
                mappings['Experiment']['experimentDay'] = 'experimentDay'
        if not reshaped_combined.empty:
            reshaped_combined.dropna(subset=['experimentDay'], inplace=True)
            return reshaped_combined
        else:

            return self.dataset

    def __reocur_columns_to_reshape(self, map_columns: dict, series_dict) -> dict:
        """
        Method returns which columns should be reshaped
        for a given slice of a mapping (mapping per concept)

        For example:
         map_columns = {'experimentDay': 'weight_d(.*)', 'hasNumericalValue': 'weight_d.*'}

         Method returns {'experimentDay': ['value_weight_d0', 'value_weight_d21'], 'weight_d.*': ['weight_d0', 'weight_d21']}

        """
        reshaped = {(v if k != 'experimentDay' and k != 'experimentHour' else k): series_dict[v] for
                    k, v in map_columns.items() if k not in self.reusable_column}

        # check if the values has the same lenght, otherwise create an empty columns for missing column headers
        if len(set([len(v) for v in list(reshaped.values())])) != 1:
            reshaped = self.__create_missing_columns(map_columns, reshaped)

        reshaped = {k: sorted(v, key=utils.num_sort) for k, v in reshaped.items()}


        return reshaped

    def get_multiple_columns(self, mappings):
        update_columns = []
        if isinstance(mappings['Experiment']['experimentDay'], list):
            experiment_days = mappings['Experiment']['experimentDay']
            # for each experiment day we check for properties that belong to it
            # only if they are in dataset


            for properties_or_classes in mappings.values():
                if isinstance(properties_or_classes, list):  # put it in a list for simplification
                    for property in properties_or_classes:
                        update_columns = update_columns + self.check_experiment_group(experiment_days,
                                                                                      self.__columns_to_reshape(
                                                                                          property))
                else:
                    update_columns = update_columns + self.check_experiment_group(experiment_days,
                                                                                  self.__columns_to_reshape(
                                                                                      properties_or_classes))

        return update_columns

    def check_experiment_group(self, experiment_days, group):
        update_columns = []
        for experiment_day in experiment_days:
            column_names = [v for v_list in group.values() for v in v_list]
            if experiment_day in column_names:
                update_columns = update_columns + column_names
        return update_columns

    def __columns_to_reshape(self, properties_classes):
        # if columns in are in dataset, they need to be reshaped

        reshaped_columns = {}
        for property, column in properties_classes.items():
            if isinstance(column, dict):
                reshaped_columns.update(self.__columns_to_reshape(column))
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

    def __create_missing_columns(self, map_columns, reshape_columns):

        max_columns = sorted(reshape_columns, key=lambda k: len(reshape_columns[k]), reverse=True)[0]

        for property in reshape_columns.keys():

            if property != max_columns:
                if max_columns not in self.column_pattern_values:
                    max_set = self.column_pattern_values[map_columns[max_columns]]

                else:
                    max_set = self.column_pattern_values[max_columns]

                if property not in self.column_pattern_values:
                    column_differences = max_set.difference(self.column_pattern_values[map_columns[property]])
                else:
                    column_differences = max_set.difference(self.column_pattern_values[property])
                if column_differences:
                    for diff in column_differences:
                        new_column_header = property.replace('.*', diff)
                        self.dataset[new_column_header] = ''



                        reshape_columns[property].append(new_column_header)


        return reshape_columns



















