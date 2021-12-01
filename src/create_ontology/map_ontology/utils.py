import re
from collections import defaultdict

import pandas as pd


def empty_data(mapping_rows: list, row: pd.DataFrame):
    return all([True if mapping_row not in row or row.get(mapping_row)=='' else False for mapping_row in mapping_rows])





def merge_spreadsheets(workbook: str, merge_field: str) -> pd.DataFrame:
    # parse dataset
    df = pd.read_excel(workbook, sheet_name=None)
    worksheets = [*df]
    combined_dataset = df[worksheets[0]]
    for index in range(1, len(worksheets)):
        combined_dataset = combined_dataset.merge(df[worksheets[index]], on=merge_field, how="outer")
    return combined_dataset


def stack_columns(df: pd.DataFrame, index: list, stack: list, rename: list) -> pd.DataFrame:
    """
    Use stack on section of a dataframe and merge stacked data back into original dataframe

    Keyword arguments:
    df -- original dataframe
    index -- columns that not need to be stacked; values are repeated in sequential rows
    stack -- columns that need to be stacked
    rename -- new names for stacked columns like [stackname,valuename]
    """

    # This part will not be stacked
    df = df.set_index(index)

    # This part will be stacked
    df_stack = df[stack].stack(dropna=False)
    df_stack.name = rename[1]

    df_stack = df_stack.reset_index()
    df_stack = df_stack.rename({'level_1': rename[0]}, axis=1)

    return df_stack


def transform_dataset(dataset, mappings, unique_columns, reocur_columns):
    reshaped_combined = pd.DataFrame()
    for ont_class, properties_or_classes in mappings.ont_mappings.items():
        if ont_class != 'Experiment':
            if not isinstance(properties_or_classes, list):
                properties_or_classes = [properties_or_classes]  # put it in a list for simplification

            for property_or_class in properties_or_classes:
                map_columns = get_reocur_columns(property_or_class)
                if map_columns:

                    # substitute the .* with columns, change column names
                    reshape_columns = columns_to_reshape(map_columns, reocur_columns)
                    uniq_reshaped = list(unique_columns) + [v for value in reshape_columns.values() for v in value]
                    new_df = dataset[uniq_reshaped]
                    reshaped = pd.lreshape(new_df, reshape_columns)

                    reshaped['experimentDay'] = reshaped['experimentDay'].astype(object)
                    if not reshaped_combined.empty:
                        reshaped_combined = reshaped_combined.merge(reshaped,
                                                                    on=list(unique_columns) + ['experimentDay'],
                                                                    how="outer")
                    else:
                        reshaped_combined = reshaped

    return reshaped_combined


def values_in_columnames(columns_dict):
    return any([True if '(.*)' in key else False for key in columns_dict.keys()])


def create_new_columns_in_df(df, columns_dict):
    new_columns = []
    for key, values in columns_dict.items():
        if '(.*)' in key:
            for column in values:
                extracted_value = re.match(key, column)[1]
                new_column_name = f'value_{column}'
                new_columns.append(new_column_name)

                df[new_column_name] = extracted_value

            columns_dict[key] = new_columns


def columns_to_reshape(map_columns, reocur_columns):
    return {(v if k != 'experimentDay' else k): sorted(reocur_columns[v]) for k, v in map_columns.items()}


def get_property_value(map_property, row, reoccur_value):
    if '(.*)' in map_property:
        return reoccur_value
    elif '.*' in map_property:
        map_property = map_property.replace('.*', reoccur_value)
        if map_property in row:
            return row[map_property]
        else:
            return ''
    elif map_property in row:
        return row[map_property]
    else:
        return map_property


def get_reocur_columns_(mapping):
    columns = set()
    for props, values in mapping.items():
        if isinstance(values, dict):
            columns.update(get_columns(values))
        if isinstance(props, str) and props[0].islower():
            columns.add(values)

    return columns


def has_reoccuring_prop(mappings):
    # if we have reocur properties {id: 'A', day:'day.*'}
    # or if there are more than one column {id:'A', day:[swab_day,weight_day]}
    return any(
        [True if '.*' in property else False for property in mappings.values()])


def get_reocur_columns(mapping):
    columns = defaultdict()
    for props, values in mapping.items():
        if isinstance(values, dict):
            columns.update(get_reocur_columns(values))
        if isinstance(props, str) and props[0].islower():
            if '.*' in values:
                columns[props] = values

    return columns


def get_class_properties(mapping):
    class_prop = defaultdict(set)
    for k, v in mapping.items():
        for props, values in v.items():
            if isinstance(values, dict):
                class_prop.update(get_class_properties({props: values}))
            if isinstance(props, str) and props[0].islower():
                class_prop[k].add(props)
            if isinstance(values, list):
                for value in values:
                    class_prop[props].update(get_class_properties({k: {props: value}})[props])

    class_properties = class_prop
    return class_prop


def get_dataset_reocur(reocur_columns, ds_columns):
    reocur_ds_columns = defaultdict(set)
    for pattern in reocur_columns:
        reocur_ds_columns[pattern] = get_recoruring_values(ds_columns, pattern)
    return reocur_ds_columns


def get_recoruring_values(column_names, column_name):
    values = set()
    for column in column_names:
        column_name_new = column_name.replace(".*", "\d{1,4}")
        if re.match(rf'{column_name_new}$', column):
            values.add(column)
    return values


def get_reocurring_map_columns(mappings):
    reocur_props = set()
    for k, v in mappings.items():
        if isinstance(v, list):
            for value in v:
                reocur_props.update(get_reocurring_map_columns({k: value}))
        else:
            for props, values in v.items():
                if isinstance(values, dict):
                    reocur_props.update(get_reocurring_map_columns({props: values}))
                if isinstance(props, str) and props[0].islower() and '.*' in values:
                    reocur_props.add(values)
                if isinstance(values, list):
                    for value in values:
                        reocur_props.update(get_reocurring_map_columns({k: {props: value}}))
    return reocur_props
