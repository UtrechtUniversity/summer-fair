import re
from collections import defaultdict

import pandas as pd
import typing


def empty_data(mapping_rows: list, row: pd.Series) -> bool:
    """
    Function checks if the value of the mapped column exists and
    returns True or False.
    This functions makes sure we do not create empty individuals
    (individuals with empty properties)

    For example:
        mapping_rows = ['animalnr_col', 'treatment', 'I, S1, S2']
        row = ('I,S1, S2', 'S2') ('animalnr_col', '3_Ge')  ('treatment', 'control')

        return True, as for each property we have a value in the row

    """
    return all([True if mapping_row not in row or row.get(mapping_row)=='' else False for mapping_row in mapping_rows])


def get_reocur_columns(mapping: dict) -> dict:
    """
    Function gets slice of mapping and
    returns  dictionary with ontology properties and
    columns from the dataset that are to them

    Example:
        mapping = {'experimentDay': 'swab.*_date',
                   'Sample': {'hasType': 'Swab', 'hasResult': 'swab.*'}, 'Pathogen': {'name': 'swab.*_value'}}
        columns = {'experimentDay': 'swab.*_date', 'hasResult': 'swab.*', 'name': 'swab.*_value'}
    """
    columns = defaultdict()
    for props, values in mapping.items():
        if isinstance(values, dict):
            columns.update(get_reocur_columns(values))
        if isinstance(props, str) and props[0].islower():
            if '.*' in values:
                columns[props] = values

    return columns

def get_reocurring_map_columns(mappings: dict) -> set:
    """
    Function gets ontology mappings and
    returns set of all the reocurring properties (where '.*' is in the column name)

    For example:
      mappings = { 'Measurement': [
                   {'experimentDay': 'BS.*_date',
                    'hasHost': 'Environment',
                    'Sample': {'result': 'BS.*', 'hasType': 'EnvironmentalSample'},
                    'Pathogen': {'name': 'BS.*_value'}}
                    ]}
      reocur_props = {'BS.*_value', 'BS.*', 'BS.*_date'}

    """
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


def num_sort(test_string: str) -> int:
    """
    Functions gets a string, searches for a digit
    and returns it.
    For example:
        testing_string = 'value_weight_d21' -> 21
    """
    return list(map(int, re.findall(r'-?\d+', test_string)))[0]