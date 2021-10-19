import re

import yaml


class Mappings:
    def __init__(self, map_file, columns):
        self.mappings = self.parse_config(map_file)
        self.meta_data = self.get_meta_data()
        self.required_field = self.mappings.get('required', None)
        self.ont_mappings = self.mappings['ontology_schema']
        self.column_names = columns
        self.reocurring_values = {}
        self.mapping_per_row = {}
        self.class_properties = {}

    def parse_config(self, config):
        with open(config, 'r') as fileobj:
            mappings = yaml.load(fileobj, Loader=yaml.SafeLoader)
        # remove empty properties
        return self.remove_empty_prop(mappings)

    def get_meta_data(self):
        return self.mappings['meta_data'] if 'meta_data' in self.mappings else ''

    def get_trans_mappings(self):
        return {k: v for k, v in self.mappings.items() if k not in self.meta_data and k != 'required'}

    def get_recoruring_values(self, column_name):
        values = set()
        for column in self.column_names:
            if re.match(column_name.replace('.*', '(\d{1,4})'), column):
                pattern = column_name.replace('.*', '(\d{1,4})')
                values.add(re.search(pattern, column)[1])

        return values

    @staticmethod
    def split_properties_or_classes(properties_or_dependants):
        properties = {}
        dependants = {}
        for property_or_dependant, values in properties_or_dependants.items():
            if property_or_dependant[0].islower():
                properties[property_or_dependant] = values
            else:
                dependants[property_or_dependant] = values
        return properties, dependants

    @staticmethod
    def has_reoccuring_prop(mappings):
        # if we have reocur properties {id: 'A', day:'day.*'}
        # or if there are more than one column {id:'A', day:[swab_day,weight_day]}
        return any(
            [True if '.*' in property or isinstance(property, list) else False for property in mappings.values()])

    def get_multiple_recoruring_values(self, properties):
        all_values_per_group = set()
        for _, value in properties.items():
            all_values_per_group.update(self.get_recoruring_values(value))
        return all_values_per_group

    def remove_empty_prop(self, map_file):
        updated = {}
        for k, v in map_file.items():
            if isinstance(v, dict):
                v = self.remove_empty_prop(v)
            if isinstance(v, list):
                new_v = []
                for element in v:
                    if isinstance(element, dict):
                        new_v.append(self.remove_empty_prop(element))
                    else:
                        new_v.append(element)
                v = new_v
            if not v in (u'', None, {}):
                updated[k] = v
        return updated
