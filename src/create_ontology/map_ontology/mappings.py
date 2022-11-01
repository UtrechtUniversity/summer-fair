
import yaml

import utils


class Mappings:
    def __init__(self, map_file: str):
        """
        Attributes:

            mappings: dict
                mapping file with removed empty properties
            meta_data: dict
                dictionary with meta data
            required_field: str
                by specifying  required field we only create concepts from the rows,
                where this field is present.
                For example, if required_field = 'animal_id', we skip all the rows,
                where the value of 'animal_id' is empty.
            ont_mappings: dict
                slice of the mapping file, that only includes ontology classes and properties
            reocur_mappings: set
                set of column names from mapping file, where '.*' is in the name
            update_values: list of dictionaries
                specifies column and values we need to update
                For example:
                update_values= [{ 'column_name': 'swab.*',
                                  'values': {'+': ['++','+*'], '-':['--','-*']}
                               }]

        """
        self.mappings = self.parse_config(map_file)
        self.meta_data = self.get_meta_data()
        self.required_field = self.mappings.get('required', None)
        self.ont_mappings = self.mappings['ontology_schema']
        self.reocur_mappings = utils.get_reocurring_map_columns(self.ont_mappings)
        self.update_values = self.mappings.get('update_values',None)


    def parse_config(self, config: str) -> dict :
        """
        Parse .yaml mapping file
        and remove empty properties
        """
        with open(config, 'r') as fileobj:
            mappings = yaml.load(fileobj, Loader=yaml.SafeLoader)
        # remove empty properties
        return self.remove_empty_prop(mappings)

    def get_meta_data(self):
        """
        If meta_data key exists in a mapping file,
        return it to a variable self.meta_data
        """
        return self.mappings['meta_data'] if 'meta_data' in self.mappings else ''


    @staticmethod
    def split_properties_or_classes(properties_or_dependants: dict) -> (dict, dict):
        """
        The mapping file has a nested structure.
        One ontology class may include another ontology class it relates to
        and specify its properties in mappings.

        This method for each ontology class splits the properties and
        mappings for the dependant classes.

        For example:
        properties_or_dependants= {'experimentDay': 'weight_d(.*)',
                                   'hasQuantity': 'BodyMass',
                                   'hasHost': 'Host',
                                   'BodyMass': {'hasPhenomenon': 'Host'},
                                   'Measure': {'hasNumericalValue': 'weight_d.*'}}
        properties = {'experimentDay': 'weight_d(.*)', 'hasQuantity': 'BodyMass'}
        dependants = {'BodyMass': {'hasPhenomenon': 'Host'}, 'Measure': {'hasNumericalValue': 'weight_d.*'}}

        """
        properties = {}
        dependants = {}
        for property_or_dependant, values in properties_or_dependants.items():
            if property_or_dependant[0].islower():
                properties[property_or_dependant] = values
            else:
                dependants[property_or_dependant] = values
        return properties, dependants


    def remove_empty_prop(self, map_file: dict) -> dict:
        """
        Recursive method that removes empty properties from mapping file

        For example:
        map_file = {'Host': {'sex': None,
                  'id': 'animalnr_col'}

        updated = {'Host': {'id':'animalnr_col'}}
        """
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

