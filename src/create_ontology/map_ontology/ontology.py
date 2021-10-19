import uuid
from collections import defaultdict
import itertools
import re
import pandas as pd
from rdflib import Graph, OWL
from rdflib import Namespace
from rdflib import URIRef, Literal
from rdflib.namespace import RDF, RDFS

import class_uri


class Individual:
    def __init__(self, ont_class, dictionary):
        self.is_a = ont_class

        # Dynamically create all the data_properties
        for ont_property, mapping_value in dictionary.items():
            setattr(self, ont_property, mapping_value)


class Ontology:
    def __init__(self, ont_file):
        self.graph = Graph().parse(ont_file, format="ttl")
        self.namespace = Namespace("http://www.semanticweb.org/trans_experiment#")
        self.om = Namespace("http://www.ontology-of-units-of-measure.org/resource/om-2/")
        self.class_names = self.get_class_names()
        self.class_relations = self.get_class_relations()
        self.individuals_per_row = defaultdict(list)
        self.data_properties = self.get_data_properties()
        self.relations = self.get_object_properties()
        self.ont_individuals = self.get_existing_individuals()


    def get_classes(self):
        return [classes for classes in self.graph.subjects(RDF.type, OWL.Class)]

    def get_data_properties(self):
        return {self.remove_namespace(ont_property): ont_property for ont_property in
                self.graph.subjects(RDF.type, OWL.DatatypeProperty)}

    def get_existing_individuals(self):
        return {self.remove_namespace(ont_individual): ont_individual for ont_individual in
                self.graph.subjects(RDF.type, OWL.NamedIndividual)}




    def get_object_properties(self):
        return {self.remove_namespace(ont_property): ont_property for ont_property in
                self.graph.subjects(RDF.type, OWL.ObjectProperty)}

    def get_class_names(self):
        classes = self.get_classes()
        return {self.remove_namespace(ont_class): ont_class for ont_class in classes}

    def remove_namespace(self, URI):
        return URI.replace(self.om, '').replace(self.namespace, '')

    def get_data_properties_of_class(self, ont_class):
        return {self.remove_namespace(ont_property): ont_property for ont_property in
                self.graph.subjects(RDFS.domain, ont_class) if ont_property in self.data_properties.values()}

    def get_subclasses(self, ont_class):
        return {subject for subject in self.graph.subjects(RDFS.subClassOf, ont_class)}

    def get_superclasses(self, ont_class):
        return {subject for subject in self.graph.subjects(RDFS.subClassOf, ont_class)}

    def update_with_subclasses(self, ont_classes):
        ont_classes.update(
            {child_class for ont_class in ont_classes for child_class in self.get_subclasses(ont_class)})
        return ont_classes

    def get_domain_property(self, ont_property):
        return [self.remove_namespace(domain) for domain in self.graph.objects(ont_property, RDFS.domain)]

    def get_range_property(self, ont_property):
        return [self.remove_namespace(range) for range in self.graph.objects(ont_property, RDFS.range)]

    def get_class_relations(self):
        class_relation = {}
        # for every object property we create a pair of domain and range
        for object_property in self.graph.subjects(RDF.type, OWL.ObjectProperty):
            domains = {domain for domain in self.graph.objects(object_property, RDFS.domain)}
            ranges = {range for range in self.graph.objects(object_property, RDFS.range)}
            # for each domain and range class, get its subclasses
            domain_classes = self.update_with_subclasses(domains)
            range_classes = self.update_with_subclasses(ranges)

            for domain in domain_classes:
                for range in range_classes:
                    class_relation[(self.remove_namespace(domain), self.remove_namespace(range))] = object_property

        return class_relation

    def create_individual(self, ont_class, map_properties, row=pd.Series([])):

        if not row.empty:
            for property, column_name in map_properties.items():
                map_properties[property] = row[column_name]

        uri = str(self.get_simple_uri(ont_class, map_properties)).replace(' ','_')
        map_properties['uri'] = URIRef(self.namespace + uri)

        self.create_properties(map_properties)
        self.assign_class(map_properties['uri'], ont_class)

        return Individual(ont_class, map_properties)

    def assign_class(self, individual, ont_class):
        self.graph.add((individual, RDF.type, self.class_names[ont_class]))
        if self.get_superclasses(ont_class):
            for super_class in self.get_superclasses(self.class_names[ont_class]):
                self.graph.add(individual, RDF.type, self.class_names[super_class])

    @staticmethod
    def get_property_value(map_property, row, reoccur_value):
        if '(.*)' in map_property:
            return reoccur_value
        elif '.*' in map_property:
            map_property=map_property.replace('.*', reoccur_value)
            if map_property in row:
                return row[map_property]
            else:
                return ''
        elif map_property in row:
            return row[map_property]
        else:
            return map_property

    def update_reocur_properties(self, map_properties, row, reoccur_value):
        for property, column_name in map_properties.items():
            if property in self.data_properties:
                value = self.get_property_value(column_name, row, reoccur_value)
                map_properties[property] = value
        return map_properties

    def split_properties(self, ont_class, map_properties):
        relations = {ont_property: map_properties[ont_property] for ont_property in map_properties.keys() if
                     ont_property in self.relations}
        properties = {ont_property: map_properties[ont_property] for ont_property in map_properties.keys() if
                      ont_property in self.data_properties and ont_property in self.get_data_properties_of_class(
                          self.class_names[ont_class])}

        return relations, properties

    def get_linked_class_properties(self, ont_class, map_properties):
        linked_class_properties = defaultdict(dict)
        for property, column in map_properties.items():
            if not property in self.get_data_properties_of_class(
                    self.class_names[ont_class]) and property in self.data_properties:
                linked_class = self.get_domain_property(self.data_properties[property])[0]
                if linked_class in class_uri.linked_classes[ont_class]:
                    linked_class_properties[linked_class][property] = column
        return linked_class_properties

    def has_linked_class(self, properties, linked_class):
        return any([True if property in self.data_properties and
                            self.get_domain_property(self.data_properties[property])[0] == linked_class else False for
                    property, column in properties.items()])

    def create_reoccur_individual(self, ont_class, map_properties, row, reoccur_value, dependant_classes=None):
        # substitute reocurring value in a property
        updated_map_properties = self.update_reocur_properties(map_properties.copy(), row, reoccur_value)
        # split the properties on a class for data properties and relations
        # Note that the relations can related to classes that are not yet created

        relations, properties = self.split_properties(ont_class, updated_map_properties)
        linked_class_properties = self.get_linked_class_properties(ont_class, updated_map_properties)

        individual = self.create_individual(ont_class, properties)

        for ont_class, properties in linked_class_properties.items():
            linked_individual = self.create_individual(ont_class, properties)
            self.create_relation(individual, linked_individual)

        all_dependant_classes = {}
        if dependant_classes:
            for dependant_class in dependant_classes:
                dependant_individual = self.create_reoccur_individual(dependant_class, dependant_classes[dependant_class],row, reoccur_value)
                all_dependant_classes[dependant_class] = dependant_individual
                self.individuals_per_row[dependant_class] = dependant_individual
                self.create_relation(individual,dependant_individual)

        for pair in itertools.combinations(all_dependant_classes.values(), 2):
            self.create_relation(pair[0], pair[1])

        #create a relations that the individual has
        for relation, individual2 in relations.items():
            if individual2 in self.ont_individuals:
                self.create_relation(individual, individual2, relation)
            else:
                if individual2 in self.individuals_per_row:
                    individual2 = self.individuals_per_row[individual2]
                if individual2 in all_dependant_classes:
                    individual2 = all_dependant_classes[individual2]
                self.create_relation(individual, individual2, relation)

        return individual

    def create_relation(self, individual1, individual2, relation=None):
        if individual2 in self.ont_individuals:
                self.graph.add((individual1.uri, self.relations[relation], self.ont_individuals[individual2]))
        else:
            classes_pairs = [(individual1, individual2), (individual2, individual1)]
            for classes_pair in classes_pairs:
                if (classes_pair[0].is_a, classes_pair[1].is_a) in self.class_relations:
                    relation = self.class_relations[(classes_pair[0].is_a, classes_pair[1].is_a)]
                    self.graph.add((classes_pair[0].uri, relation, classes_pair[1].uri))

    def get_simple_uri(self, ont_class, map_properties):
        try:
            uri_string = getattr(class_uri, ont_class).copy()
            for value in uri_string.copy():
                if value[0].isupper():
                    if hasattr(class_uri, value) and value != ont_class:
                        # we get value of the class it is based on
                        uri_string[uri_string.index(value)] = map_properties[value]['uri']
                    else:
                        continue
                else:
                    if value in map_properties:
                        uri_string[uri_string.index(value)] = str(map_properties[value])
                    elif value == 'index':
                        uri_string[uri_string.index(value)] = str(uuid.uuid4())
                    else:
                        uri_string.remove(value)
            return '_'.join(uri_string).strip()
        except AttributeError:
            print(f'Class {ont_class} has no URI template')

    def populate_ontology(self, mappings, row, columns):

        for ont_class, properties_or_classes in mappings.ont_mappings.items():
            if not isinstance(properties_or_classes, list):
                properties_or_classes = [properties_or_classes]  # put it in a list for simplification

            for property_or_class in properties_or_classes:
                properties, dependant_classes = mappings.split_properties_or_classes(property_or_class)

                if ont_class in self.class_names:
                    if mappings.has_reoccuring_prop(properties):
                        if ont_class == 'Experiment':
                            continue  # will be create by another classes
                        else:
                            # if we still did not create a Experiment
                            # that means that we will create it inside other reoccuring classes
                            if 'Experiment' not in self.individuals_per_row and self.has_linked_class(properties,
                                                                                                      'Experiment'):
                                properties.update(
                                    {k: v for k, v in mappings.ont_mappings['Experiment'].items() if
                                     k not in properties})

                            for reoccur_value in mappings.get_multiple_recoruring_values(properties):
                                self.create_reoccur_individual(ont_class, properties, row, reoccur_value,
                                                               dependant_classes)
                    else:
                        relations, properties = self.split_properties(ont_class, properties)
                        individual = self.create_individual(ont_class, properties, row)
                        self.individuals_per_row[ont_class] = individual

                        for relation, individual2 in relations.items():

                            self.create_relation(individual, self.individuals_per_row[individual2])

    def save_ontology(self, name):
        self.graph.serialize(destination=name, format='ttl')

    def create_properties(self, mappings):
        individual = mappings['uri']
        for ont_property, value in mappings.items():
            if ont_property != 'uri':
                property =  self.data_properties[ont_property]
                try:
                    value = int(float(value)) if re.search('\.0$',str(value)) else  float(value) if  isinstance(value, float) else value
                except:
                    value = value
                self.graph.add((individual, property, Literal(str(value).strip())))