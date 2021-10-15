Host = ['id']
Experiment = ['Event', 'experimentID', 'experimentDay']
Environment = ['house', 'pen', 'hospital', 'ward']
Pathogen = ['name','index']
Sample = ['SampleType', 'index', 'experimentDay', 'experimentHour', 'result']
Gene = ['name']
BodyMass = ['BodyMass', 'index']
Measure = ['hasNumericalValue']
Measurement = ['M', 'index']
SpecificViableCount = ['Quantity', 'index']

linked_classes = {'Measurement': ['Experiment']}
