Host = ['id']
Experiment = ['Event', 'experimentID', 'experimentDay']
Pathogen = ['name','index']
Environment = ['level1', 'level2', 'level3']
Sample = ['SampleType', 'index', 'experimentDay', 'experimentHour', 'result']
Gene = ['name']
BodyMass = ['BodyMass', 'index']
Measure = ['hasNumericalValue']
Measurement = ['M', 'index']
SpecificViableCount = ['Quantity', 'index']

linked_classes = {'Measurement': ['Experiment']}
