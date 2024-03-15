# Estimation algorithms R Scripts

This folder contains files to estimate transmission parameters from observations on individuals. 

## Algorithms
- LocalAlgorithm.R: Methods to arrange data and to estimate parameters. 
- GlobalAlgorithm.R: Methods to combine output from LocalAlgorithm.R methods from different data set (locations)
## Supporting scripts
- DataInterpretationRules.R: Methods ('rules') to define infection state of host. Methods in this file are required for the LocalAlgorithm.R or user needs to write their own.
- summerfair_config.yaml: configuration file for one of the methods in LocalAlgorithm.R to define estimation procedures and data interpretation rules. 
- DataQuality.R: Assess the data going into the local algoritm.
## Running the algorithms
- ExampleCode.R: To do
- TutorialAlgorithms.R: File to be used in tutorials for the methods. 
