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

##Data interpretation rules
File [DataInterpretationRules.R](src/R/DataInterpretationRules.R) contains a number of "rule" to determine the status of a host. Tha status of a host is coded: 
* Susceptible = 0
* Latent = 1
* Infectious = 2
* Recovered = 3

The overall organisation of these rules is as follows:
- a rule has at least two parameters: timeseries and var.id
  * timeseries contains one of more variables to determine the host status. The timeseries are ordered by time!
  * var.id is a vector of names of variables to use to determine the host status.
- Each rule can have (but not always implemented yet) several forms resulting in a name starting with "rule" and 1 or 2 attributes e.g. "rule.asis.recode". The first attribute defines the way the status is determined, the second attribute how to 'code' the input into positive or negative and the third is an addition to the 'coding'. 
The following status attributing methods (first attribute) are implemented:
- asis: positive = infectious based on 1 variable
- any: positive = infectious if one of more variables is positive
- all: positive = infectious if all variables are positive
- sincefirst: all timepoints infectious since first positive result based on 1 variable
- sinceany: all timepoints infectious since first positive result if one of more variables is positive
- sinceall: all timepoints infectious since first moment when all variables are positive
- consecutive: infectious after the first moment when at least n consecutive time points are positive
- testinfectioustestrecovered: one test determines infectiousness another recovery status. 
- sincefirstinfectioustestrecovered: first postive of a specific variable until first positive of other variable is infectious. Second test determines recovered status.
The following attributes denoted the way positive and negative are discerned:
- "": No attribute the data is provided as 0's and 1' and return 0's (susceptible) and 2's (infectious).
- recode: recode strings (e.g. "+","-")
- cutoff: recode measurement based on cutoff value
- detectionLimit: recode measurement based on detection limit that can be greater than or less than, and can differ per sample
-



