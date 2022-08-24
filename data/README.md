# Raw Data

## NRSA and NLA data
All water chemistry data from the National Aquatic Resource Surveys were downloaded from the [EPA website](https://www.epa.gov/national-aquatic-resource-surveys/data-national-aquatic-resource-surveys) on DATE. 

## Policy variables
### 319 grant data
All data pertaining to 319 grants were obtained from the public-facing [EPA GRTS database](https://ofmpub.epa.gov/apex/grts/f?p=109:5000::::::) using a guest login. 
We downloaded Project Summary Reports and Project Budget Reports from 1996-2019 in the Interactive Reports tab on April 1, 2022. 
The downloaded data used for analyses are located in the `grts_project-budget-report` and `grts_project-summary-report` folders.

### Nutrient criteria data

### TMDL data

## Nutrient loading predictors
To control for changes in nutrient loading which may drive patterns in nutrient concentrations across states, we summarized variables we expected to correlate with nutrient loading using principle components analysis. We divided each variable by the state area and z scored it prior to running the principle components analysis and used the primary (PC1) and/or secondary (PC2) axis of the PCA in our policy model. 

### Land cover data 

### Population data
Census data (COLUMNS) from (WEBSITE) on (DATE)

### Feed and fertilizer data
WHAT, WHERE, WHEN


# Derived Data
All derived data are located in the `clean_data` folder

