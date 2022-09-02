# Raw Data

## NRSA and NLA data
All water chemistry data from the National Aquatic Resource Surveys were downloaded from the [EPA website](https://www.epa.gov/national-aquatic-resource-surveys/data-national-aquatic-resource-surveys) on DATE. 

## Policy variables
### 319 grant data
All data pertaining to 319 grants were obtained from the public-facing [EPA GRTS database](https://ofmpub.epa.gov/apex/grts/f?p=109:5000::::::) using a guest login. 
We downloaded Project Summary Reports and Project Budget Reports from 1996-2019 in the Interactive Reports tab on April 1, 2022. 
The downloaded data used for analyses are located in the `grts_project-budget-report` and `grts_project-summary-report` folders.

### Nutrient criteria data

<<<<<<< HEAD
## Landcover data
Landcover data were downloaded on DATE from WEBSITE

## Population data
Population data summarized at the state level were obtained from the US census. 

Table 1. Annual Estimates of the Resident Population: April 1, 2010 to July 1, 2019 (PEPANNRES)
Source: U.S. Census Bureau, Population Division 
Release Date: December 2019

We used the census data from 2010 and the estimated population estiamtes produced by the census for the following years.
Data were downloaded on 26 August 2022
data are titled "census_population_estimates.csv"


### TMDL data

These data describe which sites were assessed in accordance with 303-d program. Data were accessed from https://www.epa.gov/waterdata/waters-geospatial-data-downloads#NationalGeospatialDatasets on 16 March 2022. We used the "Pre-2015 305(b) Waters As Assessed Reach Indexed Dataset Archive" dataset. This dataset describes all waters assessed within a state, both those that were found to be impaired and those that met their designated uses. 
The downloaded data used for analyses are located in TMDL_data.csv and data summarized at the state level are in
"TMDL_data_summary.csv"

## Nutrient loading predictors
To control for changes in nutrient loading which may drive patterns in nutrient concentrations across states, we summarized variables we expected to correlate with nutrient loading using principle components analysis. We divided each variable by the state area and z scored it prior to running the principle components analysis and used the primary (PC1) and/or secondary (PC2) axis of the PCA in our policy model. 

### Land cover data 
Land cover data was obtained from the national landscover database

### Population data
>>>>>>> 4ce0c44ff2569ce2521d6a16fe8b6d5db39d75ae
Census data (COLUMNS) from (WEBSITE) on (DATE)

### Feed and fertilizer data
Feed and fertilizer data were obtained from the United States Department of Agriculure Census of Agriculture


# Derived Data
All derived data are located in the `clean_data` folder

