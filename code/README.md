# Code
Code should be run in the order corresponding to the leading number in the file name.

`1-319_cleaning.R`: Loads raw 319 project data from the `data` folder, fixes obvious date entry errors, and summarizes the total spending in $USD for projects with end dates between 2007 and 2019 for each state and for each state and year combination. The summarized data are written to the `clean_data` folder inside the `data` folder.

`2-formatting_lake_data.R`: Loads raw NLA data from the 2007, 2012, and 2017 assessments, harmonizes units, removes observations with data quality flags, merges data across surveys, and outputs total nitrogen and total phosphorus data into the `clean_data` folder.

`3-formatting_NRSA_stream_rivers.R`: Loads raw NRSA data from the 2008-2009, 2013-2014, and 2018-2019 assessments, harmonizes units, removes observations with data quality flags, merges data across surveys, and outputs total nitrogen, total phosphorus data into the `clean_data` folder.

`4-summarizing_predictors_of_loading.R`: 
