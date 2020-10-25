# elections_prediction

This repo contains code and data for forecasting the US 2020 presidential election. It was created by Anees Shaikh, Jaffa Romain, and Lu Mu. The purpose is to create a report that summarises the results of a statistical model that we built. Some data is unable to be shared publicly. We detail how to get that below. The sections of this repo are: inputs, outputs, scripts.

Inputs contain data that are unchanged from their original. We use two datasets: 

- Survey data 
  To obtain this data, you will have to request access from the Nationscape site. This can be accessed at https://www.voterstudygroup.org/publication/nationscape-data-set. This data may take a bit of time to get access to, so you could get started with the ACS data
- ACS data 
  To obatain this data, you will first have to register at IPUMS https://usa.ipums.org/usa-action/menu. Once you have registerd, proceed to the *Select Data* tab and select only the 2018 ACS sample. IPUMS lists a few techniques to potentially reduce the amount of data as it is a fairly large dataset(dependent on the variables you've selected) that may hamper your local performance.

Outputs contain data that are modified from the input data, the report and supporting material.

- X, 
- Y

Scripts contain R scripts that take inputs and outputs and produce outputs. These are:

- 01_data_cleaning.R
- 02_data_preparation.R
