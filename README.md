# Decomposition of health-adjusted life expectancy (HALE)
## Purpose
The purpose for this repository is to make the analytic code for this project available.

## Organization

Files in this repo should be run in the following order:
1. create_hale_input_df.R

Takes as arguments a file path for input data and a file path for output data. Reads in the input data of gbd_to_dex_cause_map.csv and decomp_case_definitions.csv

2. get_cause_replaced_mort_rate.R

Takes as arguments a file path for input data and a file path for output data. Reads in the input data of hale_decomp_input_data.csv

3. hale_decomp.R

Takes the output of create_hale_input_df.R and get_cause_replaced_mort_rates.R and produces a unified decomposition results file. 

## Inputs
Inputs required for the code to run are:
gdb_to_dex_cause_map.csv, decomp_case_definitions.csv, and the output files from create_hale_input_df.R and get_cause_replaced_mort_rate.R
