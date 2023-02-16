# Decomposition of health-adjusted life expectancy (HALE)
## Purpose
The purpose for this repository is to make the analytic code for this project available.

## Organization

Files in this repo should be run in the following order:
1. create_hale_input_df.R

Takes as arguments a file path for input data and a file path for output data. Reads in the gbd_to_dex_cause_map.csv and decomp_case_definitions.csv as input data and returns the hale_decomp_input_data.csv. 

2. create_hale_input_adjusted_df.R

Takes as arguments a file path for input data and a file path for output data. Reads in the hale_decomp_input_data.csv as input data and returns the hale_decomp_input_data_final.csv. This file manually alters the input data to remove discrepancies in the prior input dataset.

2. get_cause_replaced_mort_rate.R

Takes as arguments a file path for input data and a file path for output data. Reads in the hale_decomp_input_data.csv as input data and returns the cause_replaced_mort_rates.csv.

3. hale_decomp.R

Reads in the hale_decomp_input_data.csv and cause_replaced_mort_rates.csv as input data and produces decomposition results files. The output files are cause_replaced_life_table.csv and decomp_results_by_draw.csv. 

## Inputs
Inputs required for the code to run are:
gdb_to_dex_cause_map.csv, decomp_case_definitions.csv, and the output files from create_hale_input_df.R (hale_decomp_input_data.csv), create_hale_input_adjusted_df.R (hale_decomp_input_data_final.csv), and get_cause_replaced_mort_rate.R (cause_replaced_mort_rates.csv)
