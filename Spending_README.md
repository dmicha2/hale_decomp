## Decomposition of health spending
## Purpose
The purpose for this repository is to make the analytic code for this project available.

### Organization
Files in this repo should be run in the following order:

1. create_spending_input_adjusted_df.R
Takes as arguments a file path for input data and a file path for output data. Reads in the gbd_to_dex_cause_map.csv and decomp_case_definitions.csv as input data and returns the spending_decomp_input_data_adjusted.csv.

2. get_cause_replaced_mort_rate_spending.R
Takes as arguments a file path for input data and a file path for output data. Reads in the spending_decomp_input_data_adjusted.csv as input data and returns the spending_cause_replaced_mort_rates.csv.

3. spending_decomp.R
Reads in the spending_decomp_input_data_adjusted.csv and cause_replaced_mort_rates_spending.csv as input data and produces decomposition results files. The output files are spending_cause_replaced_life_table.csv and spending_decomp_results_by_draw.csv.

### Inputs
Inputs required for the code to run are: gdb_to_dex_cause_map.csv, decomp_case_definitions.csv, and the output files from create_spending_input_df.R (spending_decomp_input_data_adjusted.csv) and get_cause_replaced_mort_rate_spending.R (spending_cause_replaced_mort_rates.csv)
