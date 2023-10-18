Decomposition of health spending

Purpose:
The purpose for this repository is to make the analytic code for the Spending portion of the project available.

Organization:
Files in this repo should be run in the following order:

create_spending_input_adjusted_df.R

Takes as arguments a file path for input data and a file path for output data. Reads in the gbd_to_dex_cause_map.csv and decomp_case_definitions.csv as input data and returns the spending_decomp_input_data_adjusted.csv and spending_decomp_input_data_dex_gdp_aggegate_death_agg.csv.

get_cause_replaced_mort_rate_spending.R

Takes as arguments a file path for input data and a file path for output data. Reads in the spending_decomp_input_data_dex_gdp_aggegate_death_agg.csv as input data and returns the spending_not_cause_replaced_mort_rates.csv.

spending_decomp.R

Reads in the spending_decomp_input_data_dex_gdp_aggegate_death_agg.csv and spending_not_cause_replaced_mort_rates.csv as input data and produces decomposition results files. The output files are spending_not_cause_replaced_life_table.csv, non_cr_spending_decomp_results_at0.csv (for age 0), and non_cr_spending_decomp_results_by_draw_at**age**.csv (for **age** groups 15,30,45,55,65,75,85).
Additionally, also performs decomposition analysis on mean and ui to give non_cr_spending_decomp_results_mean_ui_at**age**.csv (for **age** groups 0,15,30,45,55,65,75,85).

Inputs

Inputs required for the code to run are: gdb_to_dex_cause_map.csv, decomp_case_definitions.csv, and the output files from create_spending_input_df.R (spending_decomp_input_data_adjusted.csv and spending_decomp_input_data_dex_gdp_aggegate_death_agg.csv) and get_cause_replaced_mort_rate_spending.R (spending_cause_replaced_mort_rates_dex_gdp_aggregated_death_agg.csv)
