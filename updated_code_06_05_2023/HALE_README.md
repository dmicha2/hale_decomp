Decomposition of health-adjusted life expectancy (HALE)
Purpose
The purpose for this file is to make the analytic code for the HALE portion of the project available.

Organization
Files in this repo should be run in the following order:

create_hale_input_df.R
Takes as arguments a file path for input data and a file path for output data. Reads in the gbd_to_dex_cause_map.csv and decomp_case_definitions.csv as input data and returns the hale_decomp_input_data.csv.

create_hale_input_adjusted_df.R
Takes as arguments a file path for input data and a file path for output data. Reads in the hale_decomp_input_data.csv as input data and returns the hale_decomp_input_data_final.csv and the hale_decomp_input_data_dex_gbd_aggregate_death_avg.csv. This file manually alters the input data to remove discrepancies in the prior input dataset with the later output file having all the incorporated changes in the input data.

get_cause_replaced_mort_rate.R
Takes as arguments a file path for input data and a file path for output data. Reads in the hale_decomp_input_data_dex_gbd_aggregate_death_avg.csv as input data and returns the cause_replaced_mort_rates_dex_gbd_agg_death_avg.csv.

hale_decomp.R
Reads in the hale_decomp_input_data_dex_gbd_aggregate_death_avg.csv and cause_replaced_mort_rates_dex_gbd_agg_death_avg.csv as input data and produces decomposition results files. The output files are le_additional_variables.csv, cause_replaced_life_table_dex_gbd_agg_death_avg.csv, decomp_results_by_draw_dex_gbd_agg_death_avg.csv (for age group 0), hale_decomp_results_by_draw_at15_dex_gbd_agg_death_avg.csv (for age group 15), hale_decomp_results_by_draw_at45_dex_gbd_agg_death_avg.csv (for age group 45), and hale_decomp_results_by_draw_at65_dex_gbd_agg_death_avg.csv (for age group 65).
Additionally, also produces decomposition results by mean and ui for the age groups named hale_decomp_results_mean_ui_dex_gbd_agg_death_avg.csv (for age group 0), hale_decomp_results_mean_ui_at15_dex_gbd_agg_death_avg.csv (for age group 15), hale_decomp_results_mean_ui_at45_dex_gbd_agg_death_avg.csv (for age group 45), and hale_decomp_results_mean_ui_at65_dex_gbd_agg_death_avg.csv (for age group 65).



Inputs
Inputs required for the code to run are: gdb_to_dex_cause_map.csv, decomp_case_definitions.csv, and the output files from create_hale_input_df.R (hale_decomp_input_data.csv), create_hale_input_adjusted_df.R (hale_decomp_input_data_final.csv and hale_decomp_input_data_dex_gbd_aggregate_death_avg.csv), and get_cause_replaced_mort_rate.R (cause_replaced_mort_rates_dex_gbd_agg_death_avg.csv)
