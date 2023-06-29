Decomposition of health spending

Purpose:
The purpose for this repository is to make the analytic code for the Spending portion of the project available.

Organization:
Files in this repo should be run in the following order:

create_spending_input_adjusted_df.R

Takes as arguments a file path for input data and a file path for output data. Reads in the gbd_to_dex_cause_map.csv and decomp_case_definitions.csv as input data and returns the spending_decomp_input_data_adjusted.csv and spending_decomp_input_data_dex_gdp_aggegate_death_agg.csv.

get_cause_replaced_mort_rate_spending.R

Takes as arguments a file path for input data and a file path for output data. Reads in the spending_decomp_input_data_dex_gdp_aggegate_death_agg.csv as input data and returns the spending_cause_replaced_mort_rates_dex_gdp_aggregated_death_agg.csv.

spending_decomp.R

Reads in the spending_decomp_input_data_dex_gdp_aggegate_death_agg.csv and spending_cause_replaced_mort_rates_dex_gdp_aggregated_death_agg.csv as input data and produces decomposition results files. The output files are spending_cause_replaced_life_table_dex_gdp_aggregated_death_agg.csv, spending_decomp_results_dex_gdp_aggregated_death_agg.csv (for age 0), and spending_decomp_results_by_draw_at**age**_dex_gdp_aggregated_death_agg.csv (for **age** groups 15,30,45,55,65,75,85).
Additionally, also performs decomposition analysis on mean and ui to give spending_decomp_results_mean_ui_dex_gdp_aggregated_death_agg.csv (for age group 0), and spending_decomp_results_mean_ui_at**age**_dex_gdp_aggregated_death_agg.csv (for **age** groups 15,30,45,55,65,75,85).
Also produces year(1996, and 2016) averaged differences in spending_effect and ex_effect for all draws named spending_decomp_results_by_draw_at**age**_yr_avg.csv, and for means of draws named spending_decomp_results_mean_at**age**_yr_avg.csv.
Additionally, produces year averaged differences between age-agroups for all draws named spending_decomp_effects_age_grp.csv and for means of draws named spending_decomp_mean_draw_effects_age_grp.csv.

Inputs

Inputs required for the code to run are: gdb_to_dex_cause_map.csv, decomp_case_definitions.csv, and the output files from create_spending_input_df.R (spending_decomp_input_data_adjusted.csv and spending_decomp_input_data_dex_gdp_aggegate_death_agg.csv) and get_cause_replaced_mort_rate_spending.R (spending_cause_replaced_mort_rates_dex_gdp_aggregated_death_agg.csv)
