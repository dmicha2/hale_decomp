### Ratios for spending effect/hale effect

## Purpose
The purpose of this folder is to make the analytic code for producing the ratios

## Organization

For running the file in this folder, you need to have run the files in the parent folder as detailed in the HALE_README.md and Spending_README.md.
It reads in the decomp_results_by_draw_at**age**dex_gbd_agg_death_avg.csv files conatining the HALE decomposition results and the non_cr_spending_decomp_results_by_draw_at**age**.csv files containing the spending decomposition results with non cause-replaced mortality rates to get the ratios.
This file reports the ac_lt_spend_by_hale_effect_non_cr_1000draw_at**age**.csv files for each age groups containing the 1000 ratio mean, median, ui, and iqr estimates. Here, the 1000 draws are formed by taking draw_n of lt_spending_effect and dividing it by draw_n of hale_effect to get 1000 draws of spending_by_hale_effect measure.

## Input 
It reads in the decomp_results_by_draw_at**age**dex_gbd_agg_death_avg.csv files conatining the HALE decomposition results and the non_cr_spending_decomp_results_by_draw_at**age**.csv files containing the spending decomposition results with non cause-replaced mortality rates.
