library(data.table)
library(dplyr)
library(tidyr)
setwd("/home/j/Project/Cost_Effectiveness/BEA/BEA_data_2023/data_new_naming_convention")



##############################################################
### Ratio of spending effect by hale effect using 1000 draws###
#############################################################
#for effects for all 1000 draws
dt_hale_0 <- fread("/home/j/Project/Cost_Effectiveness/BEA/BEA_data_2023/data_new_naming_convention/HALE/decomp_results_by_draw_dex_gbd_agg_death_avg.csv")
dt_hale_15 <- fread("/home/j/Project/Cost_Effectiveness/BEA/BEA_data_2023/data_new_naming_convention/HALE/hale_decomp_results_by_draw_at15_dex_gbd_agg_death_avg.csv")
dt_hale_30 <- fread("/home/j/Project/Cost_Effectiveness/BEA/BEA_data_2023/data_new_naming_convention/HALE/hale_decomp_results_by_draw_at30_dex_gbd_agg_death_avg.csv")
dt_hale_45 <- fread("/home/j/Project/Cost_Effectiveness/BEA/BEA_data_2023/data_new_naming_convention/HALE/hale_decomp_results_by_draw_at45_dex_gbd_agg_death_avg.csv")
dt_hale_55 <- fread("/home/j/Project/Cost_Effectiveness/BEA/BEA_data_2023/data_new_naming_convention/HALE/hale_decomp_results_by_draw_at55_dex_gbd_agg_death_avg.csv")
dt_hale_65 <- fread("/home/j/Project/Cost_Effectiveness/BEA/BEA_data_2023/data_new_naming_convention/HALE/hale_decomp_results_by_draw_at65_dex_gbd_agg_death_avg.csv")
dt_hale_75 <- fread("/home/j/Project/Cost_Effectiveness/BEA/BEA_data_2023/data_new_naming_convention/HALE/hale_decomp_results_by_draw_at75_dex_gbd_agg_death_avg.csv")
dt_hale_85 <- fread("/home/j/Project/Cost_Effectiveness/BEA/BEA_data_2023/data_new_naming_convention/HALE/hale_decomp_results_by_draw_at85_dex_gbd_agg_death_avg.csv")

dt_spending_0 <- fread("/home/j/Project/Cost_Effectiveness/BEA/BEA_data_2023/data_new_naming_convention/Spending/non_cr/non_cr_spending_decomp_results_at0.csv")
dt_spending_15 <- fread("/home/j/Project/Cost_Effectiveness/BEA/BEA_data_2023/data_new_naming_convention/Spending/non_cr/non_cr_spending_decomp_results_by_draw_at15.csv")
dt_spending_30 <- fread("/home/j/Project/Cost_Effectiveness/BEA/BEA_data_2023/data_new_naming_convention/Spending/non_cr/non_cr_spending_decomp_results_by_draw_at30.csv")
dt_spending_45 <- fread("/home/j/Project/Cost_Effectiveness/BEA/BEA_data_2023/data_new_naming_convention/Spending/non_cr/non_cr_spending_decomp_results_by_draw_at45.csv")
dt_spending_55 <- fread("/home/j/Project/Cost_Effectiveness/BEA/BEA_data_2023/data_new_naming_convention/Spending/non_cr/non_cr_spending_decomp_results_by_draw_at55.csv")
dt_spending_65 <- fread("/home/j/Project/Cost_Effectiveness/BEA/BEA_data_2023/data_new_naming_convention/Spending/non_cr/non_cr_spending_decomp_results_by_draw_at65.csv")
dt_spending_75 <- fread("/home/j/Project/Cost_Effectiveness/BEA/BEA_data_2023/data_new_naming_convention/Spending/non_cr/non_cr_spending_decomp_results_by_draw_at75.csv")
dt_spending_85 <- fread("/home/j/Project/Cost_Effectiveness/BEA/BEA_data_2023/data_new_naming_convention/Spending/non_cr/non_cr_spending_decomp_results_by_draw_at85.csv")



############################################################################################################
###############for all_cause summing over numerator and denominator##################################
###############for 1000 draws#####################################################################
#########################################################################################################
perform_operations <- function(dt_hale, dt_spending, age_group) {
  
#deleting causes 375,376,501
dt_hale<-dt_hale[!(dt_hale$cause_id=="375" | dt_hale$cause_id=="376" | dt_hale$cause_id=="501")]
dt_spending<-dt_spending[!(dt_spending$cause_id=="375" | dt_spending$cause_id=="376" | dt_spending$cause_id=="501")]

# Merge the 1000 draws of spending and hale effect data
combined_data <- merge(dt_spending, dt_hale, by=c("cause_id", "gbd_cause_name", "dex_cause_id", "dex_cause_name", "sex_id", "year", "draw"))


# Calculate the sum of spending_effect and sum of hale_effect across all draws for all causes
all_causes_sum_draws <- combined_data %>% group_by(year, draw) %>%
  summarise(ac_lt_spending_effect = sum(lt_spending_effect, na.rm = TRUE),
            ac_hale_effect = sum(hale_effect, na.rm = TRUE),
            .groups = 'drop') %>%
  as.data.frame()


# Combine the summed data back into the initial dataset
combined_data <- combined_data %>%
  left_join(all_causes_sum_draws, by = c("year", "draw"))


# Perform the division
combined_data <- combined_data %>%
  mutate(spending_by_hale_effect = lt_spending_effect/hale_effect)

combined_data <- combined_data %>%
  mutate(ac_spending_by_hale_effect = ac_lt_spending_effect/ac_hale_effect)


mean_ui_dt <- combined_data %>% 
  group_by(cause_id, gbd_cause_name, dex_cause_id, dex_cause_name, sex_id, year) %>%
  mutate(median_spending_by_hale_effect_1000draw = median(spending_by_hale_effect)) %>%
  slice(which.min(abs(spending_by_hale_effect - median_spending_by_hale_effect_1000draw))) %>%
  summarise(mean_spending_by_hale_effect_1000draw= mean(spending_by_hale_effect),
            median_spending_by_hale_effect_1000draw = first(median_spending_by_hale_effect_1000draw),
            median_corresponding_lt_spending_effect = first(lt_spending_effect),
            median_corresponding_hale_effect = first(hale_effect),
            mean_lt_spending_effect_1000draw = mean(lt_spending_effect, na.rm = TRUE),
            mean_hale_effect_1000draw = mean(hale_effect, na.rm = TRUE),
            l_iqr_spending_by_hale_effect_1000draw = quantile(spending_by_hale_effect, na.rm = TRUE, 0.25),  # 25th percentile
            u_iqr_spending_by_hale_effect_1000draw = quantile(spending_by_hale_effect, na.rm = TRUE, 0.75),  # 75th percentile
            lower_ui_spending_by_hale_effect_1000draw = quantile(spending_by_hale_effect, na.rm = TRUE, 0.025),  # 2.5th percentile
            upper_ui_spending_by_hale_effect_1000draw = quantile(spending_by_hale_effect, na.rm = TRUE, 0.975),  # 97.5th percentile
            .groups = 'drop')  %>%
  as.data.frame()

# Replace Inf and -Inf with NA 
mean_ui_dt$mean_spending_by_hale_effect_1000draw <- replace(mean_ui_dt$mean_spending_by_hale_effect_1000draw, is.infinite(mean_ui_dt$mean_spending_by_hale_effect_1000draw), NA)
mean_ui_dt$median_spending_by_hale_effect_1000draw <- replace(mean_ui_dt$median_spending_by_hale_effect_1000draw, is.infinite(mean_ui_dt$median_spending_by_hale_effect_1000draw), NA)
mean_ui_dt$l_iqr_spending_by_hale_effect_1000draw <- replace(mean_ui_dt$l_iqr_spending_by_hale_effect_1000draw, is.infinite(mean_ui_dt$l_iqr_spending_by_hale_effect_1000draw), NA)
mean_ui_dt$u_iqr_spending_by_hale_effect_1000draw <- replace(mean_ui_dt$u_iqr_spending_by_hale_effect_1000draw, is.infinite(mean_ui_dt$u_iqr_spending_by_hale_effect_1000draw), NA)
#mean_ui_dt$iqr_spending_by_hale_effect_1000draw <- replace(mean_ui_dt$iqr_spending_by_hale_effect_1000draw, is.infinite(mean_ui_dt$iqr_spending_by_hale_effect_1000draw), NA)
mean_ui_dt$lower_ui_spending_by_hale_effect_1000draw <- replace(mean_ui_dt$lower_ui_spending_by_hale_effect_1000draw, is.infinite(mean_ui_dt$lower_ui_spending_by_hale_effect_1000draw), NA)
mean_ui_dt$upper_ui_spending_by_hale_effect_1000draw <- replace(mean_ui_dt$upper_ui_spending_by_hale_effect_1000draw, is.infinite(mean_ui_dt$upper_ui_spending_by_hale_effect_1000draw), NA)

# Replace Inf and -Inf with NA 
combined_data$spending_by_hale_effect <- replace(combined_data$spending_by_hale_effect, is.infinite(combined_data$spending_by_hale_effect), NA)

  # Calculate the desired summary statistics for the summed draws
all_cause_row <- combined_data %>%
  group_by(year) %>%
  mutate(median_spending_by_hale_effect_1000draw = median(ac_spending_by_hale_effect)) %>%
  slice(which.min(abs(ac_spending_by_hale_effect - median_spending_by_hale_effect_1000draw))) %>%
  summarise(cause_id = NA,  # You can set these columns to NA as appropriate
            gbd_cause_name = "all_cause",
            dex_cause_id = NA,
            dex_cause_name = "all_cause",
            sex_id = 3,
            mean_spending_by_hale_effect_1000draw = mean(ac_spending_by_hale_effect),
            median_spending_by_hale_effect_1000draw = first(median_spending_by_hale_effect_1000draw),
            median_corresponding_lt_spending_effect = first(ac_lt_spending_effect),
            median_corresponding_hale_effect = first(ac_hale_effect),
            mean_lt_spending_effect_1000draw = mean(ac_lt_spending_effect),
            mean_hale_effect_1000draw = mean(ac_hale_effect),
            l_iqr_spending_by_hale_effect_1000draw = quantile(ac_spending_by_hale_effect, na.rm = TRUE, 0.25),  # 25th percentile
            u_iqr_spending_by_hale_effect_1000draw = quantile(ac_spending_by_hale_effect, na.rm = TRUE, 0.75),  # 75th percentile
            #iqr_spending_by_hale_effect_1000draw = IQR(ac_spending_by_hale_effect, na.rm = TRUE),
            lower_ui_spending_by_hale_effect_1000draw = quantile(ac_spending_by_hale_effect, na.rm = TRUE, 0.025),  # 2.5th percentile
            upper_ui_spending_by_hale_effect_1000draw = quantile(ac_spending_by_hale_effect, na.rm = TRUE, 0.975)   # 97.5th percentile
  ) %>%
  ungroup()

# Bind the "all_cause" row to your existing data
mean_ui_dt <- rbind(mean_ui_dt, all_cause_row)

# Specify which quadrant the mean of spending_effect and hale_effect from 1000 draws will fall into
mean_ui_dt$quadrant_mean_1000draw <- "NA"

mean_ui_dt <- mean_ui_dt %>% 
  mutate(quadrant_mean_1000draw = ifelse(mean_lt_spending_effect_1000draw > 0 & mean_hale_effect_1000draw < 0, "dominated", quadrant_mean_1000draw))

mean_ui_dt <- mean_ui_dt %>% 
  mutate(
    quadrant_mean_1000draw = if_else(mean_lt_spending_effect_1000draw < 0 & mean_hale_effect_1000draw > 0, "cost-saving", quadrant_mean_1000draw))

mean_ui_dt <- mean_ui_dt %>% 
  mutate(
    quadrant_mean_1000draw = if_else(mean_lt_spending_effect_1000draw > 0 & mean_hale_effect_1000draw > 0, "icer", quadrant_mean_1000draw))

mean_ui_dt <- mean_ui_dt %>% 
  mutate(
    quadrant_mean_1000draw = if_else(mean_lt_spending_effect_1000draw < 0 & mean_hale_effect_1000draw < 0, "south-west", quadrant_mean_1000draw))

# Specify which quadrant the median corresponding value of spending_effect and hale_effect from 1000 draws will fall into
mean_ui_dt$quadrant_median_1000draw <- "NA"

mean_ui_dt <- mean_ui_dt %>% 
  mutate(quadrant_median_1000draw = ifelse(median_corresponding_lt_spending_effect > 0 & median_corresponding_hale_effect < 0, "dominated", quadrant_median_1000draw))

mean_ui_dt <- mean_ui_dt %>% 
  mutate(
    quadrant_median_1000draw = if_else(median_corresponding_lt_spending_effect < 0 & median_corresponding_hale_effect > 0, "cost-saving", quadrant_median_1000draw))

mean_ui_dt <- mean_ui_dt %>% 
  mutate(
    quadrant_median_1000draw = if_else(median_corresponding_lt_spending_effect > 0 & median_corresponding_hale_effect > 0, "icer", quadrant_median_1000draw))

mean_ui_dt <- mean_ui_dt %>% 
  mutate(
    quadrant_median_1000draw = if_else(median_corresponding_lt_spending_effect < 0 & median_corresponding_hale_effect < 0, "south-west", quadrant_median_1000draw))


#fwrite(mean_ui_dt, "./HALE_spending_combined/ac_lt_spend_by_hale_effect_non_cr_1000draw_at0.csv")
# Save the output to a CSV file
csv_filename <- paste0("./HALE_spending_combined/ac_lt_spend_by_hale_effect_non_cr_1000draw_at", age_group, ".csv")
if(file.exists(csv_filename)) {
  csv_filename <- paste0("./HALE_spending_combined/ac_lt_spend_by_hale_effect_non_cr_1000draw_at", age_group, "_new.csv")
}
fwrite(mean_ui_dt, csv_filename)
}

# List of data tables and corresponding age groups
data_list <- list(list(dt_hale = dt_hale_0, dt_spending = dt_spending_0, age_group = 0),
                  list(dt_hale = dt_hale_15, dt_spending = dt_spending_15, age_group = 15),
                  list(dt_hale = dt_hale_30, dt_spending = dt_spending_30, age_group = 30),
                  list(dt_hale = dt_hale_45, dt_spending = dt_spending_45, age_group = 45),
                  list(dt_hale = dt_hale_55, dt_spending = dt_spending_55, age_group = 55),
                  list(dt_hale = dt_hale_65, dt_spending = dt_spending_65, age_group = 65),
                  list(dt_hale = dt_hale_75, dt_spending = dt_spending_75, age_group = 75),
                  list(dt_hale = dt_hale_85, dt_spending = dt_spending_85, age_group = 85))

# Apply the function to each item in the list
lapply(data_list, function(x) perform_operations(x$dt_hale, x$dt_spending, x$age_group))



