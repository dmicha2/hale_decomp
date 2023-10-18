library(data.table)
setwd("/home/j/Project/Cost_Effectiveness/BEA/BEA_data_2023/data_new_naming_convention/Spending")
year1 <- 1996
year2 <- 2016
#output_file <- "./spending_cause_replaced_mort_rates_dex_gdp_aggregated.csv"

# Read in data. This csv should have columns for cause_id, age_group_id, year_id,
# population, cases, cs_deaths, ac_deaths, and cs_spending

#old input file
#dt <- fread("./spending_decomp_input_data_adjusted.csv")

#updated with new file with aggregated dex_gbd causes
dt <- fread("./spending_decomp_input_data_dex_gdp_aggegate_death_agg.csv")

# Convert cases and cs_deaths to case rates and mortality rates.
dt[, case_rate := cases / population]
dt[cases == 0, case_rate := 0]
dt[, deaths_per_case := cs_deaths / cases]
dt[cases == 0, deaths_per_case := 0]
dt[, ac_mr := ac_deaths / population]
dt[, cs_mr := cs_deaths / population]

# Create the cause-deleted death rate by subtracting the cause-specific cs_deaths from the total cs_deaths.
dt[, cause_del_deaths := ac_deaths - cs_deaths]
dt[, cause_del_mr := cause_del_deaths / population]
dt[, cause_del_expenditure := ac_spending - cs_spending]
dt[, cs_spending_per_case := cs_spending/cases]
dt[cases == 0, cs_spending_per_case := 0]
# dt[cases == 0 & cs_spending != 0, cases := population] # - idea for how to handle these.

cause_replace <- function(d, start_year, end_year){
  # Reshape to wide to create separate columns for separate years.
  d <- dcast(d,
             cause_id + draw + age_group_id + sex_id + location_id + age_group_years_start + age_group_years_end ~ year_id,
             value.var=c("population", "cause_del_mr", "ac_mr", "cs_mr",
                         "cases", "case_rate", "deaths_per_case",
                         "cs_spending", "cs_spending_per_case", "ac_spending", "cause_del_expenditure"))
  
  ys <- as.character(c(start_year, end_year))
  
  # # Create cause-replaced mortality rates by taking the cause-deleted mortality rate for a given year and adding
  # # to it the product of the case rate for the same year times the cs_deaths per case for the other year.
  # d[, paste0("cr_ac_mr_", ys[1]) := get(paste0("cause_del_mr_", ys[1])) + get(paste0("case_rate_", ys[1])) * get(paste0("deaths_per_case_", ys[2]))]
  # d[, paste0("cr_ac_mr_", ys[2]) := get(paste0("cause_del_mr_", ys[2])) + get(paste0("case_rate_", ys[2])) * get(paste0("deaths_per_case_", ys[1]))]
  # 
  # d[, paste0("cr_ac_spending_", ys[1]) := get(paste0("cause_del_expenditure_", ys[1])) + get(paste0("cases_", ys[1])) * get(paste0("cs_spending_per_case_", ys[2]))]
  # d[, paste0("cr_ac_spending_", ys[2]) := get(paste0("cause_del_expenditure_", ys[2])) + get(paste0("cases_", ys[2])) * get(paste0("cs_spending_per_case_", ys[1]))]
  # 
  # #New addition cr_cs_mr
  # d[, paste0("cr_cs_mr_", ys[1]) := get(paste0("case_rate_", ys[1])) * get(paste0("deaths_per_case_", ys[2]))]
  # d[, paste0("cr_cs_mr_", ys[2]) := get(paste0("case_rate_", ys[2])) * get(paste0("deaths_per_case_", ys[1]))]


  #Alternate measure where we keep the year-specific mortality fixed (call these the non-cr results)
  # Alternatively, if we want to keep mortality rate year-specific, i.e, for 1996 measures, we keep 1996 mortality rate (ie, cr_ac_mr = ac_mr and cr_cs_mr=cs_mr)
  d[, paste0("cr_ac_mr_", ys[1]) := get(paste0("cause_del_mr_", ys[1])) + get(paste0("case_rate_", ys[1])) * get(paste0("deaths_per_case_", ys[1]))]
  d[, paste0("cr_cs_mr_", ys[1]) := get(paste0("case_rate_", ys[1])) * get(paste0("deaths_per_case_", ys[1]))]

  # Alternatively, if we want to keep mortality rate year-specific, i.e, for 2016 measures, we keep 2016 mortality rate (ie, cr_ac_mr = ac_mr here)
  d[, paste0("cr_ac_mr_", ys[2]) := get(paste0("cause_del_mr_", ys[2])) + get(paste0("case_rate_", ys[2])) * get(paste0("deaths_per_case_", ys[2]))]
  d[, paste0("cr_cs_mr_", ys[2]) := get(paste0("case_rate_", ys[2])) * get(paste0("deaths_per_case_", ys[2]))]

  #The cr_ac_spending remains unchanged as we still use other year values for cs_spending_per_case
  d[, paste0("cr_ac_spending_", ys[1]) := get(paste0("cause_del_expenditure_", ys[1])) + get(paste0("cases_", ys[1])) * get(paste0("cs_spending_per_case_", ys[2]))]
  d[, paste0("cr_ac_spending_", ys[2]) := get(paste0("cause_del_expenditure_", ys[2])) + get(paste0("cases_", ys[2])) * get(paste0("cs_spending_per_case_", ys[1]))]
  d[]
  return(d)
}

# Call the cause-replace function and select columns to keep
dt <- cause_replace(dt, year1, year2)

id_vars <- c("cause_id", "draw", "sex_id", "location_id", "age_group_id",
             "age_group_years_start", "age_group_years_end")
measure_vars <- c(outer(c("cr_ac_mr", "cr_ac_spending", "ac_mr", "cs_spending", "ac_spending", 
                          "cr_cs_mr", "cs_mr",
                          "cs_spending_per_case", "cases", 
                          "case_rate", "population"), c(1996, 2016), paste, sep="_"))
dt <- melt(dt, id.vars = id_vars, measure.vars=measure_vars)
dt[, year := as.integer(gsub(".*_", "", variable))]
dt[, variable := gsub("_[0-9]+", "", variable)]
dt <- dcast(dt, cause_id + draw + sex_id + location_id + age_group_id + age_group_years_start + age_group_years_end + year ~ variable,
            value.var = "value")


fwrite(dt, "./non_cr/spending_not_cause_replaced_mort_rates.csv")

dt_draw_0 <- dt[draw == 0, ]
fwrite(dt_draw_0, "./non_cr/draw_0_spending_not_cause_replaced_mort_rates.csv")
