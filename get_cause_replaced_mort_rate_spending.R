library(data.table)
setwd("/home/j/Project/Cost_Effectiveness/BEA/BEA_data_2023/Spending")
year1 <- 1996
year2 <- 2016
output_file <- "./spending_cause_replaced_mort_rates.csv"

# Read in data. This csv should have columns for cause_id, age_group_id, year_id,
# population, cases, deaths, all_cause_deaths, and expenditure
dt <- fread("./spending_decomp_input_data_adjusted.csv")
# Convert cases and deaths to case rates and mortality rates.
dt[, case_rate := cases / population]
dt[cases == 0, case_rate := 0]
dt[, deaths_per_case := deaths / cases]
dt[cases == 0, deaths_per_case := 0]
dt[, acmr := all_cause_deaths / population]

# Create the cause-deleted death rate by subtracting the cause-specific deaths from the total deaths.
dt[, cause_del_deaths := all_cause_deaths - deaths]
dt[, cause_del_mr := cause_del_deaths / population]
dt[, cause_del_expenditure := all_cause_expenditure - expenditure]
dt[, expenditure_per_case := expenditure/cases]
dt[cases == 0, expenditure_per_case := 0]
# dt[cases == 0 & expenditure != 0, cases := population] # - idea for how to handle these.

cause_replace <- function(d, start_year, end_year){
  # Reshape to wide to create separate columns for separate years.
  d <- dcast(d,
             cause_id + draw + age_group_id + sex_id + location_id + age_group_years_start + age_group_years_end ~ year_id,
             value.var=c("population", "cause_del_mr", "acmr",
                         "cases", "case_rate", "deaths_per_case",
                         "expenditure", "expenditure_per_case", "all_cause_expenditure", "cause_del_expenditure"))
  
  ys <- as.character(c(start_year, end_year))
  # Create cause-replaced mortality rates by taking the cause-deleted mortality rate for a given year and adding
  # to it the product of the case rate for the same year times the deaths per case for the other year.
  d[, paste0("crmr_", ys[1]) := get(paste0("cause_del_mr_", ys[1])) + get(paste0("case_rate_", ys[1])) * get(paste0("deaths_per_case_", ys[2]))]
  d[, paste0("cr_expenditure_", ys[1]) := get(paste0("cause_del_expenditure_", ys[1])) + get(paste0("cases_", ys[1])) * get(paste0("expenditure_per_case_", ys[2]))]
  d[, paste0("crmr_", ys[2]) := get(paste0("cause_del_mr_", ys[2])) + get(paste0("case_rate_", ys[2])) * get(paste0("deaths_per_case_", ys[1]))]
  d[, paste0("cr_expenditure_", ys[2]) := get(paste0("cause_del_expenditure_", ys[2])) + get(paste0("cases_", ys[2])) * get(paste0("expenditure_per_case_", ys[1]))]
  d[]
  return(d)
}
# Call the cause-replace function and select columns to keep
dt <- cause_replace(dt, year1, year2)

id_vars <- c("cause_id", "draw", "sex_id", "location_id", "age_group_id",
             "age_group_years_start", "age_group_years_end")
measure_vars <- c(outer(c("crmr", "cr_expenditure", "acmr", "expenditure", 
                          "all_cause_expenditure", "expenditure_per_case", "cases", 
                          "case_rate", "population"), c(1996, 2016), paste, sep="_"))
dt <- melt(dt, id.vars = id_vars, measure.vars=measure_vars)
dt[, year := as.integer(gsub(".*_", "", variable))]
dt[, variable := gsub("_[0-9]+", "", variable)]
dt <- dcast(dt, cause_id + draw + sex_id + location_id + age_group_id + age_group_years_start + age_group_years_end + year ~ variable,
            value.var = "value")

# Write to a file, checking to make sure you don't overwrite pre-existing files.
if(!file.exists(output_file)){
  fwrite(dt, output_file)
} else{
  stop("Output file already exists. Consider deleting that file and rerunning, or changing the output file.")
}
