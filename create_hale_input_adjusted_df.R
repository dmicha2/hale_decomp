library(data.table)
library(dplyr)
source("/ihme/cc_resources/libraries/current/r/get_population.R")
source("/ihme/cc_resources/libraries/current/r/get_draws.R")
source("/ihme/cc_resources/libraries/current/r/get_age_metadata.R")
source("/ihme/cc_resources/libraries/current/r/get_cause_metadata.R")
setwd("/home/j/Project/Cost_Effectiveness/BEA/BEA_3_data/HALE")

age_metadata <- get_age_metadata(age_group_set_id = 12, gbd_round_id=6)
age_metadata[, c("age_group_weight_value", "most_detailed") := NULL]
# Adjust age group column to match with the spending calculation
age_metadata <- age_metadata[-c(1:3, 21:23), ]
# Define all_ages_set_12 variable to use in shared functions 
all_ages_set_12 <- age_metadata[, age_group_id]

# Define the years and cause_ids that we would like to decompose HALE for.
years <- c(1996, 2016)
all_ages <- age_metadata[, age_group_id]

# Create a set of cause ids for which only one sex needs to be pulled
male_cause_ids <- c(405, 486, 367)
female_cause_ids <- c(340, 572, 578, 594, 382, 384)
impacted_cause_ids <- c(male_cause_ids, female_cause_ids)

# Read in data. This csv should have columns for cause_id, age_group_id, year_id,
# population, cases, deaths, all_cause_deaths, and ylds
dt <- fread("./hale_decomp_input_data.csv")

#### Collecting data ####
# Pull death counts for all the GBD causes, plus the all-cause-aggregate
deaths_dt <- get_draws("cause_id", gbd_id=c(male_cause_ids, female_cause_ids), gbd_round_id=6, source="codcorrect",
                       measure_id=1, metric_id=1, age_group_id=all_ages_set_12, sex_id= c(1,2),
                       location_id=102, year_id=years, decomp_step="step5")
deaths_age_groups_28_160 <- get_draws("cause_id", gbd_id=c(male_cause_ids, female_cause_ids), gbd_round_id=6, source="codcorrect",
                                      measure_id=1, metric_id=1, age_group_id=c(28, 160), sex_id= c(1,2),
                                      location_id=102, year_id=years, decomp_step="step5")
deaths_age_groups_28_160[age_group_id == 28, age_group_id := 0]
deaths_dt <- rbind(deaths_dt, deaths_age_groups_28_160)

# Reshape so that different draws are different rows.
deaths_dt <- melt(deaths_dt,
                  id.vars=c("cause_id", "age_group_id", "sex_id", "location_id", "year_id"),
                  measure.vars=paste0("draw_", 0:999),
                  value.name="deaths", variable.name="draw")
deaths_dt[, draw := as.integer(gsub("draw_", "", draw))]


# Pull YLDs and merge with the rest
ylds_dt <- get_draws("cause_id", gbd_id=c(male_cause_ids, female_cause_ids), gbd_round_id=6, source="como",
                     measure_id=3, metric_id=3, age_group_id=all_ages_set_12, sex_id=c(1,2),
                     location_id=102, year_id=years, decomp_step="step5")

ylds_age_groups_28_160 <- get_draws("cause_id", gbd_id=c(male_cause_ids, female_cause_ids), gbd_round_id=6, source="como",
                                    measure_id=3, metric_id=3, age_group_id=c(28, 160), sex_id=c(1,2),
                                    location_id=102, year_id=years, decomp_step="step5")
ylds_age_groups_28_160[age_group_id == 28, age_group_id := 0]
ylds_dt <- rbind(ylds_dt, ylds_age_groups_28_160)

ylds_dt <- melt(ylds_dt,
                id.vars=c("cause_id", "age_group_id", "sex_id", "location_id", "year_id"),
                measure.vars=paste0("draw_", 0:999),
                value.name="ylds", variable.name="draw")
ylds_dt[, draw := as.integer(gsub("draw_", "", draw))]

# Pull prevalence for causes where "cases" is defined as prevalence, and incidence where "cases"
# is defined as incidence.
prev_cids <- c(405, 572, 578, 594, 367, 382, 384)
inc_cids <- c(486, 340)

prevs <- get_draws("cause_id", gbd_id=prev_cids, gbd_round_id=6, source="como",
                   measure_id=5, metric_id=3, age_group_id=all_ages_set_12, sex_id=c(1, 2),
                   location_id=102, year_id=years, decomp_step="step5")
prevs_age_groups_28_160 <- get_draws("cause_id", gbd_id=prev_cids, gbd_round_id=6, source="como",
                                     measure_id=5, metric_id=3, age_group_id=c(28, 160), sex_id=c(1, 2),
                                     location_id=102, year_id=years, decomp_step="step5")
prevs_age_groups_28_160[age_group_id == 28, age_group_id := 0]
prevs <- rbind(prevs, prevs_age_groups_28_160)

prevs <- melt(prevs,
              id.vars=c("cause_id", "age_group_id", "sex_id", "location_id", "year_id"),
              measure.vars=paste0("draw_", 0:999),
              value.name="case_rate", variable.name="draw")

incs <- get_draws("cause_id", gbd_id=inc_cids, gbd_round_id=6, source="como",
                  measure_id=6, metric_id=3, age_group_id=all_ages_set_12, sex_id=c(1, 2),
                  location_id=102, year_id=years, decomp_step="step5")

incs_age_groups_28_160 <- get_draws("cause_id", gbd_id=inc_cids, gbd_round_id=6, source="como",
                                    measure_id=6, metric_id=3, age_group_id=c(28, 160), sex_id=c(1, 2),
                                    location_id=102, year_id=years, decomp_step="step5")
incs_age_groups_28_160[age_group_id == 28, age_group_id := 0]
incs <- rbind(incs, incs_age_groups_28_160)

incs <- melt(incs,
             id.vars=c("cause_id", "age_group_id", "sex_id", "location_id", "year_id"),
             measure.vars=paste0("draw_", 0:999),
             value.name="case_rate", variable.name="draw")

cases_dt <- rbind(prevs, incs)
cases_dt[, draw := as.integer(gsub("draw_", "", draw))]

# Get population and merge with deaths & cases
pops <- get_population(age_group_id=c(all_ages_set_12, 28), sex_id=c(1, 2), location_id=102, year_id=years, gbd_round_id=6, decomp_step="step5")
# Calculating age_group 160 as it is not returned by the function
pops_160 <- get_population(age_group_id=c(31, 32, 235), sex_id=c(1, 2), location_id=102, year_id=years, gbd_round_id=6, decomp_step="step5")
pops_160 <- pops_160 %>%
  group_by(year_id, sex_id, location_id, run_id) %>%
  summarise(across(c(age_group_id, population), sum))

pops_160 <- setDT(pops_160)
pops_160[, age_group_id := 160]

pops_dt <- rbind(pops, pops_160)
pops_dt[, run_id := NULL]

#### Replacing values in the dt####

# Working with other case ids where 0 are true for both sexes. 
dropped_dalys <- setDT(readxl::read_excel("/home/j/Project/Cost_Effectiveness/BEA/dropped_dalys_summary.xlsx"))
dropped_dalys <- dropped_dalys[, c("cause_id", "age_group_id", "sex_id")]

# sum rows base on cause_id and age_group_id. So if sex_id 1 and 2 are present for same key sum is going to be 3
# otherwise it is going to be 1 or 2. 
dropped_dalys <- dropped_dalys %>%
  group_by(cause_id, age_group_id) %>%
  summarise(across(c(sex_id), sum))
setDT(dropped_dalys)

# choose only sex_id 3
dropped_dalys <- dropped_dalys[sex_id == 3, ]

for (row in 1:nrow(dropped_dalys)) {
  dropped_age_group_id  <- dropped_dalys[row, age_group_id]
  dropped_sex_id <- dropped_dalys[row, sex_id]
  dropped_cause_id <- dropped_dalys[row, cause_id]
  
  dt <- dt[(dex_cause_id == dropped_cause_id & age_group_id == dropped_age_group_id & sex_id == dropped_sex_id), deaths := 0]
  dt <- dt[(dex_cause_id == dropped_cause_id & age_group_id == dropped_age_group_id & sex_id == dropped_sex_id), cases := 0]
  dt <- dt[(dex_cause_id == dropped_cause_id & age_group_id == dropped_age_group_id & sex_id == dropped_sex_id), case_rate := 0]
  dt <- dt[(dex_cause_id == dropped_cause_id & age_group_id == dropped_age_group_id & sex_id == dropped_sex_id), ylds := 0]
}

# breaking dt into two parts in order to decrease processing time in next step.
dt_isnot_impacted <- dt[!cause_id %in% impacted_cause_ids, ]
dt <- dt[cause_id %in% impacted_cause_ids, ]

##### True for one sex #####
for (d in 0:999){
  ## CAUSE_ID - 405, age_group - 12,13,14,15,16,17, sex 1
  for (age_grp in 12:17){
    
    # There is no deaths data for the cause is 405
    #deaths_val <- deaths_dt[(cause_id == 405 & sex_id == 1 & age_group_id == age_grp & draw == d), deaths]
    #dt_test[(cause_id == 405 & sex_id == 3 & age_group_id == age_grp & draw == d), deaths := deaths_val]
    
    ylds_val <- ylds_dt[(cause_id == 405 & sex_id == 1 & age_group_id == age_grp & draw == d & year_id == 1996), ylds]
    dt[(cause_id == 405 & sex_id == 3 & age_group_id == age_grp & draw == d & year_id == 1996), ylds := ylds_val]
    
    case_rate_val <- cases_dt[(cause_id == 405 & sex_id == 1 & age_group_id == age_grp & draw == d & year_id == 1996), case_rate]
    dt[(cause_id == 405 & sex_id == 3 & age_group_id == age_grp & draw == d & year_id == 1996), case_rate := case_rate_val]
    
    pops_val <- pops_dt[(sex_id == 1 & age_group_id == age_grp & year_id == 1996), population]
    dt[(cause_id == 405 & sex_id == 3 & age_group_id == age_grp & draw == d & year_id == 1996), population := pops_val]
    
    ylds_val <- ylds_dt[(cause_id == 405 & sex_id == 1 & age_group_id == age_grp & draw == d & year_id == 2016), ylds]
    dt[(cause_id == 405 & sex_id == 3 & age_group_id == age_grp & draw == d & year_id == 2016), ylds := ylds_val]
    
    case_rate_val <- cases_dt[(cause_id == 405 & sex_id == 1 & age_group_id == age_grp & draw == d & year_id == 2016), case_rate]
    dt[(cause_id == 405 & sex_id == 3 & age_group_id == age_grp & draw == d & year_id == 2016), case_rate := case_rate_val]
    
    pops_val <- pops_dt[(sex_id == 1 & age_group_id == age_grp & year_id == 2016), population]
    dt[(cause_id == 405 & sex_id == 3 & age_group_id == age_grp & draw == d & year_id == 2016), population := pops_val]
    
    
    dt[(cause_id == 405 & sex_id == 3 & age_group_id == age_grp & draw == d), sex_id := 1]
  }
  
  ## CAUSE_ID - 486, age_group - 8, sex 1
  # deaths_val <- deaths_dt[(cause_id == 486 & sex_id == 1 & age_group_id == 8), deaths]
  # dt[(cause_id == 486 & sex_id == 1 & age_group_id == 8), deaths := deaths_val]
  
  ylds_val <- ylds_dt[(cause_id == 486 & sex_id == 1 & age_group_id == 8 & draw == d & year_id == 1996), ylds]
  dt[(cause_id == 486 & sex_id == 3 & age_group_id == 8 & draw == d & year_id == 1996), ylds := ylds_val]
  
  case_rate_val <- cases_dt[(cause_id == 486 & sex_id == 1 & age_group_id == 8 & draw == d & year_id == 1996), case_rate]
  dt[(cause_id == 486 & sex_id == 3 & age_group_id == 8 & draw == d & year_id == 1996), case_rate := case_rate_val]
  
  pops_val <- pops_dt[(sex_id == 1 & age_group_id == 8 & year_id == 1996), population]
  dt[(cause_id == 486 & sex_id == 3 & age_group_id == 8 & draw == d & year_id == 1996), population := pops_val]
  
  ylds_val <- ylds_dt[(cause_id == 486 & sex_id == 1 & age_group_id == 8 & draw == d & year_id == 2016), ylds]
  dt[(cause_id == 486 & sex_id == 3 & age_group_id == 8 & draw == d & year_id == 2016), ylds := ylds_val]
  
  case_rate_val <- cases_dt[(cause_id == 486 & sex_id == 1 & age_group_id == 8 & draw == d & year_id == 2016), case_rate]
  dt[(cause_id == 486 & sex_id == 3 & age_group_id == 8 & draw == d & year_id == 2016), case_rate := case_rate_val]
  
  pops_val <- pops_dt[(sex_id == 1 & age_group_id == 8 & year_id == 2016), population]
  dt[(cause_id == 486 & sex_id == 3 & age_group_id == 8 & draw == d & year_id == 2016), population := pops_val]
  
  dt[(cause_id == 486 & sex_id == 3 & age_group_id == 8 & draw == d), sex_id := 1]
  
  
  
  ## CAUSE_ID - 594, age_group - 6,7 sex 2
  for (age_grp in 6:7){
    deaths_val <- deaths_dt[(cause_id == 594 & sex_id == 2 & age_group_id == age_grp & draw == d & year_id == 1996), deaths]
    dt[(cause_id == 594 & sex_id == 3 & age_group_id == age_grp & draw == d & year_id == 1996), deaths := deaths_val]
    
    ylds_val <- ylds_dt[(cause_id == 594 & sex_id == 2 & age_group_id == age_grp & draw == d & year_id == 1996), ylds]
    dt[(cause_id == 594 & sex_id == 3 & age_group_id == age_grp & draw == d & year_id == 1996), ylds := ylds_val]
    
    case_rate_val <- cases_dt[(cause_id == 594 & sex_id == 2 & age_group_id == age_grp & draw == d & year_id == 1996), case_rate]
    dt[(cause_id == 594 & sex_id == 3 & age_group_id == age_grp & draw == d & year_id == 1996), case_rate := case_rate_val]
    
    pops_val <- pops_dt[(sex_id == 2 & age_group_id == age_grp & year_id == 1996), population]
    dt[(cause_id == 594 & sex_id == 3 & age_group_id == age_grp & draw == d & year_id == 1996), population := pops_val]
    
    deaths_val <- deaths_dt[(cause_id == 594 & sex_id == 2 & age_group_id == age_grp & draw == d & year_id == 2016), deaths]
    dt[(cause_id == 594 & sex_id == 3 & age_group_id == age_grp & draw == d & year_id == 2016), deaths := deaths_val]
    
    ylds_val <- ylds_dt[(cause_id == 594 & sex_id == 2 & age_group_id == age_grp & draw == d & year_id == 2016), ylds]
    dt[(cause_id == 594 & sex_id == 3 & age_group_id == age_grp & draw == d & year_id == 2016), ylds := ylds_val]
    
    case_rate_val <- cases_dt[(cause_id == 594 & sex_id == 2 & age_group_id == age_grp & draw == d & year_id == 2016), case_rate]
    dt[(cause_id == 594 & sex_id == 3 & age_group_id == age_grp & draw == d & year_id == 2016), case_rate := case_rate_val]
    
    pops_val <- pops_dt[(sex_id == 2 & age_group_id == age_grp & year_id == 2016), population]
    dt[(cause_id == 594 & sex_id == 3 & age_group_id == age_grp & draw == d & year_id == 2016), population := pops_val]
    
    dt[(cause_id == 594 & sex_id == 3 & age_group_id == age_grp & draw == d), sex_id := 2]
  }
  
  
  ## CAUSE_ID - 578, age_group - 30 sex 2
  # deaths_val <- deaths_dt[(cause_id == 578 & sex_id == 2 & age_group_id == 30 & draw == d), deaths]
  # dt[(cause_id == 578 & sex_id == 3 & age_group_id == 30 & draw == d), deaths := deaths_val]
  
  ylds_val <- ylds_dt[(cause_id == 578 & sex_id == 2 & age_group_id == 30 & draw == d & year_id == 1996), ylds]
  dt[(cause_id == 578 & sex_id == 3 & age_group_id == 30 & draw == d & year_id == 1996), ylds := ylds_val]
  
  case_rate_val <- cases_dt[(cause_id == 578 & sex_id == 2 & age_group_id == 30 & draw == d & year_id == 1996), case_rate]
  dt[(cause_id == 578 & sex_id == 3 & age_group_id == 30 & draw == d & year_id == 1996), case_rate := case_rate_val]
  
  pops_val <- pops_dt[(sex_id == 2 & age_group_id == 30 & year_id == 1996), population]
  dt[(cause_id == 578 & sex_id == 3 & age_group_id == 30 & draw == d & year_id == 1996), population := pops_val]
  
  ylds_val <- ylds_dt[(cause_id == 578 & sex_id == 2 & age_group_id == 30 & draw == d & year_id == 2016), ylds]
  dt[(cause_id == 578 & sex_id == 3 & age_group_id == 30 & draw == d & year_id == 2016), ylds := ylds_val]
  
  case_rate_val <- cases_dt[(cause_id == 578 & sex_id == 2 & age_group_id == 30 & draw == d & year_id == 2016), case_rate]
  dt[(cause_id == 578 & sex_id == 3 & age_group_id == 30 & draw == d & year_id == 2016), case_rate := case_rate_val]
  
  pops_val <- pops_dt[(sex_id == 2 & age_group_id == 30 & year_id == 2016), population]
  dt[(cause_id == 578 & sex_id == 3 & age_group_id == 30 & draw == d & year_id == 2016), population := pops_val]
  
  dt[(cause_id == 578 & sex_id == 3 & age_group_id == 30 & draw == d), sex_id := 2]
  
  ## CAUSE_ID - 572, age_group - 6, sex 2
  deaths_val <- deaths_dt[(cause_id == 572 & sex_id == 2 & age_group_id == 6 & draw == d & year_id == 1996), deaths]
  dt[(cause_id == 572 & sex_id == 3 & age_group_id == 6 & draw == d & year_id == 1996), deaths := deaths_val]
  
  ylds_val <- ylds_dt[(cause_id == 572 & sex_id == 2 & age_group_id == 6 & draw == d & year_id == 1996), ylds]
  dt[(cause_id == 572 & sex_id == 3 & age_group_id == 6 & draw == d & year_id == 1996), ylds := ylds_val]
  
  case_rate_val <- cases_dt[(cause_id == 572 & sex_id == 2 & age_group_id == 6 & draw == d & year_id == 1996), case_rate]
  dt[(cause_id == 572 & sex_id == 3 & age_group_id == 6 & draw == d & year_id == 1996), case_rate := case_rate_val]
  
  pops_val <- pops_dt[(sex_id == 2 & age_group_id == 6 & year_id == 1996), population]
  dt[(cause_id == 572 & sex_id == 3 & age_group_id == 6 & draw == d & year_id == 1996), population := pops_val]
  
  deaths_val <- deaths_dt[(cause_id == 572 & sex_id == 2 & age_group_id == 6 & draw == d & year_id == 2016), deaths]
  dt[(cause_id == 572 & sex_id == 3 & age_group_id == 6 & draw == d & year_id == 2016), deaths := deaths_val]
  
  ylds_val <- ylds_dt[(cause_id == 572 & sex_id == 2 & age_group_id == 6 & draw == d & year_id == 2016), ylds]
  dt[(cause_id == 572 & sex_id == 3 & age_group_id == 6 & draw == d & year_id == 2016), ylds := ylds_val]
  
  case_rate_val <- cases_dt[(cause_id == 572 & sex_id == 2 & age_group_id == 6 & draw == d & year_id == 2016), case_rate]
  dt[(cause_id == 572 & sex_id == 3 & age_group_id == 6 & draw == d & year_id == 2016), case_rate := case_rate_val]
  
  pops_val <- pops_dt[(sex_id == 2 & age_group_id == 6 & year_id == 2016), population]
  dt[(cause_id == 572 & sex_id == 3 & age_group_id == 6 & draw == d & year_id == 2016), population := pops_val]
  
  dt[(cause_id == 572 & sex_id == 3 & age_group_id == 6 & draw == d), sex_id := 2]
  
  ## CAUSE_ID - 340, age_group - 20,160 sex 2
  age_grps <- c(20, 160)
  for (age_grp in age_grps){
    deaths_val <- deaths_dt[(cause_id == 340 & sex_id == 2 & age_group_id == age_grp & draw == d & year_id == 1996), deaths]
    dt[(cause_id == 340 & sex_id == 3 & age_group_id == age_grp & draw == d & year_id == 1996), deaths := deaths_val]
    
    ylds_val <- ylds_dt[(cause_id == 340 & sex_id == 2 & age_group_id == age_grp & draw == d & year_id == 1996), ylds]
    dt[(cause_id == 340 & sex_id == 3 & age_group_id == age_grp & draw == d & year_id == 1996), ylds := ylds_val]
    
    case_rate_val <- cases_dt[(cause_id == 340 & sex_id == 2 & age_group_id == age_grp & draw == d & year_id == 1996), case_rate]
    dt[(cause_id == 340 & sex_id == 3 & age_group_id == age_grp & draw == d & year_id == 1996), case_rate := case_rate_val]
    
    pops_val <- pops_dt[(sex_id == 2 & age_group_id == age_grp & year_id == 1996), population]
    dt[(cause_id == 340 & sex_id == 3 & age_group_id == age_grp & draw == d & year_id == 1996), population := pops_val]
    
    deaths_val <- deaths_dt[(cause_id == 340 & sex_id == 2 & age_group_id == age_grp & draw == d & year_id == 2016), deaths]
    dt[(cause_id == 340 & sex_id == 3 & age_group_id == age_grp & draw == d & year_id == 2016), deaths := deaths_val]
    
    ylds_val <- ylds_dt[(cause_id == 340 & sex_id == 2 & age_group_id == age_grp & draw == d & year_id == 2016), ylds]
    dt[(cause_id == 340 & sex_id == 3 & age_group_id == age_grp & draw == d & year_id == 2016), ylds := ylds_val]
    
    case_rate_val <- cases_dt[(cause_id == 340 & sex_id == 2 & age_group_id == age_grp & draw == d & year_id == 2016), case_rate]
    dt[(cause_id == 340 & sex_id == 3 & age_group_id == age_grp & draw == d & year_id == 2016), case_rate := case_rate_val]
    
    pops_val <- pops_dt[(sex_id == 2 & age_group_id == age_grp & year_id == 2016), population]
    dt[(cause_id == 340 & sex_id == 3 & age_group_id == age_grp & draw == d & year_id == 2016), population := pops_val]
    
    dt[(cause_id == 340 & sex_id == 3 & age_group_id == age_grp & draw == d), sex_id := 2]
  }
  
  ## CAUSE_ID - 367, age_group - 15, sex 1
  # deaths_val <- deaths_dt[(cause_id == 367 & sex_id == 1 & age_group_id == 15 & draw == d & year_id == 1996), deaths]
  # dt[(cause_id == 367 & sex_id == 3 & age_group_id == 15 & draw == d & year_id == 1996), deaths := deaths_val]
  
  ylds_val <- ylds_dt[(cause_id == 367 & sex_id == 1 & age_group_id == 15 & draw == d & year_id == 1996), ylds]
  dt[(cause_id == 367 & sex_id == 3 & age_group_id == 15 & draw == d & year_id == 1996), ylds := ylds_val]
  
  case_rate_val <- cases_dt[(cause_id == 367 & sex_id == 1 & age_group_id == 15 & draw == d & year_id == 1996), case_rate]
  dt[(cause_id == 367 & sex_id == 3 & age_group_id == 15 & draw == d & year_id == 1996), case_rate := case_rate_val]
  
  pops_val <- pops_dt[(sex_id == 1 & age_group_id == 15 & year_id == 1996), population]
  dt[(cause_id == 367 & sex_id == 3 & age_group_id == 15 & draw == d & year_id == 1996), population := pops_val]
  
  # deaths_val <- deaths_dt[(cause_id == 367 & sex_id == 1 & age_group_id == 15 & draw == d & year_id == 2016), deaths]
  # dt[(cause_id == 367 & sex_id == 3 & age_group_id == 15 & draw == d & year_id == 2016), deaths := deaths_val]
  
  ylds_val <- ylds_dt[(cause_id == 367 & sex_id == 1 & age_group_id == 15 & draw == d & year_id == 2016), ylds]
  dt[(cause_id == 367 & sex_id == 3 & age_group_id == 15 & draw == d & year_id == 2016), ylds := ylds_val]
  
  case_rate_val <- cases_dt[(cause_id == 367 & sex_id == 1 & age_group_id == 15 & draw == d & year_id == 2016), case_rate]
  dt[(cause_id == 367 & sex_id == 3 & age_group_id == 15 & draw == d & year_id == 2016), case_rate := case_rate_val]
  
  pops_val <- pops_dt[(sex_id == 1 & age_group_id == 15 & year_id == 2016), population]
  dt[(cause_id == 367 & sex_id == 3 & age_group_id == 15 & draw == d & year_id == 2016), population := pops_val]
  
  dt[(cause_id == 367 & sex_id == 3 & age_group_id == 15 & draw == d), sex_id := 1]
  
  ## CAUSE_ID - 382, age_group - 160, sex 2
  # deaths_val <- deaths_dt[(cause_id == 382 & sex_id == 2 & age_group_id == 160 & draw == d & year_id == 1996), deaths]
  # dt[(cause_id == 382 & sex_id == 3 & age_group_id == 160 & draw == d & year_id == 1996), deaths := deaths_val]
  
  ylds_val <- ylds_dt[(cause_id == 382 & sex_id == 2 & age_group_id == 160 & draw == d & year_id == 1996), ylds]
  dt[(cause_id == 382 & sex_id == 3 & age_group_id == 160 & draw == d & year_id == 1996), ylds := ylds_val]
  
  case_rate_val <- cases_dt[(cause_id == 382 & sex_id == 2 & age_group_id == 160 & draw == d & year_id == 1996), case_rate]
  dt[(cause_id == 382 & sex_id == 3 & age_group_id == 160 & draw == d & year_id == 1996), case_rate := case_rate_val]
  
  pops_val <- pops_dt[(sex_id == 2 & age_group_id == 160 & year_id == 1996), population]
  dt[(cause_id == 382 & sex_id == 3 & age_group_id == 160 & draw == d & year_id == 1996), population := pops_val]
  
  # deaths_val <- deaths_dt[(cause_id == 382 & sex_id == 2 & age_group_id == 160 & draw == d & year_id == 2016), deaths]
  # dt[(cause_id == 382 & sex_id == 3 & age_group_id == 160 & draw == d & year_id == 2016), deaths := deaths_val]
  
  ylds_val <- ylds_dt[(cause_id == 382 & sex_id == 2 & age_group_id == 160 & draw == d & year_id == 2016), ylds]
  dt[(cause_id == 382 & sex_id == 3 & age_group_id == 160 & draw == d & year_id == 2016), ylds := ylds_val]
  
  case_rate_val <- cases_dt[(cause_id == 382 & sex_id == 2 & age_group_id == 160 & draw == d & year_id == 2016), case_rate]
  dt[(cause_id == 382 & sex_id == 3 & age_group_id == 160 & draw == d & year_id == 2016), case_rate := case_rate_val]
  
  pops_val <- pops_dt[(sex_id == 2 & age_group_id == 160 & year_id == 2016), population]
  dt[(cause_id == 382 & sex_id == 3 & age_group_id == 160 & draw == d & year_id == 2016), population := pops_val]
  
  dt[(cause_id == 382 & sex_id == 3 & age_group_id == 160 & draw == d), sex_id := 2]
  
  ## CAUSE_ID - 384, age_group - 160, sex 2
  # deaths_val <- deaths_dt[(cause_id == 384 & sex_id == 2 & age_group_id == 160 & draw == d & year_id == 1996), deaths]
  # dt[(cause_id == 384 & sex_id == 3 & age_group_id == 160 & draw == d & year_id == 1996), deaths := deaths_val]
  
  ylds_val <- ylds_dt[(cause_id == 384 & sex_id == 2 & age_group_id == 160 & draw == d & year_id == 1996), ylds]
  dt[(cause_id == 384 & sex_id == 3 & age_group_id == 160 & draw == d & year_id == 1996), ylds := ylds_val]
  
  case_rate_val <- cases_dt[(cause_id == 384 & sex_id == 2 & age_group_id == 160 & draw == d & year_id == 1996), case_rate]
  dt[(cause_id == 384 & sex_id == 3 & age_group_id == 160 & draw == d & year_id == 1996), case_rate := case_rate_val]
  
  pops_val <- pops_dt[(sex_id == 2 & age_group_id == 160 & year_id == 1996), population]
  dt[(cause_id == 384 & sex_id == 3 & age_group_id == 160 & draw == d & year_id == 1996), population := pops_val]
  
  # deaths_val <- deaths_dt[(cause_id == 384 & sex_id == 2 & age_group_id == 160 & draw == d & year_id == 2016), deaths]
  # dt[(cause_id == 384 & sex_id == 3 & age_group_id == 160 & draw == d & year_id == 2016), deaths := deaths_val]
  
  ylds_val <- ylds_dt[(cause_id == 384 & sex_id == 2 & age_group_id == 160 & draw == d & year_id == 2016), ylds]
  dt[(cause_id == 384 & sex_id == 3 & age_group_id == 160 & draw == d & year_id == 2016), ylds := ylds_val]
  
  case_rate_val <- cases_dt[(cause_id == 384 & sex_id == 2 & age_group_id == 160 & draw == d & year_id == 2016), case_rate]
  dt[(cause_id == 384 & sex_id == 3 & age_group_id == 160 & draw == d & year_id == 2016), case_rate := case_rate_val]
  
  pops_val <- pops_dt[(sex_id == 2 & age_group_id == 160 & year_id == 2016), population]
  dt[(cause_id == 384 & sex_id == 3 & age_group_id == 160 & draw == d & year_id == 2016), population := pops_val]
  
  dt[(cause_id == 384 & sex_id == 3 & age_group_id == 160 & draw == d), sex_id := 2]
  
 
}



# recalculating cases for impacted causes 
causes_impacted <- c(prev_cids, inc_cids)
dt[cause_id %in% causes_impacted, cases := population * case_rate]

dt[cause_id %in% causes_impacted, ylds := ylds * population]

# merge dt back into one
dt <- rbind(dt, dt_isnot_impacted)


####Code to change population value for causes with sex_id not equal 3#####
# Replace population for age-group and cause specific discrepancies with cause_id=297(has correct population for all age groups)

# breaking dt into two parts in order to decrease processing time in next step.
dt_isnot_impacted <- dt[!cause_id %in% impacted_cause_ids, ]
dt_impacted <- dt[cause_id %in% impacted_cause_ids, ]

# Replace sex_id of causes which have sex_id 1 or 2

for (age_grp in 0:160){
  
  #For all sex id=1
  
  dt_impacted[(sex_id == 1), sex_id := 3]
  
  #For all sex id=2
  dt_impacted[(sex_id == 2), sex_id := 3]
  
  
}

# merge dt back into one
dt <- rbind(dt_impacted, dt_isnot_impacted)


####Code to change two cause related data####
##Cause ID: 382, age group- 160, sex=3
##change cases, YLDs and deaths to zero

dt <- dt[(sex_id == 3 & cause_id == 382 & age_group_id == 160), deaths := 0]
dt <- dt[(sex_id == 3 & cause_id == 382 & age_group_id == 160), cases := 0]
dt <- dt[(sex_id == 3 & cause_id == 382 & age_group_id == 160), case_rate := 0]
dt <- dt[(sex_id == 3 & cause_id == 382 & age_group_id == 160), ylds := 0]

##Cause ID: 384, age group- all EXCEPT 0, sex=3
##change cases, YLDs and deaths to zero

dt <- dt[(sex_id == 3 & cause_id == 384 & age_group_id != 0), deaths := 0]
dt <- dt[(sex_id == 3 & cause_id == 384 & age_group_id != 0), cases := 0]
dt <- dt[(sex_id == 3 & cause_id == 384 & age_group_id != 0), case_rate := 0]
dt <- dt[(sex_id == 3 & cause_id == 384 & age_group_id != 0), ylds := 0]

#Code to change population for specific causes that had discrepancies

#for cause 340
for (age_grp in 20:160){
  
  tb_pop_1996 <- dt[(cause_id == 297 & year_id == 1996 & age_group_id == age_grp), population]
  dt <- dt[(cause_id == 340 & age_group_id == age_grp & year_id == 1996), population := tb_pop_1996]
  
  tb_pop_2016 <- dt[(cause_id == 297 & year_id == 2016 & age_group_id == age_grp), population]
  dt <- dt[(cause_id == 340 & age_group_id == age_grp & year_id == 2016), population := tb_pop_2016]
  
}

#for cause 367

tb_pop_1996 <- dt[(cause_id == 297 & year_id == 1996 & age_group_id == 15), population]
dt <- dt[(cause_id == 367 & age_group_id == 15 & year_id == 1996), population := tb_pop_1996]

tb_pop_2016 <- dt[(cause_id == 297 & year_id == 2016 & age_group_id == 15), population]
dt <- dt[(cause_id == 367 & age_group_id == 15 & year_id == 2016), population := tb_pop_2016]

#for cause 382 & 384

tb_pop_1996 <- dt[(cause_id == 297 & year_id == 1996 & age_group_id == 160), population]
dt <- dt[(cause_id == 382 & age_group_id == 160 & year_id == 1996), population := tb_pop_1996]
dt <- dt[(cause_id == 384 & age_group_id == 160 & year_id == 1996), population := tb_pop_1996]

tb_pop_2016 <- dt[(cause_id == 297 & year_id == 2016 & age_group_id == 160), population]
dt <- dt[(cause_id == 382 & age_group_id == 160 & year_id == 2016), population := tb_pop_2016]
dt <- dt[(cause_id == 384 & age_group_id == 160 & year_id == 2016), population := tb_pop_2016]

#for cause 405

for (age_grp in 12:17){
  
  tb_pop_1996 <- dt[(cause_id == 297 & year_id == 1996 & age_group_id == age_grp), population]
  dt <- dt[(cause_id == 405 & age_group_id == age_grp & year_id == 1996), population := tb_pop_1996]
  
  tb_pop_2016 <- dt[(cause_id == 297 & year_id == 2016 & age_group_id == age_grp), population]
  dt <- dt[(cause_id == 405 & age_group_id == age_grp & year_id == 2016), population := tb_pop_2016]
  
}

#for cause 486
tb_pop_1996 <- dt[(cause_id == 297 & year_id == 1996 & age_group_id == 8), population]
dt <- dt[(cause_id == 486 & age_group_id == 8 & year_id == 1996), population := tb_pop_1996]

tb_pop_2016 <- dt[(cause_id == 297 & year_id == 2016 & age_group_id == 8), population]
dt <- dt[(cause_id == 486 & age_group_id == 8 & year_id == 2016), population := tb_pop_2016]

#for cause 572
tb_pop_1996 <- dt[(cause_id == 297 & year_id == 1996 & age_group_id == 6), population]
dt <- dt[(cause_id == 572 & age_group_id == 6 & year_id == 1996), population := tb_pop_1996]

tb_pop_2016 <- dt[(cause_id == 297 & year_id == 2016 & age_group_id == 6), population]
dt <- dt[(cause_id == 572 & age_group_id == 6 & year_id == 2016), population := tb_pop_2016]

#for cause 578
tb_pop_1996 <- dt[(cause_id == 297 & year_id == 1996 & age_group_id == 30), population]
dt <- dt[(cause_id == 578 & age_group_id == 30 & year_id == 1996), population := tb_pop_1996]

tb_pop_2016 <- dt[(cause_id == 297 & year_id == 2016 & age_group_id == 30), population]
dt <- dt[(cause_id == 578 & age_group_id == 30 & year_id == 2016), population := tb_pop_2016]

#for cause 594

for (age_grp in 6:7){
  
  tb_pop_1996 <- dt[(cause_id == 297 & year_id == 1996 & age_group_id == age_grp), population]
  dt <- dt[(cause_id == 594 & age_group_id == age_grp & year_id == 1996), population := tb_pop_1996]
  
  tb_pop_2016 <- dt[(cause_id == 297 & year_id == 2016 & age_group_id == age_grp), population]
  dt <- dt[(cause_id == 594 & age_group_id == age_grp & year_id == 2016), population := tb_pop_2016]
  
}

# overwrite hale_decomp_input_data with updated values
fwrite(dt, "./hale_decomp_input_data_2_v3.csv")

dt_draw_0 <- dt[draw == 0, ]
fwrite(dt_draw_0, "/snfs1/Project/Cost_Effectiveness/BEA/BEA_data_2023/HALE/hale_decomp_input_data_final_draw0.csv")

