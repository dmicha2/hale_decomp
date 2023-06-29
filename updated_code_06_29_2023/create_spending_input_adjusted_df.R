library(data.table)
library(dplyr)
source("/ihme/cc_resources/libraries/current/r/get_population.R")
source("/ihme/cc_resources/libraries/current/r/get_draws.R")
source("/ihme/cc_resources/libraries/current/r/get_age_metadata.R")
source("/ihme/cc_resources/libraries/current/r/get_cause_metadata.R")
setwd("/home/j/Project/Cost_Effectiveness/BEA/BEA_data_2023/data_new_naming_convention/Spending")
# Read in the list of DEX causes, the map from GBD causes to aggregate DEX causes, and the definitions of "case" for each cause
cause_map <- fread("./gbd_to_dex_cause_map.csv")
setnames(cause_map, "gbd_cause_id", "cause_id")
case_defns <- fread("./decomp_case_definitions.csv")
# Pull the age metadata and drop unneeded rows
age_metadata <- get_age_metadata(age_group_set_id = 12, gbd_round_id=6)
age_metadata[, c("age_group_weight_value", "most_detailed") := NULL]
# Adjust age group column to match with the expend_decomp data frame
age_metadata <- age_metadata[-c(1:3, 21:23), ]
# Define all_ages_set_12 variable to use in shared functions 
all_ages_set_12 <- age_metadata[, age_group_id]
age_metadata <- age_metadata %>% add_row(age_group_id = 28, age_group_name = "0 to 11 months", age_group_years_start = 0, age_group_years_end = 1, .before = 1)
age_metadata <- age_metadata %>% add_row(age_group_id = 160, age_group_name = "85 plus", age_group_years_start = 85, age_group_years_end = 125)

cause_metadata <- get_cause_metadata(cause_set_id=2, gbd_round_id=6)

# Define the years and cause_ids that we would like to decompose HALE for.
years <- c(1996, 2016)
all_ages <- age_metadata[, age_group_id]
gbd_cids <- cause_map[, unique(cause_id)]

#### Step 1: Create a base data.table ####
# Create a data.table of all combinations of GBD cause ID, age-group, sex, location, year, and draw, so that
# we don't end up with NAs for population or all-cause mortality for causes where there are no cs_deaths estimated.
dt <- data.table(expand.grid(draw=0:999, cause_id=gbd_cids, age_group_id=all_ages, sex_id=3, location_id=102, year_id=years))

#### Step 2: Pull a death data frame ####
# Pull death counts for all the GBD causes, plus the all-cause-aggregate (cause_id 294) 
cs_deaths <- get_draws("cause_id", gbd_id=c(gbd_cids, 294), gbd_round_id=6, source="codcorrect",
                    measure_id=1, metric_id=1, age_group_id=all_ages_set_12, sex_id=3,
                    location_id=102, year_id=years, decomp_step="step5")
deaths_age_groups_28_160 <- get_draws("cause_id", gbd_id=c(gbd_cids, 294), gbd_round_id=6, source="codcorrect",
                                      measure_id=1, metric_id=1, age_group_id=c(28, 160), sex_id=3,
                                      location_id=102, year_id=years, decomp_step="step5")
cs_deaths <- rbind(cs_deaths, deaths_age_groups_28_160)

# Reshape so that different draws are different rows.
cs_deaths <- melt(cs_deaths,
               id.vars=c("cause_id", "age_group_id", "sex_id", "location_id", "year_id"),
               measure.vars=paste0("draw_", 0:999),
               value.name="cs_deaths", variable.name="draw")
cs_deaths[, draw := as.integer(gsub("draw_", "", draw))]

# Split the all-cause-aggregate from the rest of the causes. Later we will merge the all-cause
# aggregate on so that each row has a separate column for cause-specific cs_deaths and all-cause cs_deaths.
ac_deaths <- cs_deaths[cause_id == 294]
cs_deaths <- cs_deaths[cause_id != 294]

# Calculating ac_deaths
all_cause_deaths_dt <- cs_deaths[, c("draw","age_group_id","year_id", "cs_deaths", "cause_id")]
all_cause_deaths_dt <- all_cause_deaths_dt %>%
  group_by(draw, age_group_id, year_id) %>%
  summarise(ac_deaths = sum(cs_deaths), cause_id = cause_id)
all_cause_deaths_dt <- setDT(all_cause_deaths_dt)
all_cause_deaths_dt[, cause_id := NULL]
all_cause_deaths_dt <- unique(all_cause_deaths_dt)

dt <- merge(dt, all_cause_deaths_dt, by=c("draw", "age_group_id", "year_id"), all=TRUE)

# Add the cs_deaths df to the base data.table
dt <- merge(dt, cs_deaths, by=c("draw", "cause_id", "age_group_id", "sex_id", "location_id", "year_id"), all=TRUE)

#### Step 3: Pull a causes data frame #### 
# Pull prevalence for causes where "cases" is defined as prevalence, and incidence where "cases"
# is defined as incidence. Then combine "cases" and merge with cs_deaths.
prev_cids <- case_defns[measure_id == 5, cause_id]
prev_cids <- cause_map[dex_cause_id %in% c(prev_cids, 10*prev_cids), cause_id]
inc_cids <- case_defns[measure_id == 6, cause_id]
inc_cids <- cause_map[dex_cause_id %in% c(inc_cids, 10*inc_cids), cause_id]
pops_cids <- case_defns[measure == "population", cause_id]
pops_cids <- cause_map[dex_cause_id %in% c(pops_cids, 10*pops_cids), cause_id]
deaths_cids <- case_defns[measure == "cs_deaths", cause_id]
deaths_cids <- cause_map[dex_cause_id %in% c(deaths_cids, 10*deaths_cids), cause_id]

prevs <- get_draws("cause_id", gbd_id=prev_cids, gbd_round_id=6, source="como",
                   measure_id=5, metric_id=3, age_group_id=all_ages_set_12, sex_id=3,
                   location_id=102, year_id=years, decomp_step="step5")
prevs_age_groups_28_160 <- get_draws("cause_id", gbd_id=prev_cids, gbd_round_id=6, source="como",
                                     measure_id=5, metric_id=3, age_group_id=c(28, 160), sex_id=3,
                                     location_id=102, year_id=years, decomp_step="step5")
prevs <- rbind(prevs, prevs_age_groups_28_160)

prevs <- melt(prevs,
              id.vars=c("cause_id", "age_group_id", "sex_id", "location_id", "year_id"),
              measure.vars=paste0("draw_", 0:999),
              value.name="case_rate", variable.name="draw")

incs <- get_draws("cause_id", gbd_id=inc_cids, gbd_round_id=6, source="como",
                  measure_id=6, metric_id=3, age_group_id=all_ages_set_12, sex_id=3,
                  location_id=102, year_id=years, decomp_step="step5")

incs_age_groups_28_160 <- get_draws("cause_id", gbd_id=inc_cids, gbd_round_id=6, source="como",
                                    measure_id=6, metric_id=3, age_group_id=c(28, 160), sex_id=3,
                                    location_id=102, year_id=years, decomp_step="step5")
incs <- rbind(incs, incs_age_groups_28_160)

incs <- melt(incs,
             id.vars=c("cause_id", "age_group_id", "sex_id", "location_id", "year_id"),
             measure.vars=paste0("draw_", 0:999),
             value.name="case_rate", variable.name="draw")
cases <- rbind(prevs, incs)
cases[, draw := as.integer(gsub("draw_", "", draw))]

# Add the cases df to the base data.table
dt <- merge(dt, cases, by=c("draw", "cause_id", "age_group_id", "sex_id", "location_id", "year_id"), all=TRUE)

#### Step 4: Pull a population data frame ####
# Get population and merge with cs_deaths & cases
pops <- get_population(age_group_id=c(all_ages_set_12, 28), sex_id=3, location_id=102, year_id=years, gbd_round_id=6, decomp_step="step5")
# Calculating age_group 160 as it is not returned by the function
pops_160 <- get_population(age_group_id=c(31, 32, 235), sex_id=3, location_id=102, year_id=years, gbd_round_id=6, decomp_step="step5")
pops_160 <- pops_160 %>%
  group_by(year_id, sex_id, location_id, run_id) %>%
  summarise(across(c(age_group_id, population), sum))

pops_160 <- setDT(pops_160)
pops_160[, age_group_id := 160]

pops <- rbind(pops, pops_160)
pops[, run_id := NULL]

# Add the population df to the base data table
dt <- merge(dt, pops, by=c("age_group_id", "sex_id", "location_id", "year_id"), all=TRUE)

dt[cause_id %in% deaths_cids, cases := cs_deaths]
dt[, cases := population * case_rate]
dt[cause_id %in% pops_cids, `:=`(cases=population, case_rate=1)]

# Merge on the all-cause_deaths
setnames(ac_deaths, "cs_deaths", "ac_deaths")
ac_deaths[, cause_id := NULL]
# dt <- merge(dt, ac_deaths, by=c("draw", "age_group_id", "sex_id", "location_id", "year_id"), all=TRUE)
# Merge on age metadata, cause_names, and definitions of "case"
dt <- merge(dt, age_metadata, by="age_group_id", all=TRUE)
dt <- merge(dt, cause_map[, .(cause_id, dex_cause_id, dex_cause_name=cause_name)], by="cause_id", all.x=TRUE)
dt <- merge(dt, cause_metadata[, .(cause_id, gbd_cause_name=cause_name)], by="cause_id", all.x=TRUE)
dt <- merge(dt, case_defns[, .(dex_cause_id=cause_id, case_definition=measure)], by="dex_cause_id", all.x=TRUE)


##Incorporating male/female only causes to make data consistent with hale

# Create a set of cause ids for which only one sex needs to be pulled
#male_cause_ids <- c(405, 486, 367)
female_cause_ids <- c(340, 572, 594)
impacted_cause_ids <- c(female_cause_ids)

#### Collecting data ####
# Pull death counts for all the GBD causes, plus the all-cause-aggregate
deaths_dt <- get_draws("cause_id", gbd_id=c(female_cause_ids), gbd_round_id=6, source="codcorrect",
                       measure_id=1, metric_id=1, age_group_id=all_ages_set_12, sex_id= c(1, 2),
                       location_id=102, year_id=years, decomp_step="step5")
deaths_age_groups_28_160 <- get_draws("cause_id", gbd_id=c(female_cause_ids), gbd_round_id=6, source="codcorrect",
                                      measure_id=1, metric_id=1, age_group_id=c(28, 160), sex_id= c(1, 2),
                                      location_id=102, year_id=years, decomp_step="step5")
deaths_age_groups_28_160[age_group_id == 28, age_group_id := 0]
deaths_dt <- rbind(deaths_dt, deaths_age_groups_28_160)

# # Reshape so that different draws are different rows.
deaths_dt <- melt(deaths_dt,
                  id.vars=c("cause_id", "age_group_id", "sex_id", "location_id", "year_id"),
                  measure.vars=paste0("draw_", 0:999),
                  value.name="cs_deaths", variable.name="draw")
deaths_dt[, draw := as.integer(gsub("draw_", "", draw))]

# Pull prevalence for causes where "cases" is defined as prevalence, and incidence where "cases"
# is defined as incidence.
prev_cids <- c(572,594)
inc_cids <- c(340)

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

# # Get population and merge with cs_deaths & cases
# pops <- get_population(age_group_id=c(all_ages_set_12, 28), sex_id=c(1, 2), location_id=102, year_id=years, gbd_round_id=6, decomp_step="step5")
# # Calculating age_group 160 as it is not returned by the function
# pops_160 <- get_population(age_group_id=c(31, 32, 235), sex_id=c(1, 2), location_id=102, year_id=years, gbd_round_id=6, decomp_step="step5")
# pops_160 <- pops_160 %>%
#   group_by(year_id, sex_id, location_id, run_id) %>%
#   summarise(across(c(age_group_id, population), sum))
# 
# pops_160 <- setDT(pops_160)
# pops_160[, age_group_id := 160]
# 
# pops_dt <- rbind(pops, pops_160)
# pops_dt[, run_id := NULL]

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
  
  dt <- dt[(dex_cause_id == dropped_cause_id & age_group_id == dropped_age_group_id & sex_id == dropped_sex_id), cs_deaths := 0]
  dt <- dt[(dex_cause_id == dropped_cause_id & age_group_id == dropped_age_group_id & sex_id == dropped_sex_id), cases := 0]
  dt <- dt[(dex_cause_id == dropped_cause_id & age_group_id == dropped_age_group_id & sex_id == dropped_sex_id), case_rate := 0]
  dt <- dt[(dex_cause_id == dropped_cause_id & age_group_id == dropped_age_group_id & sex_id == dropped_sex_id), cs_ylds := 0]
}

# breaking dt into two parts in order to decrease processing time in next step.
dt_isnot_impacted <- dt[!cause_id %in% impacted_cause_ids, ]
dt <- dt[cause_id %in% impacted_cause_ids, ]


#numberlist = c(0:999)
#lapply(numberlist, function (number){

##### True for one sex #####
for (d in 0:999){
  ## CAUSE_ID - 594, age_group - 6,7 sex 2
  for (age_grp in 6:7){
    deaths_val <- deaths_dt[(cause_id == 594 & sex_id == 2 & age_group_id == age_grp & draw == d & year_id == 1996), cs_deaths]
    dt[(cause_id == 594 & sex_id == 3 & age_group_id == age_grp & draw == d & year_id == 1996), cs_deaths := deaths_val]
    
    case_rate_val <- cases_dt[(cause_id == 594 & sex_id == 2 & age_group_id == age_grp & draw == d & year_id == 1996), case_rate]
    dt[(cause_id == 594 & sex_id == 3 & age_group_id == age_grp & draw == d & year_id == 1996), case_rate := case_rate_val]
    
    deaths_val <- deaths_dt[(cause_id == 594 & sex_id == 2 & age_group_id == age_grp & draw == d & year_id == 2016), cs_deaths]
    dt[(cause_id == 594 & sex_id == 3 & age_group_id == age_grp & draw == d & year_id == 2016), cs_deaths := deaths_val]
    
    case_rate_val <- cases_dt[(cause_id == 594 & sex_id == 2 & age_group_id == age_grp & draw == d & year_id == 2016), case_rate]
    dt[(cause_id == 594 & sex_id == 3 & age_group_id == age_grp & draw == d & year_id == 2016), case_rate := case_rate_val]
    
    dt[(cause_id == 594 & sex_id == 3 & age_group_id == age_grp & draw == d), sex_id := 2]
  }
  
  ## CAUSE_ID - 572, age_group - 6, sex 2
  deaths_val <- deaths_dt[(cause_id == 572 & sex_id == 2 & age_group_id == 6 & draw == d & year_id == 1996), cs_deaths]
  dt[(cause_id == 572 & sex_id == 3 & age_group_id == 6 & draw == d & year_id == 1996), cs_deaths := deaths_val]
  
  case_rate_val <- cases_dt[(cause_id == 572 & sex_id == 2 & age_group_id == 6 & draw == d & year_id == 1996), case_rate]
  dt[(cause_id == 572 & sex_id == 3 & age_group_id == 6 & draw == d & year_id == 1996), case_rate := case_rate_val]
  
  deaths_val <- deaths_dt[(cause_id == 572 & sex_id == 2 & age_group_id == 6 & draw == d & year_id == 2016), cs_deaths]
  dt[(cause_id == 572 & sex_id == 3 & age_group_id == 6 & draw == d & year_id == 2016), cs_deaths := deaths_val]
  
  case_rate_val <- cases_dt[(cause_id == 572 & sex_id == 2 & age_group_id == 6 & draw == d & year_id == 2016), case_rate]
  dt[(cause_id == 572 & sex_id == 3 & age_group_id == 6 & draw == d & year_id == 2016), case_rate := case_rate_val]
   
  dt[(cause_id == 572 & sex_id == 3 & age_group_id == 6 & draw == d), sex_id := 2]
  
  ## CAUSE_ID - 340, age_group - 20,160 sex 2
  age_grps <- c(20, 160)
  for (age_grp in age_grps){
    deaths_val <- deaths_dt[(cause_id == 340 & sex_id == 2 & age_group_id == age_grp & draw == d & year_id == 1996), cs_deaths]
    dt[(cause_id == 340 & sex_id == 3 & age_group_id == age_grp & draw == d & year_id == 1996), cs_deaths := deaths_val]
    
    case_rate_val <- cases_dt[(cause_id == 340 & sex_id == 2 & age_group_id == age_grp & draw == d & year_id == 1996), case_rate]
    dt[(cause_id == 340 & sex_id == 3 & age_group_id == age_grp & draw == d & year_id == 1996), case_rate := case_rate_val]
    
    deaths_val <- deaths_dt[(cause_id == 340 & sex_id == 2 & age_group_id == age_grp & draw == d & year_id == 2016), cs_deaths]
    dt[(cause_id == 340 & sex_id == 3 & age_group_id == age_grp & draw == d & year_id == 2016), cs_deaths := deaths_val]
    
    case_rate_val <- cases_dt[(cause_id == 340 & sex_id == 2 & age_group_id == age_grp & draw == d & year_id == 2016), case_rate]
    dt[(cause_id == 340 & sex_id == 3 & age_group_id == age_grp & draw == d & year_id == 2016), case_rate := case_rate_val]
    
    dt[(cause_id == 340 & sex_id == 3 & age_group_id == age_grp & draw == d), sex_id := 2]
  }
  
}

# merge dt back into one
dt <- rbind(dt, dt_isnot_impacted)

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











#### Step 6: Pull a spending data frame ####
spending <- fread("/mnt/team/costeffectiveness/NPC/ghdx_submission_version/expend_decomp_input_data.csv")
spending <- spending[, c("cause_id", "draw", "age_group_id", "year_id", "sex_id", "expenditure")]

spending<- spending %>% 
  rename(
    cs_spending = expenditure
  )

# Converting
spending <- spending %>%
  group_by(cause_id, draw, age_group_id, year_id) %>%
  summarise(across(c(sex_id, cs_spending), sum))


spending <- setDT(spending)
spending[, sex_id := 3]
# Adjust age_group_id
spending[age_group_id == 33, age_group_id := 160]

# Calculate ac_spending
all_cause_expenditure_dt <- spending[, c("draw","age_group_id","year_id", "cs_spending", "cause_id")]
all_cause_expenditure_dt <- all_cause_expenditure_dt %>%
  group_by(draw, age_group_id, year_id) %>%
  summarise(ac_spending = sum(cs_spending), cause_id = cause_id)
all_cause_expenditure_dt <- setDT(all_cause_expenditure_dt)
all_cause_expenditure_dt[, cause_id := NULL]
all_cause_expenditure_dt <- unique(all_cause_expenditure_dt)

spending <- merge(spending, all_cause_expenditure_dt, by=c("draw", "age_group_id", "year_id"), all=TRUE)
setnames(spending, "cause_id", "dex_cause_id")

# Add the spending df to the base data.table
dt[age_group_id == 28, age_group_id := 0]
dt <- merge(dt, spending, by=c("dex_cause_id", "draw", "age_group_id", "year_id", "sex_id"), all.x=TRUE)

# Drop rows with age_group that we are not going to use. 2, 3, 4, 31, 32, and 235 where cs_spending is na.
dt <- dt[!(age_group_id %between% c(2, 4) | age_group_id %between% c(31, 32) | age_group_id == 235)]

# Drop rows where cause_is is NA.
dt <- dt[!is.na(cause_id), ]

# If cases are NA, but cs_deaths aren't, then it could be because "cases" are really just cs_deaths ("Indirect maternal cs_deaths", "Maternal cs_deaths aggravated
# by HIV", "Aortic aneurism"), or GBD maybe doesn't have good estimates of "cases" by our definition, as is the case for "Other ..." causes, and DEX
# causes that are aggregates of an "Other ..." GBD cause.
cases_na_deaths_not <- dt[(!is.na(cs_deaths)) & (is.na(cases)) & (draw == 0) & (year_id == 1996) & (cs_deaths != 0),
                          .SD[, .(minimum=min(age_group_years_start), maximum=max(age_group_years_end))],
                          by=c("cause_id", "dex_cause_name")]
# Intestinal infectious diseases - aggregate of GBD intest. inf. dis. plus "Typhoid and paratyphoid"
# Measles
# Indirect maternal cs_deaths - cases = cs_deaths
# Other maternal disorders - includes "Late maternal cs_deaths" and "Maternal cs_deaths aggravated by HIV/AIDS", so cases = cs_deaths (at least for some subcauses)
# Aortic aneurism - cases = cs_deaths

# If cs_deaths are NA, but cases are not, it is just a nonfatal cause, or a fatal cause that isn't fatal for all age-groups where
# incidence or prevalence is defined. e.g. Migraine is nonfatal, and Neonatal sepsis and other neonatal infections is fatal for
# < 5, but for age groups over 5, there is prevalence, but not death (prevalence because of longer-lasting complications).
deaths_na_cases_not <- dt[(!is.na(cases)) & (cases != 0) & (is.na(cs_deaths)) & (draw == 0) & (year_id == 1996),
                          .SD[, .(minimum=min(age_group_years_start), maximum=max(age_group_years_end))],
                          by=c("cause_id", "dex_cause_name")]
# dt[!is.na(cs_deaths),
#    .SD[, .(minimum=min(age_group_years_start), maximum=max(age_group_years_end))],
#    by=c("cause_id", "dex_cause_name")]

# If cs_deaths are not estimated, then we should assume they are 0.
dt[is.na(cs_deaths), cs_deaths := 0]

# If cs_spending are not estimated, then we should assume they are 0. So as well as ac_spending
dt[is.na(cs_spending), cs_spending := 0]
dt[is.na(ac_spending), ac_spending := 0]

#### Investigation ####
# dt <- dt[is.na(case_rate), case_rate := 0 ]
# dt <- dt[is.na(cases), cases := 0 ]

# #For some causes (375, 376, and 501) where cases are NA but cs_death is not NA, we take cases=cs_deaths
# # dt <- dt[(cause_id == 375), cases := cs_deaths]
# # dt <- dt[(cause_id == 376), cases := cs_deaths]
# # dt <- dt[(cause_id == 501), cases := cs_deaths]
# dt[cause_id %in% deaths_cids, cases := cs_deaths]
# dt[, cases := population * case_rate]
# dt[cause_id %in% pops_cids, `:=`(cases=population, case_rate=1)]


setcolorder(dt,
            c("cause_id", "gbd_cause_name", "dex_cause_id", "dex_cause_name", "case_definition",
              "draw",
              "age_group_id", "age_group_name", "age_group_years_start", "age_group_years_end",
              "sex_id", "location_id", "year_id",
              "population", "cases", "case_rate",
              "cs_deaths", "ac_deaths",
              "cs_spending"))


######Code by Esha####################

#Fill in all_cause_death and ac_spending for rows that are missing these values
# values for all causes are same specific to age, year and draw

## Calculate ac_spending (keeping cause=297 as it has values for all age groups)

all_cause_expenditure_fill <- dt[, c("draw","age_group_id","year_id", "cs_spending", "cause_id")]
all_cause_expenditure_fill <- all_cause_expenditure_fill %>%
  group_by(draw, age_group_id, year_id) %>%
  summarise(ac_spending = sum(cs_spending), cause_id = 297)
all_cause_expenditure_fill <- setDT(all_cause_expenditure_fill)
all_cause_expenditure_fill[, cause_id := NULL]
all_cause_expenditure_fill <- unique(all_cause_expenditure_fill)

dt <- merge(dt, all_cause_expenditure_fill, by=c("draw", "age_group_id", "year_id"), all.x=TRUE)

## Pull a dataframe for entering all_cause_death
daly_death_dt <- fread("/snfs1/Project/Cost_Effectiveness/BEA/dropped_dalys_case_death_yld.csv")

## Calculate all_cause_death (keeping cause=405 as it has values for all age groups)
dt$ac_deaths[dt$ac_deaths==0] <- daly_death_dt$ac_deaths[daly_death_dt$cause_id==405 & daly_death_dt$draw==dt$draw & daly_death_dt$year_id==dt$year_id & daly_death_dt$age_group_id==dt$age_group_id]


dt <- subset(dt, select = -c(ac_spending.x))
dt <- dt %>% rename("ac_spending" = "ac_spending.y")


#dt <- fread("./spending_decomp_input_data_adjusted.csv")

## Setting cs_deaths=0 for specific cancer in age group <15 years (listed in "cancer_in_children_less_than_15yr_gbd_dex_match.xlsx")
#for causes 417, 426, 441, 444, 447, 465, 468, 471, 480

dt <- dt[(age_group_id == 0 & cause_id == 417), cs_deaths := 0]
dt <- dt[(age_group_id == 5 & cause_id == 417), cs_deaths := 0]
dt <- dt[(age_group_id == 7 & cause_id == 426), cs_deaths := 0]
dt <- dt[(age_group_id == 6 & cause_id == 441), cs_deaths := 0]
dt <- dt[(age_group_id == 7 & cause_id == 441), cs_deaths := 0]
dt <- dt[(age_group_id == 6 & cause_id == 444), cs_deaths := 0]
dt <- dt[(age_group_id == 7 & cause_id == 444), cs_deaths := 0]
dt <- dt[(age_group_id == 6 & cause_id == 465), cs_deaths := 0]
dt <- dt[(age_group_id == 7 & cause_id == 465), cs_deaths := 0]
dt <- dt[(age_group_id == 0 & cause_id == 468), cs_deaths := 0]
dt <- dt[(age_group_id == 5 & cause_id == 468), cs_deaths := 0]
dt <- dt[(age_group_id == 6 & cause_id == 468), cs_deaths := 0]
dt <- dt[(age_group_id == 7 & cause_id == 468), cs_deaths := 0]
dt <- dt[(age_group_id == 0 & cause_id == 471), cs_deaths := 0] 
dt <- dt[(age_group_id == 6 & cause_id == 480), cs_deaths := 0]


## Aggregating rows for duplicate cs_spending data DEX -> GBD causes

# Aggregating over cs_deaths 
dex_gbd_agg_deaths <- dt[, c("cause_id", "dex_cause_id", "draw","age_group_id","year_id", "cs_deaths")]
#dt <- dt %>% select(-c(cs_deaths))
dex_gbd_agg_deaths <- dex_gbd_agg_deaths %>%
  group_by(draw, age_group_id, year_id, dex_cause_id) %>%
  summarise(deaths_sum = sum(cs_deaths), cause_id = cause_id)
dex_gbd_agg_deaths <- setDT(dex_gbd_agg_deaths)
dex_gbd_agg_deaths[, cause_id := NULL]
dex_gbd_agg_deaths <- unique(dex_gbd_agg_deaths)
dt <- merge(dt, dex_gbd_agg_deaths, by=c("draw", "age_group_id", "year_id", "dex_cause_id"), all=TRUE)

# Aggregating over cases
dex_gbd_agg_cases <- dt[, c("cause_id", "dex_cause_id", "draw","age_group_id","year_id", "cases")]
#dt <- dt %>% select(-c(cs_deaths))
dex_gbd_agg_cases <- dex_gbd_agg_cases %>%
  group_by(draw, age_group_id, year_id, dex_cause_id) %>%
  summarise(cases_sum = sum(cases), cause_id = cause_id)
dex_gbd_agg_cases <- setDT(dex_gbd_agg_cases)
dex_gbd_agg_cases[, cause_id := NULL]
dex_gbd_agg_cases <- unique(dex_gbd_agg_cases)
dt <- merge(dt, dex_gbd_agg_cases, by=c("draw", "age_group_id", "year_id", "dex_cause_id"), all=TRUE)

# Aggregating over case_rate
dex_gbd_agg_case_rate <- dt[, c("cause_id", "dex_cause_id", "draw","age_group_id","year_id", "case_rate")]
#dt <- dt %>% select(-c(cs_deaths))
dex_gbd_agg_case_rate <- dex_gbd_agg_case_rate %>%
  group_by(draw, age_group_id, year_id, dex_cause_id) %>%
  summarise(case_rate_sum = sum(case_rate), cause_id = cause_id)
dex_gbd_agg_case_rate <- setDT(dex_gbd_agg_case_rate)
dex_gbd_agg_case_rate[, cause_id := NULL]
dex_gbd_agg_case_rate <- unique(dex_gbd_agg_case_rate)
dt <- merge(dt, dex_gbd_agg_case_rate, by=c("draw", "age_group_id", "year_id", "dex_cause_id"), all=TRUE)

#deleting redundant columns
dt <- dt %>% select(-c(cs_deaths, cases, case_rate))

#deleting redundant rows
dt<-dt[!(dt$cause_id=="489" | dt$cause_id=="490" | dt$cause_id=="379" | dt$cause_id=="741" | dt$cause_id=="958" | dt$cause_id=="995" | dt$cause_id=="631"), ]
dt<-dt[!(dt$cause_id=="945" | dt$cause_id=="959" | dt$cause_id=="507" | dt$cause_id=="541" | dt$cause_id=="557" | dt$cause_id=="716"), ]

#changing names of affected/aggregated cause_id and gbd_cause_name
dt <- within(dt, {
  f <- gbd_cause_name == 'Mesothelioma' & cause_id == '483'
  gbd_cause_name[f] <- 'dex_agg other neoplasms'
  cause_id[f] <- 'dex_488'
}) 

dt <- within(dt, {
  f <- gbd_cause_name == 'Late maternal cs_deaths' & cause_id == '376'
  gbd_cause_name[f] <- 'dex_agg other maternal disorders'
  cause_id[f] <- 'dex_3790'
}) 

dt <- within(dt, {
  f <- gbd_cause_name == 'Other intestinal infectious diseases' & cause_id == '321'
  gbd_cause_name[f] <- 'dex_agg intestinal infectious diseases'
  cause_id[f] <- 'dex_318'
}) 

dt <- within(dt, {
  f <- gbd_cause_name == 'Ectopic pregnancy' & cause_id == '374'
  gbd_cause_name[f] <- 'dex_agg maternal abortion, miscarriage, and ectopic pregnancy'
  cause_id[f] <- 'dex_371'
}) 

dt <- within(dt, {
  f <- gbd_cause_name == 'Low back pain' & cause_id == '630'
  gbd_cause_name[f] <- 'dex_agg low back and neck pain'
  cause_id[f] <- 'dex_629'
}) 

dt <- within(dt, {
  f <- gbd_cause_name == 'Executions and police conflict' & cause_id == '854'
  gbd_cause_name[f] <- 'dex_agg collective violence and legal intervention'
  cause_id[f] <- 'dex_730'
})

dt <- within(dt, {
  f <- gbd_cause_name == 'Diarrheal diseases' & cause_id == '302'
  gbd_cause_name[f] <- 'dex_agg diarrheal diseases'
  cause_id[f] <- 'dex_3020'
})

dt <- within(dt, {
  f <- gbd_cause_name == 'Non-rheumatic valvular heart disease' & cause_id == '504'
  gbd_cause_name[f] <- 'dex_agg other cardiovascular and circulatory diseases'
  cause_id[f] <- 'dex_5070'
})

dt <- within(dt, {
  f <- gbd_cause_name == 'Gastroesophageal reflux disease' & cause_id == '536'
  gbd_cause_name[f] <- 'dex_agg other digestive diseases'
  cause_id[f] <- 'dex_5410'
})

dt <- within(dt, {
  f <- gbd_cause_name == 'Motor neuron disease' & cause_id == '554'
  gbd_cause_name[f] <- 'dex_agg other neurological disorders'
  cause_id[f] <- 'dex_5570'
})

dt <- within(dt, {
  f <- gbd_cause_name == 'Environmental heat and cold exposure' & cause_id == '842'
  gbd_cause_name[f] <- 'dex_agg other unintentional injuries'
  cause_id[f] <- 'dex_7160'
})


dt <- dt %>% select(-c(f))
#rename aggregated variables
dt <- rename(dt, cs_deaths = deaths_sum
       , cases = cases_sum
       , case_rate= case_rate_sum
)




## Checking results for death>cases

## Indicator for death>cases

dt$death_lt_case <- ifelse(dt$cs_deaths>dt$cases,"1","0")

#summary of death>cases for two years in file: "dex_gbd_agg_summary_death_lt_cases_draw0.xlsx"

## Aggregating rows based on age groups for death>case resolution
## For cause 332

## For death
sum_deaths <- dt[, c("cause_id", "draw", "age_group_id", "year_id", "cs_deaths")]

sum_deaths_332 <- sum_deaths %>%
  filter(cause_id ==332) %>% # apply filter condition
  filter(age_group_id %in% c("0", "5")) %>%
  group_by(draw, year_id) %>%
  summarize(sum_deaths = sum(cs_deaths))

dt <- merge(dt, sum_deaths_332, by=c("draw", "year_id"), all=TRUE)

## For cases
sum_cases <- dt[, c("cause_id", "draw", "age_group_id", "year_id", "cases")]

sum_cases_332 <- sum_cases %>%
  filter(cause_id ==332) %>% # apply filter condition
  filter(age_group_id %in% c("0", "5")) %>%
  group_by(draw, year_id) %>%
  summarize(sum_cases = sum(cases))

dt <- merge(dt, sum_cases_332, by=c("draw", "year_id"), all=TRUE)

## For case_rate
sum_case_rate <- dt[, c("cause_id", "draw", "age_group_id", "year_id", "case_rate")]

sum_case_rate_332 <- sum_case_rate %>%
  filter(cause_id ==332) %>% # apply filter condition
  filter(age_group_id %in% c("0", "5")) %>%
  group_by(draw, year_id) %>%
  summarize(sum_case_rate = sum(case_rate))

dt <- merge(dt, sum_case_rate_332, by=c("draw", "year_id"), all=TRUE)

# replacing cs_deaths column values with new values in sum_deaths column for specific cause & age-group pairings
dt <- dt %>% 
  mutate(cs_deaths = ifelse(cause_id == '332' & age_group_id == '0',sum_deaths/2,cs_deaths))
dt <- dt %>% 
  mutate(cs_deaths = ifelse(cause_id == '332' & age_group_id == '5',sum_deaths/2,cs_deaths))

# replacing cases column values with new values in sum_cases column for specific cause & age-group pairings
dt <- dt %>% 
  mutate(cases = ifelse(cause_id == '332' & age_group_id == '0',sum_cases/2,cases))
dt <- dt %>% 
  mutate(cases = ifelse(cause_id == '332' & age_group_id == '5',sum_cases/2,cases))

# replacing case_rates column values with new values in sum_case_rate column for specific cause & age-group pairings
dt <- dt %>% 
  mutate(case_rate = ifelse(cause_id == '332' & age_group_id == '0',sum_case_rate/2,case_rate))
dt <- dt %>% 
  mutate(case_rate = ifelse(cause_id == '332' & age_group_id == '5',sum_case_rate/2,case_rate))

#also deleting all the new sum_ columns after entering the relevant data into specific columns
dt = select(dt, -c('sum_deaths', 'sum_cases', 'sum_case_rate'))




## For cause 411

## For death
sum_deaths <- dt[, c("cause_id", "draw", "age_group_id", "year_id", "cs_deaths")]

sum_deaths_411 <- sum_deaths %>%
  filter(cause_id ==411) %>% # apply filter condition
  filter(age_group_id %in% c("18", "19", "20", "30", "160")) %>%
  group_by(draw, year_id) %>%
  summarize(sum_deaths = sum(cs_deaths))

dt <- merge(dt, sum_deaths_411, by=c("draw", "year_id"), all=TRUE)

## For cases
sum_cases <- dt[, c("cause_id", "draw", "age_group_id", "year_id", "cases")]

sum_cases_411 <- sum_cases %>%
  filter(cause_id ==411) %>% # apply filter condition
  filter(age_group_id %in% c("18", "19", "20", "30", "160")) %>%
  group_by(draw, year_id) %>%
  summarize(sum_cases = sum(cases))

dt <- merge(dt, sum_cases_411, by=c("draw", "year_id"), all=TRUE)

## For case_rate
sum_case_rate <- dt[, c("cause_id", "draw", "age_group_id", "year_id", "case_rate")]

sum_case_rate_411 <- sum_case_rate %>%
  filter(cause_id ==411) %>% # apply filter condition
  filter(age_group_id %in% c("18", "19", "20", "30", "160")) %>%
  group_by(draw, year_id) %>%
  summarize(sum_case_rate = sum(case_rate))

dt <- merge(dt, sum_case_rate_411, by=c("draw", "year_id"), all=TRUE)


# replacing cs_deaths column values with new values in sum_deaths column for specific cause & age-group pairings
dt <- dt %>% 
  mutate(cs_deaths = ifelse(cause_id == '411' & age_group_id == '18',sum_deaths/5,cs_deaths))
dt <- dt %>% 
  mutate(cs_deaths = ifelse(cause_id == '411' & age_group_id == '19',sum_deaths/5,cs_deaths))
dt <- dt %>% 
  mutate(cs_deaths = ifelse(cause_id == '411' & age_group_id == '20',sum_deaths/5,cs_deaths))
dt <- dt %>% 
  mutate(cs_deaths = ifelse(cause_id == '411' & age_group_id == '30',sum_deaths/5,cs_deaths))
dt <- dt %>% 
  mutate(cs_deaths = ifelse(cause_id == '411' & age_group_id == '160',sum_deaths/5,cs_deaths))

# replacing cases column values with new values in sum_cases column for specific cause & age-group pairings
dt <- dt %>% 
  mutate(cases = ifelse(cause_id == '411' & age_group_id == '18',sum_cases/5,cases))
dt <- dt %>% 
  mutate(cases = ifelse(cause_id == '411' & age_group_id == '19',sum_cases/5,cases))
dt <- dt %>% 
  mutate(cases = ifelse(cause_id == '411' & age_group_id == '20',sum_cases/5,cases))
dt <- dt %>% 
  mutate(cases = ifelse(cause_id == '411' & age_group_id == '30',sum_cases/5,cases))
dt <- dt %>% 
  mutate(cases = ifelse(cause_id == '411' & age_group_id == '160',sum_cases/5,cases))

# replacing case_rates column values with new values in sum_case_rate column for specific cause & age-group pairings
dt <- dt %>% 
  mutate(case_rate = ifelse(cause_id == '411' & age_group_id == '18',sum_case_rate/5,case_rate))
dt <- dt %>% 
  mutate(case_rate = ifelse(cause_id == '411' & age_group_id == '19',sum_case_rate/5,case_rate))
dt <- dt %>% 
  mutate(case_rate = ifelse(cause_id == '411' & age_group_id == '20',sum_case_rate/5,case_rate))
dt <- dt %>% 
  mutate(case_rate = ifelse(cause_id == '411' & age_group_id == '30',sum_case_rate/5,case_rate))
dt <- dt %>% 
  mutate(case_rate = ifelse(cause_id == '411' & age_group_id == '160',sum_case_rate/5,case_rate))


#also deleting all the new sum_ columns after entering the relevant data into specific columns
dt = select(dt, -c('sum_deaths', 'sum_cases', 'sum_case_rate'))



## For cause 417

## For death
sum_deaths <- dt[, c("cause_id", "draw", "age_group_id", "year_id", "cs_deaths")]

sum_deaths_417 <- sum_deaths %>%
  filter(cause_id ==417) %>% # apply filter condition
  filter(age_group_id %in% c("17", "18", "19", "20", "30", "160")) %>%
  group_by(draw, year_id) %>%
  summarize(sum_deaths = sum(cs_deaths))

dt <- merge(dt, sum_deaths_417, by=c("draw", "year_id"), all=TRUE)

## For cases
sum_cases <- dt[, c("cause_id", "draw", "age_group_id", "year_id", "cases")]

sum_cases_417 <- sum_cases %>%
  filter(cause_id ==417) %>% # apply filter condition
  filter(age_group_id %in% c("17", "18", "19", "20", "30", "160")) %>%
  group_by(draw, year_id) %>%
  summarize(sum_cases = sum(cases))

dt <- merge(dt, sum_cases_417, by=c("draw", "year_id"), all=TRUE)

## For case_rate
sum_case_rate <- dt[, c("cause_id", "draw", "age_group_id", "year_id", "case_rate")]

sum_case_rate_417 <- sum_case_rate %>%
  filter(cause_id ==417) %>% # apply filter condition
  filter(age_group_id %in% c("17", "18", "19", "20", "30", "160")) %>%
  group_by(draw, year_id) %>%
  summarize(sum_case_rate = sum(case_rate))

dt <- merge(dt, sum_case_rate_417, by=c("draw", "year_id"), all=TRUE)

# replacing cs_deaths column values with new values in sum_deaths column for specific cause & age-group pairings
dt <- dt %>% 
  mutate(cs_deaths = ifelse(cause_id == '417' & age_group_id == '17',sum_deaths/6,cs_deaths))
dt <- dt %>% 
  mutate(cs_deaths = ifelse(cause_id == '417' & age_group_id == '18',sum_deaths/6,cs_deaths))
dt <- dt %>% 
  mutate(cs_deaths = ifelse(cause_id == '417' & age_group_id == '19',sum_deaths/6,cs_deaths))
dt <- dt %>% 
  mutate(cs_deaths = ifelse(cause_id == '417' & age_group_id == '20',sum_deaths/6,cs_deaths))
dt <- dt %>% 
  mutate(cs_deaths = ifelse(cause_id == '417' & age_group_id == '30',sum_deaths/6,cs_deaths))
dt <- dt %>% 
  mutate(cs_deaths = ifelse(cause_id == '417' & age_group_id == '160',sum_deaths/6,cs_deaths))

# replacing cases column values with new values in sum_cases column for specific cause & age-group pairings
dt <- dt %>% 
  mutate(cases = ifelse(cause_id == '417' & age_group_id == '17',sum_cases/6,cases))
dt <- dt %>% 
  mutate(cases = ifelse(cause_id == '417' & age_group_id == '18',sum_cases/6,cases))
dt <- dt %>% 
  mutate(cases = ifelse(cause_id == '417' & age_group_id == '19',sum_cases/6,cases))
dt <- dt %>% 
  mutate(cases = ifelse(cause_id == '417' & age_group_id == '20',sum_cases/6,cases))
dt <- dt %>% 
  mutate(cases = ifelse(cause_id == '417' & age_group_id == '30',sum_cases/6,cases))
dt <- dt %>% 
  mutate(cases = ifelse(cause_id == '417' & age_group_id == '160',sum_cases/6,cases))

# replacing case_rates column values with new values in sum_case_rate column for specific cause & age-group pairings
dt <- dt %>% 
  mutate(case_rate = ifelse(cause_id == '417' & age_group_id == '17',sum_case_rate/6,case_rate))
dt <- dt %>% 
  mutate(case_rate = ifelse(cause_id == '417' & age_group_id == '18',sum_case_rate/6,case_rate))
dt <- dt %>% 
  mutate(case_rate = ifelse(cause_id == '417' & age_group_id == '19',sum_case_rate/6,case_rate))
dt <- dt %>% 
  mutate(case_rate = ifelse(cause_id == '417' & age_group_id == '20',sum_case_rate/6,case_rate))
dt <- dt %>% 
  mutate(case_rate = ifelse(cause_id == '417' & age_group_id == '30',sum_case_rate/6,case_rate))
dt <- dt %>% 
  mutate(case_rate = ifelse(cause_id == '417' & age_group_id == '160',sum_case_rate/6,case_rate))


#also deleting all the new sum_ columns after entering the relevant data into specific columns
dt = select(dt, -c('sum_deaths', 'sum_cases', 'sum_case_rate'))



## For cause 426

## For death
sum_deaths <- dt[, c("cause_id", "draw", "age_group_id", "year_id", "cs_deaths")]

sum_deaths_426 <- sum_deaths %>%
  filter(cause_id ==426) %>% # apply filter condition
  filter(age_group_id %in% c("30", "160")) %>%
  group_by(draw, year_id) %>%
  summarize(sum_deaths = sum(cs_deaths))

dt <- merge(dt, sum_deaths_426, by=c("draw", "year_id"), all=TRUE)

## For cases
sum_cases <- dt[, c("cause_id", "draw", "age_group_id", "year_id", "cases")]

sum_cases_426 <- sum_cases %>%
  filter(cause_id ==426) %>% # apply filter condition
  filter(age_group_id %in% c("30", "160")) %>%
  group_by(draw, year_id) %>%
  summarize(sum_cases = sum(cases))

dt <- merge(dt, sum_cases_426, by=c("draw", "year_id"), all=TRUE)

## For case_rate
sum_case_rate <- dt[, c("cause_id", "draw", "age_group_id", "year_id", "case_rate")]

sum_case_rate_426 <- sum_case_rate %>%
  filter(cause_id ==426) %>% # apply filter condition
  filter(age_group_id %in% c("30", "160")) %>%
  group_by(draw, year_id) %>%
  summarize(sum_case_rate = sum(case_rate))

dt <- merge(dt, sum_case_rate_426, by=c("draw", "year_id"), all=TRUE)


# replacing cs_deaths column values with new values in sum_deaths column for specific cause & age-group pairings
dt <- dt %>% 
  mutate(cs_deaths = ifelse(cause_id == '426' & age_group_id == '30',sum_deaths/2,cs_deaths))
dt <- dt %>% 
  mutate(cs_deaths = ifelse(cause_id == '426' & age_group_id == '160',sum_deaths/2,cs_deaths))

# replacing cases column values with new values in sum_cases column for specific cause & age-group pairings
dt <- dt %>% 
  mutate(cases = ifelse(cause_id == '426' & age_group_id == '30',sum_cases/2,cases))
dt <- dt %>% 
  mutate(cases = ifelse(cause_id == '426' & age_group_id == '160',sum_cases/2,cases))

# replacing case_rates column values with new values in sum_case_rate column for specific cause & age-group pairings
dt <- dt %>% 
  mutate(case_rate = ifelse(cause_id == '426' & age_group_id == '30',sum_case_rate/2,case_rate))
dt <- dt %>% 
  mutate(case_rate = ifelse(cause_id == '426' & age_group_id == '160',sum_case_rate/2,case_rate))


#also deleting all the new sum_ columns after entering the relevant data into specific columns
dt = select(dt, -c('sum_deaths', 'sum_cases', 'sum_case_rate'))



## For cause 432

## For death
sum_deaths <- dt[, c("cause_id", "draw", "age_group_id", "year_id", "cs_deaths")]

sum_deaths_432 <- sum_deaths %>%
  filter(cause_id ==432) %>% # apply filter condition
  filter(age_group_id %in% c("20", "30", "160")) %>%
  group_by(draw, year_id) %>%
  summarize(sum_deaths = sum(cs_deaths))

dt <- merge(dt, sum_deaths_432, by=c("draw", "year_id"), all=TRUE)

## For cases
sum_cases <- dt[, c("cause_id", "draw", "age_group_id", "year_id", "cases")]

sum_cases_432 <- sum_cases %>%
  filter(cause_id ==432) %>% # apply filter condition
  filter(age_group_id %in% c("20", "30", "160")) %>%
  group_by(draw, year_id) %>%
  summarize(sum_cases = sum(cases))

dt <- merge(dt, sum_cases_432, by=c("draw", "year_id"), all=TRUE)

## For case_rate
sum_case_rate <- dt[, c("cause_id", "draw", "age_group_id", "year_id", "case_rate")]

sum_case_rate_432 <- sum_case_rate %>%
  filter(cause_id ==432) %>% # apply filter condition
  filter(age_group_id %in% c("20", "30", "160")) %>%
  group_by(draw, year_id) %>%
  summarize(sum_case_rate = sum(case_rate))

dt <- merge(dt, sum_case_rate_432, by=c("draw", "year_id"), all=TRUE)


# replacing cs_deaths column values with new values in sum_deaths column for specific cause & age-group pairings
dt <- dt %>% 
  mutate(cs_deaths = ifelse(cause_id == '432' & age_group_id == '20',sum_deaths/3,cs_deaths))
dt <- dt %>% 
  mutate(cs_deaths = ifelse(cause_id == '432' & age_group_id == '30',sum_deaths/3,cs_deaths))
dt <- dt %>% 
  mutate(cs_deaths = ifelse(cause_id == '432' & age_group_id == '160',sum_deaths/3,cs_deaths))

# replacing cases column values with new values in sum_cases column for specific cause & age-group pairings
dt <- dt %>% 
  mutate(cases = ifelse(cause_id == '432' & age_group_id == '20',sum_cases/3,cases))
dt <- dt %>% 
  mutate(cases = ifelse(cause_id == '432' & age_group_id == '30',sum_cases/3,cases))
dt <- dt %>% 
  mutate(cases = ifelse(cause_id == '432' & age_group_id == '160',sum_cases/3,cases))

# replacing case_rates column values with new values in sum_case_rate column for specific cause & age-group pairings
dt <- dt %>% 
  mutate(case_rate = ifelse(cause_id == '432' & age_group_id == '20',sum_case_rate/3,case_rate))
dt <- dt %>% 
  mutate(case_rate = ifelse(cause_id == '432' & age_group_id == '30',sum_case_rate/3,case_rate))
dt <- dt %>% 
  mutate(case_rate = ifelse(cause_id == '432' & age_group_id == '160',sum_case_rate/3,case_rate))


#also deleting all the new sum_ columns after entering the relevant data into specific columns
dt = select(dt, -c('sum_deaths', 'sum_cases', 'sum_case_rate'))



## For cause 456

## For death
sum_deaths <- dt[, c("cause_id", "draw", "age_group_id", "year_id", "cs_deaths")]

sum_deaths_456 <- sum_deaths %>%
  filter(cause_id ==456) %>% # apply filter condition
  filter(age_group_id %in% c("14", "15", "16", "17", "18", "19", "20", "30", "160")) %>%
  group_by(draw, year_id) %>%
  summarize(sum_deaths = sum(cs_deaths))

dt <- merge(dt, sum_deaths_456, by=c("draw", "year_id"), all=TRUE)

## For cases
sum_cases <- dt[, c("cause_id", "draw", "age_group_id", "year_id", "cases")]

sum_cases_456 <- sum_cases %>%
  filter(cause_id ==456) %>% # apply filter condition
  filter(age_group_id %in% c("14", "15", "16", "17", "18", "19", "20", "30", "160")) %>%
  group_by(draw, year_id) %>%
  summarize(sum_cases = sum(cases))

dt <- merge(dt, sum_cases_456, by=c("draw", "year_id"), all=TRUE)

## For case_rate
sum_case_rate <- dt[, c("cause_id", "draw", "age_group_id", "year_id", "case_rate")]

sum_case_rate_456 <- sum_case_rate %>%
  filter(cause_id ==456) %>% # apply filter condition
  filter(age_group_id %in% c("14", "15", "16", "17", "18", "19", "20", "30", "160")) %>%
  group_by(draw, year_id) %>%
  summarize(sum_case_rate = sum(case_rate))

dt <- merge(dt, sum_case_rate_456, by=c("draw", "year_id"), all=TRUE)


# replacing cs_deaths column values with new values in sum_deaths column for specific cause & age-group pairings
dt <- dt %>% 
  mutate(cs_deaths = ifelse(cause_id == '456' & age_group_id == '14',sum_deaths/9,cs_deaths))
dt <- dt %>% 
  mutate(cs_deaths = ifelse(cause_id == '456' & age_group_id == '15',sum_deaths/9,cs_deaths))
dt <- dt %>% 
  mutate(cs_deaths = ifelse(cause_id == '456' & age_group_id == '16',sum_deaths/9,cs_deaths))
dt <- dt %>% 
  mutate(cs_deaths = ifelse(cause_id == '456' & age_group_id == '17',sum_deaths/9,cs_deaths))
dt <- dt %>% 
  mutate(cs_deaths = ifelse(cause_id == '456' & age_group_id == '18',sum_deaths/9,cs_deaths))
dt <- dt %>% 
  mutate(cs_deaths = ifelse(cause_id == '456' & age_group_id == '19',sum_deaths/9,cs_deaths))
dt <- dt %>% 
  mutate(cs_deaths = ifelse(cause_id == '456' & age_group_id == '20',sum_deaths/9,cs_deaths))
dt <- dt %>% 
  mutate(cs_deaths = ifelse(cause_id == '456' & age_group_id == '30',sum_deaths/9,cs_deaths))
dt <- dt %>% 
  mutate(cs_deaths = ifelse(cause_id == '456' & age_group_id == '160',sum_deaths/9,cs_deaths))

# replacing cases column values with new values in sum_cases column for specific cause & age-group pairings
dt <- dt %>% 
  mutate(cases = ifelse(cause_id == '456' & age_group_id == '14',sum_cases/9,cases))
dt <- dt %>% 
  mutate(cases = ifelse(cause_id == '456' & age_group_id == '15',sum_cases/9,cases))
dt <- dt %>% 
  mutate(cases = ifelse(cause_id == '456' & age_group_id == '16',sum_cases/9,cases))
dt <- dt %>% 
  mutate(cases = ifelse(cause_id == '456' & age_group_id == '17',sum_cases/9,cases))
dt <- dt %>% 
  mutate(cases = ifelse(cause_id == '456' & age_group_id == '18',sum_cases/9,cases))
dt <- dt %>% 
  mutate(cases = ifelse(cause_id == '456' & age_group_id == '19',sum_cases/9,cases))
dt <- dt %>% 
  mutate(cases = ifelse(cause_id == '456' & age_group_id == '20',sum_cases/9,cases))
dt <- dt %>% 
  mutate(cases = ifelse(cause_id == '456' & age_group_id == '30',sum_cases/9,cases))
dt <- dt %>% 
  mutate(cases = ifelse(cause_id == '456' & age_group_id == '160',sum_cases/9,cases))

# replacing case_rates column values with new values in sum_case_rate column for specific cause & age-group pairings
dt <- dt %>% 
  mutate(case_rate = ifelse(cause_id == '456' & age_group_id == '14',sum_case_rate/9,case_rate))
dt <- dt %>% 
  mutate(case_rate = ifelse(cause_id == '456' & age_group_id == '15',sum_case_rate/9,case_rate))
dt <- dt %>% 
  mutate(case_rate = ifelse(cause_id == '456' & age_group_id == '16',sum_case_rate/9,case_rate))
dt <- dt %>% 
  mutate(case_rate = ifelse(cause_id == '456' & age_group_id == '17',sum_case_rate/9,case_rate))
dt <- dt %>% 
  mutate(case_rate = ifelse(cause_id == '456' & age_group_id == '18',sum_case_rate/9,case_rate))
dt <- dt %>% 
  mutate(case_rate = ifelse(cause_id == '456' & age_group_id == '19',sum_case_rate/9,case_rate))
dt <- dt %>% 
  mutate(case_rate = ifelse(cause_id == '456' & age_group_id == '20',sum_case_rate/9,case_rate))
dt <- dt %>% 
  mutate(case_rate = ifelse(cause_id == '456' & age_group_id == '30',sum_case_rate/9,case_rate))
dt <- dt %>% 
  mutate(case_rate = ifelse(cause_id == '456' & age_group_id == '160',sum_case_rate/9,case_rate))


#also deleting all the new sum_ columns after entering the relevant data into specific columns
dt = select(dt, -c('sum_deaths', 'sum_cases', 'sum_case_rate'))




## For cause 465

## For death
sum_deaths <- dt[, c("cause_id", "draw", "age_group_id", "year_id", "cs_deaths")]

sum_deaths_465 <- sum_deaths %>%
  filter(cause_id ==465) %>% # apply filter condition
  filter(age_group_id %in% c("19","20", "30", "160")) %>%
  group_by(draw, year_id) %>%
  summarize(sum_deaths = sum(cs_deaths))

dt <- merge(dt, sum_deaths_465, by=c("draw", "year_id"), all=TRUE)

## For cases
sum_cases <- dt[, c("cause_id", "draw", "age_group_id", "year_id", "cases")]

sum_cases_465 <- sum_cases %>%
  filter(cause_id ==465) %>% # apply filter condition
  filter(age_group_id %in% c("19","20", "30", "160")) %>%
  group_by(draw, year_id) %>%
  summarize(sum_cases = sum(cases))

dt <- merge(dt, sum_cases_465, by=c("draw", "year_id"), all=TRUE)

## For case_rate
sum_case_rate <- dt[, c("cause_id", "draw", "age_group_id", "year_id", "case_rate")]

sum_case_rate_465 <- sum_case_rate %>%
  filter(cause_id ==465) %>% # apply filter condition
  filter(age_group_id %in% c("19","20", "30", "160")) %>%
  group_by(draw, year_id) %>%
  summarize(sum_case_rate = sum(case_rate))

dt <- merge(dt, sum_case_rate_465, by=c("draw", "year_id"), all=TRUE)


# replacing cs_deaths column values with new values in sum_deaths column for specific cause & age-group pairings
dt <- dt %>% 
  mutate(cs_deaths = ifelse(cause_id == '465' & age_group_id == '19',sum_deaths/4,cs_deaths))
dt <- dt %>% 
  mutate(cs_deaths = ifelse(cause_id == '465' & age_group_id == '20',sum_deaths/4,cs_deaths))
dt <- dt %>% 
  mutate(cs_deaths = ifelse(cause_id == '465' & age_group_id == '30',sum_deaths/4,cs_deaths))
dt <- dt %>% 
  mutate(cs_deaths = ifelse(cause_id == '465' & age_group_id == '160',sum_deaths/4,cs_deaths))

# replacing cases column values with new values in sum_cases column for specific cause & age-group pairings
dt <- dt %>% 
  mutate(cases = ifelse(cause_id == '465' & age_group_id == '19',sum_cases/4,cases))
dt <- dt %>% 
  mutate(cases = ifelse(cause_id == '465' & age_group_id == '20',sum_cases/4,cases))
dt <- dt %>% 
  mutate(cases = ifelse(cause_id == '465' & age_group_id == '30',sum_cases/4,cases))
dt <- dt %>% 
  mutate(cases = ifelse(cause_id == '465' & age_group_id == '160',sum_cases/4,cases))

# replacing case_rates column values with new values in sum_case_rate column for specific cause & age-group pairings
dt <- dt %>% 
  mutate(case_rate = ifelse(cause_id == '465' & age_group_id == '19',sum_case_rate/4,case_rate))
dt <- dt %>% 
  mutate(case_rate = ifelse(cause_id == '465' & age_group_id == '20',sum_case_rate/4,case_rate))
dt <- dt %>% 
  mutate(case_rate = ifelse(cause_id == '465' & age_group_id == '30',sum_case_rate/4,case_rate))
dt <- dt %>% 
  mutate(case_rate = ifelse(cause_id == '465' & age_group_id == '160',sum_case_rate/4,case_rate))


#also deleting all the new sum_ columns after entering the relevant data into specific columns
dt = select(dt, -c('sum_deaths', 'sum_cases', 'sum_case_rate'))


## For cause 468

## For death
sum_deaths <- dt[, c("cause_id", "draw", "age_group_id", "year_id", "cs_deaths")]

sum_deaths_468 <- sum_deaths %>%
  filter(cause_id ==468) %>% # apply filter condition
  filter(age_group_id %in% c("19","20", "30", "160")) %>%
  group_by(draw, year_id) %>%
  summarize(sum_deaths = sum(cs_deaths))

dt <- merge(dt, sum_deaths_468, by=c("draw", "year_id"), all=TRUE)

## For cases
sum_cases <- dt[, c("cause_id", "draw", "age_group_id", "year_id", "cases")]

sum_cases_468 <- sum_cases %>%
  filter(cause_id ==468) %>% # apply filter condition
  filter(age_group_id %in% c("19","20", "30", "160")) %>%
  group_by(draw, year_id) %>%
  summarize(sum_cases = sum(cases))

dt <- merge(dt, sum_cases_468, by=c("draw", "year_id"), all=TRUE)

## For case_rate
sum_case_rate <- dt[, c("cause_id", "draw", "age_group_id", "year_id", "case_rate")]

sum_case_rate_468 <- sum_case_rate %>%
  filter(cause_id ==468) %>% # apply filter condition
  filter(age_group_id %in% c("19","20", "30", "160")) %>%
  group_by(draw, year_id) %>%
  summarize(sum_case_rate = sum(case_rate))

dt <- merge(dt, sum_case_rate_468, by=c("draw", "year_id"), all=TRUE)


# replacing cs_deaths column values with new values in sum_deaths column for specific cause & age-group pairings
dt <- dt %>% 
  mutate(cs_deaths = ifelse(cause_id == '468' & age_group_id == '19',sum_deaths/4,cs_deaths))
dt <- dt %>% 
  mutate(cs_deaths = ifelse(cause_id == '468' & age_group_id == '20',sum_deaths/4,cs_deaths))
dt <- dt %>% 
  mutate(cs_deaths = ifelse(cause_id == '468' & age_group_id == '30',sum_deaths/4,cs_deaths))
dt <- dt %>% 
  mutate(cs_deaths = ifelse(cause_id == '468' & age_group_id == '160',sum_deaths/4,cs_deaths))

# replacing cases column values with new values in sum_cases column for specific cause & age-group pairings
dt <- dt %>% 
  mutate(cases = ifelse(cause_id == '468' & age_group_id == '19',sum_cases/4,cases))
dt <- dt %>% 
  mutate(cases = ifelse(cause_id == '468' & age_group_id == '20',sum_cases/4,cases))
dt <- dt %>% 
  mutate(cases = ifelse(cause_id == '468' & age_group_id == '30',sum_cases/4,cases))
dt <- dt %>% 
  mutate(cases = ifelse(cause_id == '468' & age_group_id == '160',sum_cases/4,cases))

# replacing case_rates column values with new values in sum_case_rate column for specific cause & age-group pairings
dt <- dt %>% 
  mutate(case_rate = ifelse(cause_id == '468' & age_group_id == '19',sum_case_rate/4,case_rate))
dt <- dt %>% 
  mutate(case_rate = ifelse(cause_id == '468' & age_group_id == '20',sum_case_rate/4,case_rate))
dt <- dt %>% 
  mutate(case_rate = ifelse(cause_id == '468' & age_group_id == '30',sum_case_rate/4,case_rate))
dt <- dt %>% 
  mutate(case_rate = ifelse(cause_id == '468' & age_group_id == '160',sum_case_rate/4,case_rate))


#also deleting all the new sum_ columns after entering the relevant data into specific columns
dt = select(dt, -c('sum_deaths', 'sum_cases', 'sum_case_rate'))


## For cause 477

## For death
sum_deaths <- dt[, c("cause_id", "draw", "age_group_id", "year_id", "cs_deaths")]

sum_deaths_477 <- sum_deaths %>%
  filter(cause_id ==477) %>% # apply filter condition
  filter(age_group_id %in% c("19","20", "30", "160")) %>%
  group_by(draw, year_id) %>%
  summarize(sum_deaths = sum(cs_deaths))

dt <- merge(dt, sum_deaths_477, by=c("draw", "year_id"), all=TRUE)

## For cases
sum_cases <- dt[, c("cause_id", "draw", "age_group_id", "year_id", "cases")]

sum_cases_477 <- sum_cases %>%
  filter(cause_id ==477) %>% # apply filter condition
  filter(age_group_id %in% c("19","20", "30", "160")) %>%
  group_by(draw, year_id) %>%
  summarize(sum_cases = sum(cases))

dt <- merge(dt, sum_cases_477, by=c("draw", "year_id"), all=TRUE)

## For case_rate
sum_case_rate <- dt[, c("cause_id", "draw", "age_group_id", "year_id", "case_rate")]

sum_case_rate_477 <- sum_case_rate %>%
  filter(cause_id ==477) %>% # apply filter condition
  filter(age_group_id %in% c("19","20", "30", "160")) %>%
  group_by(draw, year_id) %>%
  summarize(sum_case_rate = sum(case_rate))

dt <- merge(dt, sum_case_rate_477, by=c("draw", "year_id"), all=TRUE)


# replacing cs_deaths column values with new values in sum_deaths column for specific cause & age-group pairings
dt <- dt %>% 
  mutate(cs_deaths = ifelse(cause_id == '477' & age_group_id == '19',sum_deaths/4,cs_deaths))
dt <- dt %>% 
  mutate(cs_deaths = ifelse(cause_id == '477' & age_group_id == '20',sum_deaths/4,cs_deaths))
dt <- dt %>% 
  mutate(cs_deaths = ifelse(cause_id == '477' & age_group_id == '30',sum_deaths/4,cs_deaths))
dt <- dt %>% 
  mutate(cs_deaths = ifelse(cause_id == '477' & age_group_id == '160',sum_deaths/4,cs_deaths))

# replacing cases column values with new values in sum_cases column for specific cause & age-group pairings
dt <- dt %>% 
  mutate(cases = ifelse(cause_id == '477' & age_group_id == '19',sum_cases/4,cases))
dt <- dt %>% 
  mutate(cases = ifelse(cause_id == '477' & age_group_id == '20',sum_cases/4,cases))
dt <- dt %>% 
  mutate(cases = ifelse(cause_id == '477' & age_group_id == '30',sum_cases/4,cases))
dt <- dt %>% 
  mutate(cases = ifelse(cause_id == '477' & age_group_id == '160',sum_cases/4,cases))

# replacing case_rates column values with new values in sum_case_rate column for specific cause & age-group pairings
dt <- dt %>% 
  mutate(case_rate = ifelse(cause_id == '477' & age_group_id == '19',sum_case_rate/4,case_rate))
dt <- dt %>% 
  mutate(case_rate = ifelse(cause_id == '477' & age_group_id == '20',sum_case_rate/4,case_rate))
dt <- dt %>% 
  mutate(case_rate = ifelse(cause_id == '477' & age_group_id == '30',sum_case_rate/4,case_rate))
dt <- dt %>% 
  mutate(case_rate = ifelse(cause_id == '477' & age_group_id == '160',sum_case_rate/4,case_rate))


#also deleting all the new sum_ columns after entering the relevant data into specific columns
dt = select(dt, -c('sum_deaths', 'sum_cases', 'sum_case_rate'))


## For cause 485

## For death
sum_deaths <- dt[, c("cause_id", "draw", "age_group_id", "year_id", "cs_deaths")]

sum_deaths_485 <- sum_deaths %>%
  filter(cause_id ==485) %>% # apply filter condition
  filter(age_group_id %in% c("0", "5", "6")) %>%
  group_by(draw, year_id) %>%
  summarize(sum_deaths = sum(cs_deaths))

dt <- merge(dt, sum_deaths_485, by=c("draw", "year_id"), all=TRUE)

## For cases
sum_cases <- dt[, c("cause_id", "draw", "age_group_id", "year_id", "cases")]

sum_cases_485 <- sum_cases %>%
  filter(cause_id ==485) %>% # apply filter condition
  filter(age_group_id %in% c("0", "5", "6")) %>%
  group_by(draw, year_id) %>%
  summarize(sum_cases = sum(cases))

dt <- merge(dt, sum_cases_485, by=c("draw", "year_id"), all=TRUE)

## For case_rate
sum_case_rate <- dt[, c("cause_id", "draw", "age_group_id", "year_id", "case_rate")]

sum_case_rate_485 <- sum_case_rate %>%
  filter(cause_id ==485) %>% # apply filter condition
  filter(age_group_id %in% c("0", "5", "6")) %>%
  group_by(draw, year_id) %>%
  summarize(sum_case_rate = sum(case_rate))

dt <- merge(dt, sum_case_rate_485, by=c("draw", "year_id"), all=TRUE)


# replacing cs_deaths column values with new values in sum_deaths column for specific cause & age-group pairings
dt <- dt %>% 
  mutate(cs_deaths = ifelse(cause_id == '485' & age_group_id == '0',sum_deaths/3,cs_deaths))
dt <- dt %>% 
  mutate(cs_deaths = ifelse(cause_id == '485' & age_group_id == '5',sum_deaths/3,cs_deaths))
dt <- dt %>% 
  mutate(cs_deaths = ifelse(cause_id == '485' & age_group_id == '6',sum_deaths/3,cs_deaths))

# replacing cases column values with new values in sum_cases column for specific cause & age-group pairings
dt <- dt %>% 
  mutate(cases = ifelse(cause_id == '485' & age_group_id == '0',sum_cases/3,cases))
dt <- dt %>% 
  mutate(cases = ifelse(cause_id == '485' & age_group_id == '5',sum_cases/3,cases))
dt <- dt %>% 
  mutate(cases = ifelse(cause_id == '485' & age_group_id == '6',sum_cases/3,cases))

# replacing case_rates column values with new values in sum_case_rate column for specific cause & age-group pairings
dt <- dt %>% 
  mutate(case_rate = ifelse(cause_id == '485' & age_group_id == '0',sum_case_rate/3,case_rate))
dt <- dt %>% 
  mutate(case_rate = ifelse(cause_id == '485' & age_group_id == '5',sum_case_rate/3,case_rate))
dt <- dt %>% 
  mutate(case_rate = ifelse(cause_id == '485' & age_group_id == '6',sum_case_rate/3,case_rate))


#also deleting all the new sum_ columns after entering the relevant data into specific columns
dt = select(dt, -c('sum_deaths', 'sum_cases', 'sum_case_rate'))



## For cause 486

## For death
sum_deaths <- dt[, c("cause_id", "draw", "age_group_id", "year_id", "cs_deaths")]

sum_deaths_486 <- sum_deaths %>%
  filter(cause_id ==486) %>% # apply filter condition
  filter(age_group_id %in% c("19","20", "30", "160")) %>%
  group_by(draw, year_id) %>%
  summarize(sum_deaths = sum(cs_deaths))

dt <- merge(dt, sum_deaths_486, by=c("draw", "year_id"), all=TRUE)

## For cases
sum_cases <- dt[, c("cause_id", "draw", "age_group_id", "year_id", "cases")]

sum_cases_486 <- sum_cases %>%
  filter(cause_id ==486) %>% # apply filter condition
  filter(age_group_id %in% c("19","20", "30", "160")) %>%
  group_by(draw, year_id) %>%
  summarize(sum_cases = sum(cases))

dt <- merge(dt, sum_cases_486, by=c("draw", "year_id"), all=TRUE)

## For case_rate
sum_case_rate <- dt[, c("cause_id", "draw", "age_group_id", "year_id", "case_rate")]

sum_case_rate_486 <- sum_case_rate %>%
  filter(cause_id ==486) %>% # apply filter condition
  filter(age_group_id %in% c("19","20", "30", "160")) %>%
  group_by(draw, year_id) %>%
  summarize(sum_case_rate = sum(case_rate))

dt <- merge(dt, sum_case_rate_486, by=c("draw", "year_id"), all=TRUE)


# replacing cs_deaths column values with new values in sum_deaths column for specific cause & age-group pairings
dt <- dt %>% 
  mutate(cs_deaths = ifelse(cause_id == '486' & age_group_id == '19',sum_deaths/4,cs_deaths))
dt <- dt %>% 
  mutate(cs_deaths = ifelse(cause_id == '486' & age_group_id == '20',sum_deaths/4,cs_deaths))
dt <- dt %>% 
  mutate(cs_deaths = ifelse(cause_id == '486' & age_group_id == '30',sum_deaths/4,cs_deaths))
dt <- dt %>% 
  mutate(cs_deaths = ifelse(cause_id == '486' & age_group_id == '160',sum_deaths/4,cs_deaths))

# replacing cases column values with new values in sum_cases column for specific cause & age-group pairings
dt <- dt %>% 
  mutate(cases = ifelse(cause_id == '486' & age_group_id == '19',sum_cases/4,cases))
dt <- dt %>% 
  mutate(cases = ifelse(cause_id == '486' & age_group_id == '20',sum_cases/4,cases))
dt <- dt %>% 
  mutate(cases = ifelse(cause_id == '486' & age_group_id == '30',sum_cases/4,cases))
dt <- dt %>% 
  mutate(cases = ifelse(cause_id == '486' & age_group_id == '160',sum_cases/4,cases))

# replacing case_rates column values with new values in sum_case_rate column for specific cause & age-group pairings
dt <- dt %>% 
  mutate(case_rate = ifelse(cause_id == '486' & age_group_id == '19',sum_case_rate/4,case_rate))
dt <- dt %>% 
  mutate(case_rate = ifelse(cause_id == '486' & age_group_id == '20',sum_case_rate/4,case_rate))
dt <- dt %>% 
  mutate(case_rate = ifelse(cause_id == '486' & age_group_id == '30',sum_case_rate/4,case_rate))
dt <- dt %>% 
  mutate(case_rate = ifelse(cause_id == '486' & age_group_id == '160',sum_case_rate/4,case_rate))


#also deleting all the new sum_ columns after entering the relevant data into specific columns
dt = select(dt, -c('sum_deaths', 'sum_cases', 'sum_case_rate'))



## For cause 532

## For death
sum_deaths <- dt[, c("cause_id", "draw", "age_group_id", "year_id", "cs_deaths")]

sum_deaths_532 <- sum_deaths %>%
  filter(cause_id ==532) %>% # apply filter condition
  filter(age_group_id %in% c("0", "5", "6")) %>%
  group_by(draw, year_id) %>%
  summarize(sum_deaths = sum(cs_deaths))

dt <- merge(dt, sum_deaths_532, by=c("draw", "year_id"), all=TRUE)

## For cases
sum_cases <- dt[, c("cause_id", "draw", "age_group_id", "year_id", "cases")]

sum_cases_532 <- sum_cases %>%
  filter(cause_id ==532) %>% # apply filter condition
  filter(age_group_id %in% c("0", "5", "6")) %>%
  group_by(draw, year_id) %>%
  summarize(sum_cases = sum(cases))

dt <- merge(dt, sum_cases_532, by=c("draw", "year_id"), all=TRUE)

## For case_rate
sum_case_rate <- dt[, c("cause_id", "draw", "age_group_id", "year_id", "case_rate")]

sum_case_rate_532 <- sum_case_rate %>%
  filter(cause_id ==532) %>% # apply filter condition
  filter(age_group_id %in% c("0", "5", "6")) %>%
  group_by(draw, year_id) %>%
  summarize(sum_case_rate = sum(case_rate))

dt <- merge(dt, sum_case_rate_532, by=c("draw", "year_id"), all=TRUE)


# replacing cs_deaths column values with new values in sum_deaths column for specific cause & age-group pairings
dt <- dt %>% 
  mutate(cs_deaths = ifelse(cause_id == '532' & age_group_id == '0',sum_deaths/3,cs_deaths))
dt <- dt %>% 
  mutate(cs_deaths = ifelse(cause_id == '532' & age_group_id == '5',sum_deaths/3,cs_deaths))
dt <- dt %>% 
  mutate(cs_deaths = ifelse(cause_id == '532' & age_group_id == '6',sum_deaths/3,cs_deaths))

# replacing cases column values with new values in sum_cases column for specific cause & age-group pairings
dt <- dt %>% 
  mutate(cases = ifelse(cause_id == '532' & age_group_id == '0',sum_cases/3,cases))
dt <- dt %>% 
  mutate(cases = ifelse(cause_id == '532' & age_group_id == '5',sum_cases/3,cases))
dt <- dt %>% 
  mutate(cases = ifelse(cause_id == '532' & age_group_id == '6',sum_cases/3,cases))

# replacing case_rates column values with new values in sum_case_rate column for specific cause & age-group pairings
dt <- dt %>% 
  mutate(case_rate = ifelse(cause_id == '532' & age_group_id == '0',sum_case_rate/3,case_rate))
dt <- dt %>% 
  mutate(case_rate = ifelse(cause_id == '532' & age_group_id == '5',sum_case_rate/3,case_rate))
dt <- dt %>% 
  mutate(case_rate = ifelse(cause_id == '532' & age_group_id == '6',sum_case_rate/3,case_rate))


#also deleting all the new sum_ columns after entering the relevant data into specific columns
dt = select(dt, -c('sum_deaths', 'sum_cases', 'sum_case_rate'))



## For cause dex_5570/gbd_cause_name: dex_agg other neurological disorders (has problems in both young and old age groups)

#for age group 0-7
## For death
sum_deaths <- dt[, c("gbd_cause_name", "draw", "age_group_id", "year_id", "cs_deaths")]

sum_deaths_dex_5570 <- sum_deaths %>%
  filter(gbd_cause_name == "dex_agg other neurological disorders") %>% # apply filter condition
  filter(age_group_id %in% c("0", "5", "6", "7")) %>%
  group_by(draw, year_id) %>%
  summarize(sum_deaths = sum(cs_deaths))

dt <- merge(dt, sum_deaths_dex_5570, by=c("draw", "year_id"), all=TRUE)

## For cases
sum_cases <- dt[, c("gbd_cause_name", "draw", "age_group_id", "year_id", "cases")]

sum_cases_dex_5570 <- sum_cases %>%
  filter(gbd_cause_name == "dex_agg other neurological disorders") %>% # apply filter condition
  filter(age_group_id %in% c("0", "5", "6", "7")) %>%
  group_by(draw, year_id) %>%
  summarize(sum_cases = sum(cases))

dt <- merge(dt, sum_cases_dex_5570, by=c("draw", "year_id"), all=TRUE)

## For case_rate
sum_case_rate <- dt[, c("gbd_cause_name", "draw", "age_group_id", "year_id", "case_rate")]

sum_case_rate_dex_5570 <- sum_case_rate %>%
  filter(gbd_cause_name == "dex_agg other neurological disorders") %>% # apply filter condition
  filter(age_group_id %in% c("0", "5", "6", "7")) %>%
  group_by(draw, year_id) %>%
  summarize(sum_case_rate = sum(case_rate))

dt <- merge(dt, sum_case_rate_dex_5570, by=c("draw", "year_id"), all=TRUE)


# replacing cs_deaths column values with new values in sum_deaths column for specific cause & age-group pairings
dt <- dt %>% 
  mutate(cs_deaths = ifelse(gbd_cause_name == "dex_agg other neurological disorders" & age_group_id == '0',sum_deaths/4,cs_deaths))
dt <- dt %>% 
  mutate(cs_deaths = ifelse(gbd_cause_name == "dex_agg other neurological disorders" & age_group_id == '5',sum_deaths/4,cs_deaths))
dt <- dt %>% 
  mutate(cs_deaths = ifelse(gbd_cause_name == "dex_agg other neurological disorders" & age_group_id == '6',sum_deaths/4,cs_deaths))
dt <- dt %>% 
  mutate(cs_deaths = ifelse(gbd_cause_name == "dex_agg other neurological disorders" & age_group_id == '7',sum_deaths/4,cs_deaths))

# replacing cases column values with new values in sum_cases column for specific cause & age-group pairings
dt <- dt %>% 
  mutate(cases = ifelse(gbd_cause_name == "dex_agg other neurological disorders" & age_group_id == '0',sum_cases/4,cases))
dt <- dt %>% 
  mutate(cases = ifelse(gbd_cause_name == "dex_agg other neurological disorders" & age_group_id == '5',sum_cases/4,cases))
dt <- dt %>% 
  mutate(cases = ifelse(gbd_cause_name == "dex_agg other neurological disorders" & age_group_id == '6',sum_cases/4,cases))
dt <- dt %>% 
  mutate(cases = ifelse(gbd_cause_name == "dex_agg other neurological disorders" & age_group_id == '7',sum_cases/4,cases))

# replacing case_rates column values with new values in sum_case_rate column for specific cause & age-group pairings
dt <- dt %>% 
  mutate(case_rate = ifelse(gbd_cause_name == "dex_agg other neurological disorders" & age_group_id == '0',sum_case_rate/4,case_rate))
dt <- dt %>% 
  mutate(case_rate = ifelse(gbd_cause_name == "dex_agg other neurological disorders" & age_group_id == '5',sum_case_rate/4,case_rate))
dt <- dt %>% 
  mutate(case_rate = ifelse(gbd_cause_name == "dex_agg other neurological disorders" & age_group_id == '6',sum_case_rate/4,case_rate))
dt <- dt %>% 
  mutate(case_rate = ifelse(gbd_cause_name == "dex_agg other neurological disorders" & age_group_id == '7',sum_case_rate/4,case_rate))


#also deleting all the new sum_ columns after entering the relevant data into specific columns
dt = select(dt, -c('sum_deaths', 'sum_cases', 'sum_case_rate'))


#for age group 18-160
## For death
sum_deaths <- dt[, c("gbd_cause_name", "draw", "age_group_id", "year_id", "cs_deaths")]

sum_deaths_dex_5570 <- sum_deaths %>%
  filter(gbd_cause_name == "dex_agg other neurological disorders") %>% # apply filter condition
  filter(age_group_id %in% c("18", "19", "20", "30", "160")) %>%
  group_by(draw, year_id) %>%
  summarize(sum_deaths = sum(cs_deaths))

dt <- merge(dt, sum_deaths_dex_5570, by=c("draw", "year_id"), all=TRUE)

## For cases
sum_cases <- dt[, c("gbd_cause_name", "draw", "age_group_id", "year_id", "cases")]

sum_cases_dex_5570 <- sum_cases %>%
  filter(gbd_cause_name == "dex_agg other neurological disorders") %>% # apply filter condition
  filter(age_group_id %in% c("18", "19", "20", "30", "160")) %>%
  group_by(draw, year_id) %>%
  summarize(sum_cases = sum(cases))

dt <- merge(dt, sum_cases_dex_5570, by=c("draw", "year_id"), all=TRUE)

## For case_rate
sum_case_rate <- dt[, c("gbd_cause_name", "draw", "age_group_id", "year_id", "case_rate")]

sum_case_rate_dex_5570 <- sum_case_rate %>%
  filter(gbd_cause_name == "dex_agg other neurological disorders") %>% # apply filter condition
  filter(age_group_id %in% c("18", "19", "20", "30", "160")) %>%
  group_by(draw, year_id) %>%
  summarize(sum_case_rate = sum(case_rate))

dt <- merge(dt, sum_case_rate_dex_5570, by=c("draw", "year_id"), all=TRUE)


# replacing cs_deaths column values with new values in sum_deaths column for specific cause & age-group pairings
dt <- dt %>% 
  mutate(cs_deaths = ifelse(gbd_cause_name == "dex_agg other neurological disorders" & age_group_id == '18',sum_deaths/5,cs_deaths))
dt <- dt %>% 
  mutate(cs_deaths = ifelse(gbd_cause_name == "dex_agg other neurological disorders" & age_group_id == '19',sum_deaths/5,cs_deaths))
dt <- dt %>% 
  mutate(cs_deaths = ifelse(gbd_cause_name == "dex_agg other neurological disorders" & age_group_id == '20',sum_deaths/5,cs_deaths))
dt <- dt %>% 
  mutate(cs_deaths = ifelse(gbd_cause_name == "dex_agg other neurological disorders" & age_group_id == '30',sum_deaths/5,cs_deaths))
dt <- dt %>% 
  mutate(cs_deaths = ifelse(gbd_cause_name == "dex_agg other neurological disorders" & age_group_id == '160',sum_deaths/5,cs_deaths))

# replacing cases column values with new values in sum_cases column for specific cause & age-group pairings
dt <- dt %>% 
  mutate(cases = ifelse(gbd_cause_name == "dex_agg other neurological disorders" & age_group_id == '18',sum_cases/5,cases))
dt <- dt %>% 
  mutate(cases = ifelse(gbd_cause_name == "dex_agg other neurological disorders" & age_group_id == '19',sum_cases/5,cases))
dt <- dt %>% 
  mutate(cases = ifelse(gbd_cause_name == "dex_agg other neurological disorders" & age_group_id == '20',sum_cases/5,cases))
dt <- dt %>% 
  mutate(cases = ifelse(gbd_cause_name == "dex_agg other neurological disorders" & age_group_id == '30',sum_cases/5,cases))
dt <- dt %>% 
  mutate(cases = ifelse(gbd_cause_name == "dex_agg other neurological disorders" & age_group_id == '160',sum_cases/5,cases))

# replacing case_rates column values with new values in sum_case_rate column for specific cause & age-group pairings
dt <- dt %>% 
  mutate(case_rate = ifelse(gbd_cause_name == "dex_agg other neurological disorders" & age_group_id == '18',sum_case_rate/5,case_rate))
dt <- dt %>% 
  mutate(case_rate = ifelse(gbd_cause_name == "dex_agg other neurological disorders" & age_group_id == '19',sum_case_rate/5,case_rate))
dt <- dt %>% 
  mutate(case_rate = ifelse(gbd_cause_name == "dex_agg other neurological disorders" & age_group_id == '20',sum_case_rate/5,case_rate))
dt <- dt %>% 
  mutate(case_rate = ifelse(gbd_cause_name == "dex_agg other neurological disorders" & age_group_id == '30',sum_case_rate/5,case_rate))
dt <- dt %>% 
  mutate(case_rate = ifelse(gbd_cause_name == "dex_agg other neurological disorders" & age_group_id == '160',sum_case_rate/5,case_rate))


#also deleting all the new sum_ columns after entering the relevant data into specific columns
dt = select(dt, -c('sum_deaths', 'sum_cases', 'sum_case_rate'))



## For cause 561

## For death
sum_deaths <- dt[, c("cause_id", "draw", "age_group_id", "year_id", "cs_deaths")]

sum_deaths_561 <- sum_deaths %>%
  filter(cause_id ==561) %>% # apply filter condition
  # filter(year_id ==2016) %>%
  filter(age_group_id %in% c("0", "5", "6", "7")) %>%
  group_by(draw, year_id) %>%
  summarize(sum_deaths = sum(cs_deaths))

dt <- merge(dt, sum_deaths_561, by=c("draw", "year_id"), all=TRUE)

## For cases
sum_cases <- dt[, c("cause_id", "draw", "age_group_id", "year_id", "cases")]

sum_cases_561 <- sum_cases %>%
  filter(cause_id ==561) %>% # apply filter condition
  # filter(year_id ==2016) %>%
  filter(age_group_id %in% c("0", "5", "6", "7")) %>%
  group_by(draw, year_id) %>%
  summarize(sum_cases = sum(cases))

dt <- merge(dt, sum_cases_561, by=c("draw", "year_id"), all=TRUE)

## For case_rate
sum_case_rate <- dt[, c("cause_id", "draw", "age_group_id", "year_id", "case_rate")]

sum_case_rate_561 <- sum_case_rate %>%
  filter(cause_id ==561) %>% # apply filter condition
  # filter(year_id ==2016) %>%
  filter(age_group_id %in% c("0", "5", "6", "7")) %>%
  group_by(draw, year_id) %>%
  summarize(sum_case_rate = sum(case_rate))

dt <- merge(dt, sum_case_rate_561, by=c("draw", "year_id"), all=TRUE)

## For cs_ylds
sum_ylds <- dt[, c("cause_id", "draw", "age_group_id", "year_id", "cs_ylds")]

sum_ylds_561 <- sum_ylds %>%
  filter(cause_id ==561) %>% # apply filter condition
  # filter(year_id ==2016) %>%
  filter(age_group_id %in% c("0", "5", "6", "7")) %>%
  group_by(draw, year_id) %>%
  summarize(sum_ylds = sum(cs_ylds))

dt <- merge(dt, sum_ylds_561, by=c("draw", "year_id"), all=TRUE)

# replacing cs_deaths column values with new values in sum_deaths column for specific cause & age-group pairings
dt <- dt %>% 
  mutate(cs_deaths = ifelse(cause_id == '561' & age_group_id == '0',sum_deaths/4,cs_deaths))
dt <- dt %>% 
  mutate(cs_deaths = ifelse(cause_id == '561' & age_group_id == '5',sum_deaths/4,cs_deaths))
dt <- dt %>% 
  mutate(cs_deaths = ifelse(cause_id == '561' & age_group_id == '6',sum_deaths/4,cs_deaths))
dt <- dt %>% 
  mutate(cs_deaths = ifelse(cause_id == '561' & age_group_id == '7',sum_deaths/4,cs_deaths))

# replacing cases column values with new values in sum_cases column for specific cause & age-group pairings
dt <- dt %>% 
  mutate(cases = ifelse(cause_id == '561' & age_group_id == '0',sum_cases/4,cases))
dt <- dt %>% 
  mutate(cases = ifelse(cause_id == '561' & age_group_id == '5',sum_cases/4,cases))
dt <- dt %>% 
  mutate(cases = ifelse(cause_id == '561' & age_group_id == '6',sum_cases/4,cases))
dt <- dt %>% 
  mutate(cases = ifelse(cause_id == '561' & age_group_id == '7',sum_cases/4,cases))

# replacing case_rates column values with new values in sum_case_rate column for specific cause & age-group pairings
dt <- dt %>% 
  mutate(case_rate = ifelse(cause_id == '561' & age_group_id == '0',sum_case_rate/4,case_rate))
dt <- dt %>% 
  mutate(case_rate = ifelse(cause_id == '561' & age_group_id == '5',sum_case_rate/4,case_rate))
dt <- dt %>% 
  mutate(case_rate = ifelse(cause_id == '561' & age_group_id == '6',sum_case_rate/4,case_rate))
dt <- dt %>% 
  mutate(case_rate = ifelse(cause_id == '561' & age_group_id == '7',sum_case_rate/4,case_rate))

# replacing cs_ylds column values with new values in sum_ylds column for specific cause & age-group pairings
dt <- dt %>% 
  mutate(cs_ylds = ifelse(cause_id == '561' & age_group_id == '0',sum_ylds/4,cs_ylds))
dt <- dt %>% 
  mutate(cs_ylds = ifelse(cause_id == '561' & age_group_id == '5',sum_ylds/4,cs_ylds))
dt <- dt %>% 
  mutate(cs_ylds = ifelse(cause_id == '561' & age_group_id == '6',sum_ylds/4,cs_ylds))
dt <- dt %>% 
  mutate(cs_ylds = ifelse(cause_id == '561' & age_group_id == '7',sum_ylds/4,cs_ylds))

#also deleting all the new sum_ columns after entering the relevant data into specific columns
dt = select(dt, -c('sum_deaths', 'sum_cases', 'sum_case_rate', 'sum_ylds'))


## For cause 718

## For death
sum_deaths <- dt[, c("cause_id", "draw", "age_group_id", "year_id", "cs_deaths")]

sum_deaths_718 <- sum_deaths %>%
  filter(cause_id ==718) %>% # apply filter condition
  filter(age_group_id %in% c("19","20", "30", "160")) %>%
  group_by(draw, year_id) %>%
  summarize(sum_deaths = sum(cs_deaths))

dt <- merge(dt, sum_deaths_718, by=c("draw", "year_id"), all=TRUE)

## For cases
sum_cases <- dt[, c("cause_id", "draw", "age_group_id", "year_id", "cases")]

sum_cases_718 <- sum_cases %>%
  filter(cause_id ==718) %>% # apply filter condition
  filter(age_group_id %in% c("19","20", "30", "160")) %>%
  group_by(draw, year_id) %>%
  summarize(sum_cases = sum(cases))

dt <- merge(dt, sum_cases_718, by=c("draw", "year_id"), all=TRUE)

## For case_rate
sum_case_rate <- dt[, c("cause_id", "draw", "age_group_id", "year_id", "case_rate")]

sum_case_rate_718 <- sum_case_rate %>%
  filter(cause_id ==718) %>% # apply filter condition
  filter(age_group_id %in% c("19","20", "30", "160")) %>%
  group_by(draw, year_id) %>%
  summarize(sum_case_rate = sum(case_rate))

dt <- merge(dt, sum_case_rate_718, by=c("draw", "year_id"), all=TRUE)


# replacing cs_deaths column values with new values in sum_deaths column for specific cause & age-group pairings
dt <- dt %>% 
  mutate(cs_deaths = ifelse(cause_id == '718' & age_group_id == '19',sum_deaths/4,cs_deaths))
dt <- dt %>% 
  mutate(cs_deaths = ifelse(cause_id == '718' & age_group_id == '20',sum_deaths/4,cs_deaths))
dt <- dt %>% 
  mutate(cs_deaths = ifelse(cause_id == '718' & age_group_id == '30',sum_deaths/4,cs_deaths))
dt <- dt %>% 
  mutate(cs_deaths = ifelse(cause_id == '718' & age_group_id == '160',sum_deaths/4,cs_deaths))

# replacing cases column values with new values in sum_cases column for specific cause & age-group pairings
dt <- dt %>% 
  mutate(cases = ifelse(cause_id == '718' & age_group_id == '19',sum_cases/4,cases))
dt <- dt %>% 
  mutate(cases = ifelse(cause_id == '718' & age_group_id == '20',sum_cases/4,cases))
dt <- dt %>% 
  mutate(cases = ifelse(cause_id == '718' & age_group_id == '30',sum_cases/4,cases))
dt <- dt %>% 
  mutate(cases = ifelse(cause_id == '718' & age_group_id == '160',sum_cases/4,cases))

# replacing case_rates column values with new values in sum_case_rate column for specific cause & age-group pairings
dt <- dt %>% 
  mutate(case_rate = ifelse(cause_id == '718' & age_group_id == '19',sum_case_rate/4,case_rate))
dt <- dt %>% 
  mutate(case_rate = ifelse(cause_id == '718' & age_group_id == '20',sum_case_rate/4,case_rate))
dt <- dt %>% 
  mutate(case_rate = ifelse(cause_id == '718' & age_group_id == '30',sum_case_rate/4,case_rate))
dt <- dt %>% 
  mutate(case_rate = ifelse(cause_id == '718' & age_group_id == '160',sum_case_rate/4,case_rate))


#also deleting all the new sum_ columns after entering the relevant data into specific columns
dt = select(dt, -c('sum_deaths', 'sum_cases', 'sum_case_rate'))




#deleting death_lt_case column
dt = select(dt, -c('death_lt_case'))

#deleting causes where cases=0 (causes: 338, 385, 391, 520)
dt<-dt[!(dt$cause_id=="338" | dt$cause_id=="385" | dt$cause_id=="391" | dt$cause_id=="520")]


##Incorporate zero values for multiple causes identified in dropped_dalys_summary_mw for cases, case_rate, and deaths

#cause 340
for (age_grp in 0:19){
  dt <- dt[(cause_id == 340 & age_group_id == age_grp), cs_deaths := 0]
  dt <- dt[(cause_id == 340 & age_group_id == age_grp), cases := 0]
  dt <- dt[(cause_id == 340 & age_group_id == age_grp), case_rate := 0]
 
}

dt <- dt[(cause_id == 340 & age_group_id == 30), cs_deaths := 0]
dt <- dt[(cause_id == 340 & age_group_id == 30), cases := 0]
dt <- dt[(cause_id == 340 & age_group_id == 30), case_rate := 0]

#cause 367
dt <- dt[(cause_id == 367 & age_group_id == 15), cs_deaths := 0]
dt <- dt[(cause_id == 367 & age_group_id == 15), cases := 0]
dt <- dt[(cause_id == 367 & age_group_id == 15), case_rate := 0]

#cause 384
dt <- dt[(cause_id == 384 & age_group_id == 5), cs_deaths := 0]
dt <- dt[(cause_id == 384 & age_group_id == 5), cases := 0]
dt <- dt[(cause_id == 384 & age_group_id == 5), case_rate := 0]

#cause 393
for (age_grp in 0:6){
  dt <- dt[(cause_id == 393 & age_group_id == age_grp), cs_deaths := 0]
  dt <- dt[(cause_id == 393 & age_group_id == age_grp), cases := 0]
  dt <- dt[(cause_id == 393 & age_group_id == age_grp), case_rate := 0]
}

#cause 494
dt <- dt[(cause_id == 494 & age_group_id == 0), cs_deaths := 0]
dt <- dt[(cause_id == 494 & age_group_id == 0), cases := 0]
dt <- dt[(cause_id == 494 & age_group_id == 0), case_rate := 0]

#cause 499
for (age_grp in 0:8){
  dt <- dt[(cause_id == 499 & age_group_id == age_grp), cs_deaths := 0]
  dt <- dt[(cause_id == 499 & age_group_id == age_grp), cases := 0]
  dt <- dt[(cause_id == 499 & age_group_id == age_grp), case_rate := 0]
  
}

#cause 527
dt <- dt[(cause_id == 527 & age_group_id == 5), cs_deaths := 0]
dt <- dt[(cause_id == 527 & age_group_id == 5), cases := 0]
dt <- dt[(cause_id == 527 & age_group_id == 5), case_rate := 0]


#cause 587
dt <- dt[(cause_id == 587 & age_group_id == 0), cs_deaths := 0]
dt <- dt[(cause_id == 587 & age_group_id == 0), cases := 0]
dt <- dt[(cause_id == 587 & age_group_id == 0), case_rate := 0]


#cause 639
dt <- dt[(cause_id == 639 & age_group_id == 6), cs_deaths := 0]
dt <- dt[(cause_id == 639 & age_group_id == 6), cases := 0]
dt <- dt[(cause_id == 639 & age_group_id == 6), case_rate := 0]


#cause dex_5070
dt <- dt[(gbd_cause_name == "dex_agg other cardiovascular and circulatory diseases" & age_group_id == 0), cs_deaths := 0]
dt <- dt[(gbd_cause_name == "dex_agg other cardiovascular and circulatory diseases" & age_group_id == 0), cases := 0]
dt <- dt[(gbd_cause_name == "dex_agg other cardiovascular and circulatory diseases" & age_group_id == 0), case_rate := 0]


#manual changing of causes where cases=population (cause_id: dex_318, dex_5410)
#changing cases=population as opossed to cases being twice of population as now.Also changing case_rate corresponding to it.

dt <- dt[(gbd_cause_name == "dex_agg intestinal infectious diseases"), cases := population]
dt <- dt[(gbd_cause_name == "dex_agg intestinal infectious diseases"), case_rate := cases / population]

dt <- dt[(gbd_cause_name == "dex_agg other digestive diseases"), cases := population]
dt <- dt[(gbd_cause_name == "dex_agg other digestive diseases"), case_rate := cases / population]

#For some causes (375, 376, and 501) where cases are NA but cs_death is not NA, we take cases=cs_deaths
dt <- dt[(cause_id == 375), cases := cs_deaths]
dt <- dt[(cause_id == 376), cases := cs_deaths]
dt <- dt[(cause_id == 501), cases := cs_deaths]

#changing case_rate corresponding to these causes(375, 376, and 501)
dt[(cause_id == 375), case_rate := cases / population]
dt[(cause_id == 376), case_rate := cases / population]
dt[(cause_id == 501), case_rate := cases / population]

#changing case_definition for 376
dt[(cause_id == 376), case_definition := "deaths"]

## manually summarizing ac_deaths
#delete the ac_deaths column
dt <- dt %>% select(-c(ac_deaths))

# Calculate ac_deaths
all_cause_deaths_dt_1 <- dt[, c("cause_id", "draw","age_group_id","year_id", "cs_deaths")]

#dt <- dt %>% select(-c(cs_deaths))

all_cause_deaths_dt_1 <- all_cause_deaths_dt_1 %>%
  group_by(draw, age_group_id, year_id) %>%
  summarise(ac_deaths = sum(cs_deaths), cause_id = cause_id)
all_cause_deaths_dt_1 <- setDT(all_cause_deaths_dt_1)
all_cause_deaths_dt_1[, cause_id := NULL]
all_cause_deaths_dt_1 <- unique(all_cause_deaths_dt_1)
dt <- merge(dt, all_cause_deaths_dt_1, by=c("draw", "age_group_id", "year_id"), all=TRUE)


## manually summarizing ac_spending
#delete the ac_spending column
dt <- dt %>% select(-c(ac_spending))

# Calculate ac_spending
all_cause_expenditure_dt_1 <- dt[, c("cause_id", "draw","age_group_id","year_id", "cs_spending")]

#dt <- dt %>% select(-c(cs_deaths))

all_cause_expenditure_dt_1 <- all_cause_expenditure_dt_1 %>%
  group_by(draw, age_group_id, year_id) %>%
  summarise(ac_spending = sum(cs_spending), cause_id = cause_id)
all_cause_expenditure_dt_1 <- setDT(all_cause_expenditure_dt_1)
all_cause_expenditure_dt_1[, cause_id := NULL]
all_cause_expenditure_dt_1 <- unique(all_cause_expenditure_dt_1)
dt <- merge(dt, all_cause_expenditure_dt_1, by=c("draw", "age_group_id", "year_id"), all=TRUE)


fwrite(dt, "./spending_decomp_input_data_dex_gdp_aggegate_death_agg.csv")

dt_draw_0 <- dt[draw == 0, ]
fwrite(dt_draw_0, "./draw_0_spending_decomp_input_data_dex_gdp_aggegate_death_agg.csv")
