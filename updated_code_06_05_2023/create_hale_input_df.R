library(data.table)
library(dplyr)
source("/ihme/cc_resources/libraries/current/r/get_population.R")
source("/ihme/cc_resources/libraries/current/r/get_draws.R")
source("/ihme/cc_resources/libraries/current/r/get_age_metadata.R")
source("/ihme/cc_resources/libraries/current/r/get_cause_metadata.R")
setwd("/home/j/Project/Cost_Effectiveness/BEA/BEA_data_2023/data_new_naming_convention/HALE")
# Read in the list of DEX causes, the map from GBD causes to aggregate DEX causes, and the definitions of "case" for each cause
cause_map <- fread("./gbd_to_dex_cause_map.csv")
setnames(cause_map, "gbd_cause_id", "cause_id")
case_defns <- fread("./decomp_case_definitions.csv")
# Pull the age metadata and drop unneeded rows
age_metadata <- get_age_metadata(age_group_set_id = 12, gbd_round_id=6)
age_metadata[, c("age_group_weight_value", "most_detailed") := NULL]
# Adjust age group column to match with the spending calculation
age_metadata <- age_metadata[-c(1:3, 21:23), ]
# Define all_ages_set_12 variable to use in shared functions 
all_ages_set_12 <- age_metadata[, age_group_id]
# Add age group 28 and 160 in order to match DEX age group splitting
age_metadata <- age_metadata %>% add_row(age_group_id = 28, age_group_name = "0 to 11 months", age_group_years_start = 0, age_group_years_end = 1, .before = 1)
age_metadata <- age_metadata %>% add_row(age_group_id = 160, age_group_name = "85 plus", age_group_years_start = 85, age_group_years_end = 125)

cause_metadata <- get_cause_metadata(cause_set_id=2, gbd_round_id=6)

# Define the years and cause_ids that we would like to decompose HALE for.
years <- c(1996, 2016)
all_ages <- age_metadata[, age_group_id]
gbd_cids <- cause_map[, unique(cause_id)]

# Create a data.table of all combinations of GBD cause ID, age-group, sex, location, year, and draw, so that
# we don't end up with NAs for population or all-cause mortality for causes where there are no cs_deaths estimated.
dt <- data.table(expand.grid(draw=0:999, cause_id=gbd_cids, age_group_id=all_ages, sex_id=3, location_id=102, year_id=years))

# Pull death counts for all the GBD causes, plus the all-cause-aggregate (cause_id 294)
cs_deaths <- get_draws("cause_id", gbd_id=c(gbd_cids, 294), gbd_round_id=6, source="codcorrect",
                       measure_id=1, metric_id=1, age_group_id=all_ages_set_12, sex_id= 3,
                       location_id=102, year_id=years, decomp_step="step5")
deaths_age_groups_28_160 <- get_draws("cause_id", gbd_id=c(gbd_cids, 294), gbd_round_id=6, source="codcorrect",
                                      measure_id=1, metric_id=1, age_group_id=c(28, 160), sex_id= 3,
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

# Calculate ac_deaths
all_cause_deaths_dt <- cs_deaths[, c("cause_id", "draw","age_group_id","year_id", "cs_deaths")]
all_cause_deaths_dt <- all_cause_deaths_dt %>%
  group_by(draw, age_group_id, year_id) %>%
  summarise(ac_deaths = sum(cs_deaths), cause_id = cause_id)
all_cause_deaths_dt <- setDT(all_cause_deaths_dt)
all_cause_deaths_dt[, cause_id := NULL]
all_cause_deaths_dt <- unique(all_cause_deaths_dt)
dt <- merge(dt, all_cause_deaths_dt, by=c("draw", "age_group_id", "year_id"), all=TRUE)

dt <- merge(dt, cs_deaths, by=c("draw", "cause_id", "age_group_id", "sex_id", "location_id", "year_id"), all=TRUE)

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

dt <- merge(dt, cases, by=c("draw", "cause_id", "age_group_id", "sex_id", "location_id", "year_id"), all=TRUE)

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

# Pull cs_ylds and merge with the rest
cs_ylds <- get_draws("cause_id", gbd_id=c(gbd_cids, 294), gbd_round_id=6, source="como",
                     measure_id=3, metric_id=3, age_group_id=all_ages_set_12, sex_id=3,
                     location_id=102, year_id=years, decomp_step="step5")

ylds_age_groups_28_160 <- get_draws("cause_id", gbd_id=c(gbd_cids, 294), gbd_round_id=6, source="como",
                                    measure_id=3, metric_id=3, age_group_id=c(28, 160), sex_id=3,
                                    location_id=102, year_id=years, decomp_step="step5")

cs_ylds <- rbind(cs_ylds, ylds_age_groups_28_160)

cs_ylds <- melt(cs_ylds,
                id.vars=c("cause_id", "age_group_id", "sex_id", "location_id", "year_id"),
                measure.vars=paste0("draw_", 0:999),
                value.name="cs_ylds", variable.name="draw")
cs_ylds[, draw := as.integer(gsub("draw_", "", draw))]

ac_ylds <- cs_ylds[cause_id == 294]
ac_ylds[, cause_id := NULL]
setnames(ac_ylds, "cs_ylds", "all_cause_ylds_per_cap")
cs_ylds <- cs_ylds[cause_id != 294]
# Calculating ac_ylds
all_cause_ylds_dt <- cs_ylds[, c("draw","age_group_id","year_id", "cs_ylds", "cause_id")]
all_cause_ylds_dt <- all_cause_ylds_dt %>%
  group_by(draw, age_group_id, year_id) %>%
  summarise(ac_ylds = sum(cs_ylds), cause_id = cause_id)
all_cause_ylds_dt <- setDT(all_cause_ylds_dt)
all_cause_ylds_dt[, cause_id := NULL]
all_cause_ylds_dt <- unique(all_cause_ylds_dt)

dt <- merge(dt, all_cause_ylds_dt, by=c("draw", "age_group_id", "year_id"), all=TRUE)

dt <- merge(dt, cs_ylds, by=c("draw", "cause_id", "sex_id", "age_group_id", "location_id", "year_id"), all=TRUE)
# dt <- merge(dt, ac_ylds, by=c("draw", "sex_id", "age_group_id", "location_id", "year_id"), all=TRUE)
# cs_ylds are queryable only as a rate. Convert to count scale
dt[, cs_ylds := cs_ylds * population]
dt[, ac_ylds := ac_ylds * population]
# dt[, ac_ylds := all_cause_ylds_per_cap * population]
# dt[, all_cause_ylds_per_cap := NULL]

# Merge on the all-cause_deaths
setnames(ac_deaths, "cs_deaths", "ac_deaths")
ac_deaths[, cause_id := NULL]
# dt <- merge(dt, ac_deaths, by=c("draw", "age_group_id", "sex_id", "location_id", "year_id"), all=TRUE)
# Merge on age metadata, cause_names, and definitions of "case"
dt <- merge(dt, age_metadata, by="age_group_id", all=TRUE)
dt <- merge(dt, cause_map[, .(cause_id, dex_cause_id, dex_cause_name=cause_name)], by="cause_id", all.x=TRUE)
dt <- merge(dt, cause_metadata[, .(cause_id, gbd_cause_name=cause_name)], by="cause_id", all.x=TRUE)
dt <- merge(dt, case_defns[, .(dex_cause_id=cause_id, case_definition=measure)], by="dex_cause_id", all.x=TRUE)

# May need to do something about the non-correspondence of age/sex groups where GBD has estimates of cs_deaths, cs_ylds, and cases.
# If cs_ylds aren't estimated but cs_deaths are, then it could either be an exclusively fatal cause (e.g. "Indirect maternal cs_deaths" or "Aortic aneurism")

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
# Drop rows where cause_is is NA.
dt <- dt[!is.na(cause_id), ]
dt[age_group_id == 28, age_group_id := 0]

#### Investigation ####
dt_cases_are_na <- dt[is.na(cases), ]
fwrite(dt_cases_are_na, "/snfs1/Project/Cost_Effectiveness/BEA/BEA_data_2023/data_new_naming_convention/investigation_files/hale_input_dt_cases_are_na.csv")
dt_cause_id_456 <- dt[cause_id == 456, ]
fwrite(dt_cause_id_456, "/snfs1/Project/Cost_Effectiveness/BEA/BEA_data_2023/data_new_naming_convention/investigation_files/hale_input_dt_cause_id_456.csv")

setcolorder(dt,
            c("cause_id", "gbd_cause_name", "dex_cause_id", "dex_cause_name", "case_definition",
              "draw",
              "age_group_id", "age_group_name", "age_group_years_start", "age_group_years_end",
              "sex_id", "location_id", "year_id",
              "population", "cases", "case_rate",
              "cs_deaths", "ac_deaths",
              "cs_ylds", "ac_ylds"))

# creating a data frame containing draw 0 only.
dt_by_draw <- dt[draw == 0, ]
fwrite(dt_by_draw, "./hale_decomp_input_data_draw0.csv")

fwrite(dt, "./hale_decomp_input_data.csv")

