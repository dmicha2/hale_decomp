library(data.table)
source("gbdenv_file_path/get_population.R")
source("gbdenv_file_path/get_draws.R")
source("gbdenv_file_path/get_age_metadata.R")
source("gbdenv_file_path/get_cause_metadata.R")
setwd("data_file_path")
# Read in the list of DEX causes, the map from GBD causes to aggregate DEX causes, and the definitions of "case" for each cause
cause_map <- fread("./gbd_to_dex_cause_map.csv")
setnames(cause_map, "gbd_cause_id", "cause_id")
case_defns <- fread("./decomp_case_definitions.csv")
# Pull the age metadata and drop unneeded rows
age_metadata <- get_age_metadata(age_group_set_id = 12, gbd_round_id=6)
age_metadata[, c("age_group_weight_value", "most_detailed") := NULL]
cause_metadata <- get_cause_metadata(cause_set_id=2, gbd_round_id=6)

# Define the years and cause_ids that we would like to decompose HALE for.
years <- c(1996, 2016)
all_ages <- age_metadata[, age_group_id]
gbd_cids <- cause_map[, unique(cause_id)]

# Create a data.table of all combinations of GBD cause ID, age-group, sex, location, year, and draw, so that
# we don't end up with NAs for population or all-cause mortality for causes where there are no deaths estimated.
dt <- data.table(expand.grid(draw=0:999, cause_id=gbd_cids, age_group_id=all_ages, sex_id=3, location_id=102, year_id=years))

# Pull death counts for all the GBD causes, plus the all-cause-aggregate (cause_id 294)
deaths <- get_draws("cause_id", gbd_id=c(gbd_cids, 294), gbd_round_id=6, source="codcorrect",
                  measure_id=1, metric_id=1, age_group_id=all_ages, sex_id=3,
                  location_id=102, year_id=years, decomp_step="step5")
# Reshape so that different draws are different rows.
deaths <- melt(deaths,
           id.vars=c("cause_id", "age_group_id", "sex_id", "location_id", "year_id"),
           measure.vars=paste0("draw_", 0:999),
           value.name="deaths", variable.name="draw")
deaths[, draw := as.integer(gsub("draw_", "", draw))]

# Split the all-cause-aggregate from the rest of the causes. Later we will merge the all-cause
# aggregate on so that each row has a separate column for cause-specific deaths and all-cause deaths.
all_cause_deaths <- deaths[cause_id == 294]
deaths <- deaths[cause_id != 294]

dt <- merge(dt, deaths, by=c("draw", "cause_id", "age_group_id", "sex_id", "location_id", "year_id"), all=TRUE)

# Pull prevalence for causes where "cases" is defined as prevalence, and incidence where "cases"
# is defined as incidence. Then combine "cases" and merge with deaths.
prev_cids <- case_defns[measure_id == 5, cause_id]
prev_cids <- cause_map[dex_cause_id %in% c(prev_cids, 10*prev_cids), cause_id]
inc_cids <- case_defns[measure_id == 6, cause_id]
inc_cids <- cause_map[dex_cause_id %in% c(inc_cids, 10*inc_cids), cause_id]
pops_cids <- case_defns[measure == "population", cause_id]
pops_cids <- cause_map[dex_cause_id %in% c(pops_cids, 10*pops_cids), cause_id]
deaths_cids <- case_defns[measure == "deaths", cause_id]
deaths_cids <- cause_map[dex_cause_id %in% c(deaths_cids, 10*deaths_cids), cause_id]

prevs <- get_draws("cause_id", gbd_id=prev_cids, gbd_round_id=6, source="como",
                   measure_id=5, metric_id=3, age_group_id=all_ages, sex_id=3,
                   location_id=102, year_id=years, decomp_step="step5")
prevs <- melt(prevs,
              id.vars=c("cause_id", "age_group_id", "sex_id", "location_id", "year_id"),
              measure.vars=paste0("draw_", 0:999),
              value.name="case_rate", variable.name="draw")
incs <- get_draws("cause_id", gbd_id=inc_cids, gbd_round_id=6, source="como",
                  measure_id=6, metric_id=3, age_group_id=all_ages, sex_id=3,
                  location_id=102, year_id=years, decomp_step="step5")
incs <- melt(incs,
             id.vars=c("cause_id", "age_group_id", "sex_id", "location_id", "year_id"),
             measure.vars=paste0("draw_", 0:999),
             value.name="case_rate", variable.name="draw")
cases <- rbind(prevs, incs)
cases[, draw := as.integer(gsub("draw_", "", draw))]

dt <- merge(dt, cases, by=c("draw", "cause_id", "age_group_id", "sex_id", "location_id", "year_id"), all=TRUE)

# Get population and merge with deaths & cases
pops <- get_population(age_group_id=all_ages, sex_id=3, location_id=102, year_id=years, gbd_round_id=6, decomp_step="step5")
pops[, run_id := NULL]
dt <- merge(dt, pops, by=c("age_group_id", "sex_id", "location_id", "year_id"), all=TRUE)

dt[cause_id %in% deaths_cids, cases := deaths]
dt[, cases := population * case_rate]
dt[cause_id %in% pops_cids, `:=`(cases=population, case_rate=1)]

# Pull YLDs and merge with the rest
ylds <- get_draws("cause_id", gbd_id=c(gbd_cids, 294), gbd_round_id=6, source="como",
                  measure_id=3, metric_id=3, age_group_id=all_ages, sex_id=3,
                  location_id=102, year_id=years, decomp_step="step5")
ylds <- melt(ylds,
             id.vars=c("cause_id", "age_group_id", "sex_id", "location_id", "year_id"),
             measure.vars=paste0("draw_", 0:999),
             value.name="ylds", variable.name="draw")
ylds[, draw := as.integer(gsub("draw_", "", draw))]
all_cause_ylds <- ylds[cause_id == 294]
all_cause_ylds[, cause_id := NULL]
setnames(all_cause_ylds, "ylds", "all_cause_ylds_per_cap")
ylds <- ylds[cause_id != 294]
dt <- merge(dt, ylds, by=c("draw", "cause_id", "sex_id", "age_group_id", "location_id", "year_id"), all=TRUE)
dt <- merge(dt, all_cause_ylds, by=c("draw", "sex_id", "age_group_id", "location_id", "year_id"), all=TRUE)
# YLDs are queryable only as a rate. Convert to count scale
dt[, ylds := ylds * population]
dt[, all_cause_ylds := all_cause_ylds_per_cap * population]
dt[, all_cause_ylds_per_cap := NULL]

# Merge on the all-cause_deaths
setnames(all_cause_deaths, "deaths", "all_cause_deaths")
all_cause_deaths[, cause_id := NULL]
dt <- merge(dt, all_cause_deaths, by=c("draw", "age_group_id", "sex_id", "location_id", "year_id"), all=TRUE)
# Merge on age metadata, cause_names, and definitions of "case"
dt <- merge(dt, age_metadata, by="age_group_id", all=TRUE)
dt <- merge(dt, cause_map[, .(cause_id, dex_cause_id, dex_cause_name=cause_name)], by="cause_id", all.x=TRUE)
dt <- merge(dt, cause_metadata[, .(cause_id, gbd_cause_name=cause_name)], by="cause_id", all.x=TRUE)
dt <- merge(dt, case_defns[, .(dex_cause_id=cause_id, case_definition=measure)], by="dex_cause_id", all.x=TRUE)

# May need to do something about the non-correspondence of age/sex groups where GBD has estimates of deaths, YLDs, and cases.
# If YLDs aren't estimated but deaths are, then it could either be an exclusively fatal cause (e.g. "Indirect maternal deaths" or "Aortic aneurism")

# If cases are NA, but deaths aren't, then it could be because "cases" are really just deaths ("Indirect maternal deaths", "Maternal deaths aggravated
# by HIV", "Aortic aneurism"), or GBD maybe doesn't have good estimates of "cases" by our definition, as is the case for "Other ..." causes, and DEX
# causes that are aggregates of an "Other ..." GBD cause.
cases_na_deaths_not <- dt[(!is.na(deaths)) & (is.na(cases)) & (draw == 0) & (year_id == 1996) & (deaths != 0),
                          .SD[, .(minimum=min(age_group_years_start), maximum=max(age_group_years_end))],
                          by=c("cause_id", "dex_cause_name")]
# Intestinal infectious diseases - aggregate of GBD intest. inf. dis. plus "Typhoid and paratyphoid"
# Measles
# Indirect maternal deaths - cases = deaths
# Other maternal disorders - includes "Late maternal deaths" and "Maternal deaths aggravated by HIV/AIDS", so cases = deaths (at least for some subcauses)
# Aortic aneurism - cases = deaths

# If deaths are NA, but cases are not, it is just a nonfatal cause, or a fatal cause that isn't fatal for all age-groups where
# incidence or prevalence is defined. e.g. Migraine is nonfatal, and Neonatal sepsis and other neonatal infections is fatal for
# < 5, but for age groups over 5, there is prevalence, but not death (prevalence because of longer-lasting complications).
deaths_na_cases_not <- dt[(!is.na(cases)) & (cases != 0) & (is.na(deaths)) & (draw == 0) & (year_id == 1996),
                          .SD[, .(minimum=min(age_group_years_start), maximum=max(age_group_years_end))],
                          by=c("cause_id", "dex_cause_name")]
# dt[!is.na(deaths),
#    .SD[, .(minimum=min(age_group_years_start), maximum=max(age_group_years_end))],
#    by=c("cause_id", "dex_cause_name")]

# If deaths are not estimated, then we should assume they are 0.
dt[is.na(deaths), deaths := 0]
setcolorder(dt,
            c("cause_id", "gbd_cause_name", "dex_cause_id", "dex_cause_name", "case_definition",
              "draw",
              "age_group_id", "age_group_name", "age_group_years_start", "age_group_years_end",
              "sex_id", "location_id", "year_id",
              "population", "cases", "case_rate",
              "deaths", "all_cause_deaths",
              "ylds", "all_cause_ylds"))

fwrite(dt, "data_file_path/hale_decomp_input_data.csv")
