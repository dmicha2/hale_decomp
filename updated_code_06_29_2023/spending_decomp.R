library(data.table)
setwd("/home/j/Project/Cost_Effectiveness/BEA/BEA_data_2023/data_new_naming_convention/Spending")
# The Utility Funcs
gen_age_length <- function (dt, terminal_age = 110, terminal_length = 25, process_terminal = T) 
{
  if (!"age" %in% colnames(dt)) 
    stop("Column age not found")
  ages <- sort(unique(dt$age))
  if (max(ages) != terminal_age & process_terminal == T) 
    stop(paste0("Max age in input dataset is ", max(ages), 
                " but terminal_age argument is ", terminal_age))
  for (i in 1:(length(ages) - 1)) {
    current_age <- ages[i]
    next_age <- ages[i + 1]
    target_length <- next_age - current_age
    dt[age == current_age, `:=`(age_length, target_length)]
  }
  if (process_terminal == T) {
    dt[age == terminal_age, `:=`(age_length, terminal_length)]
  }
  else {
    dt[age == max(ages), `:=`(age_length, 5)]
  }
}

mx_to_ax <- function (m, t) 
{
  result <- t + 1/m - t/(1 - exp(-t * m))
  return(result)
}

mx_ax_to_qx <- function (m, a, t)
{
  result <- (t * m)/(1 + (t - a) * m)
  return(result)
}


lx_to_dx <- function(dt, id_vars = NULL, terminal_age = 85, assert_na = T) {
  setorder(dt, "age")
  setorderv(dt, setdiff(id_vars, "age"))
  dt[, dx := lx - shift(lx, 1, type = "lead"), by = id_vars]
  dt[age == terminal_age, dx := lx]
  #dt[age == max(age), dx := lx, by = id_vars]
  dt[]
  return(dt)
}

gen_Tx <- function(dt, id_vars) {
  # Set descending order to begin the cumulative sum at the oldest age group
  id_vars <- setdiff(id_vars, "age")
  setorderv(dt, c(id_vars, "age"), order = -1)
  dt[, Tx := cumsum(nLx), by = id_vars]
  dt[]
  return(dt)
}

gen_nLx <- function(dt, id_vars = NULL, terminal_age = 85, assert_na = T) {
  id_vars <- setdiff(id_vars, "age")
  setorderv(dt, c(id_vars, "age"), order = 1)
  dt[, nLx := age_length * shift(lx, 1, type = "lead") + ax * dx, by = id_vars]
  dt[age == terminal_age, nLx := lx/mx]
  #dt[age == max(age), nLx := lx/mx]
  dt[]
  return(dt)
}

gen_ex <- function(dt, assert_na=F) {
  dt[, ex := Tx/lx]
}

get_full_life_table <- function(data_table, by_vars = NULL){
  # Next call the "mx_to_ax" func to get the ax values
  data_table[, ax := mx_to_ax(mx, age_length)]
  
  data_table[, qx := mx_ax_to_qx(mx, ax, age_length)]
  # ?????
  data_table[age == max(age), qx := 1]
  #data_table[age_group_id == 160, qx := 1]
  
  by_vars <- setdiff(by_vars, c("age_group_id", "age"))
  data_table2 <- data_table[, c(by_vars, "age_group_id", "age", "age_length", "mx", "ax", "qx"), with = FALSE]
  
  # px
  data_table2[, px := 1 - qx]
  
  # lx
  setorderv(data_table2, c(by_vars, "age_group_id", "age"))
  
  data_table2[age_group_id != 160, lx := cumprod(px)/ px, by = by_vars]
  data_table2[, term_lx := .SD[age_group_id != 160, prod(px)], by = by_vars]
  data_table2[age_group_id == 160, lx := term_lx]
  data_table2[, term_lx := NULL]
  
  # dx
  lx_to_dx(data_table2, id_vars = by_vars)
  
  # nLx
  gen_nLx(data_table2, id_vars = by_vars)
  # Tx
  gen_Tx(data_table2, id_vars = by_vars)
  # ex
  gen_ex(data_table2)
  
  setorder(data_table2, age)
  data_table2[]
  return(data_table2)
}

# get the age map for GBD 2017 and for type lifetable, this will produce all the age-groups, age_group_ids and the 
# age start years

#updated aggregated gdp dex causes file
data_t = fread("./spending_cause_replaced_mort_rates_dex_gdp_aggregated_death_agg.csv")

setnames(data_t, "age_group_years_start", "age")
# add the age-group-length variable, in place, into our data table
gen_age_length(data_t, terminal_age = max(data_t$age))

id_vars <- c("cause_id", "draw", "sex_id", "year", "age_group_id", "age_length", "age")

# Using Lasanthi's suggestion line 117 was replaced with line 118.
# setnames(data_t, c("expenditure", "ac_spending"), c("cs_expenditure", "expenditure"))
#setnames(data_t, "expenditure", "cs_spending")

dt <- melt(data_t, id.vars = id_vars, measure.vars=c("ac_mr", "cr_ac_spending", "cr_ac_mr", "ac_spending"))
dt[, cause_replaced := as.integer(variable %like% "^cr")]
dt[, variable := ifelse(variable %like% "mr$", "mx", "ac_spending")]
dt <- dcast(dt, as.formula(paste0(paste(c(id_vars, "cause_replaced"), collapse=" + "), " ~ variable")),
            value.var = "value")

dt <- dt[, .(cause_id, draw, sex_id, year, cause_replaced, age_group_id, age, age_length, mx, ac_spending)]
id_vars <- setdiff(c(id_vars, "cause_replaced"), c("age_group_id", "age_length", "age"))

setorderv(dt, c(id_vars, "age"))

# #### TEST ####
# dt_TEST_995 <- dt[cause_id == 995, ]
# setorderv(dt_TEST_995, setdiff(id_vars, "age"))
# lt_TEST_995 <- get_full_life_table(dt_TEST_995, by_vars = id_vars)
# setorderv(lt_TEST_995, setdiff(id_vars, "age"))

lt <- get_full_life_table(dt, by_vars = id_vars)
#### TESTING ####
#DT <- lt[is.na(ex)]

#To display additional output variables
dt[, c("ax", "mx", "qx", "age_length") := NULL]
dt_le_data <- merge(lt, dt, by=c(id_vars, "age_group_id", "age"))

input_data <- fread("./spending_decomp_input_data_dex_gdp_aggegate_death_agg.csv")

dt_le_data <- merge(dt_le_data, unique(input_data[, .(cause_id, gbd_cause_name, dex_cause_id, dex_cause_name)]),
                    by="cause_id")

setcolorder(dt_le_data, c("cause_id", "gbd_cause_name", "dex_cause_id", "dex_cause_name",
                          "draw", "sex_id", "year", "age_group_id",
                          "qx", "px", "lx", "dx", "nLx", "Tx", "ex"))


fwrite(dt_le_data, "./spending_le_additional_variables.csv")

# creating a data frame containing draw 0 only.
dt_le_data_draw_0 <- dt_le_data[draw == 0, ]
fwrite(dt_le_data_draw_0, "./draw_0_spending_le_additional_variables.csv")

#For lt_spending output combined for age_groups
###Variable def: 
##haL: avg exp /person * no. of person years lived between age x&n
##lt_spending: cum sum of everyone older than x /no. of survivors beyond x

lt[, c("ax", "qx", "px", "dx", "Tx", "age_length", "mx") := NULL]
#dt[, c("ax", "mx", "qx") := NULL]
dt <- merge(lt, dt, by=c(id_vars, "age_group_id", "age"))

dt <- merge(dt, data_t[, c(setdiff(id_vars, "cause_replaced"), "age_group_id", "cases", "population"), with=FALSE],
            by=c(setdiff(id_vars, "cause_replaced"), "age_group_id"))
dt[, haL := nLx * (ac_spending / population)]

#Average spending
dt[, avg_spending := (ac_spending / population)]

#dataframe for lt_spending
setorderv(dt, c(id_vars, "age"), order = -1)
dt[, lt_spending := cumsum(haL)/lx, by=id_vars]

setorderv(dt, c(id_vars, "age"), order = 1)

id_vars <- setdiff(id_vars, "cause_replaced")

dt <- dcast(dt,
            as.formula(paste0(paste(c(id_vars, "age_group_id"), collapse=" + "), " ~ cause_replaced")),
            value.var=c("ex", "lt_spending", "ac_spending", "avg_spending"))

setnames(dt, names(dt), gsub("_1$", "_cause_replaced", names(dt)))
setnames(dt, names(dt), gsub("_0$", "", names(dt)))


input_data <- fread("./spending_decomp_input_data_dex_gdp_aggegate_death_agg.csv")
dt <- merge(dt, unique(input_data[, .(cause_id, gbd_cause_name, dex_cause_id, dex_cause_name)]),
            by="cause_id")


#renaming _cause_replaced variables to cr_
dt<- dt %>%
  rename(
    cr_lt_spending = lt_spending_cause_replaced,
    cr_ex = ex_cause_replaced,
    cr_ac_spending = ac_spending_cause_replaced,
    cr_avg_spending = avg_spending_cause_replaced
  )


setcolorder(dt, c("cause_id", "gbd_cause_name", "dex_cause_id", "dex_cause_name",
                  "draw", "sex_id", "year", "age_group_id",
                  "lt_spending", "cr_lt_spending", 
                  "ex", "cr_ex", "ac_spending", "cr_ac_spending", "avg_spending","cr_avg_spending"))

fwrite(dt, "./spending_cause_replaced_life_table_dex_gdp_aggregated_death_agg.csv")

dt_draw_0 <- dt[draw == 0, ]
fwrite(dt_draw_0, "./draw_0_spending_cause_replaced_life_table_dex_gdp_aggregated_death_agg.csv")

#creating dataframe for LE at birth/age=0

dt_0 <- dt[age_group_id == 0]
dt_0[, age_group_id := NULL]
# dt_0[, lt_spending_effect := cr_lt_spending - spending]
# dt_0[, ex_effect := cr_ex - ex]
dt_0[, spending_effect_1996 := cr_lt_spending - lt_spending]
dt_0[, spending_effect_2016 := lt_spending - cr_lt_spending]
dt_0 <- dt_0[(year==1996), lt_spending_effect := spending_effect_1996]
dt_0 <- dt_0[(year==2016), lt_spending_effect := spending_effect_2016]

dt_0[, ex_effect_1996 := cr_ex - ex]
dt_0[, ex_effect_2016 := ex - cr_ex]
dt_0 <- dt_0[(year==1996), ex_effect := ex_effect_1996]
dt_0 <- dt_0[(year==2016), ex_effect := ex_effect_2016]

#Drop the redundant columns
dt_0 <- subset(dt_0, select = -c(spending_effect_1996,spending_effect_2016, ex_effect_1996, ex_effect_2016))

setcolorder(dt, c("cause_id", "gbd_cause_name", "dex_cause_id", "dex_cause_name",
                  "draw", "sex_id", "year", 
                  "lt_spending", "cr_lt_spending",
                  "ex", "cr_ex", "ac_spending", "cr_ac_spending"))
fwrite(dt_0, "./spending_decomp_results_dex_gdp_aggregated_death_agg.csv")

# creating a data frame containing draw 0 only.
dt_0_draw_0 <- dt_0[draw == 0, ]
fwrite(dt_0_draw_0, "./draw_0_spending_decomp_results_dex_gdp_aggregated_death_agg.csv")

#Mean and UI estimates for LE at age 0

mean_ui_dt<- dt_0 %>% group_by(cause_id, gbd_cause_name, dex_cause_id, dex_cause_name, sex_id, year) %>% 
  summarise(mean_spending= mean(lt_spending),
            l_spending = quantile(lt_spending, na.rm = TRUE, probs=0.025),
            u_spending = quantile(lt_spending, na.rm = TRUE, probs=0.975),
            mean_cr_lt_spending=mean(cr_lt_spending),
            l_cr_lt_spending = quantile(cr_lt_spending, na.rm = TRUE, probs=0.025),
            u_cr_lt_spending = quantile(cr_lt_spending, na.rm = TRUE, probs=0.975),
            mean_ex=mean(ex),
            l_ex = quantile(ex, na.rm = TRUE, probs=0.025),
            u_ex = quantile(ex, na.rm = TRUE, probs=0.975),
            mean_cr_ex=mean(cr_ex),
            l_cr_ex = quantile(cr_ex, na.rm = TRUE, probs=0.025),
            u_cr_ex = quantile(cr_ex, na.rm = TRUE, probs=0.975),
            mean_ac_spending=mean(ac_spending),
            l_ac_spending = quantile(ac_spending, na.rm = TRUE, probs=0.025),
            u_ac_spending = quantile(ac_spending, na.rm = TRUE, probs=0.975),
            mean_ac_cr_lt_spending=mean(cr_ac_spending),
            l_ac_cr_lt_spending = quantile(cr_ac_spending, na.rm = TRUE, probs=0.025),
            u_ac_cr_lt_spending = quantile(cr_ac_spending, na.rm = TRUE, probs=0.975),
            mean_spending_effect= mean(lt_spending_effect),
            l_spending_effect = quantile(lt_spending_effect, na.rm = TRUE, probs=0.025),
            u_spending_effect = quantile(lt_spending_effect, na.rm = TRUE, probs=0.975),
            mean_ex_effect=mean(ex_effect),
            l_ex_effect = quantile(ex_effect, na.rm = TRUE, probs=0.025),
            u_ex_effect = quantile(ex_effect, na.rm = TRUE, probs=0.975),
            .groups = 'drop')  %>%
  as.data.frame()

fwrite(mean_ui_dt, "./spending_decomp_results_mean_ui_dex_gdp_aggregated_death_agg.csv")

## for LE at 15, we re-run the lt_spending model
dt_15 <- dt[age_group_id == 8]
dt_15[, age_group_id := NULL]
dt_15[, spending_effect_1996 := cr_lt_spending - lt_spending]
dt_15[, spending_effect_2016 := lt_spending - cr_lt_spending]
dt_15 <- dt_15[(year==1996), lt_spending_effect := spending_effect_1996]
dt_15 <- dt_15[(year==2016), lt_spending_effect := spending_effect_2016]

dt_15[, ex_effect_1996 := cr_ex - ex]
dt_15[, ex_effect_2016 := ex - cr_ex]
dt_15 <- dt_15[(year==1996), ex_effect := ex_effect_1996]
dt_15 <- dt_15[(year==2016), ex_effect := ex_effect_2016]

#Drop the redundant columns
dt_15 <- subset(dt_15, select = -c(spending_effect_1996,spending_effect_2016, ex_effect_1996, ex_effect_2016))

fwrite(dt_15, "./spending_decomp_results_by_draw_at15_dex_gdp_aggregated_death_agg.csv")

# creating a data frame containing draw 0 only.
dt_15_draw_0 <- dt_15[draw == 0, ]
fwrite(dt_15_draw_0, "./draw_0_spending_decomp_results_by_draw_at15_dex_gdp_aggregated_death_agg.csv")

#Mean and UI estimates for LE at age 15

mean_ui_dt_15<- dt_15 %>% group_by(cause_id, gbd_cause_name, dex_cause_id, dex_cause_name, sex_id, year) %>% 
  summarise(mean_spending= mean(lt_spending),
            l_spending = quantile(lt_spending, na.rm = TRUE, probs=0.025),
            u_spending = quantile(lt_spending, na.rm = TRUE, probs=0.975),
            mean_cr_lt_spending=mean(cr_lt_spending),
            l_cr_lt_spending = quantile(cr_lt_spending, na.rm = TRUE, probs=0.025),
            u_cr_lt_spending = quantile(cr_lt_spending, na.rm = TRUE, probs=0.975),
            mean_ex=mean(ex),
            l_ex = quantile(ex, na.rm = TRUE, probs=0.025),
            u_ex = quantile(ex, na.rm = TRUE, probs=0.975),
            mean_cr_ex=mean(cr_ex),
            l_cr_ex = quantile(cr_ex, na.rm = TRUE, probs=0.025),
            u_cr_ex = quantile(cr_ex, na.rm = TRUE, probs=0.975),
            mean_ac_spending=mean(ac_spending),
            l_ac_spending = quantile(ac_spending, na.rm = TRUE, probs=0.025),
            u_ac_spending = quantile(ac_spending, na.rm = TRUE, probs=0.975),
            mean_ac_cr_lt_spending=mean(cr_ac_spending),
            l_ac_cr_lt_spending = quantile(cr_ac_spending, na.rm = TRUE, probs=0.025),
            u_ac_cr_lt_spending = quantile(cr_ac_spending, na.rm = TRUE, probs=0.975),
            mean_spending_effect= mean(lt_spending_effect),
            l_spending_effect = quantile(lt_spending_effect, na.rm = TRUE, probs=0.025),
            u_spending_effect = quantile(lt_spending_effect, na.rm = TRUE, probs=0.975),
            mean_ex_effect=mean(ex_effect),
            l_ex_effect = quantile(ex_effect, na.rm = TRUE, probs=0.025),
            u_ex_effect = quantile(ex_effect, na.rm = TRUE, probs=0.975),
            .groups = 'drop')  %>%
  as.data.frame()

fwrite(mean_ui_dt_15, "./spending_decomp_results_mean_ui_at15_dex_gdp_aggregated_death_agg.csv")


## for LE at 30, we re-run the lt_spending model
dt_30 <- dt[age_group_id == 11]
dt_30[, age_group_id := NULL]
dt_30[, spending_effect_1996 := cr_lt_spending - lt_spending]
dt_30[, spending_effect_2016 := lt_spending - cr_lt_spending]
dt_30 <- dt_30[(year==1996), lt_spending_effect := spending_effect_1996]
dt_30 <- dt_30[(year==2016), lt_spending_effect := spending_effect_2016]

dt_30[, ex_effect_1996 := cr_ex - ex]
dt_30[, ex_effect_2016 := ex - cr_ex]
dt_30 <- dt_30[(year==1996), ex_effect := ex_effect_1996]
dt_30 <- dt_30[(year==2016), ex_effect := ex_effect_2016]

#Drop the redundant columns
dt_30 <- subset(dt_30, select = -c(spending_effect_1996,spending_effect_2016, ex_effect_1996, ex_effect_2016))

fwrite(dt_30, "./spending_decomp_results_by_draw_at30_dex_gdp_aggregated_death_agg.csv")

# creating a data frame containing draw 0 only.
dt_30_draw_0 <- dt_30[draw == 0, ]
fwrite(dt_30_draw_0, "./draw_0_spending_decomp_results_by_draw_at30_dex_gdp_aggregated_death_agg.csv")

#Mean and UI estimates for LE at age 30

mean_ui_dt_30<- dt_30 %>% group_by(cause_id, gbd_cause_name, dex_cause_id, dex_cause_name, sex_id, year) %>% 
  summarise(mean_spending= mean(lt_spending),
            l_spending = quantile(lt_spending, na.rm = TRUE, probs=0.025),
            u_spending = quantile(lt_spending, na.rm = TRUE, probs=0.975),
            mean_cr_lt_spending=mean(cr_lt_spending),
            l_cr_lt_spending = quantile(cr_lt_spending, na.rm = TRUE, probs=0.025),
            u_cr_lt_spending = quantile(cr_lt_spending, na.rm = TRUE, probs=0.975),
            mean_ex=mean(ex),
            l_ex = quantile(ex, na.rm = TRUE, probs=0.025),
            u_ex = quantile(ex, na.rm = TRUE, probs=0.975),
            mean_cr_ex=mean(cr_ex),
            l_cr_ex = quantile(cr_ex, na.rm = TRUE, probs=0.025),
            u_cr_ex = quantile(cr_ex, na.rm = TRUE, probs=0.975),
            mean_ac_spending=mean(ac_spending),
            l_ac_spending = quantile(ac_spending, na.rm = TRUE, probs=0.025),
            u_ac_spending = quantile(ac_spending, na.rm = TRUE, probs=0.975),
            mean_ac_cr_lt_spending=mean(cr_ac_spending),
            l_ac_cr_lt_spending = quantile(cr_ac_spending, na.rm = TRUE, probs=0.025),
            u_ac_cr_lt_spending = quantile(cr_ac_spending, na.rm = TRUE, probs=0.975),
            mean_spending_effect= mean(lt_spending_effect),
            l_spending_effect = quantile(lt_spending_effect, na.rm = TRUE, probs=0.025),
            u_spending_effect = quantile(lt_spending_effect, na.rm = TRUE, probs=0.975),
            mean_ex_effect=mean(ex_effect),
            l_ex_effect = quantile(ex_effect, na.rm = TRUE, probs=0.025),
            u_ex_effect = quantile(ex_effect, na.rm = TRUE, probs=0.975),
            .groups = 'drop')  %>%
  as.data.frame()

fwrite(mean_ui_dt_30, "./spending_decomp_results_mean_ui_at30_dex_gdp_aggregated_death_agg.csv")


## for LE at 45, we re-run the lt_spending model
dt_45 <- dt[age_group_id == 14]
dt_45[, age_group_id := NULL]
dt_45[, spending_effect_1996 := cr_lt_spending - lt_spending]
dt_45[, spending_effect_2016 := lt_spending - cr_lt_spending]
dt_45 <- dt_45[(year==1996), lt_spending_effect := spending_effect_1996]
dt_45 <- dt_45[(year==2016), lt_spending_effect := spending_effect_2016]

dt_45[, ex_effect_1996 := cr_ex - ex]
dt_45[, ex_effect_2016 := ex - cr_ex]
dt_45 <- dt_45[(year==1996), ex_effect := ex_effect_1996]
dt_45 <- dt_45[(year==2016), ex_effect := ex_effect_2016]

#Drop the redundant columns
dt_45 <- subset(dt_45, select = -c(spending_effect_1996,spending_effect_2016, ex_effect_1996, ex_effect_2016))

fwrite(dt_45, "./spending_decomp_results_by_draw_at45_dex_gdp_aggregated_death_agg.csv")

# creating a data frame containing draw 0 only.
dt_45_draw_0 <- dt_45[draw == 0, ]
fwrite(dt_45_draw_0, "./draw_0_spending_decomp_results_by_draw_at45_dex_gdp_aggregated_death_agg.csv")

#Mean and UI estimates for LE at age 45

mean_ui_dt_45<- dt_45 %>% group_by(cause_id, gbd_cause_name, dex_cause_id, dex_cause_name, sex_id, year) %>% 
  summarise(mean_spending= mean(lt_spending),
            l_spending = quantile(lt_spending, na.rm = TRUE, probs=0.025),
            u_spending = quantile(lt_spending, na.rm = TRUE, probs=0.975),
            mean_cr_lt_spending=mean(cr_lt_spending),
            l_cr_lt_spending = quantile(cr_lt_spending, na.rm = TRUE, probs=0.025),
            u_cr_lt_spending = quantile(cr_lt_spending, na.rm = TRUE, probs=0.975),
            mean_ex=mean(ex),
            l_ex = quantile(ex, na.rm = TRUE, probs=0.025),
            u_ex = quantile(ex, na.rm = TRUE, probs=0.975),
            mean_cr_ex=mean(cr_ex),
            l_cr_ex = quantile(cr_ex, na.rm = TRUE, probs=0.025),
            u_cr_ex = quantile(cr_ex, na.rm = TRUE, probs=0.975),
            mean_ac_spending=mean(ac_spending),
            l_ac_spending = quantile(ac_spending, na.rm = TRUE, probs=0.025),
            u_ac_spending = quantile(ac_spending, na.rm = TRUE, probs=0.975),
            mean_ac_cr_lt_spending=mean(cr_ac_spending),
            l_ac_cr_lt_spending = quantile(cr_ac_spending, na.rm = TRUE, probs=0.025),
            u_ac_cr_lt_spending = quantile(cr_ac_spending, na.rm = TRUE, probs=0.975),
            mean_spending_effect= mean(lt_spending_effect),
            l_spending_effect = quantile(lt_spending_effect, na.rm = TRUE, probs=0.025),
            u_spending_effect = quantile(lt_spending_effect, na.rm = TRUE, probs=0.975),
            mean_ex_effect=mean(ex_effect),
            l_ex_effect = quantile(ex_effect, na.rm = TRUE, probs=0.025),
            u_ex_effect = quantile(ex_effect, na.rm = TRUE, probs=0.975),
            .groups = 'drop')  %>%
  as.data.frame()

fwrite(mean_ui_dt_45, "./spending_decomp_results_mean_ui_at45_dex_gdp_aggregated_death_agg.csv")


## for LE at 55, we re-run the lt_spending model
dt_55 <- dt[age_group_id == 16]
dt_55[, age_group_id := NULL]
dt_55[, spending_effect_1996 := cr_lt_spending - lt_spending]
dt_55[, spending_effect_2016 := lt_spending - cr_lt_spending]
dt_55 <- dt_55[(year==1996), lt_spending_effect := spending_effect_1996]
dt_55 <- dt_55[(year==2016), lt_spending_effect := spending_effect_2016]

dt_55[, ex_effect_1996 := cr_ex - ex]
dt_55[, ex_effect_2016 := ex - cr_ex]
dt_55 <- dt_55[(year==1996), ex_effect := ex_effect_1996]
dt_55 <- dt_55[(year==2016), ex_effect := ex_effect_2016]

#Drop the redundant columns
dt_55 <- subset(dt_55, select = -c(spending_effect_1996,spending_effect_2016, ex_effect_1996, ex_effect_2016))

fwrite(dt_55, "./spending_decomp_results_by_draw_at55_dex_gdp_aggregated_death_agg.csv")

# creating a data frame containing draw 0 only.
dt_55_draw_0 <- dt_55[draw == 0, ]
fwrite(dt_55_draw_0, "./draw_0_spending_decomp_results_by_draw_at55_dex_gdp_aggregated_death_agg.csv")

#Mean and UI estimates for LE at age 55

mean_ui_dt_55<- dt_55 %>% group_by(cause_id, gbd_cause_name, dex_cause_id, dex_cause_name, sex_id, year) %>% 
  summarise(mean_spending= mean(lt_spending),
            l_spending = quantile(lt_spending, na.rm = TRUE, probs=0.025),
            u_spending = quantile(lt_spending, na.rm = TRUE, probs=0.975),
            mean_cr_lt_spending=mean(cr_lt_spending),
            l_cr_lt_spending = quantile(cr_lt_spending, na.rm = TRUE, probs=0.025),
            u_cr_lt_spending = quantile(cr_lt_spending, na.rm = TRUE, probs=0.975),
            mean_ex=mean(ex),
            l_ex = quantile(ex, na.rm = TRUE, probs=0.025),
            u_ex = quantile(ex, na.rm = TRUE, probs=0.975),
            mean_cr_ex=mean(cr_ex),
            l_cr_ex = quantile(cr_ex, na.rm = TRUE, probs=0.025),
            u_cr_ex = quantile(cr_ex, na.rm = TRUE, probs=0.975),
            mean_ac_spending=mean(ac_spending),
            l_ac_spending = quantile(ac_spending, na.rm = TRUE, probs=0.025),
            u_ac_spending = quantile(ac_spending, na.rm = TRUE, probs=0.975),
            mean_ac_cr_lt_spending=mean(cr_ac_spending),
            l_ac_cr_lt_spending = quantile(cr_ac_spending, na.rm = TRUE, probs=0.025),
            u_ac_cr_lt_spending = quantile(cr_ac_spending, na.rm = TRUE, probs=0.975),
            mean_spending_effect= mean(lt_spending_effect),
            l_spending_effect = quantile(lt_spending_effect, na.rm = TRUE, probs=0.025),
            u_spending_effect = quantile(lt_spending_effect, na.rm = TRUE, probs=0.975),
            mean_ex_effect=mean(ex_effect),
            l_ex_effect = quantile(ex_effect, na.rm = TRUE, probs=0.025),
            u_ex_effect = quantile(ex_effect, na.rm = TRUE, probs=0.975),
            .groups = 'drop')  %>%
  as.data.frame()

fwrite(mean_ui_dt_55, "./spending_decomp_results_mean_ui_at55_dex_gdp_aggregated_death_agg.csv")



## for LE at 65, we re-run the lt_spending model
dt_65 <- dt[age_group_id == 18]
dt_65[, age_group_id := NULL]
# dt_65[, lt_spending_effect := cr_lt_spending - lt_spending]
# dt_65[, ex_effect := cr_ex - ex]
dt_65[, spending_effect_1996 := cr_lt_spending - lt_spending]
dt_65[, spending_effect_2016 := lt_spending - cr_lt_spending]
dt_65 <- dt_65[(year==1996), lt_spending_effect := spending_effect_1996]
dt_65 <- dt_65[(year==2016), lt_spending_effect := spending_effect_2016]

dt_65[, ex_effect_1996 := cr_ex - ex]
dt_65[, ex_effect_2016 := ex - cr_ex]
dt_65 <- dt_65[(year==1996), ex_effect := ex_effect_1996]
dt_65 <- dt_65[(year==2016), ex_effect := ex_effect_2016]

#Drop the redundant columns
dt_65 <- subset(dt_65, select = -c(spending_effect_1996,spending_effect_2016, ex_effect_1996, ex_effect_2016))

fwrite(dt_65, "./spending_decomp_results_by_draw_at65_dex_gdp_aggregated_death_agg.csv")

# creating a data frame containing draw 0 only.
dt_65_draw_0 <- dt_65[draw == 0, ]
fwrite(dt_65_draw_0, "./draw_0_spending_decomp_results_by_draw_at65_dex_gdp_aggregated_death_agg.csv")

#Mean and UI estimates for LE at age 65

mean_ui_dt_65<- dt_65 %>% group_by(cause_id, gbd_cause_name, dex_cause_id, dex_cause_name, sex_id, year) %>% 
  summarise(mean_spending= mean(lt_spending),
            l_spending = quantile(lt_spending, na.rm = TRUE, probs=0.025),
            u_spending = quantile(lt_spending, na.rm = TRUE, probs=0.975),
            mean_cr_lt_spending=mean(cr_lt_spending),
            l_cr_lt_spending = quantile(cr_lt_spending, na.rm = TRUE, probs=0.025),
            u_cr_lt_spending = quantile(cr_lt_spending, na.rm = TRUE, probs=0.975),
            mean_ex=mean(ex),
            l_ex = quantile(ex, na.rm = TRUE, probs=0.025),
            u_ex = quantile(ex, na.rm = TRUE, probs=0.975),
            mean_cr_ex=mean(cr_ex),
            l_cr_ex = quantile(cr_ex, na.rm = TRUE, probs=0.025),
            u_cr_ex = quantile(cr_ex, na.rm = TRUE, probs=0.975),
            mean_ac_spending=mean(ac_spending),
            l_ac_spending = quantile(ac_spending, na.rm = TRUE, probs=0.025),
            u_ac_spending = quantile(ac_spending, na.rm = TRUE, probs=0.975),
            mean_ac_cr_lt_spending=mean(cr_ac_spending),
            l_ac_cr_lt_spending = quantile(cr_ac_spending, na.rm = TRUE, probs=0.025),
            u_ac_cr_lt_spending = quantile(cr_ac_spending, na.rm = TRUE, probs=0.975),
            mean_spending_effect= mean(lt_spending_effect),
            l_spending_effect = quantile(lt_spending_effect, na.rm = TRUE, probs=0.025),
            u_spending_effect = quantile(lt_spending_effect, na.rm = TRUE, probs=0.975),
            mean_ex_effect=mean(ex_effect),
            l_ex_effect = quantile(ex_effect, na.rm = TRUE, probs=0.025),
            u_ex_effect = quantile(ex_effect, na.rm = TRUE, probs=0.975),
            .groups = 'drop')  %>%
  as.data.frame()

fwrite(mean_ui_dt_65, "./spending_decomp_results_mean_ui_at65_dex_gdp_aggregated_death_agg.csv")

## for LE at 75, we re-run the lt_spending model
dt_75 <- dt[age_group_id == 20]
dt_75[, age_group_id := NULL]
dt_75[, spending_effect_1996 := cr_lt_spending - lt_spending]
dt_75[, spending_effect_2016 := lt_spending - cr_lt_spending]
dt_75 <- dt_75[(year==1996), lt_spending_effect := spending_effect_1996]
dt_75 <- dt_75[(year==2016), lt_spending_effect := spending_effect_2016]

dt_75[, ex_effect_1996 := cr_ex - ex]
dt_75[, ex_effect_2016 := ex - cr_ex]
dt_75 <- dt_75[(year==1996), ex_effect := ex_effect_1996]
dt_75 <- dt_75[(year==2016), ex_effect := ex_effect_2016]

#Drop the redundant columns
dt_75 <- subset(dt_75, select = -c(spending_effect_1996,spending_effect_2016, ex_effect_1996, ex_effect_2016))

fwrite(dt_75, "./spending_decomp_results_by_draw_at75_dex_gdp_aggregated_death_agg.csv")

# creating a data frame containing draw 0 only.
dt_75_draw_0 <- dt_75[draw == 0, ]
fwrite(dt_75_draw_0, "./draw_0_spending_decomp_results_by_draw_at75_dex_gdp_aggregated_death_agg.csv")

#Mean and UI estimates for LE at age 75

mean_ui_dt_75<- dt_75 %>% group_by(cause_id, gbd_cause_name, dex_cause_id, dex_cause_name, sex_id, year) %>% 
  summarise(mean_spending= mean(lt_spending),
            l_spending = quantile(lt_spending, na.rm = TRUE, probs=0.025),
            u_spending = quantile(lt_spending, na.rm = TRUE, probs=0.975),
            mean_cr_lt_spending=mean(cr_lt_spending),
            l_cr_lt_spending = quantile(cr_lt_spending, na.rm = TRUE, probs=0.025),
            u_cr_lt_spending = quantile(cr_lt_spending, na.rm = TRUE, probs=0.975),
            mean_ex=mean(ex),
            l_ex = quantile(ex, na.rm = TRUE, probs=0.025),
            u_ex = quantile(ex, na.rm = TRUE, probs=0.975),
            mean_cr_ex=mean(cr_ex),
            l_cr_ex = quantile(cr_ex, na.rm = TRUE, probs=0.025),
            u_cr_ex = quantile(cr_ex, na.rm = TRUE, probs=0.975),
            mean_ac_spending=mean(ac_spending),
            l_ac_spending = quantile(ac_spending, na.rm = TRUE, probs=0.025),
            u_ac_spending = quantile(ac_spending, na.rm = TRUE, probs=0.975),
            mean_ac_cr_lt_spending=mean(cr_ac_spending),
            l_ac_cr_lt_spending = quantile(cr_ac_spending, na.rm = TRUE, probs=0.025),
            u_ac_cr_lt_spending = quantile(cr_ac_spending, na.rm = TRUE, probs=0.975),
            mean_spending_effect= mean(lt_spending_effect),
            l_spending_effect = quantile(lt_spending_effect, na.rm = TRUE, probs=0.025),
            u_spending_effect = quantile(lt_spending_effect, na.rm = TRUE, probs=0.975),
            mean_ex_effect=mean(ex_effect),
            l_ex_effect = quantile(ex_effect, na.rm = TRUE, probs=0.025),
            u_ex_effect = quantile(ex_effect, na.rm = TRUE, probs=0.975),
            .groups = 'drop')  %>%
  as.data.frame()

fwrite(mean_ui_dt_75, "./spending_decomp_results_mean_ui_at75_dex_gdp_aggregated_death_agg.csv")

## for LE at 85, we re-run the lt_spending model
dt_85 <- dt[age_group_id == 160]
dt_85[, age_group_id := NULL]
dt_85[, spending_effect_1996 := cr_lt_spending - lt_spending]
dt_85[, spending_effect_2016 := lt_spending - cr_lt_spending]
dt_85 <- dt_85[(year==1996), lt_spending_effect := spending_effect_1996]
dt_85 <- dt_85[(year==2016), lt_spending_effect := spending_effect_2016]

dt_85[, ex_effect_1996 := cr_ex - ex]
dt_85[, ex_effect_2016 := ex - cr_ex]
dt_85 <- dt_85[(year==1996), ex_effect := ex_effect_1996]
dt_85 <- dt_85[(year==2016), ex_effect := ex_effect_2016]

#Drop the redundant columns
dt_85 <- subset(dt_85, select = -c(spending_effect_1996,spending_effect_2016, ex_effect_1996, ex_effect_2016))

fwrite(dt_85, "./spending_decomp_results_by_draw_at85_dex_gdp_aggregated_death_agg.csv")

# creating a data frame containing draw 0 only.
dt_85_draw_0 <- dt_85[draw == 0, ]
fwrite(dt_85_draw_0, "./draw_0_spending_decomp_results_by_draw_at85_dex_gdp_aggregated_death_agg.csv")

#Mean and UI estimates for LE at age 85

mean_ui_dt_85<- dt_85 %>% group_by(cause_id, gbd_cause_name, dex_cause_id, dex_cause_name, sex_id, year) %>% 
  summarise(mean_spending= mean(lt_spending),
            l_spending = quantile(lt_spending, na.rm = TRUE, probs=0.025),
            u_spending = quantile(lt_spending, na.rm = TRUE, probs=0.975),
            mean_cr_lt_spending=mean(cr_lt_spending),
            l_cr_lt_spending = quantile(cr_lt_spending, na.rm = TRUE, probs=0.025),
            u_cr_lt_spending = quantile(cr_lt_spending, na.rm = TRUE, probs=0.975),
            mean_ex=mean(ex),
            l_ex = quantile(ex, na.rm = TRUE, probs=0.025),
            u_ex = quantile(ex, na.rm = TRUE, probs=0.975),
            mean_cr_ex=mean(cr_ex),
            l_cr_ex = quantile(cr_ex, na.rm = TRUE, probs=0.025),
            u_cr_ex = quantile(cr_ex, na.rm = TRUE, probs=0.975),
            mean_ac_spending=mean(ac_spending),
            l_ac_spending = quantile(ac_spending, na.rm = TRUE, probs=0.025),
            u_ac_spending = quantile(ac_spending, na.rm = TRUE, probs=0.975),
            mean_ac_cr_lt_spending=mean(cr_ac_spending),
            l_ac_cr_lt_spending = quantile(cr_ac_spending, na.rm = TRUE, probs=0.025),
            u_ac_cr_lt_spending = quantile(cr_ac_spending, na.rm = TRUE, probs=0.975),
            mean_spending_effect= mean(lt_spending_effect),
            l_spending_effect = quantile(lt_spending_effect, na.rm = TRUE, probs=0.025),
            u_spending_effect = quantile(lt_spending_effect, na.rm = TRUE, probs=0.975),
            mean_ex_effect=mean(ex_effect),
            l_ex_effect = quantile(ex_effect, na.rm = TRUE, probs=0.025),
            u_ex_effect = quantile(ex_effect, na.rm = TRUE, probs=0.975),
            .groups = 'drop')  %>%
  as.data.frame()

fwrite(mean_ui_dt_85, "./spending_decomp_results_mean_ui_at85_dex_gdp_aggregated_death_agg.csv")

#####Delta effects (i.e., changes in variables across age-groupings)######

#for all_draws

#age-group 0
#Summarizing over years to get average of 1996 and 2016 values
sum_years_0 <- dt_0[, c("cause_id", "draw", "year", "lt_spending_effect", "ex_effect")]

sum_years_0 <- sum_years_0 %>%
  filter(year %in% c("1996", "2016")) %>% # apply filter condition
  group_by(draw, cause_id) %>%
  summarize(across(c(lt_spending_effect, ex_effect),mean))

dt_0 <- merge(dt_0, sum_years_0, by=c("draw", "cause_id"), all=TRUE)

#Dropping redundant rows(one year from the dataset)
dt_0<-dt_0[!(dt_0$year=="2016"),]

#dt_0 <- fread("./decomp_results_by_draw_dex_gbd_agg_death_avg.csv")
#Drop the redundant column
dt_0 <- subset(dt_0, select = -c(year,lt_spending, cr_lt_spending, ex, cr_ex, ac_spending, cr_ac_spending, avg_spending, cr_avg_spending, lt_spending_effect.x, ex_effect.x))



#renaming variables to _0 
dt_0<- dt_0 %>% 
  rename(
    lt_spending_effect_0 = lt_spending_effect.y,
    ex_effect_0 = ex_effect.y
  )

#exporting dataset containing year averaged results for age-group 0
fwrite(dt_0, "./years_averaged/spending_decomp_results_by_draw_at0_yr_avg.csv")
# creating a data frame containing draw 0 only.
dt_0_draw_0 <- dt_0[draw == 0, ]
fwrite(dt_0_draw_0, "./years_averaged/draw_0_spending_decomp_results_by_draw_at0_yr_avg.csv")


#forming new dataset delta_dt to include all age-group variables
delta_dt <- dt_0


#age-group 15
#Summarizing over years to get average of 1996 and 2016 values
sum_years_15 <- dt_15[, c("cause_id", "draw", "year", "lt_spending_effect", "ex_effect")]

sum_years_15 <- sum_years_15 %>%
  filter(year %in% c("1996", "2016")) %>% # apply filter condition
  group_by(draw, cause_id) %>%
  summarize(across(c(lt_spending_effect, ex_effect),mean))

dt_15 <- merge(dt_15, sum_years_15, by=c("draw", "cause_id"), all=TRUE)

#Dropping redundant rows(one year from the dataset)
dt_15<-dt_15[!(dt_15$year=="2016"),]

#Drop the redundant column
dt_15 <- subset(dt_15, select = -c(year,lt_spending, cr_lt_spending, ex, cr_ex, ac_spending, cr_ac_spending, avg_spending, cr_avg_spending, lt_spending_effect.x, ex_effect.x))

#renaming variables to _15 
dt_15<- dt_15 %>% 
  rename(
    lt_spending_effect_15 = lt_spending_effect.y,
    ex_effect_15 = ex_effect.y
  )


#exporting dataset containing year averaged results for age-group 15
fwrite(dt_15, "./years_averaged/spending_decomp_results_by_draw_at15_yr_avg.csv")
# creating a data frame containing draw 0 only.
dt_15_draw_0 <- dt_15[draw == 0, ]
fwrite(dt_15_draw_0, "./years_averaged/draw_0_spending_decomp_results_by_draw_at15_yr_avg.csv")


#merging with dataset delta_dt to include all age-group variables
delta_dt <- merge(delta_dt, dt_15, by=c("draw", "cause_id", "gbd_cause_name", "dex_cause_id", "dex_cause_name", "sex_id"), all=TRUE)


#age-group 30
#Summarizing over years to get average of 1996 and 2016 values
sum_years_30 <- dt_30[, c("cause_id", "draw", "year", "lt_spending_effect", "ex_effect")]

sum_years_30 <- sum_years_30 %>%
  filter(year %in% c("1996", "2016")) %>% # apply filter condition
  group_by(draw, cause_id) %>%
  summarize(across(c(lt_spending_effect, ex_effect),mean))

dt_30 <- merge(dt_30, sum_years_30, by=c("draw", "cause_id"), all=TRUE)

#Dropping redundant rows(one year from the dataset)
dt_30<-dt_30[!(dt_30$year=="2016"),]

#Drop the redundant column
dt_30 <- subset(dt_30, select = -c(year,lt_spending, cr_lt_spending, ex, cr_ex, ac_spending, cr_ac_spending, avg_spending, cr_avg_spending, lt_spending_effect.x, ex_effect.x))

#renaming variables to _30 
dt_30<- dt_30 %>% 
  rename(
    lt_spending_effect_30 = lt_spending_effect.y,
    ex_effect_30 = ex_effect.y
  )

#exporting dataset containing year averaged results for age-group 30
fwrite(dt_30, "./years_averaged/spending_decomp_results_by_draw_at30_yr_avg.csv")
# creating a data frame containing draw 0 only.
dt_30_draw_0 <- dt_30[draw == 0, ]
fwrite(dt_30_draw_0, "./years_averaged/draw_0_spending_decomp_results_by_draw_at30_yr_avg.csv")


#merging with dataset delta_dt to include all age-group variables
delta_dt <- merge(delta_dt, dt_30, by=c("draw", "cause_id", "gbd_cause_name", "dex_cause_id", "dex_cause_name", "sex_id"), all=TRUE)


#age-group 45
#Summarizing over years to get average of 1996 and 2016 values
sum_years_45 <- dt_45[, c("cause_id", "draw", "year", "lt_spending_effect", "ex_effect")]

sum_years_45 <- sum_years_45 %>%
  filter(year %in% c("1996", "2016")) %>% # apply filter condition
  group_by(draw, cause_id) %>%
  summarize(across(c(lt_spending_effect, ex_effect),mean))

dt_45 <- merge(dt_45, sum_years_45, by=c("draw", "cause_id"), all=TRUE)

#Dropping redundant rows(one year from the dataset)
dt_45<-dt_45[!(dt_45$year=="2016"),]

#Drop the redundant column
dt_45 <- subset(dt_45, select = -c(year,lt_spending, cr_lt_spending, ex, cr_ex, ac_spending, cr_ac_spending, avg_spending, cr_avg_spending, lt_spending_effect.x, ex_effect.x))

#renaming variables to _45 
dt_45<- dt_45 %>% 
  rename(
    lt_spending_effect_45 = lt_spending_effect.y,
    ex_effect_45 = ex_effect.y
  )


#exporting dataset containing year averaged results for age-group 45
fwrite(dt_45, "./years_averaged/spending_decomp_results_by_draw_at45_yr_avg.csv")
# creating a data frame containing draw 0 only.
dt_45_draw_0 <- dt_45[draw == 0, ]
fwrite(dt_45_draw_0, "./years_averaged/draw_0_spending_decomp_results_by_draw_at45_yr_avg.csv")


#merging with dataset delta_dt to include all age-group variables
delta_dt <- merge(delta_dt, dt_45, by=c("draw", "cause_id", "gbd_cause_name", "dex_cause_id", "dex_cause_name", "sex_id"), all=TRUE)


#age-group 55
#Summarizing over years to get average of 1996 and 2016 values
sum_years_55 <- dt_55[, c("cause_id", "draw", "year", "lt_spending_effect", "ex_effect")]

sum_years_55 <- sum_years_55 %>%
  filter(year %in% c("1996", "2016")) %>% # apply filter condition
  group_by(draw, cause_id) %>%
  summarize(across(c(lt_spending_effect, ex_effect),mean))

dt_55 <- merge(dt_55, sum_years_55, by=c("draw", "cause_id"), all=TRUE)

#Dropping redundant rows(one year from the dataset)
dt_55<-dt_55[!(dt_55$year=="2016"),]

#Drop the redundant column
dt_55 <- subset(dt_55, select = -c(year,lt_spending, cr_lt_spending, ex, cr_ex, ac_spending, cr_ac_spending, avg_spending, cr_avg_spending, lt_spending_effect.x, ex_effect.x))

#renaming variables to _55 
dt_55<- dt_55 %>% 
  rename(
    lt_spending_effect_55 = lt_spending_effect.y,
    ex_effect_55 = ex_effect.y
  )

#exporting dataset containing year averaged results for age-group 55
fwrite(dt_55, "./years_averaged/spending_decomp_results_by_draw_at55_yr_avg.csv")
# creating a data frame containing draw 0 only.
dt_55_draw_0 <- dt_55[draw == 0, ]
fwrite(dt_55_draw_0, "./years_averaged/draw_0_spending_decomp_results_by_draw_at55_yr_avg.csv")


#merging with dataset delta_dt to include all age-group variables
delta_dt <- merge(delta_dt, dt_55, by=c("draw", "cause_id", "gbd_cause_name", "dex_cause_id", "dex_cause_name", "sex_id"), all=TRUE)


#age-group 65
#Summarizing over years to get average of 1996 and 2016 values
sum_years_65 <- dt_65[, c("cause_id", "draw", "year", "lt_spending_effect", "ex_effect")]

sum_years_65 <- sum_years_65 %>%
  filter(year %in% c("1996", "2016")) %>% # apply filter condition
  group_by(draw, cause_id) %>%
  summarize(across(c(lt_spending_effect, ex_effect),mean))

dt_65 <- merge(dt_65, sum_years_65, by=c("draw", "cause_id"), all=TRUE)

#Dropping redundant rows(one year from the dataset)
dt_65<-dt_65[!(dt_65$year=="2016"),]

#Drop the redundant column
dt_65 <- subset(dt_65, select = -c(year,lt_spending, cr_lt_spending, ex, cr_ex, ac_spending, cr_ac_spending, avg_spending, cr_avg_spending, lt_spending_effect.x, ex_effect.x))

#renaming variables to _65 
dt_65<- dt_65 %>% 
  rename(
    lt_spending_effect_65 = lt_spending_effect.y,
    ex_effect_65 = ex_effect.y
  )

#exporting dataset containing year averaged results for age-group 65
fwrite(dt_65, "./years_averaged/spending_decomp_results_by_draw_at65_yr_avg.csv")
# creating a data frame containing draw 0 only.
dt_65_draw_0 <- dt_65[draw == 0, ]
fwrite(dt_65_draw_0, "./years_averaged/draw_0_spending_decomp_results_by_draw_at65_yr_avg.csv")


#merging with dataset delta_dt to include all age-group variables
delta_dt <- merge(delta_dt, dt_65, by=c("draw", "cause_id", "gbd_cause_name", "dex_cause_id", "dex_cause_name", "sex_id"), all=TRUE)


#age-group 75
#Summarizing over years to get average of 1996 and 2016 values
sum_years_75 <- dt_75[, c("cause_id", "draw", "year", "lt_spending_effect", "ex_effect")]

sum_years_75 <- sum_years_75 %>%
  filter(year %in% c("1996", "2016")) %>% # apply filter condition
  group_by(draw, cause_id) %>%
  summarize(across(c(lt_spending_effect, ex_effect),mean))

dt_75 <- merge(dt_75, sum_years_75, by=c("draw", "cause_id"), all=TRUE)

#Dropping redundant rows(one year from the dataset)
dt_75<-dt_75[!(dt_75$year=="2016"),]

#Drop the redundant column
dt_75 <- subset(dt_75, select = -c(year,lt_spending, cr_lt_spending, ex, cr_ex, ac_spending, cr_ac_spending, avg_spending, cr_avg_spending, lt_spending_effect.x, ex_effect.x))

#renaming variables to _75 
dt_75<- dt_75 %>% 
  rename(
    lt_spending_effect_75 = lt_spending_effect.y,
    ex_effect_75 = ex_effect.y
  )

#exporting dataset containing year averaged results for age-group 75
fwrite(dt_75, "./years_averaged/spending_decomp_results_by_draw_at75_yr_avg.csv")
# creating a data frame containing draw 0 only.
dt_75_draw_0 <- dt_75[draw == 0, ]
fwrite(dt_75_draw_0, "./years_averaged/draw_0_spending_decomp_results_by_draw_at75_yr_avg.csv")


#merging with dataset delta_dt to include all age-group variables
delta_dt <- merge(delta_dt, dt_75, by=c("draw", "cause_id", "gbd_cause_name", "dex_cause_id", "dex_cause_name", "sex_id"), all=TRUE)



#age-group 85
#Summarizing over years to get average of 1996 and 2016 values
sum_years_85 <- dt_85[, c("cause_id", "draw", "year", "lt_spending_effect", "ex_effect")]

sum_years_85 <- sum_years_85 %>%
  filter(year %in% c("1996", "2016")) %>% # apply filter condition
  group_by(draw, cause_id) %>%
  summarize(across(c(lt_spending_effect, ex_effect),mean))

dt_85 <- merge(dt_85, sum_years_85, by=c("draw", "cause_id"), all=TRUE)

#Dropping redundant rows(one year from the dataset)
dt_85<-dt_85[!(dt_85$year=="2016"),]

#Drop the redundant column
dt_85 <- subset(dt_85, select = -c(year,lt_spending, cr_lt_spending, ex, cr_ex, ac_spending, cr_ac_spending, avg_spending, cr_avg_spending, lt_spending_effect.x, ex_effect.x))

#renaming variables to _85 
dt_85<- dt_85 %>% 
  rename(
    lt_spending_effect_85 = lt_spending_effect.y,
    ex_effect_85 = ex_effect.y
  )

#exporting dataset containing year averaged results for age-group 85
fwrite(dt_85, "./years_averaged/spending_decomp_results_by_draw_at85_yr_avg.csv")
# creating a data frame containing draw 0 only.
dt_85_draw_0 <- dt_85[draw == 0, ]
fwrite(dt_85_draw_0, "./years_averaged/draw_0_spending_decomp_results_by_draw_at85_yr_avg.csv")


#merging with dataset delta_dt to include all age-group variables
delta_dt <- merge(delta_dt, dt_85, by=c("draw", "cause_id", "gbd_cause_name", "dex_cause_id", "dex_cause_name", "sex_id"), all=TRUE)


#differences in lt_spending-effects for age-groups
delta_dt <- delta_dt[,d_lt_spending_effect_0_15 := lt_spending_effect_0 - lt_spending_effect_15]
delta_dt <- delta_dt[,d_lt_spending_effect_15_30 := lt_spending_effect_15 - lt_spending_effect_30]
delta_dt <- delta_dt[,d_lt_spending_effect_30_45 := lt_spending_effect_30 - lt_spending_effect_45]
delta_dt <- delta_dt[,d_lt_spending_effect_45_55 := lt_spending_effect_45 - lt_spending_effect_55]
delta_dt <- delta_dt[,d_lt_spending_effect_55_65 := lt_spending_effect_55 - lt_spending_effect_65]
delta_dt <- delta_dt[,d_lt_spending_effect_65_75 := lt_spending_effect_65 - lt_spending_effect_75]
delta_dt <- delta_dt[,d_lt_spending_effect_75_85 := lt_spending_effect_75 - lt_spending_effect_85]

#difference in ex-effect for age-groups
delta_dt <- delta_dt[,d_ex_effect_0_15 := ex_effect_0 - ex_effect_15]
delta_dt <- delta_dt[,d_ex_effect_15_30 := ex_effect_15 - ex_effect_30]
delta_dt <- delta_dt[,d_ex_effect_30_45 := ex_effect_30 - ex_effect_45]
delta_dt <- delta_dt[,d_ex_effect_45_55 := ex_effect_45 - ex_effect_55]
delta_dt <- delta_dt[,d_ex_effect_55_65 := ex_effect_55 - ex_effect_65]
delta_dt <- delta_dt[,d_ex_effect_65_75 := ex_effect_65 - ex_effect_75]
delta_dt <- delta_dt[,d_ex_effect_75_85 := ex_effect_75 - ex_effect_85]

fwrite(delta_dt, "./spending_decomp_effects_age_grp.csv")

# creating a data frame containing draw 0 only.
delta_dt_draw_0 <- delta_dt[draw == 0, ]
fwrite(delta_dt_draw_0, "./draw_0_spending_decomp_effects_age_grp.csv")

##delta effects for mean and UI estimates##

#age-group 0
#Summarizing over years to get average of 1996 and 2016 values
m_years_0 <- mean_ui_dt[, c("cause_id", "year", "mean_spending_effect", "l_spending_effect", "u_spending_effect", "mean_ex_effect", "l_ex_effect", "u_ex_effect")]

m_years_0 <- m_years_0 %>%
  filter(year %in% c("1996", "2016")) %>% # apply filter condition
  group_by(cause_id) %>%
  summarize(across(c(mean_spending_effect, l_spending_effect, u_spending_effect, mean_ex_effect, l_ex_effect, u_ex_effect),mean))

mean_ui_dt <- merge(mean_ui_dt, m_years_0, by=c("cause_id"), all=TRUE)

#Dropping redundant rows(one year from the dataset)
mean_ui_dt<-mean_ui_dt[!(mean_ui_dt$year=="2016"),]

#Drop the redundant column
mean_ui_dt <- subset(mean_ui_dt, select = c(cause_id, gbd_cause_name, dex_cause_id, dex_cause_name, sex_id, mean_spending_effect.y, l_spending_effect.y, u_spending_effect.y, mean_ex_effect.y, l_ex_effect.y, u_ex_effect.y))

#renaming variables to _0 
mean_ui_dt<- mean_ui_dt %>% 
  rename(
    mean_spending_effect_0 = mean_spending_effect.y, 
    l_spending_effect_0 = l_spending_effect.y, 
    u_spending_effect_0 = u_spending_effect.y, 
    mean_ex_effect_0 = mean_ex_effect.y, 
    l_ex_effect_0 = l_ex_effect.y, 
    u_ex_effect_0 = u_ex_effect.y
  )

#exporting dataset containing year averaged results for age-group 0
fwrite(mean_ui_dt, "./years_averaged/spending_decomp_results_mean_at0_yr_avg.csv")

#forming new dataset delta_dt to include all age-group variables
delta_mean_ui_dt <- mean_ui_dt


#age-group 15
#Summarizing over years to get average of 1996 and 2016 values
m_years_15 <- mean_ui_dt_15[, c("cause_id", "year", "mean_spending_effect", "l_spending_effect", "u_spending_effect", "mean_ex_effect", "l_ex_effect", "u_ex_effect")]

m_years_15 <- m_years_15 %>%
  filter(year %in% c("1996", "2016")) %>% # apply filter condition
  group_by(cause_id) %>%
  summarize(across(c(mean_spending_effect, l_spending_effect, u_spending_effect, mean_ex_effect, l_ex_effect, u_ex_effect),mean))

mean_ui_dt_15 <- merge(mean_ui_dt_15, m_years_15, by=c("cause_id"), all=TRUE)

#Dropping redundant rows(one year from the dataset)
mean_ui_dt_15<-mean_ui_dt_15[!(mean_ui_dt_15$year=="2016"),]

#Drop the redundant column
mean_ui_dt_15 <- subset(mean_ui_dt_15, select = c(cause_id, gbd_cause_name, dex_cause_id, dex_cause_name, sex_id, mean_spending_effect.y, l_spending_effect.y, u_spending_effect.y, mean_ex_effect.y, l_ex_effect.y, u_ex_effect.y))

#renaming variables to _15 
mean_ui_dt_15 <- mean_ui_dt_15 %>% 
  rename(
    mean_spending_effect_15 = mean_spending_effect.y, 
    l_spending_effect_15 = l_spending_effect.y, 
    u_spending_effect_15 = u_spending_effect.y, 
    mean_ex_effect_15 = mean_ex_effect.y, 
    l_ex_effect_15 = l_ex_effect.y, 
    u_ex_effect_15 = u_ex_effect.y
  )

#exporting dataset containing year averaged results for age-group 15
fwrite(mean_ui_dt_15, "./years_averaged/spending_decomp_results_mean_at15_yr_avg.csv")

#merging with dataset delta_mean_ui_dt to include all age-group variables
delta_mean_ui_dt <- merge(delta_mean_ui_dt, mean_ui_dt_15, by=c("cause_id", "gbd_cause_name", "dex_cause_id", "dex_cause_name", "sex_id"), all=TRUE)

#age-group 30
#Summarizing over years to get average of 1996 and 2016 values
m_years_30 <- mean_ui_dt_30[, c("cause_id", "year", "mean_spending_effect", "l_spending_effect", "u_spending_effect", "mean_ex_effect", "l_ex_effect", "u_ex_effect")]

m_years_30 <- m_years_30 %>%
  filter(year %in% c("1996", "2016")) %>% # apply filter condition
  group_by(cause_id) %>%
  summarize(across(c(mean_spending_effect, l_spending_effect, u_spending_effect, mean_ex_effect, l_ex_effect, u_ex_effect),mean))

mean_ui_dt_30 <- merge(mean_ui_dt_30, m_years_30, by=c("cause_id"), all=TRUE)

#Dropping redundant rows(one year from the dataset)
mean_ui_dt_30<-mean_ui_dt_30[!(mean_ui_dt_30$year=="2016"),]

#Drop the redundant column
mean_ui_dt_30 <- subset(mean_ui_dt_30, select = c(cause_id, gbd_cause_name, dex_cause_id, dex_cause_name, sex_id, mean_spending_effect.y, l_spending_effect.y, u_spending_effect.y, mean_ex_effect.y, l_ex_effect.y, u_ex_effect.y))

#renaming variables to _30 
mean_ui_dt_30 <- mean_ui_dt_30 %>% 
  rename(
    mean_spending_effect_30 = mean_spending_effect.y, 
    l_spending_effect_30 = l_spending_effect.y, 
    u_spending_effect_30 = u_spending_effect.y, 
    mean_ex_effect_30 = mean_ex_effect.y, 
    l_ex_effect_30 = l_ex_effect.y, 
    u_ex_effect_30 = u_ex_effect.y
  )

#exporting dataset containing year averaged results for age-group 30
fwrite(mean_ui_dt_30, "./years_averaged/spending_decomp_results_mean_at30_yr_avg.csv")


#merging with dataset delta_mean_ui_dt to include all age-group variables
delta_mean_ui_dt <- merge(delta_mean_ui_dt, mean_ui_dt_30, by=c("cause_id", "gbd_cause_name", "dex_cause_id", "dex_cause_name", "sex_id"), all=TRUE)

#age-group 45
#Summarizing over years to get average of 1996 and 2016 values
m_years_45 <- mean_ui_dt_45[, c("cause_id", "year", "mean_spending_effect", "l_spending_effect", "u_spending_effect", "mean_ex_effect", "l_ex_effect", "u_ex_effect")]

m_years_45 <- m_years_45 %>%
  filter(year %in% c("1996", "2016")) %>% # apply filter condition
  group_by(cause_id) %>%
  summarize(across(c(mean_spending_effect, l_spending_effect, u_spending_effect, mean_ex_effect, l_ex_effect, u_ex_effect),mean))

mean_ui_dt_45 <- merge(mean_ui_dt_45, m_years_45, by=c("cause_id"), all=TRUE)

#Dropping redundant rows(one year from the dataset)
mean_ui_dt_45<-mean_ui_dt_45[!(mean_ui_dt_45$year=="2016"),]

#Drop the redundant column
mean_ui_dt_45 <- subset(mean_ui_dt_45, select = c(cause_id, gbd_cause_name, dex_cause_id, dex_cause_name, sex_id, mean_spending_effect.y, l_spending_effect.y, u_spending_effect.y, mean_ex_effect.y, l_ex_effect.y, u_ex_effect.y))

#renaming variables to _45 
mean_ui_dt_45 <- mean_ui_dt_45 %>% 
  rename(
    mean_spending_effect_45 = mean_spending_effect.y, 
    l_spending_effect_45 = l_spending_effect.y, 
    u_spending_effect_45 = u_spending_effect.y, 
    mean_ex_effect_45 = mean_ex_effect.y, 
    l_ex_effect_45 = l_ex_effect.y, 
    u_ex_effect_45 = u_ex_effect.y
  )

#exporting dataset containing year averaged results for age-group 45
fwrite(mean_ui_dt_45, "./years_averaged/spending_decomp_results_mean_at45_yr_avg.csv")


#merging with dataset delta_mean_ui_dt to include all age-group variables
delta_mean_ui_dt <- merge(delta_mean_ui_dt, mean_ui_dt_45, by=c("cause_id", "gbd_cause_name", "dex_cause_id", "dex_cause_name", "sex_id"), all=TRUE)

#age-group 55
#Summarizing over years to get average of 1996 and 2016 values
m_years_55 <- mean_ui_dt_55[, c("cause_id", "year", "mean_spending_effect", "l_spending_effect", "u_spending_effect", "mean_ex_effect", "l_ex_effect", "u_ex_effect")]

m_years_55 <- m_years_55 %>%
  filter(year %in% c("1996", "2016")) %>% # apply filter condition
  group_by(cause_id) %>%
  summarize(across(c(mean_spending_effect, l_spending_effect, u_spending_effect, mean_ex_effect, l_ex_effect, u_ex_effect),mean))

mean_ui_dt_55 <- merge(mean_ui_dt_55, m_years_55, by=c("cause_id"), all=TRUE)

#Dropping redundant rows(one year from the dataset)
mean_ui_dt_55<-mean_ui_dt_55[!(mean_ui_dt_55$year=="2016"),]

#Drop the redundant column
mean_ui_dt_55 <- subset(mean_ui_dt_55, select = c(cause_id, gbd_cause_name, dex_cause_id, dex_cause_name, sex_id, mean_spending_effect.y, l_spending_effect.y, u_spending_effect.y, mean_ex_effect.y, l_ex_effect.y, u_ex_effect.y))

#renaming variables to _55 
mean_ui_dt_55 <- mean_ui_dt_55 %>% 
  rename(
    mean_spending_effect_55 = mean_spending_effect.y, 
    l_spending_effect_55 = l_spending_effect.y, 
    u_spending_effect_55 = u_spending_effect.y, 
    mean_ex_effect_55 = mean_ex_effect.y, 
    l_ex_effect_55 = l_ex_effect.y, 
    u_ex_effect_55 = u_ex_effect.y
  )

#exporting dataset containing year averaged results for age-group 55
fwrite(mean_ui_dt_55, "./years_averaged/spending_decomp_results_mean_at55_yr_avg.csv")

#merging with dataset delta_mean_ui_dt to include all age-group variables
delta_mean_ui_dt <- merge(delta_mean_ui_dt, mean_ui_dt_55, by=c("cause_id", "gbd_cause_name", "dex_cause_id", "dex_cause_name", "sex_id"), all=TRUE)


#age-group 65
#Summarizing over years to get average of 1996 and 2016 values
m_years_65 <- mean_ui_dt_65[, c("cause_id", "year", "mean_spending_effect", "l_spending_effect", "u_spending_effect", "mean_ex_effect", "l_ex_effect", "u_ex_effect")]

m_years_65 <- m_years_65 %>%
  filter(year %in% c("1996", "2016")) %>% # apply filter condition
  group_by(cause_id) %>%
  summarize(across(c(mean_spending_effect, l_spending_effect, u_spending_effect, mean_ex_effect, l_ex_effect, u_ex_effect),mean))

mean_ui_dt_65 <- merge(mean_ui_dt_65, m_years_65, by=c("cause_id"), all=TRUE)

#Dropping redundant rows(one year from the dataset)
mean_ui_dt_65<-mean_ui_dt_65[!(mean_ui_dt_65$year=="2016"),]

#Drop the redundant column
mean_ui_dt_65 <- subset(mean_ui_dt_65, select = c(cause_id, gbd_cause_name, dex_cause_id, dex_cause_name, sex_id, mean_spending_effect.y, l_spending_effect.y, u_spending_effect.y, mean_ex_effect.y, l_ex_effect.y, u_ex_effect.y))

#renaming variables to _65 
mean_ui_dt_65 <- mean_ui_dt_65 %>% 
  rename(
    mean_spending_effect_65 = mean_spending_effect.y, 
    l_spending_effect_65 = l_spending_effect.y, 
    u_spending_effect_65 = u_spending_effect.y, 
    mean_ex_effect_65 = mean_ex_effect.y, 
    l_ex_effect_65 = l_ex_effect.y, 
    u_ex_effect_65 = u_ex_effect.y
  )

#exporting dataset containing year averaged results for age-group 65
fwrite(mean_ui_dt_65, "./years_averaged/spending_decomp_results_mean_at65_yr_avg.csv")


#merging with dataset delta_mean_ui_dt to include all age-group variables
delta_mean_ui_dt <- merge(delta_mean_ui_dt, mean_ui_dt_65, by=c("cause_id", "gbd_cause_name", "dex_cause_id", "dex_cause_name", "sex_id"), all=TRUE)


#age-group 75
#Summarizing over years to get average of 1996 and 2016 values
m_years_75 <- mean_ui_dt_75[, c("cause_id", "year", "mean_spending_effect", "l_spending_effect", "u_spending_effect", "mean_ex_effect", "l_ex_effect", "u_ex_effect")]

m_years_75 <- m_years_75 %>%
  filter(year %in% c("1996", "2016")) %>% # apply filter condition
  group_by(cause_id) %>%
  summarize(across(c(mean_spending_effect, l_spending_effect, u_spending_effect, mean_ex_effect, l_ex_effect, u_ex_effect),mean))

mean_ui_dt_75 <- merge(mean_ui_dt_75, m_years_75, by=c("cause_id"), all=TRUE)

#Dropping redundant rows(one year from the dataset)
mean_ui_dt_75<-mean_ui_dt_75[!(mean_ui_dt_75$year=="2016"),]

#Drop the redundant column
mean_ui_dt_75 <- subset(mean_ui_dt_75, select = c(cause_id, gbd_cause_name, dex_cause_id, dex_cause_name, sex_id, mean_spending_effect.y, l_spending_effect.y, u_spending_effect.y, mean_ex_effect.y, l_ex_effect.y, u_ex_effect.y))

#renaming variables to _75 
mean_ui_dt_75 <- mean_ui_dt_75 %>% 
  rename(
    mean_spending_effect_75 = mean_spending_effect.y, 
    l_spending_effect_75 = l_spending_effect.y, 
    u_spending_effect_75 = u_spending_effect.y, 
    mean_ex_effect_75 = mean_ex_effect.y, 
    l_ex_effect_75 = l_ex_effect.y, 
    u_ex_effect_75 = u_ex_effect.y
  )

#exporting dataset containing year averaged results for age-group 75
fwrite(mean_ui_dt_75, "./years_averaged/spending_decomp_results_mean_at75_yr_avg.csv")

#merging with dataset delta_mean_ui_dt to include all age-group variables
delta_mean_ui_dt <- merge(delta_mean_ui_dt, mean_ui_dt_75, by=c("cause_id", "gbd_cause_name", "dex_cause_id", "dex_cause_name", "sex_id"), all=TRUE)

#age-group 85
#Summarizing over years to get average of 1996 and 2016 values
m_years_85 <- mean_ui_dt_85[, c("cause_id", "year", "mean_spending_effect", "l_spending_effect", "u_spending_effect", "mean_ex_effect", "l_ex_effect", "u_ex_effect")]

m_years_85 <- m_years_85 %>%
  filter(year %in% c("1996", "2016")) %>% # apply filter condition
  group_by(cause_id) %>%
  summarize(across(c(mean_spending_effect, l_spending_effect, u_spending_effect, mean_ex_effect, l_ex_effect, u_ex_effect),mean))

mean_ui_dt_85 <- merge(mean_ui_dt_85, m_years_85, by=c("cause_id"), all=TRUE)

#Dropping redundant rows(one year from the dataset)
mean_ui_dt_85<-mean_ui_dt_85[!(mean_ui_dt_85$year=="2016"),]

#Drop the redundant column
mean_ui_dt_85 <- subset(mean_ui_dt_85, select = c(cause_id, gbd_cause_name, dex_cause_id, dex_cause_name, sex_id, mean_spending_effect.y, l_spending_effect.y, u_spending_effect.y, mean_ex_effect.y, l_ex_effect.y, u_ex_effect.y))

#renaming variables to _85 
mean_ui_dt_85 <- mean_ui_dt_85 %>% 
  rename(
    mean_spending_effect_85 = mean_spending_effect.y, 
    l_spending_effect_85 = l_spending_effect.y, 
    u_spending_effect_85 = u_spending_effect.y, 
    mean_ex_effect_85 = mean_ex_effect.y, 
    l_ex_effect_85 = l_ex_effect.y, 
    u_ex_effect_85 = u_ex_effect.y
  )

#exporting dataset containing year averaged results for age-group 85
fwrite(mean_ui_dt_85, "./years_averaged/spending_decomp_results_mean_at85_yr_avg.csv")

#merging with dataset delta_mean_ui_dt to include all age-group variables
delta_mean_ui_dt <- merge(delta_mean_ui_dt, mean_ui_dt_85, by=c("cause_id", "gbd_cause_name", "dex_cause_id", "dex_cause_name", "sex_id"), all=TRUE)


#differences in spending-effects for age-groups

delta_mean_ui_dt$d_mean_spending_effect_0_15 <- delta_mean_ui_dt$mean_spending_effect_0 - delta_mean_ui_dt$mean_spending_effect_15
delta_mean_ui_dt$d_mean_spending_effect_15_30 <- delta_mean_ui_dt$mean_spending_effect_15 - delta_mean_ui_dt$mean_spending_effect_30
delta_mean_ui_dt$d_mean_spending_effect_30_45 <- delta_mean_ui_dt$mean_spending_effect_30 - delta_mean_ui_dt$mean_spending_effect_45
delta_mean_ui_dt$d_mean_spending_effect_45_55 <- delta_mean_ui_dt$mean_spending_effect_45 - delta_mean_ui_dt$mean_spending_effect_55
delta_mean_ui_dt$d_mean_spending_effect_55_65 <- delta_mean_ui_dt$mean_spending_effect_55 - delta_mean_ui_dt$mean_spending_effect_65
delta_mean_ui_dt$d_mean_spending_effect_65_75 <- delta_mean_ui_dt$mean_spending_effect_65 - delta_mean_ui_dt$mean_spending_effect_75
delta_mean_ui_dt$d_mean_spending_effect_75_85 <- delta_mean_ui_dt$mean_spending_effect_75 - delta_mean_ui_dt$mean_spending_effect_85

#difference in ex-effect for age-groups

delta_mean_ui_dt$d_mean_ex_effect_0_15 <- delta_mean_ui_dt$mean_ex_effect_0 - delta_mean_ui_dt$mean_ex_effect_15
delta_mean_ui_dt$d_mean_ex_effect_15_30 <- delta_mean_ui_dt$mean_ex_effect_15 - delta_mean_ui_dt$mean_ex_effect_30
delta_mean_ui_dt$d_mean_ex_effect_30_45 <- delta_mean_ui_dt$mean_ex_effect_30 - delta_mean_ui_dt$mean_ex_effect_45
delta_mean_ui_dt$d_mean_ex_effect_45_55 <- delta_mean_ui_dt$mean_ex_effect_45 - delta_mean_ui_dt$mean_ex_effect_55
delta_mean_ui_dt$d_mean_ex_effect_55_65 <- delta_mean_ui_dt$mean_ex_effect_55 - delta_mean_ui_dt$mean_ex_effect_65
delta_mean_ui_dt$d_mean_ex_effect_65_75 <- delta_mean_ui_dt$mean_ex_effect_65 - delta_mean_ui_dt$mean_ex_effect_75
delta_mean_ui_dt$d_mean_ex_effect_75_85 <- delta_mean_ui_dt$mean_ex_effect_75 - delta_mean_ui_dt$mean_ex_effect_85

#keeping only delta values (i.e., differences in effects between age-groups)
delta_mean_ui_dt = subset(delta_mean_ui_dt, select = c(cause_id, gbd_cause_name, dex_cause_id, dex_cause_name, sex_id,
                                                       d_mean_spending_effect_0_15, d_mean_spending_effect_15_30, d_mean_spending_effect_30_45, d_mean_spending_effect_45_55, d_mean_spending_effect_55_65, d_mean_spending_effect_65_75, d_mean_spending_effect_75_85,
                                                       d_mean_ex_effect_0_15, d_mean_ex_effect_15_30, d_mean_ex_effect_30_45, d_mean_ex_effect_45_55, d_mean_ex_effect_55_65, d_mean_ex_effect_65_75, d_mean_ex_effect_75_85))

fwrite(delta_mean_ui_dt, "./spending_decomp_mean_draw_effects_age_grp.csv")
