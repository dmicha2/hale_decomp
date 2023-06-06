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
  # dt[age == terminal_age, dx := lx]
  dt[age == max(age), dx := lx, by = id_vars]
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
  # dt[age == terminal_age, nLx := lx/mx]
  dt[age == max(age), nLx := lx/mx]
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
  # data_table[age == max(age), qx := 1]
  data_table[age_group_id == 160, qx := 1]
  
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

#old file
#data_t = fread("./spending_cause_replaced_mort_rates.csv")

#updated aggregated gdp dex causes file
data_t = fread("./spending_cause_replaced_mort_rates_dex_gdp_aggregated_death_agg.csv")

setnames(data_t, "age_group_years_start", "age")
# add the age-group-length variable, in place, into our data table
gen_age_length(data_t, terminal_age = max(data_t$age))

id_vars <- c("cause_id", "draw", "sex_id", "year", "age_group_id", "age_length", "age")

# Using Lasanthi's suggestion line 117 was replaced with line 118.
# setnames(data_t, c("expenditure", "ac_spending"), c("cs_expenditure", "expenditure"))
#setnames(data_t, "expenditure", "cs_expenditure")

dt <- melt(data_t, id.vars = id_vars, measure.vars=c("ac_mr", "cr_ac_spending", "cr_ac_mr", "ac_spending"))
dt[, cause_replaced := as.integer(variable %like% "^cr")]
dt[, variable := ifelse(variable %like% "mr$", "mx", "ac_spending")]
dt <- dcast(dt, as.formula(paste0(paste(c(id_vars, "cause_replaced"), collapse=" + "), " ~ variable")),
            value.var = "value")

dt <- dt[, .(cause_id, draw, sex_id, year, cause_replaced, age_group_id, age, age_length, mx, ac_spending)]
id_vars <- setdiff(c(id_vars, "cause_replaced"), c("age_group_id", "age_length", "age"))

setorderv(dt, c(id_vars, "age"))

#### TEST ####
dt_TEST_995 <- dt[cause_id == 995, ]
setorderv(dt_TEST_995, setdiff(id_vars, "age"))
lt_TEST_995 <- get_full_life_table(dt_TEST_995, by_vars = id_vars)
setorderv(lt_TEST_995, setdiff(id_vars, "age"))

lt <- get_full_life_table(dt, by_vars = id_vars)
#### TESTING ####
DT <- lt[is.na(ex)]

###Variable def: 
##haL: avg exp /person * no. of person years lived between age x&n
##lt_spending: cum sum of everyone older than x /no. of survivors beyond x

lt[, c("ax", "qx", "px", "dx", "Tx", "age_length", "mx") := NULL]
dt[, c("ax", "mx", "qx") := NULL]
dt <- merge(lt, dt, by=c(id_vars, "age_group_id", "age"))

dt <- merge(dt, data_t[, c(setdiff(id_vars, "cause_replaced"), "age_group_id", "cases", "population"), with=FALSE],
            by=c(setdiff(id_vars, "cause_replaced"), "age_group_id"))
dt[, haL := nLx * (ac_spending / population)]

#dataframe for lt_spending
setorderv(dt, c(id_vars, "age"), order = -1)
dt[, lt_spending := cumsum(haL)/lx, by=id_vars]

setorderv(dt, c(id_vars, "age"), order = 1)

id_vars <- setdiff(id_vars, "cause_replaced")

dt <- dcast(dt,
            as.formula(paste0(paste(c(id_vars, "age_group_id"), collapse=" + "), " ~ cause_replaced")),
            value.var=c("ex", "lt_spending", "ac_spending"))

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
    cr_ac_spending = ac_spending_cause_replaced
  )


setcolorder(dt, c("cause_id", "gbd_cause_name", "dex_cause_id", "dex_cause_name",
                  "draw", "sex_id", "year", "age_group_id",
                  "lt_spending", "cr_lt_spending", 
                  "ex", "cr_ex", "ac_spending", "cr_ac_spending"))

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

