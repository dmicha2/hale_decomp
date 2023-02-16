library(data.table)
setwd("/home/j/Project/Cost_Effectiveness/BEA/BEA_data_2023/HALE")
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
  data_table[age == max(age), qx := 1]
  
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
data_t = fread("./cause_replaced_mort_rates.csv")

setnames(data_t, "age_group_years_start", "age")
# add the age-group-length variable, in place, into our data table
gen_age_length(data_t, terminal_age = max(data_t$age))

id_vars <- c("cause_id", "draw", "sex_id", "year", "age_group_id", "age_length", "age")

# Lasanthi suggested to replaced line 115 with line 116.
# setnames(data_t, c("ylds", "all_cause_ylds"), c("cs_ylds", "ylds"))
setnames(data_t, "ylds", "cs_ylds")

dt <- melt(data_t, id.vars = id_vars, measure.vars=c("acmr", "cr_ylds", "crmr", "all_cause_ylds"))
dt[, cause_replaced := as.integer(variable %like% "^cr")]
dt[, variable := ifelse(variable %like% "mr$", "mx", "all_cause_ylds")]
dt <- dcast(dt, as.formula(paste0(paste(c(id_vars, "cause_replaced"), collapse=" + "), " ~ variable")),
            value.var = "value")

dt <- dt[, .(cause_id, draw, sex_id, year, cause_replaced, age_group_id, age, age_length, mx, all_cause_ylds)]
id_vars <- setdiff(c(id_vars, "cause_replaced"), c("age_group_id", "age_length", "age"))

setorderv(dt, c(id_vars, "age"))

lt <- get_full_life_table(dt, by_vars = id_vars)

lt[, c("ax", "qx", "px", "dx", "Tx", "age_length", "mx") := NULL]
dt[, c("ax", "mx", "qx") := NULL]
dt <- merge(lt, dt, by=c(id_vars, "age_group_id", "age"))

dt <- merge(dt, data_t[, c(setdiff(id_vars, "cause_replaced"), "age_group_id", "cases", "population"), with=FALSE],
            by=c(setdiff(id_vars, "cause_replaced"), "age_group_id"))
dt[, haL := nLx * (1 - (all_cause_ylds / population))]

setorderv(dt, c(id_vars, "age"), order = -1)
dt[, hale := cumsum(haL)/lx, by=id_vars]
setorderv(dt, c(id_vars, "age"), order = 1)

id_vars <- setdiff(id_vars, "cause_replaced")

dt <- dcast(dt,
            as.formula(paste0(paste(c(id_vars, "age_group_id"), collapse=" + "), " ~ cause_replaced")),
            value.var=c("ex", "hale", "all_cause_ylds"))

setnames(dt, names(dt), gsub("_1$", "_cause_replaced", names(dt)))
setnames(dt, names(dt), gsub("_0$", "", names(dt)))

input_data <- fread("./hale_decomp_input_data_final.csv")

dt <- merge(dt, unique(input_data[, .(cause_id, gbd_cause_name, dex_cause_id, dex_cause_name)]),
            by="cause_id")
setcolorder(dt, c("cause_id", "gbd_cause_name", "dex_cause_id", "dex_cause_name",
                  "draw", "sex_id", "year", "age_group_id",
                  "hale", "hale_cause_replaced", "ex", "ex_cause_replaced", "all_cause_ylds", "all_cause_ylds_cause_replaced"))
fwrite(dt, "./cause_replaced_life_table.csv")

dt <- dt[age_group_id == 0]
dt[, age_group_id := NULL]
dt[, hale_effect := hale_cause_replaced - hale]

fwrite(dt, "./decomp_results_by_draw.csv")

# creating a data frame containing draw 0 only.
dt_draw_0 <- dt[draw == 0, ]
fwrite(dt_draw_0, "./draw_0_decomp_results_by_draw.csv")

#dt <- dt[, lapply(.SD, mean), by=.(cause_id, gbd_cause_name, dex_cause_id, dex_cause_name, year),
#         .SDcols=c("hale_effect", "hale", "hale_cause_replaced", "ex", "ex_cause_replaced", "all_cause_ylds", "ylds_cause_replaced")]
