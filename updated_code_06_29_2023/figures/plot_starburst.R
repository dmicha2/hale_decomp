#figure Starburst



##================================================
## Prepare Data
##================================================


library(data.table)
library(dplyr)
setwd("/home/j/Project/Cost_Effectiveness/BEA/BEA_data_2023/data_new_naming_convention/figures/starburst/spending_not_cr_mr")


# ##################################### 
# ## Get decomp data ##
# #####################################
# 

# taking 1000 draw (draw_n/draw_n) of spending by hale effect estimates dataset

#Age 0
dt_0 <- fread("/home/j/Project/Cost_Effectiveness/BEA/BEA_data_2023/data_new_naming_convention//HALE_spending_combined/ac_lt_spend_by_hale_effect_non_cr_1000draw_at0.csv")

#Import dataet linking diseases with level_1 causes (communicable, non-communicable, injuries)
dt_lvl1_cause_link <- fread("/home/j/Project/Cost_Effectiveness/BEA/BEA_data_2023/data_new_naming_convention/lvl_1_cause_link.csv")

dt_lvl1_cause_link <- dt_lvl1_cause_link %>% 
  rename(lvl1_link_gbd_cause_name = gbd_cause_name,
         cause_id = gbd_cause_id)

dt_lvl1_cause_link <- dt_lvl1_cause_link %>%dplyr::mutate(cause_id = as.character(cause_id))

dt <- merge(dt_lvl1_cause_link, dt_0, by=c("cause_id"), all=TRUE)

#subset data to exclude rows where we do not have corresponding cause_id from our hale/spending dataset
MISSING <- is.na(dt$dex_cause_id)

# Count the number of rows flagged for deletion
sum(MISSING)

# Use ! to include those that are NOT missing
dt <- subset(dt, subset = !MISSING)

# Count the number of rows kept
nrow(dt)

# Manually link diseases that were manually changed to link DEX and GBD ()

#dex_3020 (Diarrheal diseases)
dt$lvl_1_cause_id[dt$dex_cause_id == 3020] <- "295"
dt$lvl_1_cause_name[dt$dex_cause_id == 3020] <- "Communicable, maternal, neonatal, and nutritional diseases"

#dex_318 (Intestinal infectious diseases)
dt$lvl_1_cause_id[dt$dex_cause_id == 318] <- "295"
dt$lvl_1_cause_name[dt$dex_cause_id == 318] <- "Communicable, maternal, neonatal, and nutritional diseases"

#dex_371 (Maternal abortion, miscarriage, and ectopic pregnancy)
dt$lvl_1_cause_id[dt$dex_cause_id == 371] <- "295"
dt$lvl_1_cause_name[dt$dex_cause_id == 371] <- "Communicable, maternal, neonatal, and nutritional diseases"

#dex_488 (Other neoplasms)
dt$lvl_1_cause_id[dt$dex_cause_id == 488] <- "409"
dt$lvl_1_cause_name[dt$dex_cause_id == 488] <- "Non-communicable diseases"

#dex_5070 (Other cardiovascular and circulatory diseases)
dt$lvl_1_cause_id[dt$dex_cause_id == 5070] <- "409"
dt$lvl_1_cause_name[dt$dex_cause_id == 5070] <- "Non-communicable diseases"

#dex_5410 (Other digestive diseases)
dt$lvl_1_cause_id[dt$dex_cause_id == 5410] <- "409"
dt$lvl_1_cause_name[dt$dex_cause_id == 5410] <- "Non-communicable diseases"

#dex_5570 (Other neurological disorders)
dt$lvl_1_cause_id[dt$dex_cause_id == 5570] <- "409"
dt$lvl_1_cause_name[dt$dex_cause_id == 5570] <- "Non-communicable diseases"

#dex_629 (Low back and neck pain)
dt$lvl_1_cause_id[dt$dex_cause_id == 629] <- "409"
dt$lvl_1_cause_name[dt$dex_cause_id == 629] <- "Non-communicable diseases"

#dex_7160 (Other unintentional injuries)
dt$lvl_1_cause_id[dt$dex_cause_id == 7160] <- "687"
dt$lvl_1_cause_name[dt$dex_cause_id == 7160] <- "Injuries"

#dex_730 (Collective violence and legal intervention)
dt$lvl_1_cause_id[dt$dex_cause_id == 730] <- "687"
dt$lvl_1_cause_name[dt$dex_cause_id == 730] <- "Injuries"

#output the datafile used in creating the plot
fwrite(dt, "./non_cr_starburst_plot_data_age0.csv")

#to plot cube root values of median values of spending and hale effect
dt<- dt[, tr_m_spending_effect_0 := sign(median_corresponding_lt_spending_effect) * (abs(median_corresponding_lt_spending_effect))^(1/3)]
dt<- dt[, tr_m_hale_effect_0 := sign(median_corresponding_hale_effect) * (abs(median_corresponding_hale_effect))^(1/3)]

# assigning transformed values to xval and yval
dt <- dt[, yval := tr_m_spending_effect_0]
dt <- dt[, xval := tr_m_hale_effect_0]

# full_palette <- rev(c("#9E0142","#F46D43", "#c9aa3a", "#9fc45e","#66C2A5", "#3288BD", "#5E4FA2"))

#for 1996 data only
dt_1996 <- subset(dt, year == 1996)
#for 2016 data only
dt_2016 <- subset(dt, year == 2016)

#############################################
## Set color palette based on lvl_1 cause ##
#############################################
lvl_1_cause <- dt_1996$lvl_1_cause_name
full_palette <- rev(c("#9E0142","#66C2A5", "#5E4FA2"))
color_dt <- data.table(lvl_1_cause, full_palette)


###
##==========================================##
##               Plots                      ##
##==========================================##

leg_text <- 10
leg_tit <- 10
ax_text <- 10
ax_tit <- 11

## write plot func; pass in disease to subset disease and plot figures
plot_func <- function(x, logx = T, logy = T){{


    # ## change plot labels based on estimate
    
      x_lab <- "cube root of HALE effect (median of 1000 draws)"
      y_lab <- "cube root of Spending effect (median of 1000 draws)"
      tit <- "1"
      cap <- "Spending effect and HALE effect at age 0 (lifetime)"
    
}
  ## set palette
  colors <- color_dt[lvl_1_cause %in% unique(dt_1996$lvl_1_cause_name), full_palette]
  
 
  
  ## create plot
  p <- ggplot(data = dt_1996, aes(color = as.factor(lvl_1_cause_name))) + 
    geom_segment(data = dt_1996, aes(y = 0, yend = yval, x = 0, xend = xval),
                 alpha = .6, arrow = arrow(length = unit(.01, "npc")))
  
  
  p <- p + labs(x = x_lab, y = y_lab, title = cap, color = "Level 1 GBD Causes") +
    guides(color = guide_legend(ncol = 2, title.position = "top"))
  
  
  p <- p + theme_bw() + theme(panel.grid = element_blank(),
                                axis.text.x = element_text(size = ax_text, colour = "black"),
                                axis.text.y = element_text(size = ax_text, colour = "black"),
                                legend.title = element_text(size = leg_tit),
                                legend.text = element_text(size = leg_text, colour = "black"),
                                axis.title = element_text(size = ax_tit, colour = "black"),
                                panel.border = element_rect(colour = "black"),
                                legend.position = "bottom",
                                legend.justification = "top")
p <- p + scale_color_manual(values = colors)
  
  p <- p + geom_hline(yintercept = 0) + geom_vline(xintercept = 0)
  
  
  # for cube root transformation (with direct spending and hale effects)
  p <- p + coord_cartesian(ylim = c(-0.3, 0.4), xlim = c(-0.8, 0.7)) +
    scale_y_continuous(labels = comma_format(accuracy = .01)) + scale_x_continuous(labels = comma_format(accuracy = .01)) +

  # # for non cube root transformation (with direct spending and hale effects)
  # p <- p + coord_cartesian(ylim = c(-3.5e-03, 3e-02), xlim = c(-3.8e-01, 3.1e-01)) +
  #   scale_y_continuous(labels = comma_format(accuracy = .001)) + scale_x_continuous(labels = comma_format(accuracy = .1)) +

    # geom_text(data = dt, aes(x = xval, y = yval, label = cause_short), show.legend = F) + # added in this line to get all labels for the graph (for indivual quad labels use below lines)
    geom_text(data = dt_1996[cause_short %in% c("IHD", "Osteoarth", "Oral", "Skin", "Alzheimer's", "ADHD", "Aort An", "Diabetes", "Stroke", "HIV", "Colorect C")], aes(x = xval, y = yval, label = cause_short), show.legend = F, nudge_y = -0.006) +
    geom_text(data = dt_1996[cause_short %in% c("CMP", "Schiz", "Mat Late", "Uterus C", "PUD", "Breast C", "Prostate C")], aes(x = xval, y = yval, label = cause_short), show.legend = F, nudge_y = 0.006) +
    geom_text(data = dt_1996[cause_short %in% c("Drugs", "HTN HD", "F Body", "CKD", "Endocrine", "Iron", "Obst Labor", "Endocar")], aes(x = xval, y = yval, label = cause_short), show.legend = F, nudge_y = 0.008) +
    geom_text(data = dt_1996[cause_short %in% c("Alcohol", "PAD", "Parkinson's", "Tens Head", "Whooping", "AGN")], aes(x = xval, y = yval, label = cause_short), show.legend = F, nudge_y = -0.008) +

    # # for non cube root transformation (with direct spending and hale effects)
    # annotate("text", x = c(0.18, 0.18, -0.08, -0.08), y = c(0.022, -0.003, 0.022, -0.003), label = c("Increased spending;\nincreased HALE",
    #                                                                                                  "Decreased spending;\nincreased HALE",
    #                                                                                                  "Increased spending;\ndecreased HALE",
    #                                                                                                  "Decreased spending;\ndecreased HALE"))

  # for cube root transformation (with direct spending and hale effects)
  annotate("text", x = c(0.35, 0.3, -0.35, -0.3), y = c(0.35, -0.2, 0.35, -0.2), label = c("Increased spending;\nIncreased HALE",
                                                                                                   "Decreased spending;\nIncreased HALE",
                                                                                                   "Increased spending;\nDecreased HALE",
                                                                                                   "Decreased spending;\nDecreased HALE"))

  
  


  
  return(p)
}

  
  
  plot_dir <- "/homes/j/Project/Cost_Effectiveness/BEA/BEA_data_2023/data_new_naming_convention/figures/starburst/spending_not_cr_mr"
  
  starburst_plot <- plot_func()
  
  starburst <- dt_1996[, .(lvl_1_cause_name, median_corresponding_hale_effect = xval, median_corresponding_lt_spending_effect = yval)]
  
  starburst_plot
  
  ggsave("1996_median_1000draw_non_cr_starburst_plot_cube_root_cause_name.png", starburst_plot, width = 8, height = 8, dpi = 300)
  
  
  
  
                                
