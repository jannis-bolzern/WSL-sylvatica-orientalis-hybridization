#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# 10_calculating_pareto_frontier.R
#
# 1. Load simulation outputs and cost table
# 2. Summarise replicate-level outputs by strategy and year
# 3. Identify Pareto-optimal strategies
# 4. Save Pareto-front dataset for plotting
#
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table)
  library(stringr)
  library(ggplot2)
  library(terra)
  library(ggh4x)
  library(ggpubr)
  library(patchwork)
  library(readr)
  library(readxl)
  library(dplyr)
  library(forcats)
  library(scales)
  library(tidyr)
  library(forcats)
})



## load data
dt <- readRDS(file.path(res_path, "Final_dataset_replicate_level.RDS"))
cost_table <- fread(file.path(res_path,"Cost_design_table.csv"))
cost_table$proportion_orientalis <- factor(cost_table$proportion_orientalis)

dt_merged <- merge(
  dt,
  cost_table[, .(configuration, proportion_orientalis, cost = estimated_cost)],
  by = c("configuration", "proportion_orientalis"),
  all.x = TRUE
)


## calculate median 
dt_merged_summary <- dt_merged[, .(
  med_NW = median(NW, na.rm = TRUE),
  med_hyb_prop = median(Hyb_proportion, na.rm = TRUE),
  med_cost = median(cost)
), by = .(selection_type, selection_strength, configuration, proportion_orientalis, year)]

# replace NA in No introduction with 0
dt_merged_summary[configuration == "No introduction" & is.na(med_NW), med_NW := 0]

## calculate pareto frontier for each year  
dt_merged_summary[, dominated := FALSE]

dt_merged_summary[, dominated := sapply(1:.N, function(i) {
  any(
    med_NW >= med_NW[i] &
      med_hyb_prop >= med_hyb_prop[i] &
      med_cost <= med_cost[i] &
      (med_NW > med_NW[i] |
         med_hyb_prop > med_hyb_prop[i] |
         med_cost < med_cost[i])
  )
}), by = .(selection_type, selection_strength, year)]

best_str <- dt_merged_summary[dominated == FALSE]

# give a strategy ID
best_str <- best_str %>%dplyr::mutate( strategy_id = paste(configuration,proportion_orientalis, sep = "_") )

# detect gaps in the years (some strategies are good only in certain years)
best_str2 <- best_str %>%
  dplyr::arrange(selection_type, selection_strength, strategy_id, year) %>%
  dplyr::group_by(strategy_id) %>%
  dplyr::mutate(
    year_diff = year - dplyr::lag(year),
    new_segment = ifelse(is.na(year_diff) | year_diff > 20, 1, 0),
    segment_id = cumsum(new_segment)
  ) %>%
  dplyr::ungroup()


saveRDS(best_str2, file.path(res_path, "Pareto_frontier_by_year.RDS"))