library(data.table)

res_path <- "/home/stefanin/nemo/nemo_files/nemoage0.32.6b/results"

############## combine data and calculate productivity ##########

dt1 <- readRDS(file.path(res_path, "Demo_data_processed.RDS"))
dt2 <- readRDS(file.path(res_path, "Fit_data_processed.RDS")) ## individual-level data
dt3 <- readRDS(file.path(res_path, "Quanti_data_processed.RDS"))  ## individual-level data


## correct runs!!!
dt2[configuration == "No introduction", run := "r01"]
dt3[configuration == "No introduction", run := "r01"]

# create run+ replicate id and supset for adults
dt1$replicate2 <- paste0(dt1$run, "_", dt1$replicate)
dt2$replicate2 <- paste0(dt2$run, "_", dt2$replicate)
dt3$replicate2 <- paste0(dt3$run, "_", dt3$replicate)

dt1_ad <- dt1[age_class == "Stage 3"]
dt2_ad <- dt2[age_class == 3]
dt3_ad <- dt3[age_class == 3]

# median fitness across the landscape ---> from individual level to stand level
dt2_median <- dt2_ad[, .(
  W = median(W)
), by = .(configuration, proportion_orientalis,selection_type, selection_strength,year, age_class, replicate2)]


# hybrid proportion per replicate ---> from individual level to stand level
dt3_prop <- dt3_ad[, .(
  prop_hybrid = mean(P1 >= -0.9 & P1 <= 0.9)
), by = .(configuration, proportion_orientalis,selection_type, selection_strength, year, age_class, replicate2)]


# merge 
dt_merged <- Reduce(function(x, y) merge(x, y,
                                         by = c("configuration", "proportion_orientalis","selection_type", "selection_strength","replicate2", "year")),
                    list(
                      dt1_ad[, .(configuration, proportion_orientalis,selection_type, selection_strength,replicate2,year, N_stage)],
                      dt2_median[, .(configuration, proportion_orientalis,selection_type, selection_strength,replicate2,year, W = W)],
                      dt3_prop[, .(configuration, proportion_orientalis,selection_type, selection_strength,replicate2,year, Hyb_proportion = prop_hybrid)] 
                    ))


# calculate productivity
dt_merged[, NW := N_stage * W]


# extract the final value of NW at equilibrium (as the reference == 100% productivity)
dt_final_NW <- dt_merged[, .SD[which.max(year)], 
                       by = .(configuration, proportion_orientalis,selection_type, selection_strength,replicate2)]

dt_final_NW <- dt_final_NW[, .(
  configuration, proportion_orientalis,selection_type, selection_strength,replicate2,NW_final = NW
)]


# attach back
dt_merged2 <- merge(dt_merged, dt_final_NW,
                 by = c("configuration", "proportion_orientalis","selection_type", "selection_strength","replicate2"))

## calculate NW relative to the final NW at equilibrium (= within scneario dynamics = how fast the system reaches its own equilibrium)
dt_merged2[, NW_rel := NW / NW_final]



## calculate NW (and hybrid proportion) relative to the neutral baseline ( = effect of selection relative to neutral expectation)
# extract NW neutral baseline and merge it back
dt_neutral_NW <- dt_merged2[
  selection_type == "Neutral",   
  .(configuration, proportion_orientalis, replicate2,year, NW_neutral = NW)
]

dt_neutral_hyb <- dt_merged2[
  selection_type == "Neutral",   
  .(configuration, proportion_orientalis, replicate2,year, Hyb_neutral = Hyb_proportion)
]

dt_merged3 <- merge(dt_merged2,dt_neutral_NW,
                   by = c("configuration", "proportion_orientalis", "replicate2", "year"),
                   all.x = TRUE
)

dt_merged4 <- merge(dt_merged3,dt_neutral_hyb,
                   by = c("configuration", "proportion_orientalis", "replicate2","year"),
                   all.x = TRUE
)


# compute corrected NW and clean from edge cases
dt_merged4[, NW_corr_neutral := NW / NW_neutral]
dt_merged4[, Hyb_corr_neutral := Hyb_proportion / Hyb_neutral]


rm(dt1, dt2, dt3, dt1_ad, dt2_ad, dt3_ad)
rm(dt_merged, dt_merged2, dt_merged3)

## save dataset
saveRDS(dt_merged4, file.path(res_path, "Final_dataset_replicate_level.RDS"))

