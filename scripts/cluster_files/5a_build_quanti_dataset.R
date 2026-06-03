
library(data.table)
library(stringr)

# reading and parsing all quanti files and combining into two big datasets
# summarizing at whole scneario level and then at patch level
# outputs : Quanti_data.RDS


res_path <- "/home/stefanin/nemo/nemo_files/nemoage0.32.6b/results/"


####### QUANTI FILES

parse_quanti_metadata <- function(path) {
  bn <- basename(path)
  
  configuration  <- str_match(bn, "^(.*?)_p")[,2]
  proportion_orientalis <- as.numeric(str_extract(bn, "(?<=_p)\\d+"))/ 100
  generation     <- as.numeric(str_extract(bn, "(?<=_)\\d{4}(?=_)"))
  replicate      <- as.numeric(str_extract(bn, "(?<=_)\\d+(?=\\.quanti$)"))
  run            <- str_match(bn, "_(r\\d+)_")[,2]   ## changed : before --> sub(".*\\/(r[0-9]+)\\/.*", "\\1", path)  
  sel_full       <- str_match(bn, "_r\\d+_(.*?)_k")[,2]
  sel_type       <- ifelse(sel_full == "neutral","neutral",str_extract(sel_full, "^(sel_[EO]|heterosis)"))
  sel_strength   <- ifelse(sel_full == "neutral",NA,str_extract(sel_full, "(low|mid|high)$"))
    
  data.table(
    file = path,
    configuration = configuration,
    proportion_orientalis = proportion_orientalis,
    selection = sel_full,
    selection_type = sel_type, 
    selection_strength = sel_strength,
    year = generation,
    replicate = replicate,
    run = run
  )
}


quanti_files <- list.files(
  path = res_path,
  recursive = TRUE,
  full.names = TRUE,
  pattern = "\\.quanti$"
)
# do not read burnin results
quanti_files <- quanti_files[!grepl("burnin", quanti_files)]


# build table sequentially
out_file <- file.path(res_path, "quanti_combined.csv")

# remove old file if exists
if (file.exists(out_file)) file.remove(out_file)

for (i in seq_along(quanti_files)) {

  f <- quanti_files[i]
  
  # read ONE file
  dt <- fread(f)
  
  # metadata
  meta <- parse_quanti_metadata(f)
  
  # attach metadata 
  for (col in names(meta)) {
    if (col != "file") dt[, (col) := meta[[col]]]
  }
  
  # drop useless cols
  dt[, c("sex","home","ped","isMigrant","father","mother") := NULL]
  
  # write to disk
  fwrite(dt, out_file, append = TRUE)
  
  # clean memory
  rm(dt)
  if (i %% 50 == 0) {
    gc()
    cat("Processed:", i, "/", length(quanti_files), "\n")
  }
}


# read output for post processing
quanti_data <- fread(out_file)
quanti_data_clean <- copy(quanti_data)

# convert types
quanti_data_clean[, selection_type := factor(selection_type, levels = c("neutral", "sel_E", "sel_O", "heterosis"))]
quanti_data_clean[, selection_strength := factor(selection_strength, levels = c("low", "mid", "high"))]
quanti_data_clean[, configuration := factor(configuration, levels = c("dispersed", "one_cluster", "multi_cluster", "transects", "no_introduction"))]
quanti_data_clean$sim_id <- with(quanti_data_clean,paste(configuration,proportion_orientalis,selection_type,selection_strength,sep = "_"))

saveRDS(quanti_data_clean, (file.path(res_path, "Quanti_data_raw.RDS")))

gc()



##########################################################################################################

## attach cost data to the configuration 
cost_table <- read.csv(file.path(res_path,"Cost_design_table.csv"))
setDT(quanti_data_clean)
setDT(cost_table)

quanti_data <- merge(
  quanti_data_clean,
  cost_table[, .(configuration, proportion_orientalis, cost = estimated_cost)],
  by = c("configuration", "proportion_orientalis"),
  all.x = TRUE
)

rm(quanti_data_clean)

## labels for plotting
sel_map <- c(
  heterosis = "Wf1 > Weu = Wori",
  neutral = "Neutral",
  sel_E   = "Wori > Wf1 > Weu",
  sel_O   = "Weu > Wf1 > Wori"
)
quanti_data[, selection_label := sel_map[as.character(selection_type)]]

config_map <- c(
  dispersed = "Dispersed",
  multi_cluster = "Multiple clusters",
  one_cluster   = "Single cluster",
  transects   = "Transects", 
  no_introduction = "No introduction"
)
quanti_data[, config_label := config_map[as.character(configuration)]]

# simulation ID 
quanti_data$sim_id <- with(quanti_data,paste(configuration,proportion_orientalis,selection_type,selection_strength,sep = "_"))

# tidy up cols

quanti_data <-quanti_data[, c("configuration","ID","selection","selection_type") := NULL]
colnames(quanti_data)
setcolorder(quanti_data, c("sim_id", "config_label","proportion_orientalis","cost","selection_label","selection_strength","run", "replicate","year", "stage",     "age", "pop", "P1"))
colnames(quanti_data)<-  c("sim_id", "configuration","proportion_orientalis","cost","selection_type","selection_strength","run", "replicate","year", "age_class", "age", "pop" ,"P1")
quanti_data[, year := as.numeric(year)]
quanti_data[, proportion_orientalis := factor(proportion_orientalis)]

# save 
saveRDS(quanti_data, (file.path(res_path, "Quanti_data_processed.RDS")))

##########################################################################################################

## calculate genotype proportions

quanti_data_genot <- quanti_data[, .(
  prop_orientalis = mean(P1 > 0.9),
  prop_sylvatica  = mean(P1 < -0.9),
  prop_hybrid     = mean(P1 >= -0.9 & P1 <= 0.9)
),
by = .(configuration, proportion_orientalis, cost, selection_type,selection_strength, year, age_class, run, replicate)]


# summarize across replicates using quantiles
quanti_data_genot_quantiles <- quanti_data_genot[, .(
  
  q10_ori = quantile(prop_orientalis, 0.1, na.rm = TRUE),
  q50_ori = quantile(prop_orientalis, 0.5, na.rm = TRUE),
  q90_ori = quantile(prop_orientalis, 0.9, na.rm = TRUE),
  
  q10_syl = quantile(prop_sylvatica, 0.1, na.rm = TRUE),
  q50_syl = quantile(prop_sylvatica, 0.5, na.rm = TRUE),
  q90_syl = quantile(prop_sylvatica, 0.9, na.rm = TRUE),
  
  q10_hyb = quantile(prop_hybrid, 0.1, na.rm = TRUE),
  q50_hyb = quantile(prop_hybrid, 0.5, na.rm = TRUE),
  q90_hyb = quantile(prop_hybrid, 0.9, na.rm = TRUE)
  
),
by = .(configuration,proportion_orientalis,cost,selection_type, selection_strength,age_class, year)]

saveRDS(quanti_data_genot_quantiles, (file.path(res_path, "Genot_proportions_summary_replicates.RDS")))


##########################################################################################################


### summarise patch level

# subset only some years 
quanti_data_subset <- quanti_data[year %in% c(50,100,500, 1000)]


# hybrid proportions and HI values per simulation and replicate and patch (across all individuals) = 1 row x sim replicate x year x age_class x patch
quanti_data_patch <- quanti_data_subset[,.( 
  N = .N,
  N_hybrid = sum(P1 > -1 & P1 < 1),
  prop_hybrid = mean(P1 > -1 & P1 < 1)
),
by = .(configuration, proportion_orientalis, selection_type, selection_strength, year, run, replicate, age_class, cost, pop) ]

gc()

## ---> to check for variability within patch (maybe not needed)
saveRDS(quanti_data_patch,file.path(res_path, "Hyb_proportions_patch.RDS") )

# summarize across run and then replicates per patch
quanti_data_patch_summary <- quanti_data_patch[, .(
  q10_hyb = quantile(prop_hybrid, 0.1, na.rm = TRUE),
  q50_hyb = quantile(prop_hybrid, 0.5, na.rm = TRUE),
  q90_hyb = quantile(prop_hybrid, 0.9, na.rm = TRUE)
),
by = .(configuration, proportion_orientalis, selection_type,selection_strength, year, age_class, cost, pop)]


# reorder proportion orientalis 
quanti_data_patch_summary[, proportion_orientalis :=  factor(proportion_orientalis, levels = rev(sort(unique(as.numeric(as.character(proportion_orientalis))))))]


saveRDS(quanti_data_patch_summary,file.path(res_path, "Hyb_proportions_patch_summary_replicates.RDS") )


############################### spatial patterns of % orientalis genotype


# select only some years (otherwise too slow)
quanti_data_subset <- quanti_data[year %in% c(50,100,500, 1000)]

## median P1 per patch (across individuals)
p1_patch <- quanti_data_subset[, .(
  med_p1 = median(P1), 
  q25_p1= quantile(P1, 0.25),
  q75_p1 = quantile(P1, 0.75)
),
by = .(configuration, proportion_orientalis, selection_type,selection_strength, year, age_class, cost, pop, run, replicate)]

# summarise per patch across replicates 
p1_patch_summary <- p1_patch[, .(
  mean_p1 = mean(med_p1),
  sd_p1   = sd(med_p1),
  q10_p1 = quantile(med_p1, 0.1, na.rm = TRUE),
  q50_p1 = quantile(med_p1, 0.5, na.rm = TRUE),
  q90_p1 = quantile(med_p1, 0.9, na.rm = TRUE)
),
by = .(configuration, proportion_orientalis, selection_type,selection_strength, year, age_class, cost, pop)]


# reorder proportion orientalis 
p1_patch_summary[, proportion_orientalis :=  factor(proportion_orientalis, levels = rev(sort(unique(as.numeric(as.character(proportion_orientalis))))))]

saveRDS(p1_patch_summary,file.path(res_path, "Orientalis_genot_patch_summary_replicates.RDS") )



