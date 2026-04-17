

library(data.table)
library(stringr)

# reading and parsing all demo files (.txt) and combining into one dataset
# summarizing data
# outputs : Demo_data.RDS


####### DEMOGRAPHIC DATA

## parse metadata from filename
parse_demo_metadata <- function(path) {
  
  bn <- basename(path)
  
  # from filename 
  configuration  <- str_match(bn, "^(.*?)_p")[,2]
  proportion_orientalis <-  as.numeric(str_extract(bn, "(?<=_p)\\d+"))/ 100
  run            <- str_match(bn, "_(r\\d+)_")[,2]
  sel_full       <- str_match(bn, "_r\\d+_(.*?)_k")[,2]
  sel_type       <- ifelse(sel_full == "neutral","neutral",str_extract(sel_full, "^(sel_[EO]|heterosis)"))
  sel_strength   <- ifelse(sel_full == "neutral",NA,str_extract(sel_full, "(low|mid|high)$"))
  
  k <- as.numeric(str_match(bn, "_k([0-9.]+)")[,2])
  b <- as.numeric(str_match(bn, "_b([0-9]+\\.?[0-9]*)(?=\\.txt$)")[,2])
  
  data.table(
    file = path,
    configuration = configuration,
    proportion_orientalis = proportion_orientalis,
    selection = sel_full,
    selection_type = sel_type, 
    selection_strength = sel_strength,
    run = run,
    k = k,
    b = b
  )
}

## get demographic files
res_path <- "/home/stefanin/nemo/nemo_files/nemoage0.32.6b/results/"
file_pattern <- "*.txt$"

demo_files <- list.files(
  path = res_path,
  recursive = TRUE,
  full.names = TRUE,
  pattern = file_pattern
)

# do not read the _bygen files
demo_files <- demo_files[!grepl("bygen", demo_files)]

# do not read burnin results
demo_files <- demo_files[!grepl("burnin", demo_files)]

## build metadata table
meta_dt <- rbindlist(
  lapply(demo_files, parse_demo_metadata),
  fill = TRUE
)

## read and process demographic files
cols_to_read = c("replicate", "generation", "pop.tot", "a0.tot", "a1.tot","a2.tot", "a3.tot")
demo_data <- rbindlist(
  lapply(meta_dt$file, function(f) {
    
    dt <- fread(f, select = cols_to_read)
    dt[, file := f]
    
    melt(
      dt,
      id.vars = c("replicate", "generation", "file"),
      measure.vars = patterns(".tot"),
      variable.name = "stage",
      value.name = "N_stage"
    )
  }),
  fill = TRUE
)

## merge metadata
combined_pop_data <- merge(
  demo_data,
  meta_dt,
  by = "file",
  all.x = TRUE
)


## get a unique sim ID
combined_pop_data[, sim_id := paste(configuration, proportion_orientalis,selection_type,selection_strength, k, b, sep = "_")]
# convert to factors
combined_pop_data[, selection_type := factor(selection_type, levels = c("neutral", "sel_E", "sel_O", "heterosis"))]
combined_pop_data[, selection_strength := factor(selection_strength, levels = c("low", "mid", "high"))]
combined_pop_data[, configuration := factor(configuration, levels = c("dispersed", "one_cluster", "multi_cluster", "transects"))]

# convert generation column to year
setnames(combined_pop_data, "generation", "year")

##### checks 
# number of runs per combination 
aggregate(run ~ configuration + proportion_orientalis  + selection_type + selection_strength,
          data = combined_pop_data,
          FUN = function(x) length(unique(x)))

unique(combined_pop_data[, .(selection, selection_type, selection_strength)])

# check for NA values
anyNA(combined_pop_data)
colSums(is.na(combined_pop_data))

# check number of replicates
rep_check <- aggregate(replicate ~ sim_id,data = combined_pop_data,FUN = function(x) length(unique(x)))
rep_check

saveRDS(combined_pop_data, file = file.path(res_path,"Demo_data_raw.RDS"))

## labels for plotting
setDT(combined_pop_data)

sel_map <- c(
  heterosis = "Heterosis",
  neutral = "Neutral",
  sel_E   = "European b. selected against",
  sel_O   = "Oriental b. selected against"
)
combined_pop_data[, selection_label := sel_map[as.character(selection_type)]]

config_map <- c(
  dispersed = "Dispersed",
  multi_cluster = "Multiple clusters",
  one_cluster   = "Single cluster",
  transects   = "Transects"
)
combined_pop_data[, config_label := config_map[as.character(configuration)]]

stage_map <- c(
  pop.tot = "Total Population",
  a0.tot = "Stage 0", 
  a1.tot = "Stage 1", 
  a2.tot = "Stage 2", 
  a3.tot = "Stage 3")

combined_pop_data[, stage_label := stage_map[as.character(stage)]]


# tidy up cols
combined_pop_data <-combined_pop_data[, c("file", "configuration","selection","stage", "selection_type") := NULL]
setcolorder(combined_pop_data, c("sim_id", "config_label","proportion_orientalis", "selection_label", "selection_strength","run","replicate","year", "stage_label","k","b","N_stage"))
colnames(combined_pop_data)<-c("sim_id", "configuration","proportion_orientalis","selection_type","selection_strength", "run","replicate", "year", "age_class" ,"k","b","N_stage" )
combined_pop_data[, year := as.numeric(year)]
combined_pop_data[, proportion_orientalis := factor(proportion_orientalis)]

## save master file
saveRDS(combined_pop_data, file = file.path(res_path,"Demo_data_processed.RDS"))
rm(demo_data, demo_files)

###############################################################################################################

## summarize across replicates
combined_pop_data_summary <- combined_pop_data[
  , .(
    mean_N = mean(N_stage, na.rm = TRUE),
    sd_N   = sd(N_stage, na.rm = TRUE), 
    q10_N = quantile(N_stage, 0.1, na.rm = TRUE),
    q50_N = quantile(N_stage, 0.5, na.rm = TRUE),
    q90_N = quantile(N_stage, 0.9, na.rm = TRUE)
  ),
  by = .(configuration,proportion_orientalis,selection_type,selection_strength,year,age_class
  )
]

write.csv(combined_pop_data_summary, file.path(res_path, "Demo_data_summary_replicates.csv"))


##########################################################################################################


### summarise patch level

# subset only some years 
combined_pop_data_subset <- combined_pop_data[year %in% c(50,100,500,1000)]


combined_pop_data_subset_patch <- combined_pop_data_subset[
  , .(
    mean_N = mean(N_stage, na.rm = TRUE),
    sd_N   = sd(N_stage, na.rm = TRUE), 
    q10_N = quantile(N_stage, 0.1, na.rm = TRUE),
    q50_N = quantile(N_stage, 0.5, na.rm = TRUE),
    q90_N = quantile(N_stage, 0.9, na.rm = TRUE)
  ),
  by = .(configuration,proportion_orientalis,selection_type,selection_strength,year,age_class,pop,replicate,run
  )
]

gc()

## ---> to check for variability within patch (maybe not needed)
saveRDS(combined_pop_data_subset_patch,file.path(res_path, "N_stage_patch.RDS") )

# summarize across replicates
combined_pop_data_subset_patch_summary <- combined_pop_data_subset_patch[, .(
  q10_hyb = quantile(prop_hybrid, 0.1, na.rm = TRUE),
  q50_hyb = quantile(prop_hybrid, 0.5, na.rm = TRUE),
  q90_hyb = quantile(prop_hybrid, 0.9, na.rm = TRUE)
),
by = .(configuration, proportion_orientalis, selection_type,selection_strength, year, age_class, cost, pop)]


# reorder proportion orientalis 
combined_pop_data_subset_patch_summary[, proportion_orientalis :=  factor(proportion_orientalis, levels = rev(sort(unique(as.numeric(as.character(proportion_orientalis))))))]


saveRDS(combined_pop_data_subset_patch_summary,file.path(res_path, "N_stage_patch_quantile_replicates.RDS") )




