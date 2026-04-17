

### the script: 
# creates a table for the estimated cost of introduction
# reads all demographics, fit and quanti files output of Nemo simulations (except the burnin)
# reads the metadata from file names and folder names
# keep only relevant columns, save into "Data_raw.RDS" objects
# tidies up column, chnage column labels for plotting, make the object lighter
# save the processed object as "Data_processed.RDS"
# summarize data across replicates at landscape level and save into "Data_summary_replicates.RDS"
# summarize data at patch level and save into "Data_patch.RDS"
# summarize data at patch level across replicate and saves into "Data_patch_summary_replicates.RDS"

library(data.table)
library(stringr)
library(tidyr)
library(dplyr)

res_path <- "/home/stefanin/nemo/nemo_files/nemoage0.32.6b/results/"


####### ESTIMATED COST TABLE ##############

cost_per_seedling <- 2

seedlings <- data.frame(proportion_orientalis = c("0.1", "0.25", "0.4"),
                        n_seedlings = c(1240, 3120, 5000))

cost_index <- data.frame(configuration = c("one_cluster","transects","multi_cluster","dispersed"),
                         cost_multiplier = c(1.00, 1.10,1.25,1.50))

# baseline cost (seedlings only)
seedlings$baseline_cost <- seedlings$n_seedlings * cost_per_seedling

# combine all and order by cost
final_cost_table <- expand_grid(seedlings, cost_index) %>%
  mutate(estimated_cost = baseline_cost * cost_multiplier) %>%
  dplyr::arrange(proportion_orientalis, cost_multiplier)
final_cost_table

write.csv(final_cost_table,file.path(res_path,"Cost_design_table.csv" ))


####### QUANTI FILES  #########

parse_quanti_metadata <- function(path) {
  bn <- basename(path)
  
  configuration  <- str_match(bn, "^(.*?)_p")[,2]
  proportion_orientalis <- as.numeric(str_extract(bn, "(?<=_p)\\d+"))/ 100
  generation     <- as.numeric(str_extract(bn, "(?<=_)\\d{4}(?=_)"))
  replicate      <- as.numeric(str_extract(bn, "(?<=_)\\d+(?=\\.quanti$)"))
  run            <- sub(".*\\/(r[0-9]+)\\/.*", "\\1", path)
  sel_full       <- str_match(bn, "_r\\d+_(.*?)_k")[,2]
  sel_type       <- ifelse(sel_full == "neutral","neutral",str_extract(sel_full, "^(sel_[EO]|heterosis)"))
  sel_strength   <- ifelse(sel_full == "neutral",NA,str_extract(sel_full, "(min|low|mid|high)$"))
  
  
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
quanti_data_clean[, selection_strength := factor(selection_strength, levels = c("min", "low", "mid", "high"))]
quanti_data_clean[, configuration := factor(configuration, levels = c("dispersed", "one_cluster", "multi_cluster", "transects"))]
quanti_data_clean$sim_id <- with(quanti_data_clean,paste(configuration,proportion_orientalis,selection_type,selection_strength, run,sep = "_"))

saveRDS(quanti_data_clean, (file.path(res_path, "Quanti_data_raw.RDS")))



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

## labels for plotting
sel_map <- c(
  heterosis = "Heterosis",
  neutral = "Neutral",
  sel_E   = "European b. selected against",
  sel_O   = "Oriental b. selected against"
)
quanti_data[, selection_label := sel_map[as.character(selection_type)]]

config_map <- c(
  dispersed = "Dispersed",
  multi_cluster = "Multiple clusters",
  one_cluster   = "Single cluster",
  transects   = "Transects"
)
quanti_data[, config_label := config_map[as.character(configuration)]]

# simulation ID
quanti_data$sim_id <- with(quanti_data,paste(configuration,proportion_orientalis,selection_type,selection_strength,run,sep = "_"))

# tidy up cols
quanti_data <-quanti_data[, c("configuration","ID","selection","selection_type","run") := NULL]
colnames(quanti_data)
setcolorder(quanti_data, c("sim_id", "config_label","proportion_orientalis","cost","selection_label","selection_strength","run", "replicate","year", "stage",     "age", "pop", "P1"))
colnames(quanti_data)<-  c("sim_id", "configuration","proportion_orientalis","cost","selection_type","selection_strength","run", "replicate","year", "age_class", "age", "pop" ,"P1")
quanti_data[, year := as.numeric(year)]
quanti_data[, proportion_orientalis := factor(proportion_orientalis)]

# save 
saveRDS(quanti_data, (file.path(res_path, "Quanti_data_processed.RDS")))


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
by = .(configuration, proportion_orientalis, cost,
       selection_type, selection_strength,
       age_class, year)]


saveRDS(quanti_data_genot_meanrep, (file.path(res_path, "Genot_proportions_summary_replicates.RDS")))


######## summarise patch level

# subset only some years 
quanti_data_subset <- quanti_data[year %in% c(50,100,500, 1000)]

# hybrid proportions and HI values per simulation and replicate and patch (across all individuals) = 1 row x sim replicate x year x age_class x patch
quanti_data_patch <- quanti_data_subset[,.( 
  N = .N,
  N_hybrid = sum(P1 > -1 & P1 < 1),
  prop_hybrids = mean(P1 > -1 & P1 < 1),
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
by = .(configuration, proportion_orientalis, selection_type,selection_strength, year, age_class, cost)]

# reorder proportion orientalis 
quanti_data_patch_summary[, proportion_orientalis :=  factor(proportion_orientalis, levels = rev(sort(unique(as.numeric(as.character(proportion_orientalis))))))]

saveRDS(quanti_data_patch_summary,file.path(res_path, "Hyb_proportions_patch_summary_replicates.RDS") )


####### DEMOGRAPHIC FILES ######

## parse metadata from filename
parse_demo_metadata <- function(path) {
  
  bn <- basename(path)
  
  # from filename 
  configuration  <- str_match(bn, "^(.*?)_p")[,2]
  proportion_orientalis <-  as.numeric(str_extract(bn, "(?<=_p)\\d+"))/ 100
  run            <- str_match(bn, "_(r\\d+)_")[,2]
  sel_full       <- str_match(bn, "_r\\d+_(.*?)_k")[,2]
  sel_type       <- ifelse(sel_full == "neutral","neutral",str_extract(sel_full, "^(sel_[EO]|heterosis)"))
  sel_strength   <- ifelse(sel_full == "neutral",NA,str_extract(sel_full, "(min|low|mid|high)$"))
  
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
combined_pop_data[, sim_id := paste(configuration, proportion_orientalis,selection_type,selection_strength, run, k, b, sep = "_")]
# convert to factors
combined_pop_data[, selection_type := factor(selection_type, levels = c("neutral", "sel_E", "sel_O", "heterosis"))]
combined_pop_data[, selection_strength := factor(selection_strength, levels = c("min", "low", "mid", "high"))]
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
combined_pop_data$sim_id <- with(combined_pop_data,paste(configuration,proportion_orientalis,selection_type,selection_strength, run,sep = "_"))
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

# create the run+replicate variable (each run has replicate 1 to 10)
combined_pop_data$replicate2 <- paste0(combined_pop_data$run,"_",as.character(combined_pop_data$replicate))

## save master file
saveRDS(combined_pop_data, file = file.path(res_path,"Demo_data_processed.RDS"))

rm(demo_data, demo_files)

## summarize across replicates
combined_pop_data_summary <- combined_pop_data[
  , .(
    mean_N = mean(N_stage, na.rm = TRUE),
    sd_N   = sd(N_stage, na.rm = TRUE), 
    q10_N = quantile(N_stage, 0.1, na.rm = TRUE),
    q50_N = quantile(N_stage, 0.5, na.rm = TRUE),
    q90_N = quantile(N_stage, 0.9, na.rm = TRUE),
  ),
  by = .(configuration,proportion_orientalis,selection_type,selection_strength,year,age_class
  )
]

write.csv(combined_pop_data_summary, file.path(res_path, "Demo_data_summary_replicates.csv"))

####### FIT FILES ###########

# parse metadata
parse_fit_metadata <- function(path) {
  bn <- basename(path)
  
  configuration  <- str_match(bn, "^(.*?)_p")[,2]
  proportion_orientalis <- as.numeric(str_extract(bn, "(?<=_p)\\d+"))/100
  generation     <- as.numeric(str_extract(bn, "(?<=_)\\d{4}(?=_)"))
  replicate      <- as.numeric(str_extract(bn, "(?<=_)\\d+(?=\\.fit$)"))
  run            <- sub(".*\\/(r[0-9]+)\\/.*", "\\1", path)
  sel_full       <- str_match(bn, "_r\\d+_(.*?)_k")[,2]
  sel_type       <- ifelse(sel_full == "neutral","neutral",str_extract(sel_full, "^(sel_[EO]|heterosis)"))
  sel_strength   <- ifelse(sel_full == "neutral",NA,str_extract(sel_full, "(low|mid|high)$"))
  
  data.table(
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


fit_files <- list.files(
  path = res_path,
  recursive = TRUE,
  full.names = TRUE,
  pattern = "\\.fit$"
)
# do not read burnin results
fit_files <- fit_files[!grepl("burnin", fit_files)]


# build table
meta_dt <- rbindlist(
  lapply(fit_files, parse_fit_metadata),
  fill = TRUE
)

# attach data to the metadata
fit_data <- rbindlist(
  lapply(meta_dt$file, function(f) {
    dt <- fread(f, header = FALSE)
    
    # enforce correct names
    setnames(dt, c("pop", "trait", "stage", "age", "isMigrant"))
    
    dt[, file := f]
    dt
  }),
  fill = TRUE
)


fit_data <- merge(
  fit_data,
  meta_dt,
  by = "file",
  all.x = TRUE
)


## get a unique sim ID
fit_data[, sim_id := paste(configuration, proportion_orientalis,selection_type,selection_strength, run, sep = "_")]
# convert to factors
fit_data[, selection_type := factor(selection_type, levels = c("neutral", "sel_E", "sel_O", "heterosis"))]
fit_data[, selection_strength := factor(selection_strength, levels = c( "low", "mid", "high"))]
fit_data[, configuration := factor(configuration, levels = c("dispersed", "one_cluster", "multi_cluster", "transects"))]

# make sure is numeric
fit_data$year <- as.numeric(fit_data$year)
fit_data$replicate <- as.numeric(fit_data$replicate)

unique(fit_data[, .(selection, selection_type, selection_strength)])

# save master file
saveRDS(fit_data, file = file.path(res_path,"Fit_data_raw.RDS"))

## labels for plotting
setDT(fit_data)

sel_map <- c(
  heterosis = "Heterosis",
  neutral = "Neutral",
  sel_E   = "European b. selected against",
  sel_O   = "Oriental b. selected against"
)
fit_data[, selection_label := sel_map[as.character(selection_type)]]

config_map <- c(
  dispersed = "Dispersed",
  multi_cluster = "Multiple clusters",
  one_cluster   = "Single cluster",
  transects   = "Transects"
)
fit_data[, config_label := config_map[as.character(configuration)]]

# tidy up cols
fit_data <-fit_data[, c("file", "configuration","selection","selection_type","isMigrant") := NULL]

setcolorder(fit_data, c("sim_id", "config_label","proportion_orientalis", "selection_label", "selection_strength", "replicate","year","pop",  "stage", "age","trait" ))
colnames(fit_data)<-c( "sim_id", "configuration","proportion_orientalis","selection_type","selection_strength", "replicate", "year","pop" ,"age_class", "age","W" )
fit_data[, year := as.numeric(year)]
fit_data[, proportion_orientalis := factor(proportion_orientalis)]

### ADD THE NEUTRAL DATA (NOT PRESENT IN THIS DATASET BECAUSE W IS ALWAYS 1)
# read the quanti dataset for the missing data
quanti_data <- readRDS(file.path(res_path, "Quanti_data_processed.RDS"))

neutral_data <- quanti_data[selection_type == "Neutral",
                    .(sim_id,configuration, proportion_orientalis,
                      selection_type, selection_strength,
                      run, replicate, year, pop, age_class, age)]

neutral_data[, W := 1]
# check 
colnames(neutral_data) == colnames(fit_data)

# append
fit_data <- rbindlist(list(fit_data, neutral_data), fill = TRUE)
saveRDS(fit_data, file = file.path(res_path,"Fit_data_processed.RDS"))

## median across individuals of each scenario and each age class (skewed distributions) - replicate level
fit_data_median <- fit_data[,.( 
  med_W = median(W), 
  q25_W = quantile(W, 0.25, na.rm = T),
  q75_W = quantile(W, 0.75, na.rm = T)
),
by = .(sim_id, configuration, proportion_orientalis, selection_type,selection_strength, year, age_class, run, replicate) ]

gc()

# compare runs results (run = biological replicate of same scenario)
fit_data_test <- fit_data_median[
  configuration == "Dispersed" &
    proportion_orientalis == 0.1 &
    selection_type == "Heterosis" &
    selection_strength == "low" &
    year == 100 &
    age_class == 1
]
kruskal.test(med_W ~ factor(run), data = fit_data_test)
#ggplot(data = dt_test,aes( x=factor(run), y =med_W))+ geom_point()

# summarize across all replicates 
fit_data_summary <- fit_data_median[, .(
  mean_W = mean(med_W),   
  sd_W   = sd(med_W),
  q10_W = quantile(med_W, 0.1, na.rm = TRUE),
  q50_W = quantile(med_W, 0.5, na.rm = TRUE),
  q90_W = quantile(med_W, 0.9, na.rm = TRUE)
  
),
by = .(configuration, proportion_orientalis, selection_type,selection_strength, year, age_class)]

saveRDS(fit_data_summary,file.path(res_path, "Fit_data_summary_replicates.RDS") )

################# summarize patch level

# subset only some years 
fit_data_subset <- fit_data[year %in% c(50,100,500, 1000)]

# W per simulation and replicate and patch (across all individuals) = 1 row x sim replicate x year x age_class x patch
fit_data_median_patch <- fit_data_subset[,.( 
  med_W = median(W), 
  q25_W = quantile(W, 0.25),
  q75_W = quantile(W, 0.75)
),
by = .(configuration, proportion_orientalis, selection_type, selection_strength, year, run, replicate, age_class, pop) ]

gc()

## ---> to check for variability within patch (maybe not needed..?)
saveRDS(fit_data_median_patch,file.path(res_path, "W_median_patch.RDS") )

# summarize across run and then replicates per patch
fit_data_median_patch_summary <- fit_data_median_patch[, .(
  mean_W = mean(med_W),
  sd_W   = sd(med_W),
  q10_W = quantile(med_W, 0.1, na.rm = TRUE),
  q50_W = quantile(med_W, 0.5, na.rm = TRUE),
  q90_W = quantile(med_W, 0.9, na.rm = TRUE)
),
by = .(configuration, proportion_orientalis, selection_type,selection_strength, year, age_class, run, pop)]

rm(fit_data_median_patch)

# reorder proportion orientalis 
fit_data_median_patch_summary[, proportion_orientalis :=  factor(proportion_orientalis, levels = rev(sort(unique(as.numeric(as.character(proportion_orientalis))))))]

saveRDS(fit_data_median_patch_summary,file.path(res_path, "W_median_patch_summary_replicates.RDS") )


