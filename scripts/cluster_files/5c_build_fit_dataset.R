library(data.table)
library(stringr)

# reading and parsing all fit files (.txt) and combining into one dataset
# summarizing at whole scneario level and then at patch level
# outputs : Fit_data.RDS --- etc


### TO CORRECT, THERE SI AN ERROR IN READING OR PARSING OR PROCESSING RUN COLUMN --> IT IS STORED AS THE PATH

res_path <- "/home/stefanin/nemo/nemo_files/nemoage0.32.6b/results/"

########### FIT FILES

# parse metadata
parse_fit_metadata <- function(path) {
  bn <- basename(path)
  
  configuration  <- str_match(bn, "^(.*?)_p")[,2]
  proportion_orientalis <- as.numeric(str_extract(bn, "(?<=_p)\\d+"))/100
  generation     <- as.numeric(str_extract(bn, "(?<=_)\\d{4}(?=_)"))
  replicate      <- as.numeric(str_extract(bn, "(?<=_)\\d+(?=\\.fit$)"))
  run            <- str_match(bn, "_(r\\d+)_")[,2]   ### changed : before ---> sub(".*\\/(r[0-9]+)\\/.*", "\\1", path)
  sel_full       <- str_match(bn, "_r\\d+_(.*?)_k")[,2]
  sel_type       <- ifelse(sel_full == "neutral","neutral",str_extract(sel_full, "^(sel_[EO]|heterosis)"))
  sel_strength   <- ifelse(sel_full == "neutral",NA,str_extract(sel_full, "(low|mid|high)$"))
  
  data.table(
    file = path, ## needed
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
fit_data[, sim_id := paste(configuration, proportion_orientalis,selection_type,selection_strength, sep = "_")]
# convert to factors
fit_data[, selection_type := factor(selection_type, levels = c("neutral", "sel_E", "sel_O", "heterosis"))]
fit_data[, selection_strength := factor(selection_strength, levels = c( "low", "mid", "high"))]
fit_data[, configuration := factor(configuration, levels = c("dispersed", "one_cluster", "multi_cluster", "transects","no_introduction"))]

# make sure is numeric
fit_data$year <- as.numeric(fit_data$year)
fit_data$replicate <- as.numeric(fit_data$replicate)


unique(fit_data[, .(selection, selection_type, selection_strength)])


# save master file
saveRDS(fit_data, file = file.path(res_path,"Fit_data_raw.RDS"))


###################################### process dataset


## labels for plotting
setDT(fit_data)

sel_map <- c(
  heterosis = "Wf1 > Weu = Wori",
  neutral = "Neutral",
  sel_E   = "Wori > Wf1 > Weu",
  sel_O   = "Weu > Wf1 > Wori"
)
fit_data[, selection_label := sel_map[as.character(selection_type)]]

config_map <- c(
  dispersed = "Dispersed",
  multi_cluster = "Multiple clusters",
  one_cluster   = "Single cluster",
  transects   = "Transects", 
  no_introduction = "No introduction"
)
fit_data[, config_label := config_map[as.character(configuration)]]

# tidy up cols
fit_data <-fit_data[, c("file", "configuration","selection","selection_type","isMigrant") := NULL]

setcolorder(fit_data, c("sim_id", "config_label","proportion_orientalis", "selection_label", "selection_strength", "run", "replicate","year","pop",  "stage", "age","trait" ))
colnames(fit_data)  <-c("sim_id", "configuration","proportion_orientalis","selection_type","selection_strength",  "run","replicate", "year","pop" ,"age_class", "age","W" )
fit_data[, year := as.numeric(year)]
fit_data[, proportion_orientalis := factor(proportion_orientalis)]


### ADD THE NEUTRAL DATA (NOT PRESENT IN THIS DATASET BECAUSE W IS ALWAYS 1)
# read the quanti dataset for the missing data
quanti_data <- readRDS(file.path(res_path, "Quanti_data_processed.RDS"))

neutral_data <- quanti_data[selection_type == "Neutral",
                    .(sim_id,configuration, proportion_orientalis,selection_type, selection_strength,run, replicate, year, pop, age_class, age)]

neutral_data[, W := 1]
# check 
colnames(neutral_data) == colnames(fit_data)

# append
fit_data <- rbindlist(list(fit_data, neutral_data), fill = TRUE)

## save master file
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

## ---> to check for variability within patch (maybe not needed)
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

