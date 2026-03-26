


### the script: 
# reads all demographics and quanti files output of the simulations (except the burnin)
# reads the metadata from file names and folder names
# keep only relevant columns
# save all the results into single /output/analysis/Demographic_data.RDS /output/analysis/Quanti_data.RDS objects

library(data.table)
library(stringr)


## NB output folders are already present

######## demographic dataset ##########

## parse metadata from filename
parse_demo_metadata <- function(path) {
  
  bn <- basename(path)
  
  # from filename 
  configuration  <- str_match(bn, "^(.*?)_p")[,2]
  ori_proportion <- as.numeric(str_match(bn, "_p(\\d+)")[,2]) / 100
  run            <- str_match(bn, "_(r\\d+)_")[,2]
  selection      <- str_match(bn, "_r\\d+_(.*?)_k")[,2]
  
  k <- as.numeric(str_match(bn, "_k([0-9.]+)")[,2])
  b <- as.numeric(str_match(bn, "_b([0-9]+\\.?[0-9]*)(?=\\.txt$)")[,2])
  
  data.table(
    file = path,
    configuration = configuration,
    ori_proportion = ori_proportion,
    selection = selection,
    run = run,
    k = k,
    b = b
  )
}

## get demographic files
res_path <- "C:/Users/stefanin/Dropbox/WSL_PhD/Projects/Hybridization2/output/simulations"
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
combined_pop_data[, sim_id := paste(configuration, ori_proportion, selection, run, k, b, sep = "_")]

##### checks 
# number of runs per combination 
aggregate(run ~ configuration + ori_proportion + selection,
          data = combined_pop_data,
          FUN = function(x) length(unique(x)))

# check for NA values
anyNA(combined_pop_data)
colSums(is.na(combined_pop_data))

# check number of replicates
combined_pop_data$sim_id <- with(combined_pop_data,paste(configuration,ori_proportion,selection, run,sep = "_"))
rep_check <- aggregate(replicate ~ sim_id,data = combined_pop_data,FUN = function(x) length(unique(x)))
rep_check

# check generations per siulation
gen_check <- aggregate(generation ~ sim_id + replicate,data = combined_pop_data,FUN = function(x) length(unique(x)))
ggplot(gen_check, aes(sim_id, generation, fill=sim_id))+ geom_bar(stat= "identity") +guides(color= "none", fill="none")+facet_wrap(~replicate)


## save master file
saveRDS(combined_pop_data,"C:/Users/stefanin/Dropbox/WSL_PhD/Projects/Hybridization2/output/analysis/Demographic_data.RDS")

rm(combined_pop_data, combined_list, demo_data, demo_files)

######## quanti dataset (cluster) ##########

# parse metadata
parse_quanti_metadata <- function(path) {
  bn <- basename(path)
  
  configuration  <- str_match(bn, "^(.*?)_p")[,2]
  ori_proportion <- as.numeric(str_extract(bn, "(?<=_p)\\d+"))
  generation     <- str_extract(bn, "(?<=_)\\d{4}(?=_)")
  replicate      <- str_extract(bn, "(?<=_)\\d+(?=\\.quanti$)")
  run            <- sub(".*\\/(r[0-9]+)\\/.*", "\\1", path)
  s              <- str_match(bn, "_r\\d+_(.*?)_k")[,2]
  
  data.table(
    file = path,
    configuration = configuration,
    ori_proportion = ori_proportion,
    selection = s,
    generation = generation,
    replicate = replicate,
    run = run
  )
}

# get quanti files
res_path <-  "C:/Users/stefanin/Dropbox/WSL_PhD/Projects/Hybridization2/output/simulations"

quanti_files <- list.files(
  path = res_path,
  recursive = TRUE,
  full.names = TRUE,
  pattern = "\\.quanti$"
)
# do not read burnin results
quanti_files <- quanti_files[!grepl("burnin", quanti_files)]


# build table
meta_dt <- rbindlist(
  lapply(quanti_files, parse_quanti_metadata),
  fill = TRUE
)


# attach data to the metadata
quanti_data <- rbindlist(
  lapply(meta_dt$file, function(f) {
    dt <- fread(f)
    dt[, file := f]
    dt
  }),
  fill = TRUE
)

quanti_data <- merge(
  quanti_data,
  meta_dt,
  by = "file",
  all.x = TRUE
)

###### checks

# number of runs per combination 
aggregate(run ~ configuration + ori_proportion + selection,
          data = quanti_data,
          FUN = function(x) length(unique(x)))

# check for NA values
anyNA(quanti_data)
colSums(is.na(quanti_data))

# check number of replicates
quanti_data$sim_id <- with(quanti_data,paste(configuration,ori_proportion,selection, run,sep = "_"))
rep_check <- aggregate(replicate ~ sim_id,data = quanti_data,FUN = function(x) length(unique(x)))
rep_check

# check number of individuals in all simulations
indiv_check <- aggregate(ID ~ sim_id + replicate, data = quanti_data,FUN = length)
ggplot(indiv_check, aes(sim_id, ID, fill=sim_id))+ geom_bar(stat= "identity") +guides(color= "none", fill="none")+facet_wrap(~replicate)

# check generations per siulation
gen_check <- aggregate(generation ~ sim_id + replicate,data = quanti_data,FUN = function(x) length(unique(x)))
ggplot(gen_check, aes(sim_id, generation, fill=sim_id))+ geom_bar(stat= "identity") +guides(color= "none", fill="none")+facet_wrap(~replicate)

# save master file
saveRDS(quanti_data, "output/analysis/Quanti_data.rds")



######## fitness dataset ####

library(data.table)
library(stringr)

# parse metadata
parse_fit_metadata <- function(path) {
  bn <- basename(path)
  
  configuration  <- str_match(bn, "^(.*?)_p")[,2]
  ori_proportion <- as.numeric(str_extract(bn, "(?<=_p)\\d+"))
  generation     <- str_extract(bn, "(?<=_)\\d{4}(?=_)")
  replicate      <- str_extract(bn, "(?<=_)\\d+(?=\\.fit$)")
  run            <- sub(".*\\/(r[0-9]+)\\/.*", "\\1", path)
  s              <- str_match(bn, "_r\\d+_(.*?)_k")[,2]
  
  data.table(
    file = path,
    configuration = configuration,
    ori_proportion = ori_proportion,
    selection = s,
    generation = generation,
    replicate = replicate,
    run = run
  )
}

# get fit files
res_path <-  "C:/Users/stefanin/Dropbox/WSL_PhD/Projects/Hybridization2/output/simulations"

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

# attach data to the metadata (fix the wrong reading of the colnames)
fit_data <- rbindlist(
  lapply(meta_dt$file, function(f) {
    dt <- fread(f, header = FALSE)
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


# merge fit into quanti (LEFT JOIN)
final_data <- merge(
  quanti_data_clean,
  fit_data,
  by = c("configuration", "ori_proportion", "selection",
         "generation", "replicate", "run",
         "pop", "stage", "age"),
  all.x = TRUE   # keeps all quanti rows even if fit missing
)

# save master file
saveRDS(fit_data, "output/analysis/Fit_data.RDS")

######## process datasets ###########

res_path <- "C:/Users/stefanin/Dropbox/WSL_PhD/Projects/Hybridization2/output/analysis"
quanti_data <- readRDS(file.path(res_path, "Quanti_data_fitness.RDS"))
hist(quanti_data$P1)

## attach cost data to the configuration 
cost_table <- read.csv("C:/Users/stefanin/Dropbox/WSL_PhD/Projects/Hybridization2/Cost_design_table.csv")
setDT(quanti_data)
setDT(cost_table)
quanti_data[, ori_prop := ori_proportion / 100]
quanti_data <- merge(
  quanti_data,
  cost_table[, .(configuration, ori_prop, cost = estimated_cost)],
  by = c("configuration", "ori_prop"),
  all.x = TRUE
)

## labels for plotting
lab_map <- c(
  heterosis = "Heterosis",
  neutral = "Neutral",
  sel_E   = "European selected",
  sel_O   = "Oriental selected"
)
quanti_data[, selection_label := lab_map[selection]]

config_map <- c(
  dispersed = "Dispersed",
  multi_cluster = "Multiple clusters",
  one_cluster   = "Single cluster",
  transects   = "Transects"
)
quanti_data[, config_label := config_map[configuration]]


## attach fitness data

## fit files
res_path <- "C:/Users/stefanin/Dropbox/WSL_PhD/Projects/Hybridization2/output/analysis"
fit_data <- readRDS(file.path(res_path, "Fit_data_fitness.RDS"))

# assign to all individuals full fitness first
quanti_data$W <- 1

# merge fitness for stage 1 individuals
nrow(fit_data) == nrow(subset(quanti_data, selection!="Neutral"&stage==1)) ## check

# create row index for each simulation (there are multiple identical cols in fit data, as no ID per individual is stored in the file) --. NA where quanti_data is not in fit (stage!=1 and selection ==neutral)
grp <- c("configuration", "ori_proportion", "selection","generation", "replicate", "run")
quanti_data[selection != "neutral" & stage == 1,row_id := seq_len(.N), by = grp]
fit_data[, row_id := seq_len(.N), by = grp]
# add fitness to quanti data based on row id
quanti_data[fit_data,on = c(grp, "row_id"), W := i.trait]

# remove unnecessary cols
final_data <-quanti_data[, c("configuration", "ori_proportion", "file", "ID", "selection", "row_id") := NULL]

# update the sim_id with the replicate number also
final_data$sim_id <- paste0(final_data$sim_id, "_", final_data$replicate)

# tidy up colnames
colnames(final_data)<-c( "Proportion_orientalis", "pop","P1","age_class","age","year", "replicate","run",  "sim_id", "cost", "selection","configuration", "W")

# reorder
setcolorder(final_data, c("sim_id", "configuration","Proportion_orientalis", "cost","selection","run","replicate","year", "age_class","age","pop", "P1","W"))

# set col types
final_data[, year := as.numeric(year)]
final_data[, Proportion_orientalis := factor(Proportion_orientalis)]

# save 
saveRDS(final_data, (file.path(res_path, "Quanti_fit_data_fitness.RDS")))

rm(quanti_data, fit_data)

