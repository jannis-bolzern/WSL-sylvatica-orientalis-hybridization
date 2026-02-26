

### the script: 
# reads all quanti files from the results
# reads the metadata from file names and folder names
# keep only relevant columns
# save all the results into a unique Quanti_data.R object

######## demographic results ##########

library(data.table)
library(stringr)

### NB FIRST REMOVE THE EXISTING RESULTS


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
res_path <- "C:/Users/stefanin/Dropbox/WSL_PhD/Projects/Hybridization2/Results/"
file_pattern <- "*.txt$"

demo_files <- list.files(
  path = res_path,
  recursive = TRUE,
  full.names = TRUE,
  pattern = file_pattern
)

# do not read the _bygen files
demo_files <- demo_files[!grepl("bygen", demo_files)]

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

# check number of individuals in all simulations
indiv_check <- aggregate(ID ~ sim_id + replicate, data = quanti_data,FUN = length)
ggplot(indiv_check, aes(sim_id, ID, fill=sim_id))+ geom_bar(stat= "identity") +guides(color= "none", fill="none")+facet_wrap(~replicate)

# check generations per siulation
gen_check <- aggregate(generation ~ sim_id + replicate,data = combined_pop_data,FUN = function(x) length(unique(x)))
ggplot(gen_check, aes(sim_id, generation, fill=sim_id))+ geom_bar(stat= "identity") +guides(color= "none", fill="none")+facet_wrap(~replicate)


## save master file
saveRDS(combined_pop_data,file = file.path(res_path, "Demographic_data_per_simulation.RDS"))

rm(combined_pop_data, combined_list, demo_data, demo_files)

########## quanti results ##########

library(data.table)
library(stringr)


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
    selection = selection,
    generation = generation,
    replicate = replicate,
    run = run
  )
}

# get quanti files
res_path <- "C:/Users/stefanin/Dropbox/WSL_PhD/Projects/Hybridization2/Results/"

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
saveRDS(quanti_data, "Quanti_data.rds")