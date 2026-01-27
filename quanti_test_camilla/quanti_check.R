
#### quanti check ######
res_path2 <- "C:/Users/stefanin/Desktop/nemo"


# to select all the replicates
file_pattern <- ".*\\.quanti$"

library(data.table)
library(stringr)
# to select only 30 replicates
#file_pattern <- ".*_results_([0-9]{2,3})_((0[1-9])|(1[0-9])|(2[0-9])|(30))\\.quanti$"

quanti_files <- list.files(path = res_path2, recursive = TRUE,full.names = TRUE,include.dirs = TRUE,pattern = file_pattern)

quanti_list <- vector("list", length(quanti_files))

for (i in seq_along(quanti_files)) {
  
  file <- quanti_files[i]
  data <- fread(file)
  ori_proportion <- as.numeric( stringr::str_extract(basename(file), "(?<=_p)\\d+"))
  configuration <-str_match(basename(file),"_p\\d+_(.*?)_rep")[, 2]
  k <- as.numeric(sub(".*_k([0-9.]+).*", "\\1", file))
  b <- as.numeric(sub(".*_b([0-9.]+).*", "\\1", file))
  
  # generation-- any 3-digit number surrounded by _xxx_
  generation <- as.numeric( str_extract(basename(file), "(?<=_)\\d{3}(?=_)"))
  # replicate = last number before .quanti
  replicate <- as.numeric(str_extract(basename(file), "(?<=_)\\d+(?=\\.quanti$)"))
  
  data[, `:=`(
    configuration  = configuration,
    ori_proportion = ori_proportion,
    k         = k,
    b         = b,
    generation = generation,
    replicate  = replicate
  )]
  
  quanti_list[[i]] <- data
}

# Combine all files into one data.table ad save
quanti_data_all <- rbindlist(quanti_list, fill = TRUE)

hist(quanti_data_all$P1)



library(dplyr)
library(ggplot2)
## summarise data per patch = each patch has the average P1 value
quanti_patch <- quanti_data_all %>%
  group_by(configuration, ori_proportion, replicate, generation,stage,pop, k,b) %>%
  summarize(P1 = mean(P1, na.rm = T), .groups = "drop")

## quick look at P1 distirbution
hist(quanti_data_all$P1)

# grid dimensions 
n_rows = 25
n_cols = 25

### CHECK FIRST GENERATION TO SEE WHERE ORI IS PLANTED (ADULTS STAGE 2)

## plot the average P1 for one stage and one replicate
ggplot(subset(quanti_patch,stage ==2& replicate ==2&generation==1),
       aes(x = (pop - 1) %% n_rows + 1, 
           y = n_cols - ((pop - 1) %/% n_cols + 1), 
           fill = as.numeric(P1))) +
  geom_raster() +
  scale_fill_viridis_c(option = "viridis", direction = -1) +
  facet_grid(configuration+ ori_proportion~ generation) + 
  labs(x = "", y = "", fill = "Quantitative trait") +
  theme_void() +
  theme(
    aspect.ratio = 1,
    legend.position = "bottom",
    panel.spacing.x = unit(1, "lines"),
    panel.spacing.y = unit(0.1, "lines"),
    strip.text = element_text(size = 10))


## plot the average P1 for one stage and one replicate
ggplot(subset(quanti_patch,stage ==2& replicate ==2&generation==100),
       aes(x = (pop - 1) %% n_rows + 1, 
           y = n_cols - ((pop - 1) %/% n_cols + 1), 
           fill = as.numeric(P1))) +
  geom_raster() +
  scale_fill_viridis_c(option = "viridis", direction = -1) +
  facet_grid(configuration+ ori_proportion~ generation) + 
  labs(x = "", y = "", fill = "Quantitative trait") +
  theme_void() +
  theme(
    aspect.ratio = 1,
    legend.position = "bottom",
    panel.spacing.x = unit(1, "lines"),
    panel.spacing.y = unit(0.1, "lines"),
    strip.text = element_text(size = 10))

