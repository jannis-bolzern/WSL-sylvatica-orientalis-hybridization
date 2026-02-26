
### the script: 
# loads all the results from Quanti_data.R object
# create different plots


library(data.table)
library(ggplot2)

res_path <- "C:/Users/stefanin/Dropbox/WSL_PhD/Projects/Hybridization2/Results/"
quanti_data <- readRDS(file.path(res_path, "Quanti_data.RDS"))
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
  neutral = "Neutral",
  sel_E   = "S. vs E. beech",
  sel_O   = "S. vs O. beech"
)
quanti_data[, selection_label := lab_map[selection]]

config_map <- c(
  dispersed = "Dispersed",
  multi_cluster = "Multiple clusters",
  one_cluster   = "Single cluster",
  transects   = "Transects"
)
quanti_data[, config_label := config_map[configuration]]

# remove unnecessary cols
quanti_data <- quanti_data[, -c(1,8,9)]

# colnames
colnames(quanti_data)<-c( "Proportion_orientalis","pop","P1","age_class","age","ID","year", "run", "replicate", "cost", "selection","configuration")

# reorder
setcolorder(
  quanti_data,
  c("configuration",
    "Proportion_orientalis",
    "cost",
    "selection",
    "run",
    "replicate",
    "year",
    "age_class",
    "age",
    "ID",
    "pop",
    "P1")
)

# save 
saveRDS(quanti_data, (file.path(res_path, "Quanti_data_final.rds")))

rm(quanti_data)

######## plotting settings ###############

prop_ori_palette <- c(
  "0.1" = "#CE93D8", 
  "0.25" = "#8E24AA",  
  "0.4" = "#311B92"   
)

config_oriprop_palette <- c(
  # Dispersed – Blue family
  "Dispersed_0.1" = "#0072B2",
  "Dispersed_0.25" = "#56B4E9",
  "Dispersed_0.4" = "#B3D9F2",
  
  # Multiple clusters – Orange family
  "Multiple clusters_0.1" = "#D55E00",
  "Multiple clusters_0.25" = "#E69F00",
  "Multiple clusters_0.4" = "#F6C667",
  
  # Single cluster – Purple family
  "Single cluster_0.1" = "#6A3D9A",
  "Single cluster_0.25" = "#9E77CF",
  "Single cluster_0.4" = "#D4B9F2",
  
  # Transects – Teal family
  "Transects_0.1" = "#009E73",
  "Transects_0.25" = "#4CC7A1",
  "Transects_0.4" = "#A6E3CF"
)

######## check demographic results (TO DO) ###########
######## calculate Hyb proportions and HI index ############

library(data.table)
library(stringr)

dt <- readRDS(file.path(res_path, "Quanti_data_final.RDS"))
dt[, Proportion_orientalis := factor(Proportion_orientalis)]


# mean hybrid proportions and HI values per simulation and replicate (across all individuals) = 1 row x sim replicate x year x age_class
hyb_prop <- dt[,.( 
  N = .N,
  N_hybrid = sum(P1 > -1 & P1 < 1),
  prop_hybrids = mean(P1 > -1 & P1 < 1), 
  mean_HI = mean(1 - abs(P1))
),

by = .(configuration, Proportion_orientalis,selection,year,run, replicate, age_class, cost) ]

gc()

# collapse simulation replicates (also mean across all runs)
hyb_run_mean <- hyb_prop[, .(
  mean_prop = mean(prop_hybrids),
  sd_prop   = sd(prop_hybrids),
  mean_HI = mean(mean_HI),
  sd_HI = sd(mean_HI)
),
by = .(configuration, Proportion_orientalis, selection, year, age_class,cost)]

hyb_run_mean$config_prop <- paste0(hyb_run_mean$configuration, "_",hyb_run_mean$Proportion_orientalis)

######## check the genotype across time (TO DO) ############

ggplot(hyb_run_mean, aes(x = gen_f, y = bin_mid, fill = mean_prop)) +
  geom_tile() +
  facet_grid(ori_proportion ~ configuration) +
  scale_fill_viridis_c(option = "C", trans = "sqrt") +
  labs(
    title = title_prefix,
    x = "Generation",
    y = "P1 bin midpoint (−1 = sylvatica, 0 = hybrid, +1 = orientalis)",
    fill = "Mean\nproportion"
  ) +
  theme_bw(base_size = 11) +
  theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

######## plot the proportion of hybrids and HI index over time across parameters ########

# proportion of hybrids
ggplot(hyb_run_mean,
       aes(year, mean_prop, colour = factor(age_class), group = factor(age_class))) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_prop - sd_prop,
                  ymax = mean_prop + sd_prop,
                  fill = factor(age_class)),
              alpha = 0.25,
              colour = NA) +
  facet_grid( Proportion_orientalis ~ selection+configuration ) +
  theme_bw()

# plot the HI over time across parameters
ggplot(hyb_run_mean,
       aes(year, mean_HI, colour = factor(age_class), group = factor(age_class))) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_HI - sd_HI,
                  ymax = mean_HI + sd_HI,
                  fill = factor(age_class)), alpha = 0.1,  colour = NA) +
  facet_grid( Proportion_orientalis ~ selection+configuration ) +
  theme_bw()


# plot the HI over time combining age classes (as there is no difference)
ggplot(hyb_run_mean,
       aes(year, mean_HI, colour = Proportion_orientalis, group =Proportion_orientalis)) +
  geom_line(size =1) +
  geom_ribbon(aes(ymin = mean_HI - sd_HI,
                  ymax = mean_HI + sd_HI,
                  fill = factor(Proportion_orientalis)), alpha = 0.2,  colour = NA) +
  facet_grid( selection~configuration ) +
  scale_colour_manual(values = prop_ori_palette)+
  scale_fill_manual(values = prop_ori_palette)+
  guides(fill="none")+
  theme_bw()



######## line plot of HI per simulation across time (to check if is the same across age classes, but not really needed) ############
library(data.table)
library(stringr)


# group by color and give intensity to the ori proportion


ggplot(hyb_run_mean,aes(year, mean_HI,
                        colour =config_prop,
                        group  =config_prop)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = mean_HI - sd_HI,
                  ymax = mean_HI + sd_HI,
                  fill = factor(config_prop)),
              alpha = 0.1,
              colour = NA) +
  scale_colour_manual(values = config_oriprop_palette) +
  scale_fill_manual(values = config_oriprop_palette) +
  facet_grid(age_class~ selection) +
  guides(fill="none")+
  theme_bw()


######## hyb proportion and HI for one specific year across simulations ###############

# year = 100

hyb_prop_100 <- subset(hyb_prop,year == 100)

ggplot(hyb_prop_100,
       aes(configuration, mean_HI, fill = Proportion_orientalis, group = interaction(configuration, Proportion_orientalis))) +
  geom_boxplot() +
  facet_grid( age_class ~ selection) +
  scale_fill_manual(values = prop_ori_palette)+
  theme_bw()+
  labs(title = "Mean HI at year 100")

### change x and y
ggplot(hyb_prop_100,
       aes(Proportion_orientalis, mean_HI, fill = configuration, group = interaction(configuration, Proportion_orientalis))) +
  geom_boxplot() +
  facet_grid(  ~ selection) +
  theme_bw()+
  labs(title = "Mean HI at year 100")


# year = 1000
hyb_prop_1000 <- subset(hyb_prop,year == 1000)

ggplot(hyb_prop_1000,
       aes(Proportion_orientalis, mean_HI, fill = configuration, group = interaction(configuration, Proportion_orientalis))) +
  geom_boxplot() +
  facet_grid(  ~ selection) +
  theme_bw()+
  labs(title = "Mean HI at year 1000")


######## spatial patterns of HI and hybrid proportions ########

library(data.table)
library(stringr)

dt <- readRDS(file.path(res_path, "Quanti_data_final.RDS"))
dt[, Proportion_orientalis := factor(Proportion_orientalis)]


# mean hybrid proportions and HI values per simulation and replicate (across all individuals) = 1 row x sim replicate x year x age_class
hyb_prop_patch <- dt[,.( 
  N = .N,
  N_hybrid = sum(P1 > -1 & P1 < 1),
  prop_hybrids = mean(P1 > -1 & P1 < 1), 
  mean_HI = mean(1 - abs(P1))
),
by = .(configuration, Proportion_orientalis,cost,selection,year,run, replicate, age_class, pop) ]

gc()


# check replicates variability
hyb_prop_patch_sdrep <- hyb_prop_patch[, .(
  sd_HI = sd(mean_HI, na.rm = TRUE)
), by = .(configuration, Proportion_orientalis, selection, year, age_class, pop)]

## or?
hyb_prop_patch_sdrep <- hyb_prop_patch[, .(
  n_rep = .N,
  sd_HI = if (.N > 1) sd(mean_HI, na.rm = TRUE) else NA_real_
), by = .(configuration, Proportion_orientalis, selection, year, age_class, pop)]

## map the spatial uncertainty per patch (SD) for each age_class for year 100 and year 1000
# importing grid 
grid <- vect("C:/Users/stefanin/Dropbox/WSL_PhD/Projects/Hybridization2/input/input_files/Grid_4x4m_100x100m.shp")
grid_r <- terra::rast(ext = ext(grid), resolution = 4, crs = "EPSG:3035")
# grid dimensions 
n_rows = dim(grid_r)[1]
n_cols = dim(grid_r)[2]

ggplot(subset(hyb_prop_patch_sdrep,age_class ==1& year == 100),
       aes(x = (pop - 1) %% n_rows + 1,
           y = n_cols - ((pop - 1) %/% n_cols + 1),
           fill = sd_HI)) +
  geom_raster() +
  scale_fill_viridis_c(option = "magma", na.value = "white") +
  facet_grid(configuration ~selection + Proportion_orientalis) +
  theme_void() +
  labs(title = "SD of HI per patch for year 100")+
  theme(aspect.ratio = 1,
        legend.position = "bottom")

ggplot(subset(hyb_prop_patch_sdrep, age_class ==2& year == 1000),
       aes(x = (pop - 1) %% n_rows + 1,
           y = n_cols - ((pop - 1) %/% n_cols + 1),
           fill = sd_HI)) +
  geom_raster() +
  scale_fill_viridis_c(option = "magma", na.value = "white") +
  facet_grid(configuration ~selection + Proportion_orientalis) +
  theme_void() +
  labs(title = "SD of HI per patch for year 1000")+
  theme(aspect.ratio = 1,
        legend.position = "bottom")


## plot patch SD across time
sd_summary <- hyb_prop_patch_sdrep[
  !is.na(sd_HI),
  .(mean_patch_sd = mean(sd_HI),
    median_patch_sd = median(sd_HI),
    max_patch_sd = max(sd_HI)),
  by = .(configuration, Proportion_orientalis, selection, year, age_class)]

ggplot(sd_summary,
       aes(x = year, y = mean_patch_sd,
           color = factor(Proportion_orientalis))) +
  geom_line() + theme_bw()+
  facet_grid(selection ~ configuration + age_class)


### plot trends across grid

# select only year 2, 100 and 1000
hyb_prop_patch_sub <- subset(hyb_prop_patch, year %in% c(2,100,1000))

# proportion of hybrids (for one age_class and one replicate)
ggplot(subset(hyb_prop_patch_sub, age_class ==1 & replicate ==1),
       aes(x = (pop - 1) %% n_rows + 1, 
           y = n_cols - ((pop - 1) %/% n_cols + 1), 
           fill = as.numeric(mean_HI))) +
  geom_raster() +
  scale_fill_viridis_c(option = "viridis", direction = -1) +
  facet_grid( Proportion_orientalis+configuration ~selection + year) + 
  labs(x = "", y = "", fill = "Proportion of hybrids") +
  theme_void() +
  theme(
    aspect.ratio = 1,
    legend.position = "bottom",
    panel.spacing.x = unit(1, "lines"),
    panel.spacing.y = unit(0.1, "lines"),
    strip.text = element_text(size = 10))



## (if SD across replicate is small) mean HI across replicates per patch
hyb_prop_patch_meanrep <- hyb_prop_patch[, .(
  mean_HI = mean(mean_HI, na.rm = TRUE)
), by = .(configuration, Proportion_orientalis,cost, selection, year, age_class, pop)]

# select only year 2, 100 and 1000
hyb_prop_patch_meanrep_sub <- subset(hyb_prop_patch_meanrep, year %in% c(2,100,1000))

## SPATIAL DISTRIBUTION FOR SEEDLINGS
ggplot(subset(hyb_prop_patch_meanrep_sub, age_class ==1 ),
       aes(x = (pop - 1) %% n_rows + 1, 
           y = n_cols - ((pop - 1) %/% n_cols + 1), 
           fill = as.numeric(mean_HI))) +
  geom_raster() +
  scale_fill_viridis_c(option = "viridis", direction = -1) +
  facet_grid(Proportion_orientalis+configuration  ~ selection+year) + 
  labs(x = "", y = "", fill = "HI") +
  theme_void() +
  labs(title = "Spatial distribution of mean HI for age class 2-4 (seedlings)")+
  theme(
    aspect.ratio = 1,
    legend.position = "bottom",
    panel.spacing.x = unit(1, "lines"),
    panel.spacing.y = unit(0.1, "lines"),
    strip.text = element_text(size = 10))

## SPATIAL DISTRIBUTION FOR YOUNG ADULTS
ggplot(subset(hyb_prop_patch_meanrep_sub, age_class ==2 ),
       aes(x = (pop - 1) %% n_rows + 1, 
           y = n_cols - ((pop - 1) %/% n_cols + 1), 
           fill = as.numeric(mean_HI))) +
  geom_raster() +
  scale_fill_viridis_c(option = "viridis", direction = -1) +
  facet_grid(Proportion_orientalis+configuration  ~ selection+year) + 
  labs(x = "", y = "", fill = "HI") +
  theme_void() +
  labs(title = "Spatial distribution of mean HI for age class 4-39 (young adults)")+
  theme(
    aspect.ratio = 1,
    legend.position = "bottom",
    panel.spacing.x = unit(1, "lines"),
    panel.spacing.y = unit(0.1, "lines"),
    strip.text = element_text(size = 10))

## SPATIAL DISTRIBUTION FOR ADULTS
ggplot(subset(hyb_prop_patch_meanrep_sub, age_class ==3 ),
       aes(x = (pop - 1) %% n_rows + 1, 
           y = n_cols - ((pop - 1) %/% n_cols + 1), 
           fill = as.numeric(mean_HI))) +
  geom_raster() +
  scale_fill_viridis_c(option = "viridis", direction = -1) +
  facet_grid(Proportion_orientalis+configuration  ~ selection+year) + 
  labs(x = "", y = "", fill = "HI") +
  theme_void() +
  labs(title = "Spatial distribution of mean HI for ADULTS")+
  theme(
    aspect.ratio = 1,
    legend.position = "bottom",
    panel.spacing.x = unit(1, "lines"),
    panel.spacing.y = unit(0.1, "lines"),
    strip.text = element_text(size = 10))

######## plot cost versus mean HI ######


ggplot(subset(hyb_run_mean,year==100),  aes(mean_HI, cost, color = config_prop))+ 
  geom_point(aes(size = Proportion_orientalis))+ 
  theme_bw()+
  scale_color_manual(values = config_oriprop_palette)+
  labs(title = "Cost versus Mean HI after 100 years for the three target age classes", y = "Estimated cost")+
  facet_grid(age_class ~ selection)

ggplot(subset(hyb_run_mean,year==1000),  aes(mean_HI, cost, color = config_prop))+ 
  geom_point(aes(size = Proportion_orientalis))+ 
  theme_bw()+
  scale_color_manual(values = config_oriprop_palette)+
  labs(title = "Cost versus Mean HI after 1000 years")+
  facet_grid( ~ selection)
