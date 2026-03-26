
### the script: 
# loads all the results from Quanti_data.R object and process the dataset
# create different plots


library(data.table)
library(stringr)
library(ggplot2)
library(terra)
library(ggh4x)

res_path <- "C:/Users/stefanin/Dropbox/WSL_PhD/Projects/Hybridization2/output/analysis"

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

config_palette <- c(
  "Dispersed" = "#56B4E9",
  "Multiple clusters" = "#E69F00",
  "Single cluster" = "#9E77CF",
  "Transects" = "#4CC7A1"
)

######## demographic results (TO UPDATE) ###########

dt <- readRDS(file.path(res_path, "Demographic_data.RDS"))

# add label for stage
nemo_stage <- c("pop.tot" = "Total Population","a0.tot" = "Stage 0", "a1.tot" = "Stage 1", "a2.tot" = "Stage 2", "a3.tot" = "Stage 3")
dt$nemo_stage <- nemo_stage[dt$stage]
selection_label <- c( neutral = "Neutral",  sel_E   = "S. vs E. beech", sel_O   = "S. vs O. beech")
dt$selection_label <- selection_label[dt$selection]
configuration_label <- c(dispersed = "Dispersed",multi_cluster = "Multiple clusters",one_cluster   = "Single cluster",transects   = "Transects")
dt$config_label <- configuration_label[dt$configuration]
colnames(dt)[3] <- "year"
dt$run_rep <- paste0(dt$run,"_",dt$replicate)

# demographic trends average simulation
ggplot(subset(dt, nemo_stage!= "Total Population"), aes(x = year, y = N_stage, group = interaction(nemo_stage, run_rep), col = nemo_stage)) +
  geom_line(linewidth = 0.2, alpha = 0.8) +
  facet_grid(config_label ~ selection_label, scales="free_y") +
  labs( y = "Number of individuals",
        color = "Nemo Stage"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

ggsave(
  "Figures_presentations/Demographic_plot.jpg",
  plot = last_plot(),   # or assign your plot to an object and use plot = p
  width = 8,
  height = 5,
  units = "in"
)

# only adults
ggplot(subset(dt, nemo_stage== "Stage 3"), aes(x = year, y = N_stage, group = interaction(nemo_stage, run_rep), col = nemo_stage)) +
  geom_line(linewidth = 0.2, alpha = 0.8) +
  facet_grid(config_label ~ selection_label, scales="free_y") +
  labs( y = "Number of individuals",
        color = "Nemo Stage"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

ggsave(
  "Figures_presentations/Demographic_plot_adults.jpg",
  plot = last_plot(),   # or assign your plot to an object and use plot = p
  width = 8,
  height = 5,
  units = "in"
)


######## summarize across individuals and across replicates ############
dt <- readRDS(file.path(res_path, "Quanti_fit_data_fitness.RDS"))

## summarizing procedure: median (individual HI within replicate) → then mean across replicates
hist(dt$P1)
hist(dt$W)

## median across individuals of each scenario and each age class (skewed distributions)
dt_median <- dt[,.( 
  N = .N,
  N_hybrid = sum(P1 > -1 & P1 < 1),
  prop_hybrids = mean(P1 > -1 & P1 < 1),
  med_HI = median(1 - abs(P1)),  
  q25_HI = quantile(1 - abs(P1), 0.25),
  q75_HI = quantile(1 - abs(P1), 0.75),
  med_W = median(W), 
  q25_W = quantile(W, 0.25),
  q75_W = quantile(W, 0.75)
),
by = .(configuration, Proportion_orientalis, selection, year, sim_id, age_class, cost) ]

gc()

# summarize across replicates 
dt_meanrep <- dt_median[, .(
  mean_prop = mean(prop_hybrids),
  sd_prop   = sd(prop_hybrids),
  mean_HI = mean(med_HI),  ## mean of medians
  sd_HI = sd(med_HI), 
  mean_W = mean(med_W),   
  sd_W   = sd(med_W) 
),
by = .(configuration, Proportion_orientalis, selection, year, age_class, cost)]
dt_meanrep$config_prop <- paste0(dt_meanrep$configuration, "_",dt_meanrep$Proportion_orientalis)


## median values at the population level across age classes
dt_median_pop <- dt[, .(
  N = .N,
  N_hybrid = sum(P1 > -1 & P1 < 1),
  prop_hybrids = mean(P1 > -1 & P1 < 1),
  med_HI = median(1 - abs(P1)),  
  q25_HI = quantile(1 - abs(P1), 0.25),
  q75_HI = quantile(1 - abs(P1), 0.75),
  
  med_W = median(W),
  q25_W = quantile(W, 0.25),
  q75_W = quantile(W, 0.75)
  
),
by = .(configuration, Proportion_orientalis, selection, year, sim_id, cost)]

# mean across replicates 
dt_pop_meanrep <- dt_median_pop[, .(
  
  mean_prop = mean(prop_hybrids),
  sd_prop   = sd(prop_hybrids),
  mean_HI = mean(med_HI),  ## mean of medians
  sd_HI = sd(med_HI), 
  mean_W = mean(med_W),   
  sd_W   = sd(med_W) 
),
by = .(configuration, Proportion_orientalis, selection, year, cost)]

######## plot P1 across time -- Jannis plot (TO DO) ############

######## plot HI and W over time across parameters ########


# plot the HI over time across parameters
ggplot(dt_meanrep,
       aes(year, mean_HI, colour = factor(age_class), group = factor(age_class))) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_HI - sd_HI,
                  ymax = mean_HI + sd_HI,
                  fill = factor(age_class)), alpha = 0.1,  colour = NA) +
  facet_nested( Proportion_orientalis ~ selection+configuration ) +
  labs(x = "Year", y = "Mean HI")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))



# plot the HI over time combining age classes (as there is no difference)
ggplot(dt_meanrep,
       aes(year, mean_HI,
           colour = Proportion_orientalis,
           linetype = factor(age_class),
           group = interaction(Proportion_orientalis, age_class))) +
  scale_colour_manual(values = prop_ori_palette, name = "Proportion Oriental b.") +
  geom_line() +
  scale_linetype_manual(values = c("solid", "dashed", "dotted"), name = "Age class") +
  geom_ribbon(aes(ymin = mean_HI - sd_HI,
                  ymax = mean_HI + sd_HI,
                  fill = factor(Proportion_orientalis),
                  group = interaction(Proportion_orientalis, age_class)),
              alpha = 0.2, colour = NA) +
  scale_fill_manual(values = prop_ori_palette) +
  scale_y_continuous(limits=c(0,1))+
  guides(fill = "none") +
  labs(x = "Year", y = "Mean HI")+
  facet_grid(selection ~ configuration) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))


# plot the W over time 
ggplot(dt_meanrep,
       aes(year,  mean_W,
           colour = Proportion_orientalis,
           linetype = factor(age_class),
           group = interaction(Proportion_orientalis, age_class))) +
  scale_colour_manual(values = prop_ori_palette, name = "Proportion Oriental b.") +
  geom_line() +
  scale_linetype_manual(values = c("solid", "dashed", "dotted"), name = "Age class") +
  geom_ribbon(aes(ymin = mean_W - sd_W,
                  ymax = mean_W + sd_W,
                  fill = factor(Proportion_orientalis),
                  group = interaction(Proportion_orientalis, age_class)),
              alpha = 0.2, colour = NA) +
  scale_fill_manual(values = prop_ori_palette) +
  scale_y_continuous(limits=c(0.6,1))+
  guides(fill = "none") +
  labs(x = "Year", y = "Median W")+
  facet_grid(selection ~ configuration) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))



######## line plot configurations across time ############

### median HI
ggplot(dt_meanrep, aes(year, mean_HI,
                        colour =configuration,
                        group  =interaction(configuration, Proportion_orientalis),  
                        linetype = factor(Proportion_orientalis))) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = mean_HI - sd_HI,
                  ymax = mean_HI + sd_HI,
                  fill = factor(configuration)
                ),
              alpha = 0.1,
              colour = NA) +
  scale_colour_manual(values = config_palette, name = "Configuration") +
  scale_fill_manual(values = config_palette) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted"), name = "Proportion Oriental b.") +
  labs(x = "Year", y = "Mean HI")+
  facet_grid(age_class~ selection) +
  guides(fill="none")+
  theme_bw()

# zoom first years
ggplot(subset(dt_meanrep,year<=100), aes(year, mean_HI,
                         colour =configuration,
                         group  =interaction(configuration, Proportion_orientalis),  
                         linetype = factor(Proportion_orientalis))) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = mean_HI - sd_HI,
                  ymax = mean_HI + sd_HI,
                  fill = factor(configuration)
  ),
  alpha = 0.1,
  colour = NA) +
  scale_colour_manual(values = config_palette, name = "Configuration") +
  scale_fill_manual(values = config_palette) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted"), name = "Proportion Oriental b.") +
  labs(x = "Year", y = "Mean HI")+
  facet_grid(age_class~ selection) +
  guides(fill="none")+
  theme_bw()

### median W
ggplot(dt_meanrep, aes(year, mean_W,
                       colour =configuration,
                       group  =interaction(configuration, Proportion_orientalis),  
                       linetype = factor(Proportion_orientalis))) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = mean_W - sd_W,
                  ymax = mean_W + sd_W,
                  fill = factor(configuration)
  ),
  alpha = 0.1,
  colour = NA) +
  scale_colour_manual(values = config_palette, name = "Configuration") +
  scale_fill_manual(values = config_palette) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted"), name = "Proportion Oriental b.") +
  labs(x = "Year", y = "Mean W")+
  facet_grid(age_class~ selection) +
  guides(fill="none")+
  theme_bw()

## zoom first years

ggplot(subset(dt_meanrep, year <= 100), aes(as.numeric(year), mean_W,
                                                  colour =configuration,
                                                  group  =interaction(configuration, Proportion_orientalis),  
                                                  linetype = factor(Proportion_orientalis))) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = mean_W - sd_W,
                  ymax = mean_W + sd_W,
                  fill = factor(configuration)
  ),
  alpha = 0.1,
  colour = NA) +
  scale_colour_manual(values = config_palette, name = "Configuration") +
  scale_fill_manual(values = config_palette) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted"), name = "Proportion Oriental b.") +
  labs(x = "Year", y = "Mean W")+
  facet_grid(age_class~ selection) +
  guides(fill="none")+
  theme_bw()


######## HI and W for one specific year across simulations ###############

dt_median_sub <- subset(dt_median, year %in% c(50, 100,1000))

ggplot(dt_median_sub, 
       aes(Proportion_orientalis, med_HI, fill = configuration, group = interaction(configuration, Proportion_orientalis))) +
  geom_violin() +
  geom_point(data = subset(dt_pop_meanrep, year %in% c(50, 100,1000)),
             aes(x = Proportion_orientalis,
                 y = mean_HI,
                 group = interaction(configuration, Proportion_orientalis)),
             colour = "black",
             size = 2,
             position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = config_palette, name = "Configuration") +
  facet_grid( selection ~year  ) +
  theme_bw()+
  labs(x="Proportion of Oriental beech", y="Mean HI")

### check the results in numbers
HI_summary <- dt_median_sub[, .(
  mean_HI = mean(med_HI)
), by = .(year, configuration,Proportion_orientalis,selection)]

# rank configuration from highest to lowest HI
HI_summary[order(year, selection, Proportion_orientalis, -mean_HI)]
# is the pattern consistent? 
HI_summary[, .SD[which.max(mean_HI)], by = .(year,selection,Proportion_orientalis)]



ggplot(dt_median_sub, 
       aes(Proportion_orientalis, med_W, fill = configuration, group = interaction(configuration, Proportion_orientalis))) +
  geom_violin() +
  geom_point(data = subset(dt_pop_meanrep, year %in% c(50, 100,1000)),
             aes(x = Proportion_orientalis,
                 y = mean_W,
                 group = interaction(configuration, Proportion_orientalis)),
             colour = "black",
             size = 2,
             position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = config_palette, name = "Configuration") +
  facet_grid( selection ~year , scales = "free_y" ) +
  scale_y_continuous(limits = c(0.6, 1))+
  theme_bw()+
  labs(x="Proportion of Oriental beech", y="Mean W")



## chekc restuls for fitness
W_summary <- dt_median_sub[, .(
  mean_W = mean(med_W)
), by = .(year, configuration,Proportion_orientalis,selection)]

# rank configuration from highest to lowest HI
W_summary[order(year, selection, Proportion_orientalis, -mean_W)]
# is the pattern consistent? 
W_summary[, .SD[which.max(mean_W)], by = .(year,selection,Proportion_orientalis)]

######## pure and hybrid proportions across time ####
dt_pures <- dt[, .(
  N = .N,
  N_orientalis = sum(P1 == 1),
  N_sylvatica  = sum(P1 == -1),
  N_hybrid     = sum(P1 > -1 & P1 < 1),
  
  prop_orientalis = mean(P1 == 1),
  prop_sylvatica  = mean(P1 == -1),
  prop_hybrid     = mean(P1 > -1 & P1 < 1)
),

by = .(configuration, Proportion_orientalis, selection, cost, year, run, replicate, age_class)]


## plot
# add label for stage
nemo_stage <- c("1" = "Stage 1", "2" = "Stage 2", "3" = "Stage 3")
dt_pures$nemo_stage <- nemo_stage[dt_pures$age_class]
dt_pures$run_rep <- paste0(dt_pures$run,"_",dt_pures$replicate)

# collapse simulation replicates (also mean across all runs)
dt_pures_medrep <- dt_pures[, .(

  med_prop_ori = median(prop_orientalis),
  q25_prop_ori = quantile(prop_orientalis, 0.25),
  q75_prop_ori = quantile(prop_orientalis, 0.75),
  med_prop_syl = median(prop_sylvatica),
  q25_prop_syl = quantile(prop_sylvatica, 0.25),
  q75_prop_syl = quantile(prop_sylvatica, 0.75),
  med_prop_hyb = median(prop_hybrid),
  q25_prop_hyb = quantile(prop_hybrid, 0.25),
  q75_prop_hyb = quantile(prop_hybrid, 0.75)
),
by = .(configuration, Proportion_orientalis, selection, year, age_class,cost)]

dt_pures_medrep$config_prop <- paste0(dt_pures_medrep$configuration, "_",dt_pures_medrep$Proportion_orientalis)

# reshape 
dt_pures_long <- melt(
  dt_pures_medrep,
  id.vars = c("configuration", "Proportion_orientalis",
              "selection", "cost", "year", "age_class"),
  measure = patterns(med_prop = "^med_", q25 = "^q25_", q75 = "^q75_"),
  variable.name = "Genotype"
)

dt_pures_long[, Genotype := c("orientalis","sylvatica","hybrid")[Genotype]]

dt_pures_long[, Genotype := factor(Genotype,levels = c("orientalis", "sylvatica", "hybrid"))]
my_cols <- c(
  orientalis = "#482173FF", 
  sylvatica  = "#ffcc00",  
  hybrid     = "#25858EFF" 
)

ggplot(dt_pures_long,
       aes(x = year,
           y = med_prop,
           colour = Genotype,
           fill = Genotype,
           linetype= factor(age_class),
           group = interaction(Genotype, age_class))) +
  geom_line() +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  
  geom_ribbon(aes(ymin = q25,
                  ymax = q75),
              alpha = 0.2,
              colour = NA) +
  scale_colour_manual(values = my_cols) +
  scale_fill_manual(values = my_cols) +
  labs(x = "Year", y = "Proportion")+
  facet_nested( Proportion_orientalis  ~  selection +configuration) +
  theme_bw()

## zoom into the first years for each class
ggplot(subset(dt_pures_long,year<= 50&age_class ==1),
       aes(x = year,y = med_prop,colour = Genotype,fill = Genotype,linetype= factor(age_class),
           group = interaction(Genotype, age_class))) +
  geom_line() +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  geom_ribbon(aes(ymin = q25,
                  ymax = q75),
              alpha = 0.2,colour = NA) +
  scale_colour_manual(values = my_cols) +
  scale_fill_manual(values = my_cols) +
  facet_nested( Proportion_orientalis  ~  selection +configuration) +
  theme_bw()

ggplot(subset(dt_pures_long,year<= 50&age_class ==2),
       aes(x = year,y = med_prop,colour = Genotype,fill = Genotype,linetype= factor(age_class),
           group = interaction(Genotype, age_class))) +
  geom_line() +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  geom_ribbon(aes(ymin = q25,
                  ymax = q75),
              alpha = 0.2,colour = NA) +
  scale_colour_manual(values = my_cols) +
  scale_fill_manual(values = my_cols) +
  facet_nested( Proportion_orientalis  ~  selection +configuration) +
  theme_bw()

ggplot(subset(dt_pures_long,year<= 50&age_class ==3),
       aes(x = year,y = med_prop,colour = Genotype,fill = Genotype,linetype= factor(age_class),
           group = interaction(Genotype, age_class))) +
  geom_line() +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  geom_ribbon(aes(ymin = q25,
                  ymax = q75),
              alpha = 0.2,colour = NA) +
  scale_colour_manual(values = my_cols) +
  scale_fill_manual(values = my_cols) +
  facet_nested( Proportion_orientalis  ~  selection +configuration) +
  theme_bw()

######## spatial patterns of HI and W: replicates variability at patch level (TO DO) ########

dt <- readRDS(file.path(res_path, "Quanti_fit_data_fitness.RDS"))

# hybrid proportions and HI values per simulation and replicate and patch (across all individuals) = 1 row x sim replicate x year x age_class x patch
dt_median_patch <- dt[,.( 
  N = .N,
  N_hybrid = sum(P1 > -1 & P1 < 1),
  prop_hybrids = mean(P1 > -1 & P1 < 1),
  med_HI = median(1 - abs(P1)),  
  q25_HI = quantile(1 - abs(P1), 0.25),
  q75_HI = quantile(1 - abs(P1), 0.75),
  med_W = median(W), 
  q25_W = quantile(W, 0.25),
  q75_W = quantile(W, 0.75)
),
by = .(configuration, Proportion_orientalis, selection, year, sim_id,pop, age_class, cost) ]

gc()

# compute SD across replicates for each patch, for HI and W
dt_median_patch_SD <- dt_median_patch[, .(
  sd_HI = sd(med_HI, na.rm = TRUE), 
  sd_W = sd(med_W, na.rm = TRUE)
), by = .(configuration, Proportion_orientalis, selection, year, age_class, pop, W)]

# map the SD per patch for each age_class for year 150  and year 1000
grid <- vect("C:/Users/stefanin/Dropbox/WSL_PhD/Projects/Hybridization2/input/input_files/Grid_4x4m_100x100m.shp")
grid_r <- terra::rast(ext = ext(grid), resolution = 4, crs = "EPSG:3035")
n_rows = dim(grid_r)[1]
n_cols = dim(grid_r)[2]

ggplot(subset(dt_median_patch_SD,age_class ==1& year == 150),
       aes(x = (pop - 1) %% n_rows + 1,
           y = n_cols - ((pop - 1) %/% n_cols + 1),
           fill = sd_HI)) +
  geom_raster() +
  scale_fill_viridis_c(option = "magma", na.value = "white") +
  facet_grid(configuration ~selection + Proportion_orientalis) +
  theme_void() +
  labs(title = "SD of HI per patch for year 150")+
  theme(aspect.ratio = 1,
        legend.position = "bottom")

ggplot(subset(dt_hi_meanpatch_sdrep, age_class ==2& year == 1000),
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


## compute median+quantile across patches SD(HI)
sdHI_summary <- dt_hi_meanpatch_sdrep[
  !is.na(sd_HI),
  .(
    med_patch_sd = median(sd_HI),
    q25_patch_sd = quantile(sd_HI, 0.25),
    q75_patch_sd = quantile(sd_HI, 0.75),
    max_patch_sd = max(sd_HI)
  ),
  by = .(configuration, Proportion_orientalis, selection, year, age_class)]

ggplot(sdHI_summary,
       aes(x = year,
           y = med_patch_sd,
           group = interaction(factor(age_class), Proportion_orientalis),
           color = factor(Proportion_orientalis), linetype = factor(age_class))) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted"), name = "Age class") +
  scale_color_manual(values = prop_ori_palette,  name = "Proportion of Oriental b.") +
  scale_fill_manual(values = prop_ori_palette) +
  
  geom_ribbon(aes(ymin = q25_patch_sd,
                  ymax = q75_patch_sd,
                  fill = factor(Proportion_orientalis)),
              alpha = 0.1,
              color = NA) +
  labs(y = "Median SD HI per patch + IQR")+
  guides(fill = "none")+
  geom_line() +
  theme_bw() +
  facet_grid(configuration ~ selection)



sdW_summary <- dt_w_meanpatch_sdrep[
  !is.na(sd_W),
  .(
    med_patch_sd = median(sd_W),
    q25_patch_sd = quantile(sd_W, 0.25),
    q75_patch_sd = quantile(sd_W, 0.75),
    max_patch_sd = max(sd_W)
  ),
  by = .(configuration, Proportion_orientalis, selection,cost, year)]

ggplot(sdW_summary,
       aes(x = year,
           y = med_patch_sd,
           group = Proportion_orientalis,
           color = Proportion_orientalis)) +
  scale_color_manual(values = prop_ori_palette,  name = "Proportion of Oriental b.") +
  scale_fill_manual(values = prop_ori_palette) +
  
  geom_ribbon(aes(ymin = q25_patch_sd,
                  ymax = q75_patch_sd,
                  fill = factor(Proportion_orientalis)),
              alpha = 0.1,
              color = NA) +
  labs(y = "Median SD W per patch + IQR")+
  guides(fill = "none")+
  geom_line() +
  theme_bw() +
  facet_grid(configuration ~ selection)


######## spatial patterns of HI: median HI ########

dt <- readRDS(file.path(res_path, "Quanti_fit_data_fitness.RDS"))

# select only year 2, 100 and 1000 (otherwise too slow)
dt_sub <- subset(dt, year %in% c(10,50, 150,1000))


# hybrid proportions and HI values per simulation and replicate and patch (across all individuals) = 1 row x sim replicate x year x age_class x patch
dt_median_patch <- dt_sub[,.( 
  N = .N,
  N_hybrid = sum(P1 > -1 & P1 < 1),
  prop_hybrids = mean(P1 > -1 & P1 < 1),
  med_HI = median(1 - abs(P1)),  
  q25_HI = quantile(1 - abs(P1), 0.25),
  q75_HI = quantile(1 - abs(P1), 0.75),
  med_W = median(W), 
  q25_W = quantile(W, 0.25),
  q75_W = quantile(W, 0.75)
),
by = .(configuration, Proportion_orientalis, selection, year, sim_id,pop, age_class, cost) ]

gc()


# median HI across replicates per patch
dt_median_patch_mean_rep <- dt_median_patch[, .(
  med_HI = median(mean_HI, na.rm = TRUE)
), by = .(configuration, Proportion_orientalis, cost, selection, year, age_class, pop)]


# reorder proportion orientalis 
dt_hi_meanpatch_medrep_sub[, Proportion_orientalis :=  factor(Proportion_orientalis, levels = rev(sort(unique(as.numeric(as.character(Proportion_orientalis))))))]

## SPATIAL DISTRIBUTION FOR SEEDLINGS
ggplot(subset(dt_hi_meanpatch_medrep_sub, age_class == 1),
       aes(x = (pop - 1) %% n_rows + 1,
           y = n_cols - ((pop - 1) %/% n_cols + 1),
           fill = med_HI)) +
  geom_raster() +
  scale_fill_viridis_c(option = "viridis", direction = -1) +
  facet_nested(
    Proportion_orientalis + configuration ~ selection + year
  ) +
  theme(
    ggh4x.facet.nestline = element_line(size = 1),
    strip.background = element_rect(fill = "grey90"),
    #aspect.ratio = 1,
    legend.position = "bottom",
    panel.spacing.x = unit(0.1, "lines"),
    panel.spacing.y = unit(0.1, "lines"),
    strip.text = element_text(size = 10)
  )+
  labs(x = "", y = "", fill = "HI") +
  theme_void() +
  labs(title = "Spatial distribution of median HI for seedlings") 

## SPATIAL DISTRIBUTION FOR YOUNG ADULTS
ggplot(subset(dt_hi_meanpatch_medrep_sub, age_class == 2),
       aes(x = (pop - 1) %% n_rows + 1,
           y = n_cols - ((pop - 1) %/% n_cols + 1),
           fill = med_HI)) +
  geom_raster() +
  scale_fill_viridis_c(option = "viridis", direction = -1) +
  facet_nested(
    Proportion_orientalis + configuration ~ selection + year
  ) +
  theme(
    ggh4x.facet.nestline = element_line(size = 1),
    strip.background = element_rect(fill = "grey90"),
    #aspect.ratio = 1,
    legend.position = "bottom",
    panel.spacing.x = unit(0.1, "lines"),
    panel.spacing.y = unit(0.1, "lines"),
    strip.text = element_text(size = 10)
  )+
  labs(x = "", y = "", fill = "HI") +
  theme_void() +
  labs(title = "Spatial distribution of median HI for juveniles") 

## SPATIAL DISTRIBUTION FOR ADULTS
ggplot(subset(dt_hi_meanpatch_medrep_sub, age_class == 3),
       aes(x = (pop - 1) %% n_rows + 1,
           y = n_cols - ((pop - 1) %/% n_cols + 1),
           fill = med_HI)) +
  geom_raster() +
  scale_fill_viridis_c(option = "viridis", direction = -1) +
  facet_nested(
    Proportion_orientalis + configuration ~ selection + year
  ) +
  theme(
    ggh4x.facet.nestline = element_line(size = 1),
    strip.background = element_rect(fill = "grey90"),
    #aspect.ratio = 1,
    legend.position = "bottom",
    panel.spacing.x = unit(0.1, "lines"),
    panel.spacing.y = unit(0.1, "lines"),
    strip.text = element_text(size = 10)
  )+
  labs(x = "", y = "", fill = "HI") +
  theme_void() +
  labs(title = "Spatial distribution of median HI for adults") 


######## spatial patterns of W: median W ########

# median HI across replicates per patch
dt_w_meanpatch_medrep <- dt_w_meanpatch[, .(
  med_W = median(mean_W, na.rm = TRUE)
), by = .(configuration, Proportion_orientalis, cost, selection, year, pop)]

# select only year 2, 100 and 1000
dt_w_meanpatch_medrep_sub <- subset(dt_w_meanpatch_medrep, year %in% c(10,50, 150,1000))

# reorder proportion orientalis 
dt_w_meanpatch_medrep_sub[, Proportion_orientalis :=  factor(Proportion_orientalis, levels = rev(sort(unique(as.numeric(as.character(Proportion_orientalis))))))]

## SPATIAL DISTRIBUTION FOR whole population
ggplot(dt_w_meanpatch_medrep_sub,
       aes(x = (pop - 1) %% n_rows + 1,
           y = n_cols - ((pop - 1) %/% n_cols + 1),
           fill = med_W)) +
  geom_raster() +
  scale_fill_viridis_c(option = "viridis", direction = -1) +
  facet_nested(
    Proportion_orientalis + configuration ~ selection + year
  ) +
  theme(
    ggh4x.facet.nestline = element_line(size = 1),
    strip.background = element_rect(fill = "grey90"),
    #aspect.ratio = 1,
    legend.position = "bottom",
    panel.spacing.x = unit(0.1, "lines"),
    panel.spacing.y = unit(0.1, "lines"),
    strip.text = element_text(size = 10)
  )+
  labs(x = "", y = "", fill = "w") +
  theme_void() +
  labs(title = "Spatial distribution of median w") 

######## plot cost versus mean HI ######

stage_label <- c("1" = "Seedlings", "2" = "Juveniles", "3" = "Adults")
dt_meanrep$stage_label <- stage_label[dt_meanrep$age_class]
dt_meanrep$stage_label <- factor(dt_meanrep$stage_label,levels = c("Adults", "Juveniles", "Seedlings"))

rng <- range(dt_meanrep$cost)
breaks <- seq(rng[1], rng[2], length.out = 4)
breaks


ggplot(subset(dt_meanrep,year==150),  aes(mean_HI, cost, 
                                            color = configuration, 
                                            group = interaction(configuration, Proportion_orientalis)))+ 
  geom_point(aes(size = Proportion_orientalis))+ 
  scale_color_manual(values = config_palette, name = "Configuration")+
  scale_y_continuous(
    breaks = (breaks[-1] + breaks[-length(breaks)]) / 2,
    labels = c("Low", "Medium", "High")
  )+
  labs(title = "Cost versus HI after 150 years for the three target age classes", y = "Estimated cost", x = "Mean HI")+
  facet_grid(stage_label ~ selection, scales = "free_x")+
  theme_bw()


ggplot(subset(dt_meanrep,year==1000),  aes(mean_HI, cost, 
                                             color = configuration, 
                                             group = interaction(configuration, Proportion_orientalis)))+ 
  geom_point(aes(size = Proportion_orientalis))+ 
  theme_bw()+
  scale_y_continuous(
    breaks = (breaks[-1] + breaks[-length(breaks)]) / 2,
    labels = c("Low", "Medium", "High")
  )+
  scale_color_manual(values = config_palette, name = "Configuration")+
  labs(title = "Cost versus HI after 1000 years for the three target age classes", y = "Estimated cost", x = "Mean HI")+
  facet_grid(stage_label ~ selection, scales = "free_x")

######## plot cost versus mean W ######

ggplot(subset(dt_meanrep,year==150),  aes(mean_W, cost, 
                                            color = configuration, 
                                            group = interaction(configuration, Proportion_orientalis)))+ 
  geom_point(aes(size = Proportion_orientalis))+ 
  theme_bw()+
  scale_y_continuous(
    breaks = (breaks[-1] + breaks[-length(breaks)]) / 2,
    labels = c("Low", "Medium", "High")
  )+
  scale_color_manual(values = config_palette, name = "Configuration")+
  labs(title = "Cost versus W after 150 years", y = "Estimated cost", x = "Fitness (W)")+
  facet_grid(stage_label ~ selection)

ggplot(subset(dt_meanrep,year==1000),  aes(mean_W, cost, 
                                             color = configuration, 
                                             group = interaction(configuration, Proportion_orientalis)))+ 
  geom_point(aes(size = Proportion_orientalis))+ 
  theme_bw()+
  scale_y_continuous(
    breaks = (breaks[-1] + breaks[-length(breaks)]) / 2,
    labels = c("Low", "Medium", "High")
  )+
  scale_color_manual(values = config_palette, name = "Configuration")+
  labs(title = "Cost versus Median W after 1000 years", y = "Estimated cost", x = "Fitness (W)")+
  facet_grid(stage_label ~ selection)

