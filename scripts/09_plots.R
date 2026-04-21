
### the script: 
# loads all the results from Quanti_data.R object and process the dataset
# create different plots

library(data.table)
library(stringr)
library(ggplot2)
library(terra)
library(ggh4x)
library(ggpubr)

res_path <- "C:/Users/stefanin/Dropbox/WSL_PhD/Projects/Hybridization2/output/analysis"
#dir.create("C:/Users/stefanin/Dropbox/WSL_PhD/Projects/Hybridization2/Figures_manuscript")
fig_path <- "C:/Users/stefanin/Dropbox/WSL_PhD/Projects/Hybridization2/Figures_manuscript"

######## plotting settings ###############

prop_ori_palette <- c(
  "0.1" = "#CE93D8", 
  "0.25" = "#8E24AA",  
  "0.4" = "#311B92"   
)

## order colol by cost
config_palette <- c(
  "Dispersed" = "#d7191c",
  "Multiple clusters" = "#E69F00",
  "Transects" = "#abd9e9",
  "Single cluster" = "#2c7bb6"
)


theme_fig <- theme(
  axis.text.x = element_text(hjust = 0.5),
  strip.text = element_text(size = 15),
  strip.background = element_blank(),
  axis.title = element_text(size = 16),
  axis.text  = element_text(size = 10),
  legend.title = element_text(size = 12),
  legend.text  = element_text(size = 10),
  plot.title = element_text(size = 16, hjust = 0.5),
  panel.grid = element_blank(),
  panel.background = element_blank(), 
  axis.line = element_line(linewidth = 0.3, colour = "black"),
  axis.ticks = element_line(linewidth = 0.3),
  axis.ticks.length = unit(1.5, "mm"),
  legend.key = element_blank(),
  legend.position = "right"
)



######## demographic results ###########
dt1 <- readRDS(file.path(res_path, "Demo_data_processed.RDS"))
dt1$replicate2 <- paste0(dt1$run, "_",dt1$replicate)

dt1$selection_type <- factor(dt1$selection_type, levels = c("Neutral","European b. selected against","Oriental b. selected against","Heterosis"))

############################### demographic trends average simulation 
ggplot(subset(dt1, age_class!= "Total Population"), aes(x = year, y = N_stage, group = interaction(age_class, replicate2), col = age_class)) +
  geom_line(linewidth = 0.2, alpha = 0.8) +
  facet_nested(configuration+proportion_orientalis ~ selection_type+selection_strength, scales="free_y") +
  labs( y = "Number of individuals",
        color = "Nemo Stage"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

ggsave(
  "Figures_presentations/Demographic_allstages.jpg",
  plot = last_plot(),   # or assign your plot to an object and use plot = p
  width = 15,
  height = 13,
  units = "in"
)

# only adults
ggplot(subset(dt1, age_class== "Stage 3"), aes(x = year, y = N_stage, group = interaction(age_class, replicate2), col = age_class)) +
  geom_line(linewidth = 0.2, alpha = 0.8) +
  facet_nested(configuration+proportion_orientalis ~ selection_type+selection_strength, scales="free_y") +
  labs( y = "Number of individuals",
        color = "Nemo Stage"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

ggsave(
  paste0(fig_path,"/Demographic_plot_adults.jpg"),
  plot = last_plot(),   # or assign your plot to an object and use plot = p
  width = 15,
  height = 13,
  units = "in"
)

############################### compare n adults after 100 years ("biomass production")

dt1_sub <- subset(dt1,  year==100 & age_class %in% "Stage 3")

### plot the mean number of adults 

ggplot(dt1_sub, 
       aes(factor(configuration), N_stage, 
           fill = configuration,group = configuration)) +
  geom_boxplot() +
  scale_fill_manual(values = config_palette, name = "Configuration") +
  scale_color_manual(values = config_palette, name = "Configuration") +
  
  facet_nested( proportion_orientalis ~ selection_type+selection_strength ) +
  stat_compare_means(aes(group = configuration), method = "kruskal.test"  ,  label = "p.signif", 
                     hide.ns = TRUE,  label.y = 70) +
  labs(x="Configuration", y="Mean N adults (year 100)")+
  theme_bw(base_size = 12) +
  guides(fill = "none", color = "none")+ theme( axis.text.x = element_text( hjust = 1,angle = 90))


rm(dt1_sub)

######## fitness results ########
dt2 <- readRDS(file.path(res_path, "Fit_data_processed.RDS"))
dt2_summary <- readRDS(file.path(res_path, "Fit_data_summary_replicates.RDS") )

dt2$selection_type <- factor(dt2$selection_type, levels = c("Neutral","European b. selected against","Oriental b. selected against","Heterosis"))
dt2_summary$selection_type <- factor(dt2_summary$selection_type, levels = c("Neutral","European b. selected against","Oriental b. selected against","Heterosis"))

###############################  W over time

ggplot(dt2_summary,
       aes(year,  q50_W,
           colour = proportion_orientalis,
           linetype = factor(age_class),
           fill= proportion_orientalis,
           group = interaction(proportion_orientalis, age_class))) +
  scale_colour_manual(values = prop_ori_palette, name = "Proportion Oriental b.") +
  geom_line() +
  scale_linetype_manual(values = c("solid", "dashed", "dotted"), name = "Age class") +
  geom_ribbon(aes(ymin = q10_W, ymax = q90_W),alpha = 0.1,colour = NA) +
  scale_fill_manual(values = prop_ori_palette) +
  scale_y_continuous(limits=c(0.6,1))+
  guides(fill = "none") +
  labs(x = "Year", y = "Median W")+
  facet_nested(configuration  ~ selection_type + selection_strength) +
  theme_bw()

############################### W line plot configurations across time

ggplot(dt2_summary, aes(year, q50_W,
                       colour =configuration,
                       fill= configuration,
                       group  =interaction(configuration, proportion_orientalis),  
                       linetype = factor(proportion_orientalis))) +
  geom_line(size = 1, alpha = 0.5) +
  geom_ribbon(aes(ymin = q10_W, ymax = q90_W),alpha = 0.1,colour = NA) +
  scale_colour_manual(values = config_palette, name = "Configuration") +
  scale_fill_manual(values = config_palette) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted"), name = "Proportion Oriental b.") +
  labs(x = "Year", y = "Mean W")+
  facet_nested(age_class~ selection_type + selection_strength) +
  guides(fill="none")+
  theme_bw()

## zoom first years

ggplot(subset(dt2_summary,year <=100), 
              aes(year, q50_W,
                        colour =configuration,
                        fill= configuration,
                        group  =interaction(configuration, proportion_orientalis),  
                        linetype = factor(proportion_orientalis))) +
  geom_line(size = 1, alpha = 0.5) +
  geom_ribbon(aes(ymin = q10_W, ymax = q90_W),alpha = 0.1,colour = NA) +
  scale_colour_manual(values = config_palette, name = "Configuration") +
  scale_fill_manual(values = config_palette) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted"), name = "Proportion Oriental b.") +
  labs(x = "Year", y = "Mean W")+
  facet_nested(age_class~ selection_type + selection_strength) +
  guides(fill="none")+
  theme_bw()

############################### W for year 100

## ALL AGE CLASSES POOLED
dt2_median <- dt2[,.( 
  med_W = median(W), 
  q25_W = quantile(W, 0.25, na.rm = T),
  q75_W = quantile(W, 0.75, na.rm = T)
),
by = .(sim_id, configuration, proportion_orientalis, selection_type,selection_strength, year, age_class, run, replicate) ]

ggplot(subset(dt2_median,  year==100),
       aes(configuration, med_W, fill = configuration, group = interaction(configuration))) +
  geom_violin() +
  geom_point(data = subset(dt2_summary,  year==100),
             aes(x = configuration,
                 y = q50_W,
                 group = interaction(configuration)),
             colour = "black",
             size = 2,
             position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = config_palette, name = "Configuration") +
  #stat_compare_means(aes(group = configuration), method = "kruskal.test"  ,  label = "p.signif", hide.ns = TRUE,  label.y = 0.7) +
  facet_nested( proportion_orientalis ~ selection_type+selection_strength ) +
  theme_bw()+
  labs(x="Configuration", y="Median W")+
  theme( axis.text.x = element_text( hjust = 1,angle = 90))


############################### spatial patterns of W: replicates variability at patch level (TO DO)
            
dt2_median_patch <-  readRDS(file.path(res_path, "W_median_patch.RDS") )
            
# compute SD across replicates for each patch
dt2_median_patch_SD <- dt2_median_patch[, .(
  sd_W = sd(med_W, na.rm = TRUE)
), by = .(configuration, proportion_orientalis, selection_type, selection_strength, year,age_class, pop)]

# map the SD per patch for each age_class for year 150  and year 1000
grid <- vect("C:/Users/stefanin/Dropbox/WSL_PhD/Projects/Hybridization2/input/input_files/Grid_4x4m_100x100m.shp")
grid_r <- terra::rast(ext = ext(grid), resolution = 4, crs = "EPSG:3035")
n_rows = dim(grid_r)[1]
n_cols = dim(grid_r)[2]

ggplot(subset(dt2_median_patch_SD,age_class ==1& year == 100),
       aes(x = (pop - 1) %% n_rows + 1,
           y = n_cols - ((pop - 1) %/% n_cols + 1),
           fill = sd_W)) +
  geom_raster() +
  scale_fill_viridis_c(option = "magma", na.value = "white") +
  facet_nested(configuration + proportion_orientalis~ selection_type+selection_strength ) +
  theme_void() +
  labs(title = "SD of W per patch for year 100")+
  theme(aspect.ratio = 1,
        legend.position = "bottom")


ggplot(subset(dt2_median_patch_SD, age_class ==2& year == 1000),
       aes(x = (pop - 1) %% n_rows + 1,
           y = n_cols - ((pop - 1) %/% n_cols + 1),
           fill = sd_W)) +
  geom_raster() +
  scale_fill_viridis_c(option = "magma", na.value = "white") +
  facet_nested(configuration + proportion_orientalis~ selection_type+selection_strength ) +
  theme_void() +
  labs(title = "SD of HI per patch for year 1000")+
  theme(aspect.ratio = 1,
        legend.position = "bottom")


sdW_summary <- dt2_median_patch_SD[
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

rm(dt2_median_patch_SD) 


############################### spatial patterns of W: mean W across replicates 
dt2_median_patch <-  readRDS(file.path(res_path, "W_median_patch.RDS") )
dt2_median_patch_summary  <- readRDS(file.path(res_path, "W_median_patch_summary_replicates.RDS") )

# select only year 2, 100 and 1000
dt2_median_patch_summary_sub <- subset(dt2_median_patch_summary, year %in% c(100, 500))

# reorder proportion orientalis 
dt2_median_patch_summary_sub[, proportion_orientalis :=  factor(proportion_orientalis, levels = rev(sort(unique(as.numeric(as.character(proportion_orientalis))))))]
dt2_median_patch_summary_sub$selection_type <- factor(dt2_median_patch_summary_sub$selection_type, levels = c("Neutral","European b. selected against","Oriental b. selected against","Heterosis"))


######  t = 100

## SPATIAL DISTRIBUTION FOR SEEDLINGS
ggplot(subset(dt2_median_patch_summary_sub, age_class == 1& year==100),
       aes(x = (pop - 1) %% n_rows + 1,
           y = n_cols - ((pop - 1) %/% n_cols + 1),
           fill = q50_W)) +
  geom_raster() +
  scale_fill_viridis_c(option = "magma") +
  facet_nested(
    proportion_orientalis + configuration ~ selection_type + selection_strength + year
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
  labs(x = "", y = "", fill = "Median W") +
  theme_void() +
  labs(title = "Spatial distribution of W SEEDLINGS at t = 100") 

## SPATIAL DISTRIBUTION FOR ADULTS
ggplot(subset(dt2_median_patch_summary_sub, age_class == 3& year==100),
       aes(x = (pop - 1) %% n_rows + 1,
           y = n_cols - ((pop - 1) %/% n_cols + 1),
           fill = q50_W)) +
  geom_raster() +
  scale_fill_viridis_c(option = "magma") +
  facet_nested(
    proportion_orientalis + configuration ~ selection_type + selection_strength + year
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
  labs(x = "", y = "", fill = "Median W") +
  theme_void() +
  labs(title = "Spatial distribution of W ADULTS at t = 100") 

######  t = 500

## SPATIAL DISTRIBUTION FOR SEEDLINGS
ggplot(subset(dt2_median_patch_summary_sub, age_class == 1& year==500),
       aes(x = (pop - 1) %% n_rows + 1,
           y = n_cols - ((pop - 1) %/% n_cols + 1),
           fill = q50_W)) +
  geom_raster() +
  scale_fill_viridis_c(option = "magma") +
  facet_nested(
    proportion_orientalis + configuration ~ selection_type + selection_strength + year
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
  labs(x = "", y = "", fill = "Median W") +
  theme_void() +
  labs(title = "Spatial distribution of W SEEDLINGS at t = 500") 

## SPATIAL DISTRIBUTION FOR ADULTS
ggplot(subset(dt2_median_patch_summary_sub, age_class == 3& year==500),
       aes(x = (pop - 1) %% n_rows + 1,
           y = n_cols - ((pop - 1) %/% n_cols + 1),
           fill = q50_W)) +
  geom_raster() +
  scale_fill_viridis_c(option = "magma") +
  facet_nested(
    proportion_orientalis + configuration ~ selection_type + selection_strength + year
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
  labs(x = "", y = "", fill = "Median W") +
  theme_void() +
  labs(title = "Spatial distribution of W ADULTS at t = 500") 
            

######## quanti results ############

dt3 <- readRDS(file.path(res_path, "Quanti_data_processed.RDS"))
dt3$selection_type <- factor(dt3$selection_type, levels = c("Neutral","European b. selected against","Oriental b. selected against","Heterosis"))

hist(dt3$P1)
gc()

###############################  pure and hybrid proportions across time 


dt3_prop <- dt3[,.( 
  prop_orientalis = mean(P1 > 0.9),
  prop_sylvatica  = mean(P1 < -0.9),
  prop_hybrid     = mean(P1 >= -0.9 & P1 <= 0.9)
),
by = .(configuration, proportion_orientalis, cost, selection_type,selection_strength, year, age_class, run, replicate) ]


dt3_genot_long <- dt3_prop %>%
  tidyr::pivot_longer(
    cols = starts_with("prop_"),
    names_to = c(".value", "Genotype"),
    names_pattern = "(prop)_(.*)"
  )

my_cols1 <- c(
  orientalis = "#482173FF", 
  sylvatica  = "#ffcc00",  
  hybrid     = "#25858EFF" 
)

dt3_genot_long$replicate2<- paste0(dt3_genot_long$run, "_",dt3_genot_long$replicate)

# one line per replicate
ggplot(dt3_genot_long, 
       aes(x = year,
           y = prop,
           color =factor(Genotype),
           linetype= factor(configuration),
           group = interaction(replicate2, configuration, Genotype))) +
  geom_line() +
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "longdash")) +
  
  scale_colour_manual(values = my_cols1) +
  scale_fill_manual(values = my_cols1) +
  labs(x = "Year", y = "Proportion")+
  facet_nested(proportion_orientalis  ~  selection_type + selection_strength) +
  theme_bw()


# summarize across replicates using quantiles
dt3_prop_quant <- dt3_prop[, .(
  
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
by = .(configuration, proportion_orientalis, cost,selection_type, selection_strength,age_class, year)]


dt3_genot_quant_long <- dt3_prop_quant %>%
  tidyr::pivot_longer(
    cols = starts_with("q"),
    names_to = c("quantile", "Genotype"),
    names_pattern = "(q[0-9]+)_(.*)"
  )

# get the ribbons
dt3_genot_quant_wide <- dt3_genot_quant_long %>%
  tidyr::pivot_wider(
    names_from = quantile,
    values_from = value
  )

my_cols2 <- c(
  ori = "#482173FF", 
  syl  = "#ffcc00",  
  hyb     = "#25858EFF" 
)

########### GOOD FIGURE

dt3_genot_quant_wide$selection_type <- factor(dt3_genot_quant_wide$selection_type, levels = c("Neutral","European b. selected against","Oriental b. selected against","Heterosis"))


ggplot(dt3_genot_quant_wide,
       aes(x = year,
           y = q50,
           colour = Genotype,
           fill = Genotype,
           linetype = factor(configuration),
           group = interaction(Genotype, configuration))) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = q10, ymax = q90),alpha = 0.1,colour = NA) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "longdash")) +
  scale_colour_manual(values = my_cols2) +
  scale_fill_manual(values = my_cols2) +
  labs(x = "Year", y = "Proportion") +
  facet_nested(proportion_orientalis ~ selection_type + selection_strength) +
  theme_fig



ggsave(
  paste0(fig_path,"/Genotypes_trend.png"),
  plot = last_plot(),   # or assign your plot to an object and use plot = p
  width = 20,
  height = 5,
  units = "in"
)

## only adults and mid selection 

ggplot(subset(dt3_genot_quant_wide,selection_strength %in% c("mid", NA) & age_class ==3),
       aes(x = year,
           y = q50,
           colour = Genotype,
           fill = Genotype,
           linetype = factor(configuration),
           group = interaction(Genotype, configuration))) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = q10, ymax = q90),alpha = 0.1,colour = NA) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "longdash")) +
  scale_colour_manual(values = my_cols2) +
  scale_fill_manual(values = my_cols2) +
  labs(x = "Year", y = "Proportion") +
  facet_nested(proportion_orientalis ~ selection_type + selection_strength) +
  theme_bw()



## ADULTS ONLY, ZOOM IN THE FIRST YEARS
ggplot(subset(dt3_genot_quant_wide, year <= 150& age_class==3), 
       aes(x = year,
           y = q50,
           colour = Genotype,
           fill = Genotype,
           linetype = factor(configuration),
           group = interaction(Genotype, configuration))) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = q10, ymax = q90),alpha = 0.1,colour = NA) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "longdash")) +
  scale_colour_manual(values = my_cols2) +
  scale_fill_manual(values = my_cols2) +
  labs(x = "Year", y = "Proportion") +
  facet_nested(proportion_orientalis ~ selection_type + selection_strength) +
  theme_bw()



###############################   HI/hyb proportions over time - line plot configurations

### mean hybriud proporitons IN ADULTS
ggplot(subset(dt3_genot_quant_wide,age_class=="3"& Genotype=="hyb"),  aes(year, q50,
                        colour =configuration,fill=configuration,
                        group  =interaction(configuration, proportion_orientalis),  
                        )) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = q10, ymax = q90),alpha = 0.1,colour = NA) +
  scale_colour_manual(values = config_palette, name = "Configuration") +
  scale_fill_manual(values = config_palette) +
  labs(x = "Year", y = "Hybrid proportions")+
  facet_nested(proportion_orientalis~ selection_type + selection_strength) +
  guides(fill="none")+
  theme_bw()


############################### hyb_proportions for year 100

ggplot(subset(dt3_prop,year==100&age_class==3), 
       aes(configuration, prop_hybrid, fill = configuration, group = interaction(configuration))) +
  geom_violin() +
  stat_summary(fun.y=median, geom="point", size=2, color="black")+
  scale_fill_manual(values = config_palette, name = "Configuration") +
  #stat_compare_means(aes(group = configuration), method = "kruskal.test"  ,  label = "p.signif", hide.ns = TRUE,  label.y = 0.7) +
  facet_nested( proportion_orientalis ~ selection_type+selection_strength ) +
  theme_bw()+
  labs(x="Configuration", y="Mean Hybrid proportions")+
  theme( axis.text.x = element_text( hjust = 1,angle = 90))


###################################### time to reach X % hybrids

##  hybrid proportion within replicate
dt3_prop <- dt3[,.( 
  prop_orientalis = mean(P1 > 0.9),
  prop_sylvatica  = mean(P1 < -0.9),
  prop_hybrid     = mean(P1 >= -0.9 & P1 <= 0.9)
),
by = .(configuration, proportion_orientalis, cost, selection_type,selection_strength, year, age_class, run, replicate) ]


# select only adults
dt3_prop_adults <- dt3_prop[age_class == 3]
# remove 1000 years?
dt3_prop_adults <- dt3_prop_adults[year<1000]

setorder(dt3_prop_adults,
         configuration, proportion_orientalis,selection_type, selection_strength,run, replicate, year)

# function to extract first crossing time
get_time_to_threshold_full <- function(dt, threshold, label) {
  
  # all replicate IDs
  all_reps <- unique(dt[, .(
    configuration, proportion_orientalis,selection_type, selection_strength,run, replicate
  )])
  
  # first crossing
  t_cross <- dt[prop_hybrid >= threshold,
                .SD[1],
                by = .(configuration, proportion_orientalis,selection_type, selection_strength,run, replicate)]
  
  # merge back
  t_full <- merge(all_reps, t_cross,
                  by = c("configuration", "proportion_orientalis","selection_type", "selection_strength","run", "replicate"),
                  all.x = TRUE)
  
  # remove replicates when not reached
  t_full[, time_to_threshold := ifelse(is.na(year), NA, year)]
  t_full[, reached := !is.na(year)]
  t_full[, threshold := label]
  
  return(t_full)
}

t10_full <- get_time_to_threshold_full(dt3_prop_adults, 0.1, "10%")
t30_full <- get_time_to_threshold_full(dt3_prop_adults, 0.3, "30%")
t50_full <- get_time_to_threshold_full(dt3_prop_adults, 0.5, "50%")

t_cross_full <- rbind(t30_full, t50_full)

ggplot(t_cross_full,
  #subset(t_cross_full,threshold=="50%"), 
       aes(x = factor(proportion_orientalis),
           y = time_to_threshold,
           fill = configuration,color = configuration,
           alpha = proportion_orientalis,
           group = interaction(proportion_orientalis, configuration))) +
  geom_boxplot(position = position_dodge(preserve = "single")) +
  #geom_violin(trim=T)+
  stat_compare_means(aes(group = interaction(configuration, proportion_orientalis)), method = "kruskal.test"  ,  label = "p.signif", hide.ns = TRUE) +
  facet_nested(threshold ~ selection_type + selection_strength) +
  scale_fill_manual(values = config_palette, name = "Configuration") +
  scale_color_manual(values = config_palette, name = "Configuration") +
  labs(x = "Proportion of Oriental beech introduced",
       y = "Time to reach hybrid threshold (years)") +
  guides(alpha="none")+
  theme_bw()

  



##################################### spatial patterns of HI: replicates variability at patch level (computationally intensive!!!) -- TO DO

dt3_median_patch<- readRDS(file.path(res_path, "Hyb_proportions_patch.RDS") )
dt3_median_patch$selection_type <- factor(dt3_median_patch$selection_type, levels = c("Neutral","European b. selected against","Oriental b. selected against","Heterosis"))

# compute SD across replicates for each patch
dt3_median_patch_SD <- dt3_median_patch[, .(
  sd_HI = sd(med_HI, na.rm = TRUE)
), by = .(configuration, proportion_orientalis, selection_type, selection_strength,year, age_class, cost,pop)]

# map the SD per patch for each age_class for year 100  and year 1000
grid <- vect("C:/Users/stefanin/Dropbox/WSL_PhD/Projects/Hybridization2/input/input_files/Grid_4x4m_100x100m.shp")
grid_r <- terra::rast(ext = ext(grid), resolution = 4, crs = "EPSG:3035")
n_rows = dim(grid_r)[1]
n_cols = dim(grid_r)[2]

ggplot(subset(dt3_median_patch_SD,age_class ==1& year == 100),
       aes(x = (pop - 1) %% n_rows + 1,
           y = n_cols - ((pop - 1) %/% n_cols + 1),
           fill = sd_HI)) +
  geom_raster() +
  scale_fill_viridis_c(option = "magma", na.value = "white") +
  facet_nested(configuration + proportion_orientalis~ selection_type+selection_strength ) +
  theme_void() +
  labs(title = "SD of HI per patch for year 100")+
  theme(aspect.ratio = 1,
        legend.position = "bottom")


ggplot(subset(dt3_median_patch_SD, age_class ==2& year == 1000),
       aes(x = (pop - 1) %% n_rows + 1,
           y = n_cols - ((pop - 1) %/% n_cols + 1),
           fill = sd_HI)) +
  geom_raster() +
  scale_fill_viridis_c(option = "magma", na.value = "white") +
  facet_nested(configuration + proportion_orientalis~ selection_type+selection_strength ) +
  theme_void() +
  labs(title = "SD of HI per patch for year 1000")+
  theme(aspect.ratio = 1,
        legend.position = "bottom")


## compute median+quantile across patches SD(HI)
sdHI_summary <- dt3_median_patch_SD[
  !is.na(sd_HI),
  .(
    med_patch_sd = median(sd_HI),
    q25_patch_sd = quantile(sd_HI, 0.25),
    q75_patch_sd = quantile(sd_HI, 0.75),
    max_patch_sd = max(sd_HI)
  ),
  by = .(configuration, proportion_orientalis, selection, year, age_class)]

ggplot(sdHI_summary,
       aes(x = year,
           y = med_patch_sd,
           group = interaction(factor(age_class), proportion_orientalis),
           color = factor(proportion_orientalis), linetype = factor(age_class))) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted"), name = "Age class") +
  scale_color_manual(values = prop_ori_palette,  name = "Proportion of Oriental b.") +
  scale_fill_manual(values = prop_ori_palette) +
  
  geom_ribbon(aes(ymin = q25_patch_sd,
                  ymax = q75_patch_sd,
                  fill = factor(proportion_orientalis)),
              alpha = 0.1,
              color = NA) +
  labs(y = "Median SD HI per patch + IQR")+
  guides(fill = "none")+
  geom_line() +
  theme_bw() +
  facet_grid(configuration ~  selection_type+selection_strength)



############################### spatial patterns of hybrid proportions

# map the SD per patch for each age_class for year 100  and year 1000
grid <- vect("C:/Users/stefanin/Dropbox/WSL_PhD/Projects/Hybridization2/input/input_files/Grid_4x4m_100x100m.shp")
grid_r <- terra::rast(ext = ext(grid), resolution = 4, crs = "EPSG:3035")
n_rows = dim(grid_r)[1]
n_cols = dim(grid_r)[2]


dt3_median_patch<- readRDS(file.path(res_path, "Hyb_proportions_patch.RDS") )            

dt3_median_patch_summary <- dt3_median_patch[, .(
  q10_hyb = quantile(prop_hybrid, 0.1, na.rm = TRUE),
  q50_hyb = quantile(prop_hybrid, 0.5, na.rm = TRUE),
  q90_hyb = quantile(prop_hybrid, 0.9, na.rm = TRUE)
),
by = .(configuration, proportion_orientalis, selection_type,selection_strength, year, age_class, cost, pop)]




# select only year 100 and 1000 (otherwise too slow)
dt3_median_patch_mean_rep_sub <- subset(dt3_median_patch_summary, year %in% c(100, 500))

dt3_median_patch_mean_rep_sub$selection_type <- factor(dt3_median_patch_mean_rep_sub$selection_type, levels = c("Neutral","European b. selected against","Oriental b. selected against","Heterosis"))
dt3_median_patch_mean_rep_sub[, proportion_orientalis :=  factor(proportion_orientalis, levels = rev(sort(unique(as.numeric(as.character(proportion_orientalis))))))]


######  t = 100

## SPATIAL DISTRIBUTION FOR SEEDLINGS
ggplot(subset(dt3_median_patch_mean_rep_sub, age_class == 1& year==100),
       aes(x = (pop - 1) %% n_rows + 1,
           y = n_cols - ((pop - 1) %/% n_cols + 1),
           fill = q50_hyb)) +
  geom_raster() +
  scale_fill_viridis_c(option = "viridis", direction = -1) +
  facet_nested(
    proportion_orientalis + configuration ~ selection_type + selection_strength + year
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
  labs(x = "", y = "", fill = "Median hybrid proportion") +
  theme_void() +
  labs(title = "Spatial distribution of hybrid SEEDLINGS at t = 100") 

## SPATIAL DISTRIBUTION FOR ADULTS
ggplot(subset(dt3_median_patch_mean_rep_sub, age_class == 3& year==100),
       aes(x = (pop - 1) %% n_rows + 1,
           y = n_cols - ((pop - 1) %/% n_cols + 1),
           fill = q50_hyb)) +
  geom_raster() +
  scale_fill_viridis_c(option = "viridis", direction = -1) +
  facet_nested(
    proportion_orientalis + configuration ~ selection_type + selection_strength + year
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
  labs(x = "", y = "", fill = "Median hybrid proportion") +
  theme_void() +
  labs(title = "Spatial distribution of hybrid ADULTS at t = 100") 

######  t = 500

## SPATIAL DISTRIBUTION FOR SEEDLINGS
ggplot(subset(dt3_median_patch_mean_rep_sub, age_class == 1& year==500),
       aes(x = (pop - 1) %% n_rows + 1,
           y = n_cols - ((pop - 1) %/% n_cols + 1),
           fill = q50_hyb)) +
  geom_raster() +
  scale_fill_viridis_c(option = "viridis", direction = -1) +
  facet_nested(
    proportion_orientalis + configuration ~ selection_type + selection_strength + year
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
  labs(x = "", y = "", fill = "Median hybrid proportion") +
  theme_void() +
  labs(title = "Spatial distribution of hybrid SEEDLINGS at t = 500") 

## SPATIAL DISTRIBUTION FOR ADULTS
ggplot(subset(dt3_median_patch_mean_rep_sub, age_class == 3& year==500),
       aes(x = (pop - 1) %% n_rows + 1,
           y = n_cols - ((pop - 1) %/% n_cols + 1),
           fill = q50_hyb)) +
  geom_raster() +
  scale_fill_viridis_c(option = "viridis", direction = -1) +
  facet_nested(
    proportion_orientalis + configuration ~ selection_type + selection_strength + year
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
  labs(x = "", y = "", fill = "Median hybrid proportion") +
  theme_void() +
  labs(title = "Spatial distribution of hybrid ADULTS at t = 500") 

############################### spatial patterns of % orientalis genotype

dt <- readRDS(file.path(res_path, "Orientalis_genot_patch_summary_replicates.RDS") )

######  t = 100
# map the SD per patch for each age_class for year 100  and year 1000
grid <- vect("C:/Users/stefanin/Dropbox/WSL_PhD/Projects/Hybridization2/input/input_files/Grid_4x4m_100x100m.shp")
grid_r <- terra::rast(ext = ext(grid), resolution = 4, crs = "EPSG:3035")
n_rows = dim(grid_r)[1]
n_cols = dim(grid_r)[2]

# rescale p1 value from 0 to 1 in the plot

## SPATIAL DISTRIBUTION FOR SEEDLINGS
ggplot(subset(dt, age_class == 1& year==100),
       aes(x = (pop - 1) %% n_rows + 1,
           y = n_cols - ((pop - 1) %/% n_cols + 1),
           fill = (q50_p1 + 1) / 2)) +
  geom_raster() +
  scale_fill_viridis_c(option = "viridis", direction = -1) +
  facet_nested(
    proportion_orientalis + configuration ~ selection_type + selection_strength 
  ) +
  theme(
    ggh4x.facet.nestline = element_line(size = 1),
    strip.background = element_rect(fill = "grey90"),
    aspect.ratio = 1,
    legend.position = "bottom",
    panel.spacing.x = unit(0.1, "lines"),
    panel.spacing.y = unit(0.1, "lines"),
    strip.text = element_text(size = 20)
  )+
  labs(x = "", y = "", fill = "Proportion of Oriental beech ancestry") +
  theme_void() +
  labs(title = "SEEDLINGS at t = 100") 

## SPATIAL DISTRIBUTION FOR ADULTS
ggplot(subset(dt, age_class == 3& year==100),
       aes(x = (pop - 1) %% n_rows + 1,
           y = n_cols - ((pop - 1) %/% n_cols + 1),
           fill = (q50_p1 + 1) / 2)) +
  geom_raster() +
  scale_fill_viridis_c(option = "viridis", direction = -1) +
  facet_nested(
    proportion_orientalis + configuration ~ selection_type + selection_strength 
  ) +
  theme(
    ggh4x.facet.nestline = element_line(size = 1),
    strip.background = element_rect(fill = "grey90"),
    aspect.ratio = 1,
    legend.position = "bottom",
    panel.spacing.x = unit(0.1, "lines"),
    panel.spacing.y = unit(0.1, "lines"),
    strip.text = element_text(size = 20)
  )+
  labs(x = "", y = "", fill = "Proportion of Oriental beech ancestry") +
  theme_void() +
  labs(title = "ADULTS at t = 100") 

######  t = 500

## SPATIAL DISTRIBUTION FOR SEEDLINGS
ggplot(subset(dt, age_class == 1& year==500),
       aes(x = (pop - 1) %% n_rows + 1,
           y = n_cols - ((pop - 1) %/% n_cols + 1),
           fill = (q50_p1 + 1) / 2)) +
  geom_raster() +
  scale_fill_viridis_c(option = "viridis", direction = -1) +

  facet_nested(
    proportion_orientalis + configuration ~ selection_type + selection_strength 
  ) +
  theme(
    ggh4x.facet.nestline = element_line(size = 1),
    strip.background = element_rect(fill = "grey90"),
    aspect.ratio = 1,
    legend.position = "bottom",
    panel.spacing.x = unit(0.1, "lines"),
    panel.spacing.y = unit(0.1, "lines"),
    strip.text = element_text(size = 15)
  )+
  labs(x = "", y = "", fill = "Proportion of Oriental beech ancestry") +
  theme_void() +
  labs(title = "SEEDLINGS at t = 500") 

## SPATIAL DISTRIBUTION FOR ADULTS
ggplot(subset(dt, age_class == 3& year==500),
       aes(x = (pop - 1) %% n_rows + 1,
           y = n_cols - ((pop - 1) %/% n_cols + 1),
           fill = (q50_p1 + 1) / 2)) +
  geom_raster() +
  scale_fill_viridis_c(option = "viridis", direction = -1) +
  facet_nested(
    proportion_orientalis + configuration ~ selection_type + selection_strength 
  ) +
  theme(
    ggh4x.facet.nestline = element_line(size = 1),
    strip.background = element_rect(fill = "grey90"),
    aspect.ratio = 1,
    legend.position = "bottom",
    panel.spacing.x = unit(0.1, "lines"),
    panel.spacing.y = unit(0.1, "lines"),
    strip.text = element_text(size = 15)
  )+
  labs(x = "", y = "", fill = "Proportion of Oriental beech ancestry") +
  theme_void() +
  labs(title = "ADULTS at t = 500") 


############## combined variables ##########

dt1 <- readRDS(file.path(res_path, "Demo_data_processed.RDS"))
dt2 <- readRDS(file.path(res_path, "Fit_data_processed.RDS")) ## individual-level data
dt3 <- readRDS(file.path(res_path, "Quanti_data_processed.RDS"))  ## individual-level data

dt1$replicate2 <- paste0(dt1$run, "_", dt1$replicate)

# compute hybrid proportion per replicate
dt3$replicate2 <- paste0(dt3$run, "_", dt3$replicate)
dt3_prop <- dt3[, .(
  prop_hybrid = mean(P1 >= -0.9 & P1 <= 0.9)
), by = .(configuration, proportion_orientalis,selection_type, selection_strength, year, age_class, replicate2)]

# keep adults only and reorder
dt3_prop <- dt3_prop[age_class == 3 & year < 1000]
setorder(dt3_prop,configuration, proportion_orientalis,selection_type, selection_strength,replicate2, year)

## check 
ggplot(dt3_prop, 
       aes(year,  prop_hybrid,
           colour =configuration,fill=configuration, 
           group = interaction(configuration, replicate2)
       )
)+  
  
  geom_line() +
  guides(fill = "none") +
  labs(x = "Year", y = "NW per replicate")+
  facet_nested(proportion_orientalis  ~ selection_type + selection_strength) +
  theme_bw()


####################################### plot time to reach X % NW and X % hybrids

## compute the time to threshold but after the initial fluctuation: 
get_time_to_threshold <- function(dt, var, threshold, label, min_year = 0) {
  
  all_reps <- unique(dt[, .(
    configuration, proportion_orientalis,selection_type, selection_strength,replicate2
  )])
  
  # apply filter for transient removal
  dt_sub <- dt[year >= min_year]
  
  t_cross <- dt_sub[get(var) >= threshold,
                    .SD[1],
                    by = .(configuration, proportion_orientalis,selection_type, selection_strength,replicate2)]
  
  t_full <- merge(all_reps, t_cross,
                  by = c("configuration", "proportion_orientalis","selection_type", "selection_strength", "replicate2"),
                  all.x = TRUE)
  
  t_full[, value := year]   # still absolute time
  t_full[, metric := label]
  
  return(t_full[, .(configuration, proportion_orientalis,selection_type, selection_strength,replicate2, value, metric)])
}

t_hyb <- get_time_to_threshold(dt3_prop, "prop_hybrid", 0.5, "Time to 50% hybrids",min_year = 75)


# median fitness
dt2$replicate2 <- paste0(dt2$run, "_", dt2$replicate)
dt2_median <- dt2[, .(
  W = median(W)
), by = .(configuration, proportion_orientalis,selection_type, selection_strength,year, age_class, replicate2)]

# adults only
dt1_ad <- dt1[age_class == "Stage 3"]
dt2_ad <- dt2_median[age_class == 3]

# merge N and W and compute productivity NW
dt_prod <- merge(
  dt1_ad[, .(configuration, proportion_orientalis,selection_type, selection_strength,replicate2, year, N_stage)],
  dt2_ad[, .(configuration, proportion_orientalis,selection_type, selection_strength,replicate2, year, W)],
  by = c("configuration", "proportion_orientalis","selection_type", "selection_strength","replicate2", "year")
)

dt_prod[, NW := N_stage * W]


## check productivity per scenario along years
# median across replciates
dt_prod_median <-dt_prod[, .(
    NW_median = median(NW)
  ), by = .(configuration, proportion_orientalis,selection_type, selection_strength,year)]

ggplot(subset(dt_prod_median, year <= 100), 
       aes(year,  NW_median,
           colour =configuration,fill=configuration, 
           #group = interaction(configuration, replicate2)
           )
       )+  
  
  geom_line() +
  guides(fill = "none") +
  labs(x = "Year", y = "NW per replicate")+
  facet_nested(proportion_orientalis  ~ selection_type + selection_strength) +
  theme_bw()



# get final year NW
dt_final_NW <- dt_prod[, .SD[which.max(year)], 
                       by = .(configuration, proportion_orientalis,selection_type, selection_strength,replicate2)]

dt_final_NW <- dt_final_NW[, .(
  configuration, proportion_orientalis,selection_type, selection_strength,replicate2,NW_final = NW
)]

dt_prod <- merge(dt_prod, dt_final_NW,
                 by = c("configuration", "proportion_orientalis","selection_type", "selection_strength","replicate2"))
dt_prod[, NW_rel := NW / NW_final]

## set as NA the cases where productivity is 0 (if any)
dt_prod[NW_final == 0, NW_rel := NA]

t_prod <- get_time_to_threshold(dt_prod, "NW_rel", 0.8, "Time to 80% of final productivity",  min_year = 75)

# combine
dt_final <- rbindlist(list(t_hyb,t_prod))
dt_final[, metric := factor(metric,
                            levels = c("Time to 50% hybrids",
                                       "Time to 80% of final productivity")
)]
dt_final$selection_type <- factor(dt_final$selection_type, levels = c("Neutral","European b. selected against","Oriental b. selected against","Heterosis"))


## plot 
ggplot(dt_final,
       aes(x = factor(proportion_orientalis),
           y = value,
           fill = configuration,
           color = configuration,
           alpha = proportion_orientalis,
           group = interaction(proportion_orientalis, configuration))) +
  geom_boxplot(outlier.shape = NA,  position = position_dodge(width = 0.8, preserve = "single")) +
  scale_y_continuous( limits = c(0, 700), breaks = seq(0, 700, by = 50))+
  stat_compare_means(aes(group = interaction(configuration, proportion_orientalis)), 
                     method = "kruskal.test",  label = "p.signif", hide.ns = TRUE) +
  scale_fill_manual(values = config_palette) +
  scale_color_manual(values = config_palette) +
  facet_nested(metric ~ selection_type + selection_strength,scales = "free") +
  
  labs(x = "Proportion of Oriental beech introduced",
       y = "Time (years)") +
  guides(color = "none", alpha = "none") +
  theme_bw() +
  theme(axis.text.x = element_text(hjust = 0.5))

## subset only MID selection
ggplot(subset(dt_final,selection_strength %in% c("mid",NA)), 
       aes(x = factor(proportion_orientalis),
           y = value,
           fill = configuration,
           color = configuration,
           alpha = proportion_orientalis,
           group = interaction(proportion_orientalis, configuration))) +
  #geom_violin(trim = T)+
  geom_boxplot(outlier.shape = NA,  position = position_dodge(width = 0.7, preserve = "single")) +
  scale_y_continuous( limits = c(0, 700), breaks = seq(0, 700, by = 50))+
  stat_compare_means(aes(group = interaction(configuration, proportion_orientalis)), 
                     method = "kruskal.test",  label = "p.signif", hide.ns = TRUE, label.y = 600) +
  scale_fill_manual(values = config_palette) +
  scale_color_manual(values = config_palette) +
  facet_nested(metric ~ selection_type + selection_strength,scales = "free") +
  labs(x = "Proportion of Oriental beech introduced",
       y = "Time (years)") +
  guides(color = "none", alpha = "none") +
  theme_bw() +
  theme(axis.text.x = element_text(hjust = 0.5))



####################################### plot NW and hybrid % at year X

## define year
y <- 500

dt1_sub <- subset(dt1,age_class=="Stage 3" & year==y)
dt2_sub <- subset(dt2_median, age_class==3 & year==y)
dt3_sub <- subset(dt3_prop, age_class==3 & year==y)


## create a combined metric N adults * W
dt_merged <- Reduce(function(x, y) merge(x, y,
                                         by = c("configuration", "proportion_orientalis","selection_type", "selection_strength","replicate2")),
                    list(
                      dt1_sub[, .(configuration, proportion_orientalis,selection_type, selection_strength,replicate2, N_stage)],
                      dt2_sub[, .(configuration, proportion_orientalis,selection_type, selection_strength,replicate2, W = W)],
                      dt3_sub[, .(configuration, proportion_orientalis,selection_type, selection_strength,replicate2, Hyb_proportion = prop_hybrid)]
                    ))

dt_merged[, NW := N_stage*W]

# reshape
dt_long <- melt(dt_merged,
                measure.vars = c("N_stage", "W", "Hyb_proportion", "NW"),
                variable.name = "metric",
                value.name = "value")

dt_long$selection_type <- factor(dt_long$selection_type, levels = c("Neutral","European b. selected against","Oriental b. selected against","Heterosis"))


## add the neutral data range as a shaded color background in the other facests: 

# extrreact the neutral ranges

neutral_ranges <- dt_long[
  selection_type == "Neutral" & metric %in% c("Hyb_proportion", "NW"),
  .(
    ymin = quantile(value,0.1, na.rm = TRUE),
    ymax = quantile(value,0.9, na.rm = TRUE)
  ),
  by = .(metric, proportion_orientalis)
]
neutral_ranges[, x := as.numeric(factor(proportion_orientalis))]
neutral_ranges[, `:=`(xmin = x-0.4,xmax = x+0.4 )]

# FIGURE S3 --> add t = 

ggplot(subset(dt_long,metric%in%c("Hyb_proportion","NW")& selection_type != "Neutral"),
       aes(x = factor(proportion_orientalis),
           y = value,
           fill = configuration,
           color = configuration,
           alpha = proportion_orientalis,
           group = interaction(proportion_orientalis, configuration))) +
  geom_rect(
    data = neutral_ranges,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE,
    fill = "grey60",
    alpha = 0.2
  ) +
  geom_boxplot() +
  scale_fill_manual(values = config_palette) +
  scale_color_manual(values = config_palette) +
  stat_compare_means(aes(group = interaction(configuration, proportion_orientalis)), method = "kruskal.test"  ,  label = "p.signif", hide.ns = TRUE) +
  facet_nested(metric ~ selection_type + selection_strength,scales = "free_y") +
  labs(x = "Proportion of Oriental beech introduced", y = NULL) +
  guides(color = "none", alpha = "none") +
  theme_fig+
  theme(panel.background = element_rect(fill = "white", colour = "black"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        
        strip.text = element_text(),
        legend.key = element_rect(colour = "white"),
        legend.key.spacing.y = unit(0.4, 'cm'), 
        strip.background = element_rect(colour = "white", fill = "white"), 
        plot.title = element_text(hjust = 0.5))


# FIGURE MAIN - only selection against E beech?


 ggplot(subset(dt_long,metric%in%c("Hyb_proportion","NW")&selection_type=="European b. selected against"),
        aes(x = factor(proportion_orientalis),
            y = value,
            fill = configuration,
            color = configuration,
            alpha = proportion_orientalis,
            group = interaction(proportion_orientalis, configuration))) +
   geom_rect(
     data = neutral_ranges,
     aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
     inherit.aes = FALSE,
     fill = "grey60",
     alpha = 0.2
   ) +
   geom_boxplot() +
   scale_fill_manual(values = config_palette) +
   scale_color_manual(values = config_palette) +
   stat_compare_means(aes(group = interaction(configuration, proportion_orientalis)), method = "kruskal.test"  ,  label = "p.signif", hide.ns = TRUE) +
   facet_nested(metric ~ selection_type + selection_strength,scales = "free_y") +
   labs(x = "Proportion of Oriental beech introduced", y = NULL) +
   guides(color = "none", alpha = "none") +
   theme_fig+
   theme(panel.background = element_rect(fill = "white", colour = "black"), 
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         
         strip.text = element_text(),
         legend.key = element_rect(colour = "white"),
         legend.key.spacing.y = unit(0.4, 'cm'), 
         strip.background = element_rect(colour = "white", fill = "white"), 
         plot.title = element_text(hjust = 0.5))
       


############## pareto optimization ##########
 
 
 ## function to calculate pareto frontier for each year
 
 process_year <- function(y) {
   
   dt1_sub <- subset(dt1, age_class == "Stage 3" & year == y)
   dt2_sub <- subset(dt2_median, age_class == 3 & year == y)
   dt3_sub <- subset(dt3_prop, age_class == 3 & year == y)
   
   dt_merged <- Reduce(function(x, y) merge(x, y,
                                            by = c("configuration", "proportion_orientalis",
                                                   "selection_type", "selection_strength", "replicate2")),
                       list(
                         dt1_sub[, .(configuration, proportion_orientalis, selection_type, selection_strength, replicate2, N_stage)],
                         dt2_sub[, .(configuration, proportion_orientalis, selection_type, selection_strength, replicate2, W = W)],
                         dt3_sub[, .(configuration, proportion_orientalis, selection_type, selection_strength, replicate2, Hyb_proportion = prop_hybrid)]
                       ))
   
   dt_merged[, NW := N_stage * W]
   
   dt_merged <- merge(
     dt_merged,
     cost_table[, .(configuration, proportion_orientalis, cost = estimated_cost)],
     by = c("configuration", "proportion_orientalis"),
     all.x = TRUE
   )
   
   dt_merged_summary <- dt_merged[, .(
     med_NW = median(NW, na.rm = TRUE),
     med_hyb_prop = median(Hyb_proportion, na.rm = TRUE),
     med_cost = median(cost)
   ), by = .(selection_type, selection_strength, configuration, proportion_orientalis)]
   
   dt_merged_summary[, dominated := FALSE]
   
   dt_merged_summary[, dominated := sapply(1:.N, function(i) {
     any(
       med_NW >= med_NW[i] &
         med_hyb_prop >= med_hyb_prop[i] &
         med_cost <= med_cost[i] &
         (med_NW > med_NW[i] |
            med_hyb_prop > med_hyb_prop[i] |
            med_cost < med_cost[i])
     )
   }), by = .(selection_type, selection_strength)]
   
   best_str <- dt_merged_summary[dominated == FALSE]
   
   best_str <- best_str %>%
     dplyr::group_by(selection_type, selection_strength) %>%
     dplyr::mutate(size_scaled = scales::rescale(med_hyb_prop, to = c(1, 10))) %>%
     dplyr::ungroup()
   
   dt_merged_summary$year <- y
   best_str$year <- y
   
   list(summary = dt_merged_summary, best = best_str)
 }
 
 ## apply function to selected years and bind
 years <- c(100, 500)
 results <- lapply(years, process_year)
 dt_all   <- rbindlist(lapply(results, `[[`, "summary"))
 best_all <- rbindlist(lapply(results, `[[`, "best"))
 
 
 ## plot results for both years
 dt_all$selection_type <- factor(dt_all$selection_type, levels = c("Neutral","European b. selected against","Oriental b. selected against","Heterosis"))
 best_all$selection_type <- factor(best_all$selection_type, levels = c("Neutral","European b. selected against","Oriental b. selected against","Heterosis"))
 
 
 ggplot(dt_all, aes(x = med_cost, y = med_NW)) +
   geom_point(color = "black") +
   geom_point(data = best_all,aes(size = size_scaled, color = configuration ))+
   geom_line(data = best_all,
             aes(group = interaction(selection_type, selection_strength)),
             color = "red") +
   geom_text(data = best_all,
             aes(label = proportion_orientalis),
             vjust = -0.7) +
   scale_color_manual(values = config_palette) +
   scale_size_identity(
     guide = "legend",
     breaks = c(2, 5, 10),
     labels = c("low", "medium", "high")
   ) +
   labs(y = "Productivity (N x W)",
        x = "Estimated cost") +
   
   facet_nested(year ~ selection_type + selection_strength, scales = "free_x") +
   
   theme_fig +
   theme(
     panel.background = element_rect(fill = "white", colour = "black"),
     panel.grid = element_blank(),
     strip.background = element_rect(colour = "white", fill = "white"),
     plot.title = element_text(hjust = 0.5)
   )
 
 
 


