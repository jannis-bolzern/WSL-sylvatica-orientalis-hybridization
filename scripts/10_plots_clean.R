
### the script: 
# loads all the results from Quanti_data.R object and process the dataset
# create different plots

library(data.table)
library(stringr)
library(ggplot2)
library(terra)
library(ggh4x)
library(ggpubr)
library(patchwork)

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
  "Single cluster" = "#2c7bb6", 
  "No introduction" = "grey"
)


theme_fig <- theme(
  axis.text.x = element_text(hjust = 0.5),
  axis.line = element_line(linewidth = 0.3, colour = "black"),
  axis.ticks = element_line(linewidth = 0.3),
  axis.ticks.length = unit(1.5, "mm"),
  axis.title = element_text(size = 22),
  axis.text  = element_text(size =15),
  
  strip.text = element_text(size = 20),
  strip.background = element_blank(),
  legend.title = element_text(size = 12),
  legend.text  = element_text(size = 15),
  legend.key = element_blank(),
  legend.position = "right",
  
  plot.title = element_text(size = 22, hjust = 0.5),
  panel.grid = element_blank(),
  panel.background = element_blank(), 

)


######## starting scenarios (cURRENT FIGURE 1) ##########
cfg_labels <- c(
  "dispersed" = "Dispersed",
  "multiple_clusters" = "Multiple clusters",
  "single_cluster" = "Single clusters",
  "transects" = "Transects"
)

prop_labels <- c(
  "0.10" = "10%",
  "0.25" = "25%",
  "0.40" = "40%"
)

## modified script from 04_qc_layourt_overviews.R

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(terra)
  library(cowplot)
})

source('scripts/00_control_panel.R')

INPUT_ROOT <- if (exists('IN') && is.list(IN) && !is.null(IN$files)) IN$files else file.path('input', 'input_files')
QC_ROOT    <- if (exists('OUT') && is.list(OUT) && !is.null(OUT$qc)) OUT$qc else file.path('output', 'qc')

MANIFEST   <- file.path(SCRIPTS$manifests, 'manifest_scenarios.tsv')
GRID_SHP   <- file.path(INPUT_ROOT, 'grid', 'Grid_4x4m_100x100m.shp')
PLOT_DIR   <- file.path(QC_ROOT, 'layout_overviews')

dir.create(PLOT_DIR, showWarnings = FALSE, recursive = TRUE)

if (!file.exists(MANIFEST)) {
  stop('Missing ', MANIFEST, '\nRun the scenario-generation script first (e.g., scripts/02_*).', call. = FALSE)
}
if (!file.exists(GRID_SHP)) {
  # fallback: first .shp found under input/grid
  hits <- list.files(file.path(INPUT_ROOT, 'grid'), pattern = '\\.(shp)$', full.names = TRUE)
  if (length(hits) == 0) stop('Could not find grid shapefile under: ', file.path(INPUT_ROOT, 'grid'), call. = FALSE)
  GRID_SHP <- hits[1]
}

# ---- helpers
coords_from_grid <- function(grid) {
  cent <- terra::centroids(grid)
  xy   <- as.data.frame(terra::crds(cent))
  names(xy) <- c('x', 'y')
  
  # assumes patchID exists and is 1..N^2 in raster order
  patchID <- as.integer(grid$patchID)
  n_patches <- length(patchID)
  N <- as.integer(round(sqrt(n_patches)))
  if (N * N != n_patches) stop('Grid is not square (n_patches=', n_patches, ').', call. = FALSE)
  
  coords <- data.table(patchID = patchID, x = xy$x, y = xy$y)
  coords[, row := ((patchID - 1L) %/% N) + 1L]
  coords[, col := ((patchID - 1L) %%  N) + 1L]
  coords[, y_plot := N - row + 1L]
  coords[, x_plot := col]
  coords[]
}

plot_layout_binary <- function(conf_dt, coords_dt, title = NULL) {
  df <- merge(coords_dt[, .(patchID, x_plot, y_plot)], conf_dt, by = 'patchID', all.x = TRUE)
  df[, patch_value := fifelse(is.na(patch_value), 'S', patch_value)]
  
  ggplot(df, aes(x = x_plot, y = y_plot)) +
    geom_tile(aes(fill = patch_value), color = 'black', linewidth = 0.15) +
    coord_equal() +
    scale_fill_manual(values = c(S = '#ffcc00', O = '#482173FF')) +
    labs(title = title, x = '', y = '', fill = NULL) +
    theme_void(base_size = 12) +  ## changed
    theme(
      panel.grid = element_blank(),
      legend.position = 'none',
      plot.title = element_text(hjust = 0.5, size = 10),
      plot.background = element_rect(fill = 'white', color = NA),
      panel.background = element_rect(fill = 'white', color = NA)
    )
}

# ---- load inputs 
manifest <- fread(MANIFEST)
need <- c('sim_name', 'cfg_file', 'configuration', 'prop_orientalis', 'n_orientalis')
if (!all(need %in% names(manifest))) {
  stop('manifest_scenarios.tsv missing columns: ', paste(setdiff(need, names(manifest)), collapse = ', '), call. = FALSE)
}

grid   <- terra::vect(GRID_SHP)
coords <- coords_from_grid(grid)
n_patches <- nrow(coords)

manifest[, scenario_key := sub('_r[0-9]+$', '', sim_name)]

groups <- manifest[
  , .(
    cfg_files    = list(cfg_file),
    sims         = list(sim_name),
    configuration = configuration[1],
    prop         = prop_orientalis[1],
    n_orientalis = n_orientalis[1]
  ),
  by = .(scenario_key)
]

# ---- CREATE PANELS BUT NOT CREATE PNG
plot_list <- list()

for (i in seq_len(nrow(groups))) {
  key <- groups$scenario_key[i]
  cfg <- groups$configuration[i]
  p   <- groups$prop[i]
  
  cfg_files <- unlist(groups$cfg_files[i])
  sims      <- unlist(groups$sims[i])
  
  ord <- order(sims)
  cfg_files <- cfg_files[ord]
  sims      <- sims[ord]
  
  plist <- lapply(seq_along(cfg_files), function(j) {
    if (!file.exists(cfg_files[j])) return(NULL)
    dt <- fread(cfg_files[j], showProgress = FALSE)[, .(patchID, patch_value)]
    nO_run <- sum(dt$patch_value == 'O')
    rep_lab <- sprintf('r%02d | %d (%.2f%%)', j, nO_run, 100 * nO_run / n_patches)
    plot_layout_binary(dt, coords, NULL)
  })
  plist <- Filter(Negate(is.null), plist)
  if (length(plist) == 0) next
  
  main_title <- sprintf(
    '%s | requested %.0f%% (%.1f/%d) | used %d/%d = %.2f%%',
    cfg, 100 * p, p * n_patches, n_patches, 
    groups$n_orientalis[i], n_patches,
    100 * groups$n_orientalis[i] / n_patches
  )
  
  panel <- plist[[1]]   # select only first run for plotting
  final <- cowplot::plot_grid(
    cowplot::ggdraw() + cowplot::draw_label(main_title, fontface = 'bold', size = 20),
    panel,
    ncol = 1,
    rel_heights = c(0.18, 1)
  )
  
  plot_list[[key]] <- panel
}

plot_list


configs <- sort(unique(groups$configuration))
props   <- sort(unique(groups$prop))

                  configs <- c("dispersed", "multi_cluster", "one_cluste", "transects")
                  props   <- c(0.10, 0.25, 0.40)
                  
                  # column headers
                  col_titles <- lapply(cfg_labels[configs], function(lbl) {
                    cowplot::ggdraw() +
                      cowplot::draw_label(lbl, fontface = "bold", size = 14)
                  })
                  
                  col_header <- cowplot::plot_grid(plotlist = col_titles, ncol = length(configs))
                  
                  
                  row_panels <- list()
                  
                  for (cfg in configs) {
                    
                    key <- groups$scenario_key[
                      groups$configuration == cfg & groups$prop == p
                    ]
                    
                    if (length(key) == 0 || is.na(key)) {
                      # create empty placeholder
                      row_panels[[cfg]] <- cowplot::ggdraw()
                      
                    } else {
                      key <- key[1]  # ensure single value
                      row_panels[[cfg]] <- plot_list[[key]]
                    }
                  }
                  
                  
                  final_grid <- cowplot::plot_grid(
                    col_header,
                    plotlist = rows,
                    ncol = 1,
                    rel_heights = c(0.1, 1)
                  )
                  
                  final_grid

                  
                  
  ## old one                
panel_grid <- list()

for (p in props) {
  for (cfg in configs) {
    
    key <- groups$scenario_key[
      groups$configuration == cfg & groups$prop == p
    ]
    
    panel_grid[[paste(cfg, p, sep = "_")]] <- plot_list[[key]]
  }
}

final_grid <- cowplot::plot_grid(
  plotlist = panel_grid,
  ncol = length(configs)
)
final_grid


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


# only adults
ggplot(subset(dt1, age_class== "Stage 3"), aes(x = year, y = N_stage, group = interaction(age_class, replicate2), col = age_class)) +
  geom_line(linewidth = 0.2, alpha = 0.8) +
  facet_nested(configuration+proportion_orientalis ~ selection_type+selection_strength, scales="free_y") +
  labs( y = "Number of individuals",
        color = "Nemo Stage"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()



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

gc()

############################### genotype P1 TREND #############

dt <- copy(dt3)

# P1 bins
dt[, P1_clean := round(P1, 1)]

# Year bins (20-year intervals)
dt[, year_bin := cut(year,
                     breaks = seq(0, max(year) + 20, by = 20),
                     include.lowest = TRUE)]


# midpoint of bins for axis label
dt[, year_mid := floor(year / 20) * 20 + 10]


# proprotion of individuals in each P1 bin for each replicate
dt_rep <- dt[, .N, 
             by = .(configuration,proportion_orientalis,selection_type,selection_strength,age_class,run,replicate,year_mid,P1_clean)]
dt_rep[, prop := N / sum(N), 
       by = .(configuration,proportion_orientalis,selection_type,selection_strength,age_class,run,replicate, year_mid)]

#median across replicates
dt_med <- dt_rep[, .(
  prop_median = median(prop)
), by = .(configuration, proportion_orientalis,selection_type,selection_strength,age_class,year_mid, P1_clean)]

dt_med$selection_type <- factor(dt_med$selection_type, levels = c("Neutral","European b. selected against","Oriental b. selected against","Heterosis"))

## plot all to check differences (FOR SUPPLEMENTARY??)
ggplot(
  dt_med[age_class == 3 ],
  aes(x = year_mid, y = P1_clean, alpha = prop_median)
) +
  geom_tile(width = 20, height = 0.1, fill = "black")+
  scale_fill_viridis_c() +
  facet_nested(proportion_orientalis+ configuration~ selection_type + selection_strength) +
  theme_minimal() +
  theme_fig+
  theme(strip.text.y = element_text(angle = 0))+
  labs(
    x = "Time (years)",
    y = "(Median) proportion of individuals\nfor each genotype (P1)",
    alpha = "Proportion of individuals"
  ) +
  scale_x_continuous(breaks = seq(0, max(dt_med$year_mid), by = 400)) +
  scale_y_continuous(breaks = c(-1, -0.5, 0, 0.5, 1))


#subset
ggplot(
  dt_med[age_class == 3 & configuration == "Multiple clusters" & proportion_orientalis == 0.25 & selection_strength %in% c("mid", NA)],
  aes(x = year_mid, y = P1_clean, alpha = prop_median)
) +
  geom_tile(width = 20, height = 0.1, fill = "black")+
  scale_fill_viridis_c() +
  facet_nested(proportion_orientalis+ configuration~ selection_type + selection_strength) +
  theme_minimal() +
  theme_fig+
  theme(strip.text.y = element_text(angle = 0))+
  labs(
    x = "Time (years)",
    y = "(Median) proportion of individuals\nfor each genotype (P1)",
    alpha = "Proportion of individuals"
  ) +
  scale_x_continuous(breaks = seq(0, max(dt_med$year_mid), by = 400)) +
  scale_y_continuous(breaks = c(-1, -0.5, 0, 0.5, 1))

      
  
###############################  pure and hybrid proportions TRENDS ##############

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


## check replicates by plotting each replicate = 1 line
dt3_genot_long$replicate2<- paste0(dt3_genot_long$run, "_",dt3_genot_long$replicate)

my_cols1 <- c(
  orientalis = "#482173FF", 
  sylvatica  = "#ffcc00",  
  hybrid     = "#25858EFF" 
)

# remove no introduction (no hybrids)
ggplot(subset(dt3_genot_long, configuration != "No introduction"), 
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

# get the ribbons back
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

############# all genotypes time trends

dt3_genot_quant_wide$selection_type <- factor(dt3_genot_quant_wide$selection_type, levels = c("Neutral","European b. selected against","Oriental b. selected against","Heterosis"))

ggplot(subset(dt3_genot_quant_wide, age_class ==3),
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

## subset only adults and mid selection 

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


######################## HYBRID PROPORTIONS TREND ############

dt3 <- readRDS(file.path(res_path, "Quanti_data_processed.RDS"))

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

dt3_genot_long$replicate2<- paste0(dt3_genot_long$run, "_",dt3_genot_long$replicate)

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

# get the ribbons back
dt3_genot_quant_wide <- dt3_genot_quant_long %>%
  tidyr::pivot_wider(
    names_from = quantile,
    values_from = value
  )

############ only E beech selected against --- removed selection type from facte nested!

## get neutral data to plot under
neutral_data <- dt3_genot_quant_wide %>%
  dplyr::filter(selection_type == "Neutral",age_class == 3 & year < 600 & year > 75)

non_neutral <- dt3_genot_quant_wide %>%
  dplyr::filter(selection_type != "Neutral",age_class == 3 & year < 600 & year > 75)

facet_combos <- non_neutral %>%
  dplyr::distinct(selection_type, selection_strength)
neutral_expanded <- merge( neutral_data, facet_combos, by = NULL)


hybprop_trend_syl <- ggplot(subset(non_neutral,age_class=="3"& Genotype=="hyb"&selection_type =="European b. selected against" & configuration != "No introduction"),  
                            aes(year, q50,colour =configuration,fill=configuration,
                                linetype = proportion_orientalis,
                                group  =interaction(configuration, proportion_orientalis),  
                            )) +
  # neutral baseline in all facets
  geom_ribbon(data = subset(neutral_expanded,age_class=="3"& Genotype=="hyb"& selection_type.y =="European b. selected against" & configuration != "No introduction"), 
              aes(x = year,
                  ymin = q10,
                  ymax = q90, 
                  alpha = proportion_orientalis,
                  group = interaction(selection_strength.y, proportion_orientalis, configuration)),
              inherit.aes = FALSE,
              fill = "grey70",
  ) +
  
  geom_line(size = 1) +
  # geom_ribbon(aes(ymin = q10, ymax = q90),alpha = 0.05,colour = NA) +
  geom_vline(xintercept=100, linetype="dashed")+
  
  scale_colour_manual(values = config_palette, name = "Configuration") +
  scale_fill_manual(values = config_palette) +
  labs(x = "Year", y = "Hybrid proportions")+
  scale_linetype_manual(values = c("solid", "dashed", "dotted"), name = "Proportion Oriental b.") +
  facet_nested(~  selection_strength) +
  guides(fill="none", color = "none", linetype = "none")+
  theme_bw()+theme_fig
hybprop_trend_syl



############ other selection scnearios --- keep selection type on facet nested

hybprop_trend_supp <- ggplot(subset(non_neutral,age_class=="3"& Genotype=="hyb"&selection_type%in% c("Oriental b. selected against", "Heterosis") & configuration != "No introduction"),  
                             aes(year, q50,colour =configuration,fill=configuration,
                                 linetype = proportion_orientalis,
                                 group  =interaction(configuration, proportion_orientalis),  
                             )) +
  # neutral baseline in all facets
  geom_ribbon(data = subset(neutral_expanded,age_class=="3"& Genotype=="hyb"&selection_type.y %in% c("Oriental b. selected against", "Heterosis")& configuration != "No introduction"), 
              aes(x = year,
                  ymin = q10,
                  ymax = q90, 
                  alpha = proportion_orientalis,
                  group = interaction(selection_strength.y, proportion_orientalis, configuration)),
              inherit.aes = FALSE,
              fill = "grey70",
  ) +
  
  
  geom_line(size = 1) +
  # geom_ribbon(aes(ymin = q10, ymax = q90),alpha = 0.05,colour = NA) +
  
  geom_vline(xintercept=100, linetype="dashed")+
  scale_colour_manual(values = config_palette, name = "Configuration") +
  scale_fill_manual(values = config_palette) +
  labs(x = "Year", y = "Hybrid proportions")+
  scale_linetype_manual(values = c("solid", "dashed", "dotted"), name = "Proportion Oriental b.") +
  facet_nested(~ selection_type + selection_strength) +
  guides(fill="none", color = "none", linetype = "none")+
  theme_bw()+theme_fig
hybprop_trend_supp

 
######################## NW TIME TRENDS #################

dt <- readRDS(file.path(res_path, "Final_dataset_replicate_level.RDS"))

# summarize across replicates using quantiles
dt_summary <- dt[, .(
  
  q10_NW = quantile(NW, 0.1, na.rm = TRUE),
  q50_NW = quantile(NW, 0.5, na.rm = TRUE),
  q90_NW = quantile(NW, 0.9, na.rm = TRUE)
  
),
by = .(configuration, proportion_orientalis, year,selection_type, selection_strength)]

# replace NA in No introduction with 0
dt_summary[configuration == "No introduction" & is.na(q50_NW), q50_NW := 0]
dt_summary$selection_type <- factor(dt_summary$selection_type, levels = c("Neutral","European b. selected against","Oriental b. selected against","Heterosis"))

############ only E beech selected against

library(dplyr)
## needs to be tibble
neutral_data <- dt_summary %>%
  as_tibble() %>%
  filter(selection_type == "Neutral", year < 600 & year > 75)

non_neutral <- dt_summary %>%
  as_tibble() %>%
  filter(selection_type != "Neutral",year < 600 &  year > 75)

facet_combos <- non_neutral %>%
  distinct(selection_type, selection_strength)

neutral_expanded <- merge(neutral_data, facet_combos, by = NULL)

NW_trend_syl <- ggplot()+
  # neutral baseline in all facets
  geom_ribbon(data = subset(neutral_expanded,selection_type.y =="European b. selected against" & configuration != "No introduction"), 
              aes(x = year,
                  ymin = q10_NW,
                  ymax = q90_NW,  
                  alpha = proportion_orientalis,
                  group = interaction(selection_strength.y, proportion_orientalis, configuration)),
              inherit.aes = FALSE,
              fill = "grey90",
  ) +
  
  geom_line(data = subset(non_neutral,selection_type =="European b. selected against" & configuration != "No introduction"),  
            aes(x = year, 
                y = q50_NW,
                colour =configuration,
                linetype = proportion_orientalis,
                group =interaction(configuration, proportion_orientalis)
            )) +
  
  # median NW for case of No introduction
  geom_area(data = subset(non_neutral,configuration=="No introduction"& year <700& year >50),
            aes(x = year,y = q50_NW),
            color = "black",
            linewidth = 1) +
  geom_vline(xintercept=100, linetype="dashed")+
  
  scale_colour_manual(values = config_palette, name = "Configuration") +
  scale_fill_manual(values = config_palette) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted"), name = "Proportion Oriental b.") +
  
  labs(x = "Year", y = "Median Productivity (N x W)")+
  facet_nested(~  selection_strength) +
  guides(fill="none")+
  theme_bw()+theme_fig
NW_trend_syl



######## other selection types

NW_trend_supp <- ggplot()+
  # neutral baseline in all facets
  geom_ribbon(data = subset(neutral_expanded,selection_type.y%in% c("Oriental b. selected against", "Heterosis") & configuration != "No introduction"),  
              aes(x = year,
                  ymin = q10_NW,
                  ymax = q90_NW,  
                  alpha = proportion_orientalis,
                  group = interaction(selection_strength.y, proportion_orientalis, configuration)),
              inherit.aes = FALSE,
              fill = "grey90",
  ) +
  
  geom_line(data = subset(non_neutral,selection_type%in% c("Oriental b. selected against", "Heterosis") & configuration != "No introduction"),  
            aes(x = year, 
                y = q50_NW,
                colour =configuration,
                linetype = proportion_orientalis,
                group =interaction(configuration, proportion_orientalis)
            )) +
  geom_vline(xintercept=100, linetype="dashed")+
  
  scale_colour_manual(values = config_palette, name = "Configuration") +
  scale_fill_manual(values = config_palette) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted"), name = "Proportion Oriental b.") +
  
  labs(x = "Year", y = "Median Productivity (N x W)")+
  facet_nested(~ selection_type + selection_strength) +
  guides(fill="none")+
  theme_bw()+theme_fig
NW_trend_supp


######################## NW AND HYBRID PROPORTIONS AT YEAR X ###############

dt <- reaRDS(file.path(res_path, "Final_dataset_replicate_level.RDS"))

## target year 
y = 100

dt_sub <- subset(dt, year == y)

# reshape
dt_long <- melt(dt_sub,
                measure.vars = c("Hyb_proportion", "Hyb_corr_neutral", "NW","NW_corr_neutral"),
                variable.name = "metric",
                value.name = "value")

dt_long$selection_type <- factor(dt_long$selection_type, levels = c("Neutral","European b. selected against","Oriental b. selected against","Heterosis"))

# extract the neutral ranges
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


# subset selection against E.beech


t100_syl_hyb <- ggplot(subset(dt_long,metric%in%c("Hyb_proportion")&selection_type=="European b. selected against"& proportion_orientalis != 0),
                       aes(x = factor(proportion_orientalis),
                           y = value,
                           fill = configuration,
                           color = configuration,
                           alpha = proportion_orientalis,
                           group = interaction(proportion_orientalis, configuration))) +
  geom_rect(
    data = subset(neutral_ranges,metric%in%c("Hyb_proportion")),
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE,
    fill = "grey60",
    alpha = 0.2
  ) +
  geom_boxplot() +
  scale_fill_manual(values = config_palette) +
  scale_color_manual(values = config_palette) +
  stat_compare_means(aes(group = interaction(configuration, proportion_orientalis)), method = "kruskal.test"  ,  label = "p.signif", hide.ns = TRUE) +
  facet_nested(metric ~  selection_strength,scales = "free_y", labeller=labeller(metric=c("Hyb_proportion"="Hybrid proportions", "NW_corr_neutral"="NW / NW(neutral)"))) +
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

t100_syl_hyb

t100_syl_NW <- ggplot(subset(dt_long,metric%in%c("NW")&selection_type=="European b. selected against"& proportion_orientalis != 0),
                      aes(x = factor(proportion_orientalis),
                          y = value,
                          fill = configuration,
                          color = configuration,
                          alpha = proportion_orientalis,
                          group = interaction(proportion_orientalis, configuration))) +
  geom_rect(
    data = subset(neutral_ranges,metric%in%c("NW")),
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE,
    fill = "grey60",
    alpha = 0.2
  ) +
  geom_boxplot() +
  scale_fill_manual(values = config_palette) +
  scale_color_manual(values = config_palette) +
  stat_compare_means(aes(group = interaction(configuration, proportion_orientalis)), method = "kruskal.test"  ,  label = "p.signif", hide.ns = TRUE) +
  facet_nested(metric ~  selection_strength,scales = "free_y", labeller=labeller(metric=c("Hyb_proportion"="Hybrid proportions"))) +
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

t100_syl_NW


#### SUPPLEMENTARY

t100_supp_hyb <- ggplot(subset(dt_long,metric%in%c("Hyb_proportion")&selection_type%in% c("Oriental b. selected against", "Heterosis") & proportion_orientalis != 0),
                        aes(x = factor(proportion_orientalis),
                            y = value,
                            fill = configuration,
                            color = configuration,
                            alpha = proportion_orientalis,
                            group = interaction(proportion_orientalis, configuration))) +
  geom_rect(
    data = subset(neutral_ranges,metric%in%c("Hyb_proportion")),
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE,
    fill = "grey60",
    alpha = 0.2
  ) +
  
  geom_boxplot() +
  scale_fill_manual(values = config_palette) +
  scale_color_manual(values = config_palette) +
  stat_compare_means(aes(group = interaction(configuration, proportion_orientalis)), method = "kruskal.test",  label = "p.signif", hide.ns = TRUE) +
  facet_nested(metric ~ selection_type + selection_strength,scales = "free_y", labeller=labeller(metric=c("Hyb_proportion"="Hybrid proportions", "NW_corr_neutral"="NW / NW(neutral)"))) +
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

t100_supp_hyb


t100_supp_nw <- ggplot(subset(dt_long,metric%in%c("Hyb_proportion")&selection_type%in% c("Oriental b. selected against", "Heterosis") & proportion_orientalis != 0),
                       aes(x = factor(proportion_orientalis),
                           y = value,
                           fill = configuration,
                           color = configuration,
                           alpha = proportion_orientalis,
                           group = interaction(proportion_orientalis, configuration))) +
  geom_rect(
    data = subset(neutral_ranges,metric%in%c("Hyb_proportion")),
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE,
    fill = "grey60",
    alpha = 0.2
  ) +
  
  geom_boxplot() +
  scale_fill_manual(values = config_palette) +
  scale_color_manual(values = config_palette) +
  stat_compare_means(aes(group = interaction(configuration, proportion_orientalis)), method = "kruskal.test",  label = "p.signif", hide.ns = TRUE) +
  facet_nested(metric ~ selection_type + selection_strength,scales = "free_y", labeller=labeller(metric=c("Hyb_proportion"="Hybrid proportions", "NW_corr_neutral"="NW / NW(neutral)"))) +
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

t100_supp_nw



####################### COMBINED ABOVE ############

## COMBINE EUROPEAN BECH MAIN
# combine plots
syl_combined <- (hybprop_trend_syl / t100_syl_hyb / NW_trend_syl / t100_syl_NW) +
  plot_layout(guides = "collect", heights = c(1,1,1,1)) +
  plot_annotation(tag_levels = "a") &  
  theme(
    strip.text.y = element_blank(),
    legend.position = "right",
    plot.tag = element_text(size = 25, face = "bold")
  )
syl_combined


ggsave(
  paste0(fig_path,"/t100_sylvatica.png"),
  plot = last_plot(),   # or assign your plot to an object and use plot = p
  width =18,
  height = 15,
  units = "in"
)


### SUPPLEMENTARY
supp_combined <- (hybprop_trend_supp / t100_supp_hyb / NW_trend_supp / t100_supp_nw) +
  plot_layout(guides = "collect", heights = c(1,1,1,1)) +
  plot_annotation(tag_levels = "a") &  
  theme(
    strip.text.y = element_blank(),
    legend.position = "right",
    plot.tag = element_text(size = 25, face = "bold")
  )
supp_combined


ggsave(
  paste0(fig_path,"/t100_supp.png"),
  plot = last_plot(),   # or assign your plot to an object and use plot = p
  width =18,
  height = 15,
  units = "in"
)




############################### spatial patterns of % orientalis genotype at t = X ###############

dt <- readRDS(file.path(res_path, "Orientalis_genot_patch_summary_replicates.RDS") )

######  t = 100

# map the SD per patch for each age_class for year 100  and year 1000
grid <- vect("C:/Users/stefanin/Dropbox/WSL_PhD/Projects/Hybridization2/input/input_files/Grid_4x4m_100x100m.shp")
grid_r <- terra::rast(ext = ext(grid), resolution = 4, crs = "EPSG:3035")
n_rows = dim(grid_r)[1]
n_cols = dim(grid_r)[2]

# rescale p1 value from 0 to 1

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


####################################### time to reach X % NW and X % hybrids ##################

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

# read main dataset with all metrics replicate level
dt <- readRDS(file.path(res_path, "Final_dataset_replicate_level.RDS"))

t_hyb <- get_time_to_threshold(dt, "Hyb_proportion", 0.5, "Time to 50% hybrids",min_year = 75)
t_prod <- get_time_to_threshold(dt, "NW_rel", 0.8, "Time to 80% of final productivity",  min_year = 75)

# combine
dt_final <- rbindlist(list(t_hyb,t_prod))
dt_final[, metric := factor(metric,
                            levels = c("Time to 50% hybrids",
                                       "Time to 80% of final productivity")
)]
dt_final$selection_type <- factor(dt_final$selection_type, levels = c("Neutral","European b. selected against","Oriental b. selected against","Heterosis"))


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
  scale_fill_manual(values = config_palette, name = "Configuration") +
  scale_color_manual(values = config_palette) +
  facet_nested(metric ~ selection_type + selection_strength,scales = "free") +
  
  labs(x = "Proportion of Oriental beech introduced",
       y = "Time (years)") +
  guides(color = "none", alpha = "none") +
  theme_bw() +
  theme( axis.text.x = element_text(hjust = 0.5),
         axis.line = element_line(linewidth = 0.3, colour = "black"),
         axis.ticks = element_line(linewidth = 0.3),
         axis.ticks.length = unit(1.5, "mm"),
         axis.title = element_text(size = 22),
         axis.text  = element_text(size =15),
         
         strip.text = element_text(size = 20),
         strip.background = element_blank(),
         legend.title = element_text(size = 12),
         legend.text  = element_text(size = 15),
         legend.key = element_blank(),
         legend.position = "right",
         panel.background = element_blank(), 
        panel.grid = element_line()
  )


ggsave(
  paste0(fig_path,"/Time_to_reach.png"),
  plot = last_plot(), 
  width =20,
  height = 9,
  units = "in"
)




######## pareto optimization ##########

dt <- readRDS(file.path(res_path, "Final_dataset_replicate_level.RDS"))
cost_table <- fread(file.path(res_path,"Cost_design_table.csv"))
cost_table$proportion_orientalis <- factor(cost_table$proportion_orientalis)

dt_merged <- merge(
  dt,
  cost_table[, .(configuration, proportion_orientalis, cost = estimated_cost)],
  by = c("configuration", "proportion_orientalis"),
  all.x = TRUE
)


dt_merged_summary <- dt_merged[, .(
  med_NW = median(NW, na.rm = TRUE),
  med_hyb_prop = median(Hyb_proportion, na.rm = TRUE),
  med_cost = median(cost)
), by = .(selection_type, selection_strength, configuration, proportion_orientalis, year)]

# replace NA in No introduction with 0
dt_merged_summary[configuration == "No introduction" & is.na(med_NW), med_NW := 0]

## calculate pareto frontier for each year  
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
}), by = .(selection_type, selection_strength, year)]
  
best_str <- dt_merged_summary[dominated == FALSE]


############################## PARETO TRENDS ###########

# give a strategy ID
best_str <- best_str %>%dplyr::mutate( strategy_id = paste(configuration,proportion_orientalis, sep = "_") )

# detect gaps in the years (some strategies are good only in certain years)
best_str2 <- best_str %>%
  dplyr::arrange(selection_type, selection_strength, strategy_id, year) %>%
  dplyr::group_by(strategy_id) %>%
  dplyr::mutate(
    year_diff = year - dplyr::lag(year),
    new_segment = ifelse(is.na(year_diff) | year_diff > 20, 1, 0),
    segment_id = cumsum(new_segment)
  ) %>%
  dplyr::ungroup()


## get neutral data to plot under
neutral_data <- best_str2 %>%
  dplyr::filter(selection_type == "Neutral",
                year < 700 & year > 75)

non_neutral <- best_str2 %>%
  dplyr::filter(selection_type != "Neutral",
                year < 700& year > 75)

facet_combos <- non_neutral %>%
  dplyr::distinct(selection_type, selection_strength)
neutral_expanded <- merge( neutral_data, facet_combos, by = NULL)

## neutral grey area band
setDT(neutral_expanded)
neutral_area <-  neutral_expanded[
  selection_type.y == "European b. selected against",
  .(
    ymin = quantile(med_NW, 0.1, na.rm = TRUE),
    ymax = quantile(med_NW, 0.9, na.rm = TRUE),
    ymed = median(med_NW, na.rm = TRUE)
  ),
  by = .(year, selection_strength.y)
]


## ONLY EUROPEAN BEECH S (MAIN)

pareto_trend_syl <- ggplot() +
  # neutral baseline in all facets
  geom_ribbon(data = neutral_area,
              aes(x = year,
                  ymin = ymin,
                  ymax = ymax,
                  group = selection_strength.y),
              inherit.aes = FALSE,
              fill = "grey70",
              alpha = 0.5) +
  # median NW for case of No introduction
  geom_area(data = subset(dt_merged_summary,configuration=="No introduction"& year <700& year >50),
            aes(x = year,
                y = med_NW),
            color = "black",
            linewidth = 1) +
  # non-neutral best strategies
  geom_line(data = subset(non_neutral,selection_type == "European b. selected against"),
            aes(x = year,
                y = med_NW,
                color = configuration, 
                linetype = proportion_orientalis, 
                group = interaction(strategy_id, segment_id)),
            linewidth = 0.8) +
  
  geom_point(data = subset(non_neutral,selection_type == "European b. selected against"),
             aes(x = year,
                 y = med_NW,
                 color = configuration,
                 group = interaction(strategy_id, segment_id)),
             size = 1) +
  geom_vline(xintercept = c(100, 500),  linetype= "dashed", color = "black")+
  scale_x_continuous(breaks = seq(0, 700, by = 100))+
  scale_color_manual(values = config_palette, name = "Configuration") +
  scale_linetype_manual(values = c("solid", "dashed", "dotted"), name = "Proportion Oriental b.") +
  facet_nested( ~ selection_type +selection_strength) +
  theme_fig +
  theme(
    panel.background = element_rect(fill = "white", colour = "black"),
    panel.grid = element_blank(),
    strip.background = element_rect(colour = "white", fill = "white"),
    plot.title = element_text(hjust = 0.5)
  )+
  labs(
    x = "Year",
    y = "Median Productivity (N x W)"
  )
pareto_trend_syl

## OTHER SELECTION TYPES

# plot along time
pareto_tred_supp <- ggplot() +
  # neutral baseline in all facets
  geom_ribbon(data = neutral_area,
              aes(x = year,
                  ymin = ymin,
                  ymax = ymax,
                  group = selection_strength.y),
              inherit.aes = FALSE,
              fill = "grey70",
              alpha = 0.5) +
  
  # non-neutral strategies
  geom_line(data = subset(non_neutral,selection_type != "European b. selected against"),
            aes(x = year,
                y = med_NW,
                color = configuration, linetype = proportion_orientalis, 
                group = interaction(strategy_id, segment_id)),
            linewidth = 0.8,
            alpha = 0.7) +
  
  geom_point(data = subset(non_neutral,selection_type != "European b. selected against"),
             aes(x = year,
                 y = med_NW,
                 color = configuration, linetype = proportion_orientalis, 
                 group = interaction(strategy_id, segment_id)),
             size = 1) +
  geom_vline(xintercept = c(100, 500),  linetype= "dashed", color = "black")+
  scale_x_continuous(breaks = seq(0, 700, by = 100))+
  facet_nested( ~ selection_type + selection_strength, scales = "free_y", 
               labeller = labeller(selection_type = c( "Neutral" = "Neutral\n(No maladaptation)"))) +
  
  scale_linetype_manual(values = c("solid", "dashed", "dotted"), name = "Proportion Oriental b.") +
  scale_color_manual(values = config_palette, name = "Configuration") +
  theme_fig +
  theme(
    panel.background = element_rect(fill = "white", colour = "black"),
    panel.grid = element_blank(),
    strip.background = element_rect(colour = "white", fill = "white"),
    plot.title = element_text(hjust = 0.5)
  )+
  labs(
    x = "Year",
    y = "Median Productivity (N x W)"
  )
pareto_tred_supp



############################## PARETO AT YEAR X ##############

dt_merged_summary$selection_type <- factor(dt_merged_summary$selection_type, levels = c("Neutral","European b. selected against","Oriental b. selected against","Heterosis"))
best_str$selection_type <- factor(best_str$selection_type, levels = c("Neutral","European b. selected against","Oriental b. selected against","Heterosis"))
range_vals <- range(best_str$med_hyb_prop, na.rm = TRUE)

# get the min (+10% otherwise 0 disappears), mid and max hybrid proportions values for legend
legend_vals <- c(
  round(range_vals[1], 2)+1/10, 
  round(mean(range_vals), 2),
  round(range_vals[2], 2)
)


## ONLY EUROPEAN BEECH S (MAIN?)

# remove "No introduction" scenario -- keep only selection against slyvatica
dt_sub <- subset(dt_merged_summary,year %in% c(100,500) & selection_type %in% c("European b. selected against")& configuration!= "No introduction")
best_sub <- subset(best_str,year %in% c(100,500)& selection_type %in% c("European b. selected against")& configuration!= "No introduction")

pareto_syl <- ggplot(best_sub, aes(x = med_cost, y = med_NW)) +
  geom_point(data = dt_sub,
             aes(x = med_cost, y = med_NW),
             inherit.aes = FALSE,
             color = "black",
             alpha = 0.8)+

  geom_point(data = best_sub,aes(size = med_hyb_prop, color = configuration ), alpha = 0.8)+
  #geom_line(data = best_sub, aes(group = interaction(selection_type, selection_strength)),color = "red") +
  geom_text(data =best_sub, aes(label = proportion_orientalis), vjust = -0.8) +
  scale_color_manual(values = config_palette, name = "Configuration") +
  scale_size_continuous(
    name = "Hybrid proportion",
    breaks = legend_vals,
    labels = scales::percent_format(accuracy = 1),
    range = c(2, 10)
  )+
  labs(y = "Median Productivity (N x W)",
       x = "Estimated cost") +
  facet_nested(year ~ selection_type + selection_strength, scales = "free_y", 
               labeller = labeller(selection_type = c("European b. selected against" = "European b. selected against"))) +
  theme_fig +
  theme(
    panel.background = element_rect(fill = "white", colour = "black"),
    panel.grid = element_blank(),
    strip.background = element_rect(colour = "white", fill = "white"),
    plot.title = element_text(hjust = 0.5)
  )
pareto_syl

### OTHER SELECTION TYPES - supplementary

dt_sub2 <- subset(dt_merged_summary,year %in% c(100,500) & selection_type %in% c("Oriental b. selected against", "Heterosis") & configuration!= "No introduction")
best_sub2 <- subset(best_str,year %in% c(100,500)& selection_type %in% c("Oriental b. selected against", "Heterosis")& configuration!= "No introduction")

pareto_supp <- ggplot(dt_sub2, aes(x = med_cost, y = med_NW)) +
  geom_point(color = "black") +
  geom_point(data = best_sub2,aes(size = med_hyb_prop, color = configuration ), alpha = 0.8)+
  #geom_line(data = best_sub2, aes(group = interaction(selection_type, selection_strength)),color = "red") +
  geom_text(data =best_sub2, aes(label = proportion_orientalis), vjust = -0.9) +
  scale_color_manual(values = config_palette, name = "Configuration") +
  scale_size_continuous(
    name = "Hybrid proportion",
    breaks = legend_vals,
    labels = scales::percent_format(accuracy = 1),
    range = c(2, 10)
  )+
  labs(y = "Median Productivity (N x W)",
       x = "Estimated cost") +
  facet_nested(year ~ selection_type + selection_strength, scales = "free_y") +
  theme_fig +
  theme(
    panel.background = element_rect(fill = "white", colour = "black"),
    panel.grid = element_blank(),
    strip.background = element_rect(colour = "white", fill = "white"),
    plot.title = element_text(hjust = 0.5)
  )
pareto_supp



############################## COMBINE ABOVE #############

# combine plots
syl_combined <- (pareto_tred_syl / pareto_syl) +
  plot_layout(guides = "collect", heights = c(1, 2)) +
  plot_annotation(tag_levels = "a") &  
  theme(
    legend.position = "right",
    plot.tag = element_text(size = 25, face = "bold")
  )
syl_combined


ggsave(
  paste0(fig_path,"/Pareto_sylvatica.png"),
  plot = last_plot(),   # or assign your plot to an object and use plot = p
  width = 15,
  height = 12,
  units = "in"
)


# combine plots
others_combined <- (pareto_tred_supp / pareto_supp) +
  plot_layout(guides = "collect", heights = c(1, 2)) +
  plot_annotation(tag_levels = "a") &  
  theme(
    legend.position = "right",
    plot.tag = element_text(size = 25, face = "bold")
  )
others_combined


ggsave(
  paste0(fig_path,"/Pareto_supplementary.png"),
  plot = last_plot(),   # or assign your plot to an object and use plot = p
  width = 22,
  height = 14,
  units = "in"
)
