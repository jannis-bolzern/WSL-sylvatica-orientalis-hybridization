
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
library(readr)
library(readxl)
library(dplyr)
library(forcats)
library(scales)
library(tidyr)

res_path <- "C:/Users/stefanin/Dropbox/WSL_PhD/Projects/Hybridization2/output/analysis"
#dir.create("C:/Users/stefanin/Dropbox/WSL_PhD/Projects/Hybridization2/Figures_manuscript")
fig_path <- "C:/Users/stefanin/Dropbox/WSL_PhD/Projects/Hybridization2/Figures_manuscript"

## for Evolutionary applications: 
# figure legends double-spaced on a separate sheet = CAPTION / LEGEND TEXT BELOW THE FIGUREM SUBMITTED SEPARATELY IN THE MANUSCRIPT FILE
# final size of lettering on figures at least 1.5 mm 
# capital letters to label figure parts
# save in PDF format
# resolution at least 300 dpi

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

# 1 point = 0.3528 mm
# 1.5 mm = 4.25 pt minimum
# Use >= 8 pt in final figure to be safe after reduction


theme_fig <- theme_bw(base_size = 10) +
  theme(
    axis.title = element_text(size = 11),
    axis.text  = element_text(size = 8),
    axis.line = element_line(linewidth = 0.3, colour = "black"),
    axis.ticks = element_line(linewidth = 0.3),
    axis.ticks.length = unit(1.5, "mm"),
    
    strip.text = element_text(size = 13),
    strip.background = element_blank(),
    
    legend.title = element_text(size = 9),
    legend.text  = element_text(size = 8),
    legend.key = element_blank(),
    
    plot.title = element_text(size = 11, hjust = 0.5),
    plot.tag = element_text(size = 14, face = "bold"),
    plot.background = element_rect(fill = "white", colour = NA),
    
    panel.grid = element_blank(),
    panel.background = element_blank()
    
  )


######## FIGURE 1A           (starting scenarios) ##########

##### add dispersed - etc labels
##### add proportion orientalis labels
##### uniform the style

cfg_labels <- c(
  "dispersed" = "Dispersed",
  "multi_cluster" = "Multiple clusters",
  "one_cluster" = "Single cluster",
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
    geom_tile(aes(fill = patch_value), color = 'black', linewidth = 0.10) +
    coord_equal(expand = FALSE) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_manual(
      values = c(S = '#ffcc00', O = '#482173FF'),
      labels = c(S = "European beech", O = "Oriental beech")
    ) +
    labs(title = title, x = NULL, y = NULL, fill = NULL) +
    theme_void(base_size = 9) +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, size = 8),
      plot.margin = margin(1, 1, 1, 1, "mm"),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
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

# ---- CREATE PANELS 

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

configs <- c("dispersed", "multi_cluster", "one_cluster", "transects")
props   <- c(0.10, 0.25, 0.40)

panel_grid <- list()

for (p in props) {
  for (cfg in configs) {
    
    key <- groups$scenario_key[
      groups$configuration == cfg & groups$prop == p
    ][1]
    
    panel_grid[[paste(cfg, p, sep = "_")]] <- plot_list[[key]]
  }
}

layout_grid <- cowplot::plot_grid(
  plotlist = panel_grid,
  ncol = length(configs),
  align = "hv",
  axis = "tblr"
)

col_labels <- cowplot::plot_grid(
  plotlist = lapply(configs, function(cfg) {
    cowplot::ggdraw() +
      cowplot::draw_label(
        cfg_labels[[cfg]],
        size = 10,
        hjust = 0.5,
        vjust = 0.5
      )
  }),
  ncol = length(configs),
  align = "h"
)

row_labels <- cowplot::plot_grid(
  plotlist = lapply(props, function(p) {
    cowplot::ggdraw() +
      cowplot::draw_label(
        prop_labels[[sprintf("%.2f", p)]],
        size = 10,
        angle = 270,
        hjust = 0.5,
        vjust = 0.5
      )
  }),
  ncol = 1,
  align = "v"
)

empty_corner <- cowplot::ggdraw()

final_grid <- cowplot::plot_grid(
  cowplot::plot_grid(
    col_labels, empty_corner,
    ncol = 2,
    rel_widths = c( 1, 0.08)
  ),
  cowplot::plot_grid(
    layout_grid,row_labels,
    ncol = 2,
    rel_widths = c(1, 0.08)
  ),
  ncol = 1,
  rel_heights = c(0.08, 1)
)

final_grid


######## FIGURE 1b           (selection scheme) #########

library(tidyverse)

selection_strength_map <- list(
  Low = c(40, 60, 120, 180),
  Intermediate = c(20, 30, 80, 120),
  High = c(10, 15, 50, 80)
)

fitness <- function(z, theta, w2) {
  1 - ((z - theta)^2) / w2
}

stages <- c("Seedlings", "Saplings", "Juveniles", "Adults")

genotypes <- tibble(
  genotype = c("European beech", "F1", "Oriental beech"),
  z = c(-1, 0, 1)
)

theta_df <- tibble(
  theta_name = c("Wori > Wf1 > Weu", "Weu > Wf1 > Wori", "Wf1 > Weu = Wori"),
  theta = c(1, -1, 0)
)

plot_data <- expand_grid(
  strength = names(selection_strength_map),
  theta_df,
  genotypes,
  stage = stages
) %>%
  group_by(strength, theta_name) %>%
  mutate(
    stage_id = match(stage, stages),
    w2 = selection_strength_map[[unique(strength)]][stage_id],
    W = fitness(z, theta, w2)
  ) %>%
  ungroup() %>%
  mutate(
    strength = factor(strength, levels = c("Low", "Intermediate", "High")),
    stage = factor(stage, levels = stages),
    theta_name = factor(
      theta_name,
      levels = c("Wori > Wf1 > Weu", "Weu > Wf1 > Wori", "Wf1 > Weu = Wori")
    )
  )


beech_cols <- c(
  "European beech" = "#ffcc00",
  "F1"   = "#25858EFF",
  "Oriental beech" = "#482173FF"
)

selection <- ggplot(plot_data, aes(stage, W, color = genotype, group = genotype)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.6) +
  facet_grid(
    strength ~ theta_name,
    labeller = labeller(
      theta_name = as_labeller(c(
        "Wori > Wf1 > Weu" = "W[Ori]~'>'~W[F1]~'>'~W[Eu]",
        "Weu > Wf1 > Wori" = "W[Eu]~'>'~W[F1]~'>'~W[Ori]",
        "Wf1 > Weu = Wori" = "W[F1]~'>'~W[Eu]~'='~W[Ori]"
      ), label_parsed)
    )
  ) +
  scale_y_continuous(limits = c(0.5, 1), breaks = seq(0.5, 1, 0.1)) +
  scale_color_manual(values = beech_cols) +
  labs(
    x = "Age class",
    y = "Fitness (W)",
    color = "Genotype"
  ) +
  theme_fig +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1),
    legend.position = "right"
  )
selection

######## FIGURE 1 #########

# combine plots
plot1 <- (
  (final_grid +
     theme(plot.margin = margin(b = 0))) /
    (selection + theme(plot.margin = margin(t = 0, b = 5))) 
) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A") &
  theme(
    legend.position = "right",
    plot.tag = element_text(size = 14, face = "bold")
  )
plot1

ggsave(
  filename = file.path(fig_path, "Figure1.png"),
  plot = plot1,
  width = 9,
  height = 12,
  units = "in"
)

ggsave(
  filename = file.path(fig_path, "Figure1.pdf"),
  plot = plot1,
  width = 9,
  height = 12,
  units = "in",
  device = cairo_pdf
)

######## FIGURE S1       (questionnaires plots) ##########

path <- "C:/Users/stefanin/Dropbox/WSL_PhD/Projects/Hybridization2/questionnaires/"
file <- file.path(path, "Questionnaire_pag1_summary.xlsx")
dat <- read_excel(file, sheet = 1)

dat <- dat %>%
  mutate(
    respondent_group = str_to_title(respondent_group),
    answer_label = factor(
      answer_label,
      levels = c("does_not_apply", "partially_applies", "assumed", "applies", "unknown"),
      labels = c("Does not apply", "Partially applies", "Assumed", "Applies", "Unknown")
    )
  )

dat_plot <- dat %>%
  mutate(
    respondent_group = factor(respondent_group, levels = c("Forester", "Researcher")),
    answer_label = factor(
      answer_label,
      levels = c("Does not apply", "Partially applies", "Assumed", "Applies", "Unknown")
    )
  )


############ FISHER TEST ON FORESTERS/RESEARCHERS ANSWERS 
library(dplyr)
library(tidyr)
library(purrr)
library(broom)

dat_test <- dat_plot %>%
  mutate(
    response_binary = case_when(
      answer_label %in% c("Applies", "Partially applies") ~ "Positive",
      answer_label %in% c("Does not apply") ~ "Negative",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(response_binary)) %>%
  group_by(
    question_type,
    question_group,
    question,
    species,
    respondent_group,
    response_binary
  ) %>%
  summarise(
    n = sum(n),
    .groups = "drop"
  )


# complete combinations to avoid uncomplete data
dat_test_complete <- dat_test %>%
  group_by(
    question_type,
    question_group,
    question,
    species,
    respondent_group
  ) %>%
  complete(
    response_binary = c("Positive", "Negative"),
    fill = list(n = 0)
  ) %>%
  ungroup()

## filter valid tests
results_fisher <- dat_test_complete %>%
  pivot_wider(
    names_from = response_binary,
    values_from = n,
    values_fill = 0
  ) %>%
  group_by(
    question_type,
    question_group,
    question,
    species
  ) %>%
  filter(n_distinct(respondent_group) == 2) %>%
  group_modify(~{
    
    tab <- .x %>%
      arrange(respondent_group) %>%
      select(Positive, Negative) %>%
      as.matrix()
    
    # skip empty tables
    if (sum(tab) == 0) {
      return(tibble(
        p_value = NA_real_,
        odds_ratio = NA_real_
      ))
    }
    
    test <- fisher.test(tab)
    
    tibble(
      p_value = test$p.value,
      odds_ratio = unname(test$estimate)
    )
  }) %>%
  ungroup() %>%
  mutate(
    p_adjust = p.adjust(p_value, method = "BH")
  ) %>%
  arrange(p_adjust)


ggplot(results_fisher,
       aes(
         x = log2(odds_ratio),
         y = fct_reorder(question, log2(odds_ratio)),
         color = p_adjust < 0.05
       )) +
  geom_point(size = 3) +
  geom_vline(xintercept = 0,
             linetype = "dashed") +
  facet_grid(question_type ~ species)+
  labs(
    x = "Log2 odds ratio\n(Foresters more positive →)",
    y = NULL
  ) +
  theme_bw()

######### SPLIT RESEARCHERS AND FORESTERS

dat_div <- dat_plot %>%
  mutate(
    answer_side = case_when(
      answer_label %in% c("Does not apply") ~ -1,
      answer_label %in% c("Assumed", "Partially applies","Applies", "Unknown") ~ 1
    ),
    prop_div = proportion * answer_side,
    group_question = paste(respondent_group, question, sep = " — ")
  )


dat_div <- dat_div %>%
  mutate(
    respondent_short = recode(
      respondent_group,
      "Forester" = "F",
      "Researcher" = "R"
    ),
    question = str_wrap(question, width = 45),
    group_question = respondent_short
  )


## set order based on Applies % of researchers
question_order_existing <- dat_div %>%
  filter(
    str_detect(question_type, "existing introductions"),
    respondent_group == "Researcher",
    answer_label == "Applies"
  ) %>%
  group_by(species, question_group, question) %>%
  summarise(prop_applies_researcher = sum(proportion, na.rm = TRUE), .groups = "drop")

# join back the order
dat_div_existing <- dat_div %>%
  filter(str_detect(question_type, "existing introductions")) %>%
  left_join(
    question_order_existing,
    by = c("species", "question_group", "question")
  ) %>%
  mutate(
    prop_applies_researcher = replace_na(prop_applies_researcher, 0),
    question_ord = fct_reorder(
      question,
      prop_applies_researcher,
      .fun = mean,
      .desc = TRUE
    )
  )


p_existing <- ggplot(
  dat_div_existing %>% filter(answer_label != "Unknown"),
  aes(
    x = prop_div,
    y = fct_rev(fct_inorder(group_question)),
    fill = answer_label
  )
) +
  geom_col(width = 0.75, color = "white", linewidth = 0.2) +
  geom_vline(xintercept = 0, linewidth = 0.3) +
  facet_nested(
    question_group + question_ord ~ species,
    scales = "free_y",
    space = "free_y",
    switch = "y"
  ) +
  scale_x_continuous(
    labels = function(x) scales::percent(abs(x)),
    limits = c(-1, 1)
  ) +
  scale_fill_manual(
    values = c(
      "Applies" = "#009E73",
      "Partially applies" = "#78C679",
      "Assumed" = "#0072B2",
      "Does not apply" = "#CC79A7",
      "Unknown" = "#999999"
    ))+
  labs(
    x = "Percentage of answers",
    y = NULL,
    fill = "Answer category"
  ) +
  theme_bw() +
  theme(
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text.y.left = element_text(angle = 0, hjust = 1),
    strip.text.y = element_text(angle = 90),
    axis.title.y = element_blank(),
    legend.position = "bottom"
  )

p_existing


########## GOOD SUPP FIGURE 1A

library(tidytext)
library(tidyr)

dat_plot <- dat_plot %>%
  mutate(
    answer_side = case_when(
      answer_label %in% c("Does not apply", "Unknown") ~ -1,
      answer_label %in% c("Assumed", "Partially applies","Applies") ~ 1
    ),
    prop_div = proportion * answer_side
  )

dat_existing_sum <- dat_plot %>%
  filter(str_detect(question_type, "Assessment of the existing introductions based on presentations and your own experience")) %>%
  group_by(question_type,species,question_group,question_id,question,answer_label
  ) %>%
  summarise(
    n = sum(n, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(question_type, species, question_group, question_id, question) %>%
  mutate(
    proportion = n / sum(n, na.rm = TRUE),
    answer_side = case_when(
      answer_label %in% c("Does not apply","Unknown") ~ -1,
      answer_label %in% c("Assumed", "Partially applies", "Applies") ~ 1
    ),
    prop_div = proportion * answer_side
  ) %>%
  ungroup()


question_order_ref <- dat_existing_sum %>%
  filter(
    species == "Oriental beech from the Greater Caucasus",
    answer_label %in% c("Applies", "Partially applies")
  ) %>%
  group_by(question_type, question_group, question_id, question) %>%
  summarise(
    prop_positive = sum(proportion, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(question_group, desc(prop_positive), question_id)


dat_existing_plot <- dat_existing_sum %>%
  left_join(
    question_order_ref %>%
      select(question_type, question_group, question_id, prop_positive),
    by = c("question_type", "question_group", "question_id")
  ) %>%
  mutate(
    prop_positive = replace_na(prop_positive, 0),
    
    question_ord = reorder_within(
      question,
      prop_positive,
      interaction(question_type, question_group),
      fun = mean,
      .desc = TRUE
    )
  )

## reorder question group
dat_existing_plot$question_group <- factor(
  dat_existing_plot$question_group,
  levels = unique(
    dat_plot$question_group[
      str_detect(dat_plot$question_type, "Assessment of the existing introductions based on presentations and your own experience")
    ]
  )
)

# plot (no "other")
p_existing <- ggplot(
  subset(dat_existing_plot,question!= "Other"),
  aes(
    x = prop_div,
    y = question_ord,
    fill = answer_label
  )
) +
  geom_col(width = 0.75, color = "white", linewidth = 0.2) +
  geom_vline(xintercept = 0, linewidth = 0.3) +
  facet_nested(
    question_group ~ species,
    scales = "free_y",
    space = "free_y",
    switch = "y"
  ) +
  scale_y_reordered(drop = TRUE)+
  scale_x_continuous(
    labels = function(x) scales::percent(abs(x)),
    limits = c(-1, 1)
  ) +
  scale_fill_manual(
    values = c(
      "Applies" = "#009E73",
      "Partially applies" = "#78C679",
      "Assumed" = "#0072B2",
      "Does not apply" = "#CC79A7",
      "Unknown" = "#999999"
    )
  )+
  labs(
    x = "Percentage of answers",
    y = NULL,
    fill = "Answer category", 
    tag = "A"
  ) +
  theme_fig +
  theme(
    #ggh4x.facet.nestline = element_line(linewidth = 1),
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text.y.left = element_text(angle = 0, hjust = 1),
    strip.text.y = element_text(angle = 90),
    axis.title.y = element_blank(),
    legend.position = "bottom",
    plot.tag = element_text(size = 14,face = "bold"),
    plot.tag.position = c(0.01, 0.99)
  )
  

p_existing

ggsave(
  filename = file.path(fig_path, "FigureS1a.png"),
  plot = p_existing,
  width = 10.5,
  height = 12,
  units = "in"
)

ggsave(
  filename = file.path(fig_path, "FigureS1a.pdf"),
  plot = p_existing,
  width = 10.5,
  height = 12,
  units = "in",
  device = cairo_pdf
)

##### future introductions
dat_future_sum <- dat_plot %>%
  filter(str_detect(question_type, "Assessment of future introductions" )) %>%
  group_by(question_type,species,question_group,question_id,question,answer_label
  ) %>%
  summarise(
    n = sum(n, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(question_type, species, question_group, question_id, question) %>%
  mutate(
    proportion = n / sum(n, na.rm = TRUE),
    answer_side = case_when(
      answer_label %in% c("Does not apply","Unknown") ~ -1,
      answer_label %in% c("Assumed", "Partially applies", "Applies") ~ 1
    ),
    prop_div = proportion * answer_side
  ) %>%
  ungroup()


question_order_ref <- dat_future_sum %>%
  filter(
    species == "Oriental beech from the Greater Caucasus",
    answer_label %in% c("Applies", "Partially applies")
  ) %>%
  group_by(question_type, question_group, question_id, question) %>%
  summarise(
    prop_positive = sum(proportion, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(question_group, desc(prop_positive), question_id)


dat_future_plot <- dat_future_sum %>%
  left_join(
    question_order_ref %>%
      select(question_type, question_group, question_id, prop_positive),
    by = c("question_type", "question_group", "question_id")
  ) %>%
  mutate(
    prop_positive = replace_na(prop_positive, 0),
    
    question_ord = reorder_within(
      question,
      prop_positive,
      interaction(question_type, question_group),
      fun = mean,
      .desc = TRUE
    )
  )

## reorder question group
dat_future_plot$question_group <- factor(
  dat_future_plot$question_group,
  levels = unique(
    dat_plot$question_group[
      str_detect(dat_plot$question_type, "Assessment of future introductions")
    ]
  )
)

# plot (remove "other")
p_future <- ggplot(
  subset(dat_future_plot,question!= "Other"),
  aes(
    x = prop_div,
    y = question_ord,
    fill = answer_label
  )
) +
  geom_col(width = 0.75, color = "white", linewidth = 0.2) +
  geom_vline(xintercept = 0, linewidth = 0.3) +
  facet_nested(
    question_group ~ species,
    scales = "free_y",
    space = "free_y",
    switch = "y", 
    labeller = labeller(species = c("Reasons to introduce more Oriental beech from the Greater Caucasus" = "Reasons to introduce more Oriental beech\nfrom the Greater Caucasus", 
                        "Reasons to introduce “other Oriental beeches”, which one?" = "Reasons to introduce “other Oriental beeches”,\n which one?"))
  ) +
  scale_y_reordered(drop = TRUE)+
  scale_x_continuous(
    labels = function(x) scales::percent(abs(x)),
    limits = c(-1, 1)
  ) +
  scale_fill_manual(
    values = c(
      "Applies" = "#009E73",
      "Partially applies" = "#78C679",
      "Assumed" = "#0072B2",
      "Does not apply" = "#CC79A7",
      "Unknown" = "#999999"
    )
  )+
  labs(
    x = "Percentage of answers",
    y = NULL,
    fill = "Answer category", 
    tag = "B"
  ) +
  theme_fig +
  theme(
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text.y.left = element_text(angle = 0, hjust = 1),
    strip.text.y = element_text(angle = 90),
    axis.title.y = element_blank(),
    legend.position = "bottom",
    plot.tag = element_text(
      size = 14,
      face = "bold"
    ),
    
    plot.tag.position = c(0.01, 0.99)
  )

p_future

ggsave(
  filename = file.path(fig_path, "FigureS1b.png"),
  plot = p_future,
  width = 10.5,
  height = 12,
  units = "in"
)

ggsave(
  filename = file.path(fig_path, "FigureS1b.pdf"),
  plot = p_future,
  width = 10.5,
  height = 12,
  units = "in",
  device = cairo_pdf
)

######## demographic results ###########

dt1 <- readRDS(file.path(res_path, "Demo_data_processed.RDS"))
dt1$replicate2 <- paste0(dt1$run, "_",dt1$replicate)
dt1$selection_type <- factor(dt1$selection_type, levels = c("Neutral","Wori > Wf1 > Weu","Weu > Wf1 > Wori","Wf1 > Weu = Wori"))

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
dt2$selection_type <- factor(dt2$selection_type, levels = c("Neutral","Wori > Wf1 > Weu","Weu > Wf1 > Wori","Wf1 > Weu = Wori"))
dt2_summary$selection_type <- factor(dt2_summary$selection_type, levels = c("Neutral","Wori > Wf1 > Weu","Weu > Wf1 > Wori","Wf1 > Weu = Wori"))

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
dt2_median_patch_summary_sub$selection_type <- factor(dt2_median_patch_summary_sub$selection_type, levels = c("Neutral","Wori > Wf1 > Weu","Weu > Wf1 > Wori","Wf1 > Weu = Wori"))


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


######## FIGURE S2  (demographic / density-regulation parameterization) ##########

suppressPackageStartupMessages({
  library(data.table)
  library(stringr)
  library(ggplot2)
  library(scales)
  library(patchwork)
})

## ---- paths: point this at the parameterization runs ----
param_root <- file.path("manuscript", "supplementary", "parameterization")
runs_root  <- file.path(param_root, "parameterization_runs")

## ---- which slice to summarise ----
configuration          <- "dispersed"   # representative config used for calibration
layout_run             <- 1
p_focus                <- 25             # 25% introduction
gen_max                <- 1000
max_internal_replicate <- 50
GEN_MAX_PLOT           <- 200            # x-axis window for the figure

## ---- find demography .txt files for this config + run ----
run_str     <- sprintf("r%02d", as.integer(layout_run))
demog_files <- list.files(file.path(runs_root, configuration),
                          recursive = TRUE, full.names = TRUE, pattern = "\\.txt$")
path_norm   <- gsub("\\\\", "/", demog_files)
demog_files <- demog_files[
  grepl("/run/[^/]+\\.txt$", path_norm) &
    !grepl("_bygen\\.txt$", path_norm) &
    !grepl("\\.log$", path_norm) &
    grepl(paste0("/", run_str, "/"), path_norm)
]
if (!is.na(p_focus)) {
  demog_files <- demog_files[grepl(sprintf("_p%d_", as.integer(p_focus)),
                                   basename(demog_files))]
}
stopifnot(length(demog_files) > 0)

parse_meta <- function(path) {
  m <- str_match(
    basename(path),
    "^(.+?)_p(\\d+)_r(\\d+)_k(\\d+(?:\\.\\d+)?)_b(\\d+(?:\\.\\d+)?)(?:_|\\.|$)"
  )
  if (any(is.na(m))) stop("Could not parse metadata from: ", basename(path))
  data.table(configuration = m[, 2], ori_proportion = as.integer(m[, 3]),
             layout_run = as.integer(m[, 4]),
             k = as.numeric(m[, 5]), b = as.numeric(m[, 6]))
}

cols_to_read <- c("replicate", "generation", "pop.tot",
                  "off.tot", "a0.tot", "a1.tot", "a2.tot", "a3.tot")

## read + reshape once (one rbindlist; no redundant work inside a loop)
demog_long <- rbindlist(lapply(demog_files, function(f) {
  meta   <- parse_meta(f)
  header <- names(fread(f, nrows = 0))
  dt     <- fread(f, select = intersect(cols_to_read, header))
  if ("off.tot" %in% names(dt) && !"a0.tot" %in% names(dt)) {
    setnames(dt, "off.tot", "a0.tot")            # harmonise offspring column name
  }
  dt <- cbind(dt, meta)
  melt(dt,
       id.vars = c("replicate", "generation", "configuration",
                   "ori_proportion", "layout_run", "k", "b"),
       measure.vars  = patterns("\\.tot$"),
       variable.name = "stage", value.name = "N")
}), use.names = TRUE, fill = TRUE)

demog_long <- demog_long[replicate %between% c(1, max_internal_replicate) &
                           generation <= gen_max]
if (!is.na(p_focus)) demog_long <- demog_long[ori_proportion == as.integer(p_focus)]

## ---- stage labels: match the age classes used in Fig. 1b ----
stage_labs <- c("pop.tot" = "Total", "a0.tot" = "Seedlings", "a1.tot" = "Saplings",
                "a2.tot" = "Juveniles", "a3.tot" = "Adults")
demog_long[, stage_label := stage_labs[as.character(stage)]]   # index by name, not factor code

stage_levels <- c("Seedlings", "Saplings", "Juveniles", "Adults")

## Okabe-Ito (colour-blind safe), ordered seedling -> adult; adults = strong blue
stage_pal <- c("Seedlings" = "#E69F00", "Saplings" = "#009E73",
               "Juveniles" = "#56B4E9", "Adults" = "#0072B2")

## ---- median + IQR across replicates (per k, b, stage, generation) ----
demog_iqr <- demog_long[
  stage_label %in% stage_levels & generation <= GEN_MAX_PLOT,
  .(med = median(N, na.rm = TRUE),
    q25 = quantile(N, 0.25, na.rm = TRUE),
    q75 = quantile(N, 0.75, na.rm = TRUE)),
  by = .(k, b, stage_label, generation)
]
demog_iqr[, stage_label := factor(stage_label, levels = stage_levels)]

## retained (K, b) combination to outline in both panels
retained <- data.table(k = 30, b = 0.03)

## italic K / b strip labels (matches the math styling used in Figs 2-4)
lab_kb <- label_bquote(rows = italic(b) == .(b), cols = italic(K) == .(k))

TARGET_LO <- 40; TARGET_HI <- 60; TARGET_MID <- 50

## ---- Panel A: all life stages ----
pA <- ggplot(demog_iqr, aes(generation, colour = stage_label)) +
  geom_ribbon(aes(ymin = q25, ymax = q75, fill = stage_label),
              alpha = 0.18, linewidth = 0, show.legend = FALSE) +
  geom_line(aes(y = med), linewidth = 0.6) +
  geom_rect(data = retained, inherit.aes = FALSE,
            aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
            fill = NA, colour = "grey15", linewidth = 0.9) +
  facet_grid(b ~ k, scales = "free_y", labeller = lab_kb) +
  scale_colour_manual(values = stage_pal, drop = FALSE) +
  scale_fill_manual(values = stage_pal, drop = FALSE, guide = "none") +
  scale_x_continuous(limits = c(0, GEN_MAX_PLOT), breaks = c(0, 50, 100, 150, 200),
                     expand = expansion(mult = c(0.01, 0.02))) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Years", y = "Abundance", colour = "Life stage") +
  guides(colour = guide_legend(override.aes = list(linewidth = 2, alpha = 1))) +
  theme_fig +
  theme(legend.position = "bottom",
        strip.text = element_text(size = 10))

## ---- Panel B: adults only, with target band (40-60) and reference at 50 ----
adult_iqr  <- demog_iqr[stage_label == "Adults"]
facet_keys <- unique(adult_iqr[, .(k, b)])

pB <- ggplot(adult_iqr, aes(generation)) +
  geom_rect(data = facet_keys, inherit.aes = FALSE,
            aes(xmin = -Inf, xmax = Inf, ymin = TARGET_LO, ymax = TARGET_HI),
            fill = "grey80", alpha = 0.5) +
  geom_hline(yintercept = TARGET_MID, linetype = "dashed",
             linewidth = 0.3, colour = "grey40") +
  geom_ribbon(aes(ymin = q25, ymax = q75),
              fill = stage_pal[["Adults"]], alpha = 0.35, linewidth = 0) +
  geom_line(aes(y = med), colour = stage_pal[["Adults"]], linewidth = 0.8) +
  geom_rect(data = retained, inherit.aes = FALSE,
            aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
            fill = NA, colour = "grey15", linewidth = 0.9) +
  facet_grid(b ~ k, scales = "free_y", labeller = lab_kb) +
  scale_x_continuous(limits = c(0, GEN_MAX_PLOT), breaks = c(0, 50, 100, 150, 200),
                     expand = expansion(mult = c(0.01, 0.02))) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 4), labels = scales::comma,
                     expand = expansion(mult = c(0.02, 0.06))) +
  labs(x = "Years", y = "Adult abundance") +
  theme_fig +
  theme(strip.text = element_text(size = 10))

## ---- combine: A (all stages) over B (adults) ----
figS2 <- pA / pB +
  plot_layout(heights = c(2, 1.35), guides = "collect") +
  plot_annotation(tag_levels = "A") &
  theme(legend.position = "bottom",
        plot.tag = element_text(size = 14, face = "bold"))

figS2

ggsave(file.path(fig_path, "FigureS2.png"), figS2,
       width = 9, height = 11, units = "in", dpi = 600, bg = "white")
ggsave(file.path(fig_path, "FigureS2.pdf"), figS2,
       width = 9, height = 11, units = "in", device = cairo_pdf)


######## FIGURE 2 AND FIGURE S3 (genotype P1 trends) #############

dt <- readRDS(file.path(res_path, "Quanti_data_processed.RDS"))

dt$selection_type <- factor(dt$selection_type, levels = c("Neutral","Wori > Wf1 > Weu","Weu > Wf1 > Wori","Wf1 > Weu = Wori"))
gc()

# P1 bins
dt[, P1_clean := round(P1, 1)]

# Year bins (20-year intervals)
dt[, year_bin := cut(year,breaks = seq(0, max(year) + 20, by = 20),include.lowest = TRUE)]

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

dt_med$selection_type <- factor(dt_med$selection_type, levels = c("Neutral","Wori > Wf1 > Weu","Weu > Wf1 > Wori","Wf1 > Weu = Wori"))

# remove NA from neutral 
dt_med[, selection_strength := as.character(selection_strength)]
dt_med[, selection_strength := factor(selection_strength,levels = c("low", "mid", "high", NA))]
dt_med[dt_med$selection_type=="Neutral","selection_strength"] <- " "

## SUPP FIGURE 2
p1_plot_full <- ggplot(
  dt_med[age_class == 3 & configuration != "No introduction"],
  aes(x = year_mid, y = P1_clean, alpha = prop_median, fill = P1_clean)
) +
  geom_tile(width = 20, height = 0.1) +
  scale_fill_gradientn(
    colours = c("#ffcc00", "#25858EFF", "#482173FF"),
    values = scales::rescale(c(-1, 0, 1)),
    limits = c(-1, 1),
    breaks = c(-1, 0, 1),
    labels = c("European beech", "Hybrid", "Oriental beech"),
    name = "Genotype",
    guide = guide_colorbar(
      title.position = "top",
      barwidth = 8,
      barheight = 0.8
    )
  )+
  scale_alpha_continuous(
    range = c(0.25, 1),
    name = "Proportion of individuals",
    guide = guide_legend(
      override.aes = list(fill = "grey50"),
      title.position = "top" 

    )
  )+
  scale_x_continuous(breaks = seq(0, max(dt_med$year_mid), by = 400)) +
  scale_y_continuous(breaks = c(-1, 0, 1))+
  facet_nested(
    proportion_orientalis + configuration ~ selection_type + selection_strength,
    labeller = labeller(
      selection_strength = c(
        "low" = "Low",
        "mid" = "Intermediate",
        "high" = "High"
      ),
      proportion_orientalis = c("0.1" = "10%", "0.25" = "25%", "0.4" = "40%"),
      selection_type = as_labeller(c(
        "Neutral" = "Neutral",
        "Wori > Wf1 > Weu" = "W[Ori]~'>'~W[F1]~'>'~W[Eu]",
        "Weu > Wf1 > Wori" = "W[Eu]~'>'~W[F1]~'>'~W[Ori]",
        "Wf1 > Weu = Wori" = "W[F1]~'>'~W[Eu]~'='~W[Ori]"
      ), label_parsed)
    )
  )+
  theme_fig +
  theme(
    ggh4x.facet.nestline = element_line(size = 1),
    strip.text = element_text(size = 10),
    strip.background = element_blank(),
    strip.text.y = element_text(angle = 0),
    plot.title = element_text(size = 11, hjust = 0.5),
    plot.tag = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 9),
    legend.text  = element_text(size = 8),
    legend.key = element_blank(),
    legend.position = "bottom", 
    legend.spacing.x = unit(8, "mm"),
    legend.box.spacing = unit(4, "mm")
  ) +
  labs(
    x = "Year",
    y = "Median genotype value",
    alpha = "Proportion of individuals"
  ) 
p1_plot_full

ggsave(
  filename = file.path(fig_path, "FigureS3.png"),
  plot = p1_plot_full,
  width = 10.8,
  height = 8.5,
  units = "in"
)

ggsave(
  filename = file.path(fig_path, "FigureS3.pdf"),
  plot = p1_plot_full,
  width = 10.8,
  height = 8.5,
  units = "in",
  device = cairo_pdf
)

### MAIN FIGURE 2
sub <-  dt_med[age_class == 3 & configuration != "No introduction" & proportion_orientalis == 0.25 & configuration == "Multiple clusters"]

## place neutral on top
## assign Intermediate to neutral data
sub[, selection_strength := as.character(selection_strength)]
sub[, selection_strength := factor(selection_strength,levels = c("low", "mid", "high", NA))]
sub[sub$selection_type=="Neutral","selection_strength"] <- "mid"


p1_plot_main <- ggplot(sub,
  aes(x = year_mid, y = P1_clean, alpha = prop_median, fill = P1_clean)
) +
  geom_tile(width = 20, height = 0.1) +
  scale_fill_gradientn(
    colours = c("#ffcc00", "#25858EFF", "#482173FF"),
    values = scales::rescale(c(-1, 0, 1)),
    limits = c(-1, 1),
    breaks = c(-1, 0, 1),
    labels = c("European beech", "Hybrid", "Oriental beech"),
    name = "Genotype",
    guide = guide_colorbar(
      title.position = "top",
      barwidth = 1,
      barheight = 6
    )
  )+
  scale_alpha_continuous(
    range = c(0.25, 1),
    name = "Proportion of individuals", 
    guide = guide_legend(
      override.aes = list(fill = "grey50"),
      title.position = "top" 
      
    )
  )+
  scale_x_continuous(breaks = seq(0, max(dt_med$year_mid), by = 400)) +
  scale_y_continuous(breaks = c(-1,0, 1))+
  facet_nested(
    selection_strength ~ selection_type,
    labeller = labeller(selection_strength = c(
      "low" = "Low",
      "mid" = "Intermediate",
      "high" = "High"
    ),
    selection_type = as_labeller(c(
      "Neutral" = "Neutral",
      "Wori > Wf1 > Weu" = "W[Ori]~'>'~W[F1]~'>'~W[Eu]",
      "Weu > Wf1 > Wori" = "W[Eu]~'>'~W[F1]~'>'~W[Ori]",
      "Wf1 > Weu = Wori" = "W[F1]~'>'~W[Eu]~'='~W[Ori]"
    ), label_parsed)
    )
  )+
  #theme_fig +
  theme(
   
    axis.text.x = element_text(hjust = 0.5),
    axis.line = element_line(linewidth = 0.3, colour = "black"),
    axis.ticks = element_line(linewidth = 0.3),
    axis.ticks.length = unit(1.5, "mm"),
    axis.title = element_text(size = 11),
    axis.text  = element_text(size = 9),
    
    strip.text = element_text(size = 11),
    strip.text.y = element_text(angle = 0, hjust = 0),
    strip.background = element_blank(),
    
    legend.title = element_text(size = 10),
    legend.text  = element_text(size = 8),
    legend.key = element_blank(),
    legend.position = "right",
    
    panel.grid = element_blank(),
    panel.background = element_blank(), 
    
    plot.title = element_text(size = 11, hjust = 0.5),
    plot.tag = element_text(size = 14, face = "bold"),
    plot.background = element_rect(fill = "white", colour = NA),
    
    
  )+
  labs(
    x = "Year",
    y = "Median genotype value",
    alpha = "Proportion of individuals"
  ) 
p1_plot_main


ggsave(
  filename = file.path(fig_path, "Figure2.png"),
  plot = p1_plot_main,
  width = 10,
  height = 6,
  units = "in"
)

ggsave(
  filename = file.path(fig_path, "Figure2.pdf"),
  plot = p1_plot_main,
  width = 10,
  height = 6,
  units = "in",
  device = cairo_pdf
)


######## FIGURE S4       (pure and hybrid proportions trends) ##############

### unify the selection strength labels
dt <- readRDS(file.path(res_path, "Quanti_data_processed.RDS"))

dt$selection_type <- factor(dt$selection_type, levels = c("Neutral","Wori > Wf1 > Weu","Weu > Wf1 > Wori","Wf1 > Weu = Wori"))
gc()

dt3_prop <- dt[,.( 
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


    # check replicates variability across years (spread q90-q10)
    
    ggplot(
      dt3_prop_quant %>%
        mutate(spread_hyb = q90_hyb - q10_hyb),
      aes(year, spread_hyb)
    ) +
      geom_line(aes(group = factor(age_class), color = factor(age_class))) +
      stat_summary(fun = median,geom = "line",linewidth = 0.5 )+
      facet_nested(selection_type+selection_strength~configuration+proportion_orientalis)
    
    ggplot(
      dt3_prop_quant %>%
        mutate(spread_ori = q90_ori - q10_ori),
      aes(year, spread_ori)
    ) +
      geom_line(aes(group = factor(age_class), color = factor(age_class))) +
      stat_summary(fun = median,geom = "line",linewidth = 0.5 )+
      facet_nested(selection_type+selection_strength~configuration+proportion_orientalis)



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

############# FIGURE S4

dt3_genot_quant_wide$selection_type <- factor(dt3_genot_quant_wide$selection_type, levels = c("Neutral","Wori > Wf1 > Weu","Weu > Wf1 > Wori","Wf1 > Weu = Wori"))
dt3_genot_quant_wide$selection_strength <- as.character(dt3_genot_quant_wide$selection_strength)
dt3_genot_quant_wide$selection_strength <- factor(dt3_genot_quant_wide$selection_strength,levels = c("low", "mid", "high", " "))
dt3_genot_quant_wide[dt3_genot_quant_wide$selection_type=="Neutral","selection_strength"] <- " "

genotypes <- ggplot(subset(dt3_genot_quant_wide, age_class ==3& configuration!= "No introduction"),
       aes(x = year,
           y = q50,
           colour = Genotype,
           fill = Genotype,
           linetype = factor(configuration),
           group = interaction(Genotype, configuration))) +
  geom_line(linewidth = 0.5) +
  geom_ribbon(aes(ymin = q10, ymax = q90),alpha = 0.1,colour = NA) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "longdash"), name = "Configuration") +
  scale_colour_manual(
    values = my_cols2,
    name = "Genotype",
    labels = c(
      ori = "Oriental beech",
      hyb = "Hybrids",
      syl = "European beech"
    )
  ) +
  
  scale_fill_manual(
    values = my_cols2,
    name = "Genotype",
    labels = c(
      ori = "Oriental beech",
      hyb = "Hybrids",
      syl = "European beech"
    )
  ) +
  labs(x = "Year", y = "Proportion") +
  facet_nested(proportion_orientalis ~ selection_type + selection_strength,
    labeller = labeller(selection_strength = c(
      "low" = "Low",
      "mid" = "Intermediate",
      "high" = "High"
    ),proportion_orientalis = c("0.1" = "10%", "0.25" = "25%", "0.4" = "40%")),
    )+
  theme_fig+
  theme(  
          legend.title = element_text(size = 9),
          legend.text  = element_text(size = 8),
          legend.key = element_blank(),
          legend.position = "bottom"
  )
genotypes

ggsave(
  filename = file.path(fig_path, "FigureS4.png"),
  plot = genotypes,   
  width = 11,
  height = 4,
  units = "in"
)

ggsave(
  filename = file.path(fig_path, "FigureS4.pdf"),
  plot = genotypes,
  width = 11,
  height = 4,
  units = "in",
  device = cairo_pdf
)


######## FIGURE S5       (time to reach 80% NW and 50% hybrids) ##################

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
t_prod <- get_time_to_threshold(dt, "NW_rel", 0.8, "Time to 80% NW",  min_year = 75)

# combine
dt_final <- rbindlist(list(t_hyb,t_prod))
dt_final[, metric := factor(metric,levels = c("Time to 50% hybrids","Time to 80% NW"))]
dt_final$selection_type <- factor(dt_final$selection_type, levels = c("Neutral","Wori > Wf1 > Weu","Weu > Wf1 > Wori","Wf1 > Weu = Wori"))
dt_final[, selection_strength := as.character(selection_strength)]
dt_final[, selection_strength := factor(selection_strength,levels = c("low", "mid", "high", NA))]
dt_final[dt_final$selection_type=="Neutral","selection_strength"] <- "mid"


time_hyb <- ggplot(subset(dt_final,metric == "Time to 50% hybrids" & configuration!="No introduction" ),
                   aes(x = factor(proportion_orientalis),
                       y = value,
                       fill = configuration,
                       group = interaction(proportion_orientalis, configuration)), 
                   colour = "black") +
  geom_boxplot(outlier.shape = NA,width=0.4,  position = position_dodge(width = 0.6, preserve = "single")) +
  stat_compare_means(aes(group = interaction(configuration, proportion_orientalis)), 
                     method = "kruskal.test",  label = "p.signif", hide.ns = TRUE, label.y=600) +
  scale_fill_manual(values = config_palette, name = "Configuration") +
  scale_color_manual(values = config_palette) +
  scale_x_discrete(labels = c("0.1" = "10%","0.25" = "25%", "0.4" = "40%"))+
  scale_y_continuous( limits = c(0, 700), breaks = seq(0, 700, by = 100),  expand = c(0, 0))+
  facet_nested(
    selection_strength ~ selection_type  ,scales = "free",
    labeller = labeller(
      selection_strength = c(
        "low" = "Low",
        "mid" = "Intermediate",
        "high" = "High"
      ),
      selection_type = as_labeller(c(
        "Neutral" = "Neutral",
        "Wori > Wf1 > Weu" = "W[Ori]~'>'~W[F1]~'>'~W[Eu]",
        "Weu > Wf1 > Wori" = "W[Eu]~'>'~W[F1]~'>'~W[Ori]",
        "Wf1 > Weu = Wori" = "W[F1]~'>'~W[Eu]~'='~W[Ori]"
      ), label_parsed)
    )
  )+
  
  labs(x = "Introduction Intensity",
       y = "Years") +
  guides() +
  theme_fig+ 
  theme(panel.grid.major = element_line(color="grey80"))
time_hyb


time_nw <-  ggplot(subset(dt_final,metric == "Time to 80% NW" & configuration!="No introduction" ),
                   aes(x = factor(proportion_orientalis),
                       y = value,
                       fill = configuration,
                       group = interaction(proportion_orientalis, configuration)), 
                   colour = "black") +
  geom_boxplot(outlier.shape = NA,width=0.4,  position = position_dodge(width = 0.6, preserve = "single")) +
  
  scale_y_continuous(limits = c(0, 700), breaks = seq(0, 700, by = 100),  expand = c(0, 0))+
  stat_compare_means(aes(group = interaction(configuration, proportion_orientalis)), 
                     method = "kruskal.test",  label = "p.signif", hide.ns = TRUE, label.y=600) +
  scale_fill_manual(values = config_palette, name = "Configuration") +
  scale_x_discrete(labels = c("0.1" = "10%","0.25" = "25%", "0.4" = "40%"))+
  scale_color_manual(values = config_palette) +
  facet_nested(
    selection_strength ~ selection_type  ,scales = "free",
    labeller = labeller(
      selection_strength = c(
        "low" = "Low",
        "mid" = "Intermediate",
        "high" = "High"
      ),
      selection_type = as_labeller(c(
        "Neutral" = "Neutral",
        "Wori > Wf1 > Weu" = "W[Ori]~'>'~W[F1]~'>'~W[Eu]",
        "Weu > Wf1 > Wori" = "W[Eu]~'>'~W[F1]~'>'~W[Ori]",
        "Wf1 > Weu = Wori" = "W[F1]~'>'~W[Eu]~'='~W[Ori]"
      ), label_parsed)
    )
  )+    
  labs(x = "Introduction Intensity",
       y = "Years") +
  guides(color = "none") +
  theme_fig+ 
  theme(panel.grid.major = element_line(color="grey80"))
time_nw


time <- (time_hyb / time_nw) +
  plot_layout(guides = "collect", heights = c(1, 1)) +
  plot_annotation(tag_levels = "A") &
  theme(
    legend.position = "right",
    plot.tag = element_text(size = 14, face = "bold")
  )
time


ggsave(
  filename = file.path(fig_path, "FigureS5.png"),
  plot = time, 
  width =10,
  height = 9,
  units = "in", 
  dpi = 600
)

ggsave(
  filename = file.path(fig_path, "FigureS5.pdf"),
  plot = time, 
  width =10,
  height = 9,
  units = "in", 
  device = cairo_pdf
)


######## FIGURE 3a           (hybrid proportions trends) ############

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

dt3_genot_quant_wide$selection_type <- factor(dt3_genot_quant_wide$selection_type, levels = c("Neutral","Wori > Wf1 > Weu","Weu > Wf1 > Wori","Wf1 > Weu = Wori"))


############ only orientalis optimum --- removed selection type from facte nested!

## get neutral data to plot under
neutral_data <- dt3_genot_quant_wide %>%
  dplyr::filter(selection_type == "Neutral",age_class == 3 & year < 500 )

non_neutral <- dt3_genot_quant_wide %>%
  dplyr::filter(selection_type != "Neutral",age_class == 3 & year < 500 )

facet_combos <- non_neutral %>%
  dplyr::distinct(selection_type, selection_strength)
neutral_expanded <- merge( neutral_data, facet_combos, by = NULL)


hybprop_trend_syl <- ggplot(subset(non_neutral,age_class=="3"& Genotype=="hyb"&selection_type =="Wori > Wf1 > Weu" & configuration != "No introduction"),  
                            aes(year, q50,colour =configuration,fill=configuration,
                                linewidth = proportion_orientalis,
                                group  =interaction(configuration, proportion_orientalis) 
                            )) +
  # neutral baseline in all facets
  geom_ribbon(data = subset(neutral_expanded,age_class=="3"& Genotype=="hyb"& selection_type.y =="Wori > Wf1 > Weu" & configuration != "No introduction"), 
              aes(x = year,
                  ymin = q10,
                  ymax = q90, 
                  alpha = proportion_orientalis,
                  group = interaction(selection_strength.y, proportion_orientalis, configuration)),
              inherit.aes = FALSE,
              fill = "grey80",
  ) +
  
  geom_line(size = 1) +
  geom_vline(xintercept=100, linetype="dashed")+
  
  scale_colour_manual(values = config_palette, name = "Configuration") +
  scale_fill_manual(values = config_palette) +
  scale_alpha_manual(values = c("0.1" = 0.3,"0.25" = 0.7,"0.4" = 0.9), labels = c("0.1" = "10%", "0.25" = "25%", "0.4" = "40%"),name = "Introduction Intensity")+
  scale_linewidth_manual(values = c( "0.1" = 0.5,"0.25" = 0.8, "0.4" = 1.20), labels = c("0.1" = "10%", "0.25" = "25%", "0.4" = "40%"),name = "Introduction Intensity" )+
  scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  
  facet_nested(
    ~   selection_strength,
    labeller = labeller(selection_strength = c(
      "low" = "Low",
      "mid" = "Intermediate",
      "high" = "High"
    )
    )
  )+
  labs(x = "Year", y = "Hybrid proportions")+
  guides(fill="none", color = "none", linetype = "none")+
  theme_fig

hybprop_trend_syl

######## FIGURE S6a #############

hybprop_trend_supp <- ggplot(subset(non_neutral,Genotype=="hyb"&selection_type%in% c("Weu > Wf1 > Wori", "Wf1 > Weu = Wori") & configuration != "No introduction"),  
                             aes(year, q50,colour =configuration,fill=configuration,
                                 linewidth = proportion_orientalis,
                                 group  =interaction(configuration, proportion_orientalis),  
                             )) +
  # neutral baseline in all facets
  geom_ribbon(data = subset(neutral_expanded,Genotype=="hyb"&selection_type.y %in% c("Weu > Wf1 > Wori", "Wf1 > Weu = Wori")& configuration != "No introduction"), 
              aes(x = year,
                  ymin = q10,
                  ymax = q90, 
                  alpha = proportion_orientalis,
                  group = interaction(selection_strength.y, proportion_orientalis, configuration)),
              inherit.aes = FALSE,
              fill = "grey70",
  ) +
  geom_line(size = 1) +
  geom_vline(xintercept=100, linetype="dashed")+
  scale_colour_manual(values = config_palette, name = "Configuration") +
  scale_fill_manual(values = config_palette) +
  scale_alpha_manual(values = c("0.1" = 0.3,"0.25" = 0.7,"0.4" = 0.9), labels = c("0.1" = "10%", "0.25" = "25%", "0.4" = "40%"),name = "Introduction Intensity")+
  scale_linewidth_manual(values = c( "0.1" = 0.5,"0.25" = 0.8, "0.4" = 1.20), labels = c("0.1" = "10%", "0.25" = "25%", "0.4" = "40%"),name = "Introduction Intensity" )+
  scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  
  labs(x = "Year", y = "Hybrid proportions")+

  facet_nested(
    ~  selection_type+ selection_strength,
    labeller = labeller(selection_strength = c(
      "low" = "Low",
      "mid" = "Intermediate",
      "high" = "High"
    ),
    selection_type = as_labeller(c(
      "Neutral" = "Neutral",
      "Wori > Wf1 > Weu" = "W[Ori]~'>'~W[F1]~'>'~W[Eu]",
      "Weu > Wf1 > Wori" = "W[Eu]~'>'~W[F1]~'>'~W[Ori]",
      "Wf1 > Weu = Wori" = "W[F1]~'>'~W[Eu]~'='~W[Ori]"
    ), label_parsed)
    )
  )+
  
  guides(fill="none", color = "none", linetype = "none")+
  theme_fig


hybprop_trend_supp

 
######## FIGURE 3c           (productivity trends) #################

dt <- readRDS(file.path(res_path, "Final_dataset_replicate_level.RDS"))
dt$selection_type <- factor(dt$selection_type, levels = c("Neutral","Wori > Wf1 > Weu","Weu > Wf1 > Wori","Wf1 > Weu = Wori"))

#### overall NW trends 
ggplot(dt,
       aes(year,  NW,
           group = interaction(configuration, replicate2) ))+  
  geom_line() +
  labs(x = "Year", y = "Productivity (N x W)")+
  facet_nested(proportion_orientalis+ configuration~ selection_type + selection_strength, labeller = labeller(selection_strength = c("low"="Low", "mid"="Intermediate", "high"="High"))) +
  theme_minimal() +

  theme(strip.text.y = element_text(angle = 0), 
        legend.position = "bottom", 
  )

ggsave(
  paste0(fig_path,"/NW_trend.png"),
  plot = last_plot(),   # or assign your plot to an object and use plot = p
  width = 16,
  height = 9,
  units = "in"
)


# summarize across replicates using quantiles
dt_summary <- dt[, .(
  
  q10_NW = quantile(NW, 0.1, na.rm = TRUE),
  q50_NW = quantile(NW, 0.5, na.rm = TRUE),
  q90_NW = quantile(NW, 0.9, na.rm = TRUE)
  
),
by = .(configuration, proportion_orientalis, year,selection_type, selection_strength)]

# replace NA in No introduction with 0
dt_summary[configuration == "No introduction" & is.na(q50_NW), q50_NW := 0]
dt_summary$selection_type <- factor(dt_summary$selection_type, levels = c("Neutral","Wori > Wf1 > Weu","Weu > Wf1 > Wori","Wf1 > Weu = Wori"))

############ only oreintal favoured

## needs to be tibble
neutral_data <- dt_summary %>%
  as_tibble() %>%
  filter(selection_type == "Neutral", year < 500)

non_neutral <- dt_summary %>%
  as_tibble() %>%
  filter(selection_type != "Neutral",year < 500)

facet_combos <- non_neutral %>%
  distinct(selection_type, selection_strength)

neutral_expanded <- merge(neutral_data, facet_combos, by = NULL)

NW_trend_syl <- ggplot()+
  # neutral baseline in all facets
  geom_ribbon(data = subset(neutral_expanded,selection_type.y =="Wori > Wf1 > Weu" & configuration != "No introduction"), 
              aes(x = year,
                  ymin = q10_NW,
                  ymax = q90_NW,  
                  alpha = proportion_orientalis,
                  group = interaction(selection_strength.y, proportion_orientalis, configuration)),
              inherit.aes = FALSE,
              fill = "grey80",
  ) +
  
  geom_line(data = subset(non_neutral,selection_type =="Wori > Wf1 > Weu" & configuration != "No introduction"),  
            aes(x = year, 
                y = q50_NW,
                colour =configuration,
                linewidth = proportion_orientalis,
                group =interaction(configuration, proportion_orientalis)
            )) +
  
  # median NW for case of No introduction
  geom_line(data = subset(non_neutral,configuration=="No introduction"& year <700),
            aes(x = year,y = q50_NW),
            color = "black",
            linewidth = 1) +
  geom_vline(xintercept=100, linetype="dashed")+
  
  scale_colour_manual(values = config_palette, name = "Configuration") +
  scale_fill_manual(values = config_palette) +
  scale_alpha_manual(values = c("0.1" = 0.3,"0.25" = 0.7,"0.4" = 0.9), labels = c("0.1" = "10%", "0.25" = "25%", "0.4" = "40%"),name = "Introduction Intensity")+
  scale_linewidth_manual(values = c( "0.1" = 0.5,"0.25" = 0.8, "0.4" = 1.20), labels = c("0.1" = "10%", "0.25" = "25%", "0.4" = "40%"),name = "Introduction Intensity" )+
  scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  
  labs(x = "Year", y = "Productivity (N x W)")+
  facet_nested( ~  selection_strength, labeller = labeller(selection_strength = c("low"="Low", "mid"="Intermediate", "high"="High"))) +
  guides(fill="none", alpha = "none", linewidth = "none")+
  theme_fig
NW_trend_syl


######## FIGURE S6c #############

NW_trend_supp <- ggplot()+
  # neutral baseline in all facets
  geom_ribbon(data = subset(neutral_expanded,selection_type.y%in% c("Weu > Wf1 > Wori", "Wf1 > Weu = Wori") & configuration != "No introduction"),  
              aes(x = year,
                  ymin = q10_NW,
                  ymax = q90_NW,  
                  alpha = proportion_orientalis,
                  group = interaction(selection_strength.y, proportion_orientalis, configuration)),
              inherit.aes = FALSE,
              fill = "grey80",
  ) +
  
  geom_line(data = subset(non_neutral,selection_type%in% c("Weu > Wf1 > Wori", "Wf1 > Weu = Wori") & configuration != "No introduction"),  
            aes(x = year, 
                y = q50_NW,
                colour =configuration,
                linewidth = proportion_orientalis,
                group =interaction(configuration, proportion_orientalis)
            )) +
  geom_vline(xintercept=100, linetype="dashed")+
  
  scale_colour_manual(values = config_palette, name = "Configuration") +
  scale_fill_manual(values = config_palette) +
  scale_alpha_manual(values = c("0.1" = 0.3,"0.25" = 0.7,"0.4" = 0.9), labels = c("0.1" = "10%", "0.25" = "25%", "0.4" = "40%"),name = "Introduction Intensity")+
  scale_linewidth_manual(values = c( "0.1" = 0.5,"0.25" = 0.8, "0.4" = 1.20), labels = c("0.1" = "10%", "0.25" = "25%", "0.4" = "40%"),name = "Introduction Intensity" )+
  scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  
  labs(x = "Year", y = "Productivity (N x W)")+
  facet_nested(
    ~  selection_type+ selection_strength,
    labeller = labeller(selection_strength = c(
      "low" = "Low",
      "mid" = "Intermediate",
      "high" = "High"
    ),
    selection_type = as_labeller(c(
      "Neutral" = "Neutral",
      "Wori > Wf1 > Weu" = "W[Ori]~'>'~W[F1]~'>'~W[Eu]",
      "Weu > Wf1 > Wori" = "W[Eu]~'>'~W[F1]~'>'~W[Ori]",
      "Wf1 > Weu = Wori" = "W[F1]~'>'~W[Eu]~'='~W[Ori]"
    ), label_parsed)
    )
  )+
  
  guides(fill="none", alpha = "none", linewidth = "none")+
  theme_fig
NW_trend_supp

######## FIGURE 3b and d     (HYBRID PROPORTIONS and NW at t=100) ###############

dt <- readRDS(file.path(res_path, "Final_dataset_replicate_level.RDS"))

## target year 
y = 100

dt_sub <- subset(dt, year == y)

# reshape
dt_long <- melt(dt_sub,
                measure.vars = c("Hyb_proportion", "Hyb_corr_neutral", "NW","NW_corr_neutral"),
                variable.name = "metric",
                value.name = "value")

dt_long$selection_type <- factor(dt_long$selection_type, levels = c("Neutral","Wori > Wf1 > Weu","Weu > Wf1 > Wori","Wf1 > Weu = Wori"))

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

# subset orientals favoured
t100_syl_hyb <- ggplot(subset(dt_long,metric%in%c("Hyb_proportion")&selection_type=="Wori > Wf1 > Weu"& proportion_orientalis != 0),
                       aes(x = factor(proportion_orientalis),
                           y = value,
                           fill = configuration,
                           group = interaction(proportion_orientalis, configuration)), 
                       color = "black") +
  geom_rect(
    data = subset(neutral_ranges,metric%in%c("Hyb_proportion")),
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, alpha = proportion_orientalis),
    inherit.aes = FALSE,
    fill = "grey70"
  ) +
  geom_boxplot() +
  scale_fill_manual(values = config_palette, name = "Configuration") +
  scale_color_manual(values = config_palette) +
  scale_alpha_manual(values = c("0.1" = 0.5,"0.25" = 0.7,"0.4" = 0.9))+
  scale_x_discrete(labels = c("0.1" = "10%","0.25" = "25%","0.4" = "40%"))+
  stat_compare_means(aes(group = interaction(configuration, proportion_orientalis)), method = "kruskal.test", label = "p.signif", hide.ns = TRUE, label.y = 0.63) +
  facet_nested(metric ~  selection_strength,scales = "free_y", labeller=labeller(metric=c("Hyb_proportion"="Hybrid proportions"), selection_strength = c("low"="Low", "mid"="Intermediate", "high"="High"))) +
  labs(x = "Introduction Intensity", y = "Hybrid proportions") +
  guides(color = "none", alpha = "none") +
  theme_fig+
  theme(panel.background = element_rect(fill = "white", colour = "black"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        
        strip.text = element_text(),
        strip.background = element_rect(colour = "white", fill = "white"), 
        
        legend.key = element_rect(colour = "white"),
        legend.key.spacing.y = unit(0.4, 'cm'), 
        
        plot.title = element_text(hjust = 0.5))

t100_syl_hyb

t100_syl_NW <- ggplot(subset(dt_long,metric%in%c("NW")&selection_type=="Wori > Wf1 > Weu"& proportion_orientalis != 0),
                      aes(x = factor(proportion_orientalis),
                          y = value,
                          fill = configuration,
                         
                          group = interaction(proportion_orientalis, configuration)), 
                      colour = "black") +
  geom_rect(
    data = subset(neutral_ranges,metric%in%c("NW")),
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, alpha = proportion_orientalis),
    inherit.aes = FALSE,
    fill = "grey80"
  ) +
  geom_boxplot() +
  scale_fill_manual(values = config_palette, name = "Configuration") +
  scale_color_manual(values = config_palette) +
  scale_alpha_manual(values = c("0.1" = 0.5,"0.25" = 0.7,"0.4" = 0.9))+
  scale_x_discrete(labels = c("0.1" = "10%","0.25" = "25%","0.4" = "40%"))+
  
  stat_compare_means(aes(group = interaction(configuration, proportion_orientalis)), method = "kruskal.test"  ,  label = "p.signif", hide.ns = TRUE, label.y = 63) +
  facet_nested(metric ~  selection_strength,scales = "free_y", labeller=labeller(metric=c("Hyb_proportion"="Hybrid proportions"),  selection_strength = c("low"="Low", "mid"="Intermediate", "high"="High"))) +
  labs(x = "Introduction Intensity", y = "Productivity (N x W)") +
  guides(color = "none", alpha = "none") +
  theme_fig+
  theme(panel.background = element_rect(fill = "white", colour = "black"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        
        strip.text = element_text(),
        strip.background = element_rect(colour = "white", fill = "white"), 
        
        legend.key = element_rect(colour = "white"),
        legend.key.spacing.y = unit(0.4, 'cm'), 
        
        plot.title = element_text(hjust = 0.5))

t100_syl_NW

######## FIGURE S6b and d    (HYBRID PROPORTIONS and NW at t=100) ###############

t100_supp_hyb <- ggplot(subset(dt_long,metric%in%c("Hyb_proportion")&selection_type%in% c("Weu > Wf1 > Wori", "Wf1 > Weu = Wori") & proportion_orientalis != 0),
                        aes(x = factor(proportion_orientalis),
                            y = value,
                            fill = configuration,
                           
                            group = interaction(proportion_orientalis, configuration)), 
                        colour = "black") +
  geom_rect(
    data = subset(neutral_ranges,metric%in%c("Hyb_proportion")),
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, alpha = proportion_orientalis),
    inherit.aes = FALSE,
    fill = "grey80"
  ) +
  
  geom_boxplot() +
  scale_fill_manual(values = config_palette, name = "Configuration") +
  scale_color_manual(values = config_palette) +
  scale_alpha_manual(values = c("0.1" = 0.5,"0.25" = 0.7,"0.4" = 0.9))+
  scale_x_discrete(labels = c("0.1" = "10%","0.25" = "25%","0.4" = "40%"))+
  
  stat_compare_means(aes(group = interaction(configuration, proportion_orientalis)), method = "kruskal.test",  label = "p.signif", hide.ns = TRUE,label.y = 0.63) +
  facet_nested(metric ~ selection_type + selection_strength,scales = "free_y", 
               labeller=labeller(metric=c("Hyb_proportion"="Hybrid proportions"), 
                                 selection_strength = c("low"="Low", "mid"="Intermediate", "high"="High"),
                                 selection_type = as_labeller(c(
                                   "Neutral" = "Neutral",
                                   "Wori > Wf1 > Weu" = "W[Ori]~'>'~W[F1]~'>'~W[Eu]",
                                   "Weu > Wf1 > Wori" = "W[Eu]~'>'~W[F1]~'>'~W[Ori]",
                                   "Wf1 > Weu = Wori" = "W[F1]~'>'~W[Eu]~'='~W[Ori]"
                                 ), label_parsed)
        )
      )+
  labs(x = "Introduction Intensity", y = "Hybrid proportions") +
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


t100_supp_nw <- ggplot(subset(dt_long,metric%in%c("NW")&selection_type%in% c("Weu > Wf1 > Wori", "Wf1 > Weu = Wori") & proportion_orientalis != 0),
                       aes(x = factor(proportion_orientalis),
                           y = value,
                           fill = configuration,
                          
                           group = interaction(proportion_orientalis, configuration)), 
                       colour = "black") +
  geom_rect(
    data = subset(neutral_ranges,metric%in%c("NW")),
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, alpha = proportion_orientalis),
    inherit.aes = FALSE,
    fill = "grey80"
  ) +
  
  geom_boxplot() +
  scale_fill_manual(values = config_palette, name = "Configuration") +
  scale_color_manual(values = config_palette) +
  scale_alpha_manual(values = c("0.1" = 0.5,"0.25" = 0.7,"0.4" = 0.9))+
  scale_x_discrete(labels = c("0.1" = "10%","0.25" = "25%","0.4" = "40%"))+
  
  stat_compare_means(aes(group = interaction(configuration, proportion_orientalis)), method = "kruskal.test",  label = "p.signif", hide.ns = TRUE, label.y = 70) +
  facet_nested(metric ~ selection_type + selection_strength,scales = "free_y", 
               labeller=labeller(metric=c("Hyb_proportion"="Hybrid proportions"), 
                                 selection_strength = c("low"="Low", "mid"="Intermediate", "high"="High"),
                                 selection_type = as_labeller(c(
                                   "Neutral" = "Neutral",
                                   "Wori > Wf1 > Weu" = "W[Ori]~'>'~W[F1]~'>'~W[Eu]",
                                   "Weu > Wf1 > Wori" = "W[Eu]~'>'~W[F1]~'>'~W[Ori]",
                                   "Wf1 > Weu = Wori" = "W[F1]~'>'~W[Eu]~'='~W[Ori]"
                                 ), label_parsed)
               )
  )+
  labs(x = "Introduction Intensity", y = "Productivity (N x W)") +
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


######## FINAL FIGURE 3 ############

## COMBINE EUROPEAN BECH MAIN
# combine plots
syl_combined <- (
  (hybprop_trend_syl +
    theme(plot.margin = margin(b = 0))) /
    (t100_syl_hyb + theme(strip.text.x = element_blank(), strip.text.y = element_blank(),plot.margin = margin(t = 0, b = 5))) /
    (NW_trend_syl + theme(strip.text.x = element_blank(),plot.margin = margin(b = 0))) /
    (t100_syl_NW + theme(strip.text.x = element_blank(), strip.text.y = element_blank(), plot.margin = margin(t = 0, b = 5)))
) +
  plot_layout(guides = "collect", heights = c(1.3,0.8,1.3,0.8)) +
  plot_annotation(tag_levels = "A") &
  theme(
    legend.position = "right",
    plot.tag = element_text(size = 14, face = "bold")
  )
syl_combined

ggsave(
  filename = file.path(fig_path, "Figure3.png"),
  plot = syl_combined, 
  width =10,
  height = 10,
  units = "in", 
  dpi = 600
)

ggsave(
  filename = file.path(fig_path, "Figure3.pdf"),
  plot = syl_combined, 
  width =10,
  height =10,
  units = "in", 
  device = cairo_pdf
)

######## FINAL FIGURE S6 ############

supp_combined <- (hybprop_trend_supp / 
                    (t100_supp_hyb  + theme(strip.text.x = element_blank(), strip.text.y = element_blank(), plot.margin = margin(t = 0, b = 5))) /
                    (NW_trend_supp  + theme(strip.text.x = element_blank(),plot.margin = margin(b = 0))) /
                    (t100_supp_nw + theme(strip.text.x = element_blank(), strip.text.y = element_blank(), plot.margin = margin(t = 0, b = 5)))
)+
  plot_layout(guides = "collect",heights = c(1.3,0.8,1.3,0.8)) +
  plot_annotation(tag_levels = "A") &  
  theme(
    strip.text.y = element_blank(),
    legend.position = "right",
    plot.tag = element_text(size = 14, face = "bold")
  )
supp_combined

ggsave(
  filename = file.path(fig_path, "FigureS6.png"),
  plot = supp_combined, 
  width =15,
  height = 10,
  units = "in", 
  dpi = 600
)

ggsave(
  filename = file.path(fig_path, "FigureS6.pdf"),
  plot = supp_combined, 
  width =15,
  height = 10,
  units = "in", 
  device = cairo_pdf
)

######## FIGURE S7       (spatial patterns of % orientalis genotype at t = 100) ###############

dt <- readRDS(file.path(res_path, "Orientalis_genot_patch_summary_replicates.RDS") )
dt$selection_type <- factor(dt$selection_type, levels = c("Neutral","Wori > Wf1 > Weu","Weu > Wf1 > Wori","Wf1 > Weu = Wori"))
dt$selection_strength <- as.character(dt$selection_strength)
dt$selection_strength <- factor(dt$selection_strength,levels = c("low", "mid", "high", " "))
dt[dt$selection_type=="Neutral","selection_strength"] <- " "

######  t = 100

# map the SD per patch for each age_class for year 100  and year 1000
grid <- vect("C:/Users/stefanin/Dropbox/WSL_PhD/Projects/Hybridization2/input/input_files/Grid_4x4m_100x100m.shp")
grid_r <- terra::rast(ext = ext(grid), resolution = 4, crs = "EPSG:3035")
n_rows = dim(grid_r)[1]
n_cols = dim(grid_r)[2]

# rescale p1 value from 0 to 1

## SPATIAL DISTRIBUTION FOR ADULTS
grid_plot <- ggplot(subset(dt, age_class == 3& year==100 & configuration != "No introduction"),
                    aes(x = (pop - 1) %% n_rows + 1,
                        y = n_cols - ((pop - 1) %/% n_cols + 1),
                        fill = (q50_p1 + 1) / 2)) +
  geom_raster() +
  scale_fill_viridis_c(option = "viridis", direction = -1) +
  facet_nested(
    proportion_orientalis + configuration ~ selection_type + selection_strength , 
    labeller = labeller(selection_strength = c(
      "low" = "Low",
      "mid" = "Intermediate",
      "high" = "High"
    ),proportion_orientalis = c("0.1" = "10%", "0.25" = "25%", "0.4" = "40%"),
    selection_type = as_labeller(c(
      "Neutral" = "Neutral",
      "Wori > Wf1 > Weu" = "W[Ori]~'>'~W[F1]~'>'~W[Eu]",
      "Weu > Wf1 > Wori" = "W[Eu]~'>'~W[F1]~'>'~W[Ori]",
      "Wf1 > Weu = Wori" = "W[F1]~'>'~W[Eu]~'='~W[Ori]"
    ), label_parsed)
    )
  )+theme_void()+
  theme(
    ggh4x.facet.nestline = element_line(size = 1),
    panel.background = element_rect(fill = "white"),
    panel.spacing.x = unit(0.3, "lines"),
    panel.spacing.y = unit(0.3, "lines"),
    strip.text = element_text(size = 9),
    strip.background = element_rect(fill = "white"),
    #plot.background = element_blank(),
    aspect.ratio = 1,
    legend.position = "bottom",
    ,
    axis.title = element_text(size = 11),
    
    legend.title = element_text(size = 9),
    legend.text  = element_text(size = 8),
    legend.key = element_blank(),
    
  )+
  labs(x = "", y = "", fill = "Proportion of Oriental beech ancestry") 
grid_plot

ggsave(
  filename = file.path(fig_path, "FigureS7.png"),
  plot = grid_plot,   
  width =12,
  height = 10,
  units = "in",
  dpi = 600
)

ggsave(
  filename = file.path(fig_path, "FigureS7.pdf"),
  plot = grid_plot,
  width =12,
  height = 10,
  units = "in",
  device = cairo_pdf
)



######## FIGURE 4a           (pareto optimization trends) ##########

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
                year < 700 )

non_neutral <- best_str2 %>%
  dplyr::filter(selection_type != "Neutral",
                year < 700)

facet_combos <- non_neutral %>%
  dplyr::distinct(selection_type, selection_strength)
neutral_expanded <- merge( neutral_data, facet_combos, by = NULL)

non_neutral$selection_type <- factor(non_neutral$selection_type, levels = c("Neutral","Wori > Wf1 > Weu","Weu > Wf1 > Wori","Wf1 > Weu = Wori"))

## neutral grey area band
setDT(neutral_expanded)
neutral_area <-  neutral_expanded[
  selection_type.y == "Wori > Wf1 > Weu",
  .(
    ymin = quantile(med_NW, 0.1, na.rm = TRUE),
    ymax = quantile(med_NW, 0.9, na.rm = TRUE),
    ymed = median(med_NW, na.rm = TRUE)
  ),
  by = .(year, selection_strength.y)
]


pareto_A_main <- ggplot() +
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
  geom_line(data = subset(dt_merged_summary,configuration=="No introduction"& year <600),
            aes(x = year,
                y = med_NW),
            color = "black",
            linewidth = 1) +
  # non-neutral best strategies
  geom_line(
    data = subset(non_neutral, selection_type == "Wori > Wf1 > Weu"),
    aes(x = year,
        y = med_NW,
        color = configuration,
        linewidth = proportion_orientalis,  
        group = interaction(strategy_id, segment_id)),
  ) +
  
  geom_point(data = subset(non_neutral,selection_type == "Wori > Wf1 > Weu"),
             aes(x = year,
                 y = med_NW,
                 color = configuration,
                 group = interaction(strategy_id, segment_id)),
             size = 0.7) +
  geom_vline(xintercept = c(100, 500),  linetype= "dashed", color = "black")+

  scale_color_manual(values = config_palette, name = "Configuration") +
  scale_linewidth_manual(values = c( "0.1" = 0.4,"0.25" = 0.8, "0.4" = 1.2),labels = c("0.1" = "10%", "0.25" = "25%", "0.4" = "40%"),name = "Introduction Intensity" )+
  scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  facet_nested( ~ selection_strength, labeller = labeller(selection_strength = c("low"="Low", "mid"="Intermediate", "high"="High"))) +
  labs(
    x = "Year",
    y = "Productivity (N x W)"
  )+
  theme_fig 

pareto_A_main


######## FIGURE S8a      (pareto optimization trends) ##########

# plot along time
pareto_A_supp <- ggplot() +
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
  geom_line(data = subset(non_neutral,selection_type != "Wori > Wf1 > Weu"),
            aes(x = year,
                y = med_NW,
                color = configuration,
                linewidth = proportion_orientalis,  
                group = interaction(strategy_id, segment_id)),
  ) +
  
  geom_point(data = subset(non_neutral,selection_type != "Wori > Wf1 > Weu"),
             aes(x = year,
                 y = med_NW,
                 color = configuration, linetype = proportion_orientalis, 
                 group = interaction(strategy_id, segment_id)),
             size = 0.7) +
  geom_vline(xintercept = c(100, 500),  linetype= "dashed", color = "black")+
  
  facet_nested( ~ selection_type + selection_strength, scales = "free_y", 
               labeller = labeller( selection_strength = c("low"="Low", "mid"="Intermediate", "high"="High"),
                                   selection_type = as_labeller(c(
                                     "Neutral" = "Neutral",
                                     "Wori > Wf1 > Weu" = "W[Ori]~'>'~W[F1]~'>'~W[Eu]",
                                     "Weu > Wf1 > Wori" = "W[Eu]~'>'~W[F1]~'>'~W[Ori]",
                                     "Wf1 > Weu = Wori" = "W[F1]~'>'~W[Eu]~'='~W[Ori]"
                                   ), label_parsed)
                                   
                                   )) +
  scale_linewidth_manual(values = c( "0.1" = 0.4,"0.25" = 0.8, "0.4" = 1.2),labels = c("0.1" = "10%", "0.25" = "25%", "0.4" = "40%"),name = "Introduction Intensity" )+
  scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
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
    y = "Productivity (N x W)"
  )
pareto_A_supp


######## FIGURE 4b           (pareto opt at t=100,500 ) ##############

dt_merged_summary$selection_type <- factor(dt_merged_summary$selection_type, levels = c("Neutral","Wori > Wf1 > Weu","Weu > Wf1 > Wori","Wf1 > Weu = Wori"))
best_str$selection_type <- factor(best_str$selection_type, levels = c("Neutral","Wori > Wf1 > Weu","Weu > Wf1 > Wori","Wf1 > Weu = Wori"))
range_vals <- range(best_str$med_hyb_prop, na.rm = TRUE)

# get the min (+10% otherwise 0 disappears), mid and max hybrid proportions values for legend
legend_vals <- c(
  round(range_vals[1], 2)+1/10, 
  round(mean(range_vals), 2),
  round(range_vals[2], 2)
)


# remove "No introduction" scenario -- keep only selection against slyvatica
dt_sub <- subset(dt_merged_summary,year %in% c(100,500) & selection_type %in% c("Wori > Wf1 > Weu")& configuration!= "No introduction")
best_sub <- subset(best_str,year %in% c(100,500)& selection_type %in% c("Wori > Wf1 > Weu")& configuration!= "No introduction")

pareto_B_main <- ggplot(best_sub, aes(x = med_cost, y = med_NW)) +
  geom_point(data = dt_sub,
             aes(x = med_cost, y = med_NW),
             inherit.aes = FALSE,
             color = "black",
             alpha = 0.7)+

  geom_point(data = best_sub,aes(size = med_hyb_prop, color = configuration,shape = factor(proportion_orientalis) ), alpha = 1)+
  #geom_line(data = best_sub, aes(group = interaction(selection_type, selection_strength)),color = "red") +
  #geom_text(data =best_sub, aes(label = proportion_orientalis), vjust = -0.8) +
  
  ## point for no introduction --?
  geom_point(data = subset(dt_merged_summary,configuration=="No introduction"& year ==100),
             aes(x = year,
                 y = med_NW),
             color = "black",
             shape = 4, size = 2) +
  scale_color_manual(values = config_palette, name = "Configuration") +
  scale_size_continuous(
    name = "Hybrid proportion",
    breaks = legend_vals,
    labels = scales::percent_format(accuracy = 1),
    range = c(2, 10)
  )+
  scale_shape_manual( name = "Introduction Intensity", values = c(1, 10, 16),  labels = c("0.1" = "10%", "0.25" = "25%", "0.4" = "40%") ) +
  
  labs(y = "Productivity (N x W)",
       x = "Estimated cost") +
  facet_nested(year ~  selection_strength, scales = "free_y", 
               labeller = labeller(selection_strength = c("low"="Low", "mid"="Intermediate", "high"="High"))) +
  theme_fig 
  theme(
    panel.background = element_rect(fill = "white", colour = "black"),
    panel.grid = element_blank(),
    strip.background = element_rect(colour = "white", fill = "white"),
    plot.title = element_text(hjust = 0.5)
  )
  
pareto_B_main


######## FIGURE S8b      (pareto opt at t=100) ##############

dt_sub2 <- subset(dt_merged_summary,year %in% c(100,500) & selection_type %in% c("Weu > Wf1 > Wori", "Wf1 > Weu = Wori") & configuration!= "No introduction")
best_sub2 <- subset(best_str,year %in% c(100,500)& selection_type %in% c("Weu > Wf1 > Wori", "Wf1 > Weu = Wori")& configuration!= "No introduction")

pareto_B_supp <- ggplot(dt_sub2, aes(x = med_cost, y = med_NW)) +
  geom_point(color = "black",alpha = 0.7) +
  geom_point(data = best_sub2,aes(size = med_hyb_prop, color = configuration,shape = factor(proportion_orientalis)  ), alpha = 1)+

  
  #geom_line(data = best_sub2, aes(group = interaction(selection_type, selection_strength)),color = "red") +
  #geom_text(data =best_sub2, aes(label = proportion_orientalis), vjust = -0.9) +
  scale_color_manual(values = config_palette, name = "Configuration") +
  scale_size_continuous(
    name = "Hybrid proportion",
    breaks = legend_vals,
    labels = scales::percent_format(accuracy = 1),
    range = c(2, 10)
  )+
  scale_shape_manual(
    name = "Introduction Intensity",
    values = c(1, 10, 16),  labels = c("0.1" = "10%", "0.25" = "25%", "0.4" = "40%") 
    #labels = scales::percent_format(accuracy = 1)
  ) +
  
  labs(y = "Productivity (N x W)",
       x = "Estimated cost") +
  facet_nested(year ~ selection_type + selection_strength, scales = "free_y",    labeller = labeller(selection_strength = c("low"="Low", "mid"="Intermediate", "high"="High"))) +
  theme_fig 
  theme(
    panel.background = element_rect(fill = "white", colour = "black"),
    panel.grid = element_blank(),
    strip.background = element_rect(colour = "white", fill = "white"),
    plot.title = element_text(hjust = 0.5)
  )
  pareto_B_supp



######## FINAL FIGURE 4      (pareto optimization) #############

# combine plots
pareto_main <- ((pareto_A_main +
                   theme(plot.margin = margin(b = 0))) / (pareto_B_main+ theme(strip.text.x = element_blank()))) +
  plot_layout(heights = c(1, 1)) +
  plot_annotation(tag_levels = "A") &  
  theme(
    legend.position = "right",
    plot.tag = element_text(size =14, face = "bold")
  )
pareto_main


ggsave(
  filename = file.path(fig_path, "Figure4.png"),
  plot = pareto_main, 
  width =10,
  height = 8,
  units = "in", 
  dpi = 600
)

ggsave(
  filename = file.path(fig_path, "Figure4.pdf"),
  plot = pareto_main, 
  width =10,
  height =8,
  units = "in", 
  device = cairo_pdf
)



######## FINAL FIGURE S8 (pareto optimizaion) #############
pareto_supp <- ((pareto_A_supp  +
                       theme(plot.margin = margin(b = 0))) / (pareto_B_supp+ theme(strip.text.x = element_blank()))) +
  plot_layout(heights = c(1, 1)) +
  plot_annotation(tag_levels = "A") &  
  theme(
    legend.position = "right",
    plot.tag = element_text(size =14, face = "bold")
  )
pareto_supp


ggsave(
  filename = file.path(fig_path, "FigureS8.png"),
  plot = pareto_supp, 
  width =13,
  height = 8,
  units = "in", 
  dpi = 600
)

ggsave(
  filename = file.path(fig_path, "FigureS8.pdf"),
  plot = pareto_supp, 
  width =13,
  height =8,
  units = "in", 
  device = cairo_pdf
)


