#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(stringr)
  library(ggplot2)
  library(scales)
  library(viridis)
  library(patchwork)
})

configuration          <- "dispersed"
layout_run             <- 1
p_focus                <- 25
gen_max                <- 1000
max_internal_replicate <- 50

param_root <- file.path("manuscript", "supplementary", "parameterization")
runs_root  <- file.path(param_root, "parameterization_runs")

plot_dir  <- file.path(param_root, "plots")
dir.create(plot_dir,  recursive = TRUE, showWarnings = FALSE)

# ---- helpers ----
theme_plot <- theme_bw()

save_plot <- function(p, name, w = 20, h = 10, dpi = 220) {
  fn <- file.path(plot_dir, paste0(name, ".png"))
  ggsave(fn, p, width = w, height = h, dpi = dpi, bg = "white")
  invisible(fn)
}

parse_meta_from_basename <- function(path) {
  bname <- basename(path)
  m <- str_match(
    bname,
    "^(.+?)_p(\\d+)_r(\\d+)_k(\\d+(?:\\.\\d+)?)_b(\\d+(?:\\.\\d+)?)(?:_|\\.|$)"
  )
  if (any(is.na(m))) stop("Could not parse metadata from filename: ", bname)
  data.table(
    configuration  = m[, 2],
    ori_proportion = as.integer(m[, 3]),
    layout_run     = as.integer(m[, 4]),
    k              = as.numeric(m[, 5]),
    b              = as.numeric(m[, 6])
  )
}

# ---- find demography .txt files for this config + run ----
run_str <- sprintf("r%02d", as.integer(layout_run))

demog_files <- list.files(
  file.path(runs_root, configuration),
  recursive  = TRUE,
  full.names = TRUE,
  pattern    = "\\.txt$"
)

norm <- gsub("\\\\", "/", demog_files)
demog_files <- demog_files[
  grepl("/run/[^/]+\\.txt$", norm) &
    !grepl("_bygen\\.txt$", norm) &
    !grepl("\\.log$", norm) &
    grepl(paste0("/", run_str, "/"), norm)
]

if (!is.na(p_focus)) {
  ptag <- sprintf("_p%d_", as.integer(p_focus))
  demog_files <- demog_files[grepl(ptag, basename(demog_files))]
}

message("Found ", length(demog_files), " demography .txt files.")
if (!length(demog_files)) stop("No demography .txt files found.")

p_tag <- if (is.na(p_focus)) "pall" else paste0("p", as.integer(p_focus))

cols_to_read <- c("replicate", "generation", "pop.tot", "off.tot", "a0.tot", "a1.tot", "a2.tot", "a3.tot")

out <- vector("list", length(demog_files))
  
for (i in seq_along(demog_files)) {{
    f <- demog_files[i]
    meta <- parse_meta_from_basename(f)
    
    header <- names(fread(f, nrows = 0))
    keep <- intersect(cols_to_read, header)
    dt <- fread(f, select = keep)
    
    # harmonize off.tot -> a0.tot if needed
    if ("off.tot" %in% names(dt) && !"a0.tot" %in% names(dt)) {
      setnames(dt, "off.tot", "a0.tot")
    }
    
    dt <- cbind(dt, meta)
    
    out[[i]] <- melt(
      dt,
      id.vars = c("replicate", "generation", "configuration", "ori_proportion", "layout_run", "k", "b"),
      measure.vars = patterns("\\.tot$"),
      variable.name = "stage",
      value.name = "N"
    )
  }
  
  demog_long <- rbindlist(out, use.names = TRUE, fill = TRUE)
  demog_long <- demog_long[replicate %between% c(1, max_internal_replicate) & generation <= gen_max]
  if (!is.na(p_focus)) demog_long <- demog_long[ori_proportion == as.integer(p_focus)]
}

stage_labs <- c(
  "pop.tot" = "Total population",
  "a0.tot"  = "Stage 0",
  "a1.tot"  = "Stage 1",
  "a2.tot"  = "Stage 2",
  "a3.tot"  = "Stage 3"
)
demog_long[, stage_label := stage_labs[stage]]
demog_long[, kb := paste0("k", k, " b", b)]

demog_mean <- demog_long[
  , .(meanN = mean(N, na.rm = TRUE)),
  by = .(generation, stage_label, ori_proportion, k, b)
]
demog_mean[, kb := paste0("k", k, " b", b)]

# ---- PLOT 1: cloud (all stages) ----
p_dem_cloud <- ggplot() +
  geom_line(
    data = demog_long,
    aes(generation, N, group = interaction(stage_label, replicate), color = stage_label),
    linewidth = 0.15, alpha = 0.4
  ) +
  geom_line(
    data = demog_mean,
    aes(generation, meanN, color = stage_label),
    linewidth = 0.7
  ) +
  facet_grid(ori_proportion ~ kb, scales = "free_y") +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Burn-in demography: replicate variability (faint) + mean (bold)",
    x = "Generation", y = "Abundance", color = "Stage"
  ) +
  theme_plot

save_plot(p_dem_cloud, paste0("run_demography_cloud_all_stages_", p_tag))

# ---- PLOT 2: cloud (adults only) ----
dem_ad_long <- demog_long[stage_label == "Stage 3"]
dem_ad_mean <- demog_mean[stage_label == "Stage 3"]

p_dem_cloud_adults <- ggplot() +
  geom_line(
    data = dem_ad_long,
    aes(generation, N, group = replicate),
    linewidth = 0.25, alpha = 0.35
  ) +
  geom_line(
    data = dem_ad_mean,
    aes(generation, meanN),
    linewidth = 0.9
  ) +
  facet_grid(ori_proportion ~ kb, scales = "free_y") +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Adult demography cloud (replicates) + mean (bold)",
    x = "Generation", y = "Adult abundance (Stage 3)"
  ) +
  theme_plot

save_plot(p_dem_cloud_adults, paste0("run_demography_cloud_adults_only_", p_tag))

# ---- PLOT 3: combined (median + IQR) ----
stage_levels <- c("Stage 0", "Stage 1", "Stage 2", "Stage 3")

stage_pal <- c(
  "Stage 0" = "#ff7f00",
  "Stage 1" = "#984ea3",
  "Stage 2" = "#4daf4a",
  "Stage 3" = "#377eb8"
)

lab_k <- function(x) paste0("K = ", x)
lab_b <- function(x) paste0("B = ", x)

GEN_MAX_PLOT <- 200
TARGET_LO <- 40
TARGET_HI <- 60
TARGET_MID <- 50

theme_demog2 <- theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey88", linewidth = 0.25),
    strip.background  = element_rect(fill = "grey95", colour = NA),
    strip.text        = element_text(face = "bold", color = "grey10"),
    legend.position   = "bottom",
    legend.title      = element_text(face = "bold"),
    legend.key.width  = unit(18, "pt"),
    plot.title        = element_text(face = "bold"),
    plot.subtitle     = element_text(color = "grey25", margin = margin(b = 6)),
    plot.margin       = margin(10, 12, 10, 12)
  )


# Prep dats

demog_long2 <- demog_long %>%
  filter(stage_label != "Total population") %>%
  mutate(stage_label = factor(stage_label, levels = stage_levels)) %>%
  filter(generation <= GEN_MAX_PLOT)


# Summaries: median + IQR across replicates (per k,b,stage,generation)

demog_iqr <- demog_long2 %>%
  group_by(k, b, stage_label, generation) %>%
  summarise(
    med = median(N, na.rm = TRUE),
    q25 = quantile(N, 0.25, na.rm = TRUE),
    q75 = quantile(N, 0.75, na.rm = TRUE),
    .groups = "drop"
  )


# Panel A: all stages (median line + IQR ribbon)

p_all <- ggplot(demog_iqr, aes(x = generation, color = stage_label)) +
  geom_ribbon(
    aes(ymin = q25, ymax = q75, fill = stage_label),
    alpha = 0.18, linewidth = 0,
    show.legend = FALSE
  ) +
  geom_line(aes(y = med), linewidth = 0.75) +
  facet_grid(
    b ~ k,
    scales = "free_y",
    labeller = labeller(k = lab_k, b = lab_b)
  ) +
  scale_color_manual(values = stage_pal, drop = FALSE) +
  scale_fill_manual(values = stage_pal, drop = FALSE, guide = "none") +
  scale_x_continuous(
    limits = c(0, GEN_MAX_PLOT),
    breaks = c(0, 50, 100, 150, 200),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Demography across parameter combinations",
    subtitle = "Line = median across replicates; ribbon = IQR (25–75%)",
    x = "Years",
    y = "Abundance",
    color = "Life stage"
  ) +
  guides(
    color = guide_legend(override.aes = list(linewidth = 2.2, alpha = 1))
  ) +
  theme_demog2

# Panel B: adults only (Stage 3): median + IQR + target band

adult_iqr <- demog_iqr %>% filter(stage_label == "Stage 3")
facet_keys <- adult_iqr %>% distinct(k, b)

p_adults <- ggplot(adult_iqr, aes(x = generation)) +
  geom_rect(
    data = facet_keys,
    aes(xmin = -Inf, xmax = Inf, ymin = TARGET_LO, ymax = TARGET_HI),
    inherit.aes = FALSE,
    fill = "red",
    alpha = 0.12,
    colour = NA
  ) +
  geom_hline(
    data = facet_keys,
    aes(yintercept = TARGET_MID),
    inherit.aes = FALSE,
    linewidth = 0.35,
    linetype = "dashed",
    color = "grey35"
  ) +
  geom_ribbon(aes(ymin = q25, ymax = q75), fill = stage_pal["Stage 3"], alpha = 0.4, linewidth = 0) +
  geom_line(aes(y = med), color = stage_pal["Stage 3"], linewidth = 0.9) +
  facet_grid(
    b ~ k,
    scales = "free_y",
    labeller = labeller(k = lab_k, b = lab_b)
  ) +
  scale_x_continuous(
    limits = c(0, GEN_MAX_PLOT),
    breaks = c(0, 50, 100, 150, 200),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    breaks = pretty_breaks(n = 4),
    labels = comma,
    expand = expansion(mult = c(0.02, 0.06))
  ) +
  labs(
    title = "Adults only (Stage 3): target ≈ 50",
    subtitle = "Line = median; ribbon = IQR; shaded red band = 40–60; dashed line = 50",
    x = "Years",
    y = "Adults"
  ) +
  theme_demog2 +
  theme(legend.position = "none")

# Combine

final_demog_plot <- p_all / p_adults + plot_layout(heights = c(2, 1.35))
save_plot(final_demog_plot, paste0("run_demography_median_iqr_", p_tag), w = 20, h = 13)

# ---- equilibrium summaries + heatmaps + boxplot ----
EQ_START <- 100
EQ_END   <- 500

ad_eq <- demog_long2[
  stage_label == "Stage 3" & generation >= EQ_START & generation <= EQ_END
]

ad_eq_rep <- ad_eq[
  , .(eq_mean_adults = mean(N, na.rm = TRUE)),
  by = .(k, b, replicate)
]

ad_eq_sum <- ad_eq_rep[
  , .(
    mean_adults = mean(eq_mean_adults, na.rm = TRUE),
    med_adults  = median(eq_mean_adults, na.rm = TRUE),
    q25        = quantile(eq_mean_adults, 0.25, na.rm = TRUE),
    q75        = quantile(eq_mean_adults, 0.75, na.rm = TRUE),
    hit_rate   = mean(eq_mean_adults >= TARGET_LO & eq_mean_adults <= TARGET_HI, na.rm = TRUE)
  ),
  by = .(k, b)
]
ad_eq_sum[, `:=`(k_f = factor(k), b_f = factor(b))]

p_heat_mean <- ggplot(ad_eq_sum, aes(k_f, b_f, fill = mean_adults)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label = sprintf("%.0f", mean_adults)), size = 3.6) +
  scale_fill_viridis_c(option = "C", end = 0.95) +
  labs(
    title = "Equilibrium adult abundance (Stage 3)",
    subtitle = sprintf("Mean across internal replicates; equilibrium window = %d–%d", EQ_START, EQ_END),
    x = "K (patch carrying capacity)", y = "b (competition coefficient)", fill = "Mean adults"
  ) +
  theme_plot +
  theme(panel.grid = element_blank())

p_heat_hit <- ggplot(ad_eq_sum, aes(k_f, b_f, fill = hit_rate)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label = percent(hit_rate, accuracy = 1)), size = 3.6) +
  scale_fill_viridis_c(option = "B", end = 0.95, limits = c(0, 1), labels = percent) +
  labs(
    title = "Adults in target band (40–60)",
    subtitle = "Hit rate = fraction of replicates whose equilibrium mean is within 40–60",
    x = "K", y = "b", fill = "Hit rate"
  ) +
  theme_plot +
  theme(panel.grid = element_blank())

heat_combo <- p_heat_mean / p_heat_hit
save_plot(heat_combo, paste0("run_equilibrium_heatmaps_", p_tag), w = 16, h = 12)

p_box <- ggplot(
  ad_eq_rep[, .(k_f = factor(k), b_f = factor(b), eq_mean_adults)],
  aes(k_f, eq_mean_adults)
) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = TARGET_LO, ymax = TARGET_HI),
            inherit.aes = FALSE, alpha = 0.12) +
  geom_hline(yintercept = TARGET_LO, linetype = "dashed", linewidth = 0.4) +
  geom_hline(yintercept = TARGET_HI, linetype = "dashed", linewidth = 0.4) +
  geom_boxplot(width = 0.7, outlier.alpha = 0.25) +
  facet_wrap(~ b_f, nrow = 1) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Equilibrium adults (Stage 3) across replicates",
    subtitle = sprintf("Equilibrium window = %d–%d", EQ_START, EQ_END),
    x = "K", y = "Equilibrium mean adults"
  ) +
  theme_plot +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())

save_plot(p_box, paste0("run_equilibrium_boxplots_", p_tag), w = 20, h = 6)
