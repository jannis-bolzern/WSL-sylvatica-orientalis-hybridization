#!/usr/bin/env Rscript

### the script: 
# reads one quanti file
# filters only adult stage
# computes mean P1 per patch
# returns summarized patch-level data
# ?


suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(terra)
  library(stringr)
  library(scales)
  library(patchwork)
})

source("scripts/00_control_panel.R")

INPUT_ROOT <- if (exists("IN") && is.list(IN) && !is.null(IN$files)) {
  IN$files
} else {
  file.path("input", "input_files")
}

RES_ROOT <- if (exists("OUT") && is.list(OUT) && !is.null(OUT$simulations)) {
  OUT$simulations
} else {
  file.path("output", "simulations")
}

QC_ROOT <- if (exists("OUT") && is.list(OUT) && !is.null(OUT$qc)) {
  OUT$qc
} else {
  file.path("output", "qc")
}

PLOT_DIR <- file.path(QC_ROOT, "spacial_outcomes")
dir.create(PLOT_DIR, showWarnings = FALSE, recursive = TRUE)

# Scenarios + proportions to show
CONFIGS <- c("dispersed", "one_cluster", "multi_cluster", "transects")
PCTS    <- c(10, 25, 40)   # % Orientalis introduced
RUN_ID  <- 1               # biological replicate r01
SHOW_REP <- 1              # Nemo replicate to visualize on maps
TREATMENT <- "neutral"

# Times (generations/years) to plot adult P1 maps
TIMES <- c(50, 100, 300, 600, 1000)

# Burn-in output TXT (auto-detect if NA)
BURNIN_TXT <- NA_character_

# Which stage is "adult" in your model?
ADULT_STAGE <- 3  # stage3

# Helpers
find_grid_shp <- function(input_root) {
  cand <- c(
    file.path(input_root, "grid", "Grid_4x4m_100x100m.shp"),
    file.path(input_root, "grid", "grid_4x4m_100x100m.shp"),
    file.path(input_root, "Grid_4x4m_100x100m.shp"),
    file.path(input_root, "grid_4x4m_100x100m.shp")
  )
  cand <- cand[file.exists(cand)]
  if (length(cand) > 0) return(cand[1])
  
  hits <- list.files(input_root, pattern = "\\.shp$", recursive = TRUE, full.names = TRUE)
  hits <- hits[grepl("4x4m_100x100m", hits)]
  if (length(hits) == 0) stop("Could not find grid shapefile under: ", input_root)
  hits[1]
}

make_coords_from_grid <- function(grid_shp) {
  g <- terra::vect(grid_shp)
  pid_col <- intersect(names(g), c("patchID", "patch.ID", "patch_ID", "patchId"))
  if (length(pid_col) == 0) stop("Grid shapefile has no patchID-like column. Columns: ", paste(names(g), collapse=", "))
  pid_col <- pid_col[1]
  
  att <- terra::values(g)
  patchID <- as.integer(att[[pid_col]])
  
  cent <- terra::centroids(g)
  xy <- as.data.frame(terra::crds(cent))
  setDT(xy); setnames(xy, c("x", "y"))
  
  coords <- data.table(patchID = patchID, x = xy$x, y = xy$y)
  
  pid_like <- grep("^patchID", names(coords), value = TRUE)
  if (!"patchID" %in% names(coords) && length(pid_like) > 0) {
    setnames(coords, pid_like[1], "patchID")
  }
  
  # matrix-like plotting indices from patchID (assumes row-wise IDs 1..N^2)
  n_patches <- nrow(coords)
  N <- as.integer(round(sqrt(n_patches)))
  coords[, row := ((patchID - 1) %/% N) + 1]
  coords[, col := ((patchID - 1) %%  N) + 1]
  coords[, y_plot := N - row + 1]
  coords[, x_plot := col]
  coords[]
}

# nearest-neighbour summary in map units (your grid looks like meters)
nearest_neighbor_summary <- function(coords_dt, present_patch_ids) {
  pts <- coords_dt[patchID %in% present_patch_ids, .(x, y)]
  n <- nrow(pts)
  
  if (n <= 1) {
    return(list(
      n = n,
      mean = NA_real_,
      q25 = NA_real_,
      median = NA_real_,
      q75 = NA_real_,
      iqr = NA_real_
    ))
  }
  
  d <- as.matrix(dist(pts))
  diag(d) <- Inf
  nn <- apply(d, 1, min)
  
  qs <- as.numeric(quantile(nn, probs = c(0.25, 0.5, 0.75), na.rm = TRUE))
  list(
    n = n,
    mean = mean(nn, na.rm = TRUE),
    q25 = qs[1],
    median = qs[2],
    q75 = qs[3],
    iqr = qs[3] - qs[1]
  )
}

# Burn-in TXT has wide columns like a3.fem.p1 ... a3.fem.p625
extract_stage_fem_patch_counts <- function(txt_path, stage = 3, gen_id, rep_id = 1) {
  stopifnot(file.exists(txt_path))
  
  hdr <- names(fread(txt_path, nrows = 0))
  hdr <- trimws(hdr)
  hdr2 <- gsub("^off\\.", "a0.", hdr)
  
  stage_pat <- paste0("^a", stage, "\\.fem\\.p\\d+$")
  stage_cols <- grep(stage_pat, hdr2, value = TRUE)
  if (length(stage_cols) == 0) stop("No columns matching ", stage_pat, " in: ", txt_path)
  
  dt <- fread(txt_path, showProgress = FALSE)
  setnames(dt, trimws(names(dt)))
  setnames(dt, gsub("^off\\.", "a0.", names(dt)))
  
  row_dt <- dt[replicate == rep_id & generation == gen_id]
  if (nrow(row_dt) != 1) stop("Could not find exactly one row for replicate=", rep_id, " generation=", gen_id, " in ", txt_path)
  
  cols <- grep(stage_pat, names(row_dt), value = TRUE)
  pnum <- as.integer(sub(paste0("^a", stage, "\\.fem\\.p"), "", cols))
  ord  <- order(pnum)
  vals <- as.numeric(unlist(row_dt[, ..cols])[ord])
  
  data.table(patchID = pnum[ord], N_adults = vals)
}

auto_find_burnin_txt <- function(res_root) {
  hits <- list.files(res_root, pattern = "burnin.*\\.txt$", recursive = TRUE, full.names = TRUE)
  hits <- hits[!grepl("_bygen\\.txt$", hits)]
  if (length(hits) == 0) stop("No burnin*.txt found under: ", res_root)
  hits[which.max(file.info(hits)$mtime)]
}

burnin_end_generation <- function(txt_path) {
  rg <- fread(txt_path, select = c(1,2), showProgress = FALSE)
  setnames(rg, trimws(names(rg)))
  max(rg$generation, na.rm = TRUE)
}

find_config_file <- function(input_root, cfg, pct, run_id) {
  sim <- sprintf("%s_p%02d_r%02d", cfg, pct, run_id)
  cand <- c(
    file.path(input_root, "configs_txt", paste0(sim, ".txt")),
    file.path(input_root, "cfg", paste0(sim, ".txt")),
    file.path(input_root, "configs", paste0(sim, ".txt"))
  )
  cand <- cand[file.exists(cand)]
  if (length(cand) > 0) return(cand[1])
  
  hits <- list.files(input_root, pattern = paste0("^", sim, "\\.txt$"), recursive = TRUE, full.names = TRUE)
  if (length(hits) == 0) stop("Cannot find config file for ", sim, " under: ", input_root)
  hits[1]
}

get_orientalis_patch_ids <- function(cfg_file) {
  dt <- fread(cfg_file, showProgress = FALSE)
  setnames(dt, trimws(names(dt)))
  if (!all(c("patchID","patch_value") %in% names(dt))) {
    stop("Config file missing required columns patchID/patch_value: ", cfg_file)
  }
  as.integer(dt[patch_value == "O", patchID])
}

find_quanti_file <- function(res_root, treatment, cfg, pct, run_id, gen, rep = 1) {
  sim     <- sprintf("%s_p%02d_r%02d", cfg, pct, run_id)
  gen_tag <- sprintf("%04d", gen)
  rep_tag <- sprintf("%02d", rep)
  
  # exact expected location
  cand <- file.path(
    res_root, "run", treatment, cfg,
    sprintf("p%02d", pct),
    sprintf("r%02d", run_id),
    "run", "quanti",
    sprintf("%s_*_%s_%s.quanti", sim, gen_tag, rep_tag)
  )
  
  hits <- Sys.glob(cand)
  if (length(hits) > 0) return(hits[1])
  
  # fallback: search within this treatment only (still fast enough)
  rx <- paste0("^", sim, ".*_", gen_tag, "_", rep_tag, "\\.quanti$")
  hits2 <- list.files(
    file.path(res_root, "run", treatment),
    pattern = rx, recursive = TRUE, full.names = TRUE
  )
  
  if (length(hits2) == 0) return(NA_character_)
  hits2[1]
}


read_quanti_adult_patch_mean <- function(path, adult_stage = 3) {
  dt <- fread(path, showProgress = FALSE)
  setnames(dt, trimws(names(dt)))
  
  p1_col <- intersect(names(dt), c("P1", "p1"))
  if (length(p1_col) == 0) stop("No P1 column in quanti: ", path)
  p1_col <- p1_col[1]
  
  stage_col <- intersect(names(dt), c("stage", "Stage", "age_class", "ageClass", "age", "Age"))
  if (length(stage_col) == 0) stop("No stage/age column in quanti: ", path)
  stage_col <- stage_col[1]
  
  patch_col <- intersect(names(dt), c("patch", "patchID", "patch_id", "deme", "pop", "Patch"))
  if (length(patch_col) == 0) stop("No patch column in quanti: ", path)
  patch_col <- patch_col[1]
  
  dt <- dt[get(stage_col) == adult_stage]
  if (nrow(dt) == 0) return(data.table(patchID = integer(0), meanP1 = numeric(0)))
  
  dt[, .(meanP1 = mean(get(p1_col), na.rm = TRUE)), by = .(patchID = as.integer(get(patch_col)))]
}

# Main workflow
# 0) Grid + coords
grid_shp <- find_grid_shp(INPUT_ROOT)
coords <- make_coords_from_grid(grid_shp)
n_patches <- nrow(coords)
N_side <- max(coords$x_plot)

message("Grid: ", grid_shp, " | patches: ", n_patches)

# 1) Burn-in adults (presence) + NN stats (used in combined starting-conditions plot subtitle)
if (is.na(BURNIN_TXT)) BURNIN_TXT <- auto_find_burnin_txt(RES_ROOT)
message("Using burn-in txt: ", BURNIN_TXT)

BURNIN_END_GEN <- burnin_end_generation(BURNIN_TXT)
burn_adults <- extract_stage_fem_patch_counts(
  BURNIN_TXT,
  stage = ADULT_STAGE,
  gen_id = BURNIN_END_GEN,
  rep_id = SHOW_REP
)

burn_present <- burn_adults[N_adults > 0, patchID]
nn_burn <- nearest_neighbor_summary(coords, burn_present)

# baseline adult_present table for overlays
baseline_adults_dt <- merge(coords[, .(patchID)], burn_adults, by = "patchID", all.x = TRUE)
baseline_adults_dt[is.na(N_adults), N_adults := 0]
baseline_adults_dt[, adult_present := N_adults > 0]

# 2) Starting conditions plot INCLUDING burn-in as first column
CONFIGS_LAYOUT <- c("burnin", CONFIGS)

layout_dt <- rbindlist(list(
  # burn-in column replicated across pct (no planting)
  rbindlist(lapply(PCTS, function(pct) {
    df <- merge(copy(coords), baseline_adults_dt[, .(patchID, adult_present)], by = "patchID", all.x = TRUE)
    df[is.na(adult_present), adult_present := FALSE]
    df[, planted := FALSE]
    df[, state := fifelse(adult_present, "Sylvatica adult present", "No adult")]
    df[, configuration := "burnin"]
    df[, pct := pct]
    df
  })),
  
  # scenario columns (planting differs by config + pct)
  rbindlist(lapply(CONFIGS, function(cfg) {
    rbindlist(lapply(PCTS, function(pct) {
      cfg_file <- find_config_file(INPUT_ROOT, cfg, pct, RUN_ID)
      O_ids <- get_orientalis_patch_ids(cfg_file)
      
      df <- merge(copy(coords), baseline_adults_dt[, .(patchID, adult_present)], by = "patchID", all.x = TRUE)
      df[is.na(adult_present), adult_present := FALSE]
      df[, planted := patchID %in% O_ids]
      df[, state := fifelse(planted, "Orientalis planted (cleared)",
                            fifelse(adult_present, "Sylvatica adult present", "No adult"))]
      df[, configuration := cfg]
      df[, pct := pct]
      df
    }))
  }))
), fill = TRUE)

layout_dt[, configuration := factor(configuration, levels = CONFIGS_LAYOUT)]
layout_dt[, pct := factor(pct, levels = PCTS)]

start_sub <- if (nn_burn$n <= 1 || is.na(nn_burn$median)) {
  sprintf("Burn-in gen %d (rep %d): adult patches=%d | NN: NA",
          BURNIN_END_GEN, SHOW_REP, nn_burn$n)
} else {
  sprintf("Burn-in gen %d (rep %d): adult patches=%d | median=%.1f m | IQR=%.1f–%.1f m",
          BURNIN_END_GEN, SHOW_REP, nn_burn$n,
          nn_burn$median, nn_burn$q25, nn_burn$q75)
}

p_layout <- ggplot(layout_dt, aes(x = x_plot, y = y_plot)) +
  geom_tile(aes(fill = state), color = "grey85", linewidth = 0.15) +
  coord_equal() +
  facet_grid(
    pct ~ configuration,
    labeller = labeller(configuration = c(burnin = "burn-in"))
  ) +
  scale_fill_manual(
    values = c("No adult" = "grey75",
               "Sylvatica adult present" = "#2b6cb0",
               "Orientalis planted (cleared)" = "#c53030")
  ) +
  labs(
    title = "Starting conditions: burn-in Sylvatica adults + planted Orientalis patches (red)",
    subtitle = start_sub,
    x = "", y = "", fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

ggsave(
  file.path(PLOT_DIR, "adult_initial_conditions_with_burnin_column.png"),
  p_layout,
  width = 16.5, height = 10.5, dpi = 250
)

# 3) Adult hybrid index (mean P1) maps + keep NN stats for overview tables
nn_stats_list <- list()

for (gen in TIMES) {
  
  # read quanti + compute mean adult P1 per patch for each facet
  p1_dt_all <- rbindlist(lapply(CONFIGS, function(cfg) {
    rbindlist(lapply(PCTS, function(pct) {
      
      qfile <- find_quanti_file(RES_ROOT, TREATMENT, cfg, pct, RUN_ID, gen, rep = SHOW_REP)
      if (is.na(qfile)) {
        warning("Missing quanti file for ", cfg, " p", pct, " gen ", gen, " rep ", SHOW_REP)
        return(NULL)
      }
      
      p1 <- read_quanti_adult_patch_mean(qfile, adult_stage = ADULT_STAGE)
      p1[, configuration := cfg]
      p1[, pct := pct]
      p1
      
    }), fill = TRUE)
  }), fill = TRUE)
  
  if (is.null(p1_dt_all) || nrow(p1_dt_all) == 0) {
    warning("No quanti data found for gen ", gen, " (skipping plot).")
    next
  }
  
  # complete patch × (configuration,pct) grid so NA meanP1 stays inside facets
  base_grid <- CJ(
    patchID       = coords$patchID,
    configuration = CONFIGS,
    pct           = PCTS,
    unique = TRUE
  )
  base_grid <- merge(base_grid, coords, by = "patchID", all.x = TRUE)
  
  map_dt <- merge(
    base_grid,
    p1_dt_all,
    by = c("patchID", "configuration", "pct"),
    all.x = TRUE
  )
  
  map_dt[, configuration := factor(configuration, levels = CONFIGS)]
  map_dt[, pct := factor(pct, levels = PCTS)]
  
  # NN stats per facet (mean + median + IQR)
  stats_dt <- map_dt[!is.na(meanP1),
                     .(adult_patches = uniqueN(patchID)),
                     by = .(configuration, pct)]
  
  # ensure all facets exist even if 0 adults
  stats_dt <- merge(
    CJ(configuration = factor(CONFIGS, levels = CONFIGS),
       pct = factor(PCTS, levels = PCTS), unique = TRUE),
    stats_dt,
    by = c("configuration", "pct"),
    all.x = TRUE
  )
  stats_dt[is.na(adult_patches), adult_patches := 0L]
  
  stats_dt[, `:=`(nn_mean = NA_real_, nn_median = NA_real_, nn_q25 = NA_real_, nn_q75 = NA_real_)]
  for (i in seq_len(nrow(stats_dt))) {
    cfg_i <- stats_dt$configuration[i]
    pct_i <- stats_dt$pct[i]
    
    present <- map_dt[configuration == cfg_i & pct == pct_i & !is.na(meanP1), patchID]
    s <- nearest_neighbor_summary(coords, present)
    
    stats_dt$nn_mean[i]   <- s$mean
    stats_dt$nn_median[i] <- s$median
    stats_dt$nn_q25[i]    <- s$q25
    stats_dt$nn_q75[i]    <- s$q75
  }
  
  stats_dt[, label := ifelse(
    adult_patches <= 1 | is.na(nn_median),
    sprintf("adult patches=%d\nNN: NA", adult_patches),
    sprintf("adult patches=%d\nNN median=%.1f m\nIQR=%.1f–%.1f m",
            adult_patches, nn_median, nn_q25, nn_q75)
  )]
  
  # store stats for overview tables
  nn_stats_list[[as.character(gen)]] <- copy(stats_dt)[, gen := gen]
  
  # footer band for labels
  FOOTER_ROWS <- 5
  y_min <- 1 - FOOTER_ROWS
  stats_dt[, `:=`(x_annot = 1.2, y_annot = y_min + 0.3)]
  
  p_p1 <- ggplot(map_dt, aes(x = x_plot, y = y_plot)) +
    geom_tile(aes(fill = meanP1), color = "grey85", linewidth = 0.15) +
    
    geom_segment(
      data = stats_dt,
      x = 0.5, xend = N_side + 0.5, y = 0.5, yend = 0.5,
      inherit.aes = FALSE,
      color = "grey85",
      linewidth = 0.3
    ) +
    geom_text(
      data = stats_dt,
      aes(x = x_annot, y = y_annot, label = label),
      inherit.aes = FALSE,
      hjust = 0, vjust = 0,
      size = 3.1,
      lineheight = 0.95
    ) +
    
    facet_grid(pct ~ configuration) +
    
    scale_fill_gradient2(
      low = "#2b6cb0", mid = "white", high = "#c53030",
      midpoint = 0, limits = c(-1, 1), oob = squish,
      na.value = "grey75"
    ) +
    
    scale_x_continuous(limits = c(1, N_side), expand = c(0, 0)) +
    scale_y_continuous(limits = c(y_min, N_side), expand = c(0, 0)) +
    coord_equal(clip = "off") +
    
    labs(
      title = sprintf("Adult hybrid index (mean P1) at year %d", gen),
      subtitle = sprintf("Nemo replicate shown: %02d (maps use adult stage %d).", SHOW_REP, ADULT_STAGE),
      x = "", y = "", fill = "Mean adult P1"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(face = "bold")
    )
  
  ggsave(
    file.path(PLOT_DIR, sprintf("adult_hybrid_index_P1_gen%03d.png", gen)),
    p_p1,
    width = 13.2, height = 11.5, dpi = 250
  )
}

# 4) Overview table
if (length(nn_stats_list) > 0) {
  nn_all <- rbindlist(nn_stats_list, fill = TRUE)
  
  # clean types for tables
  nn_all[, configuration := as.character(configuration)]
  nn_all[, pct := as.integer(as.character(pct))]
  
  # a) Long table (one row per scenario × year)
  nn_long <- nn_all[, .(
    gen,
    configuration,
    pct,
    adult_patches,
    nn_mean,
    nn_median,
    nn_q25,
    nn_q75
  )][order(gen, pct, configuration)]
  
  # b) Wide table of mean NN (meters): scenario rows, year columns
  nn_long[, scenario := sprintf("%s | %d%%", configuration, pct)]
  nn_wide_mean <- dcast(nn_long, scenario ~ gen, value.var = "nn_mean")
  
  # identify the year columns robustly
  year_cols <- setdiff(names(nn_wide_mean), "scenario")
  
  # Total column = mean across years per scenario
  nn_wide_mean[, Total := rowMeans(.SD, na.rm = TRUE), .SDcols = year_cols]
  
  # Total row = mean across scenarios per year (and for Total column too)
  total_row <- nn_wide_mean[, c(
    list(scenario = "TOTAL"),
    lapply(.SD, function(x) mean(x, na.rm = TRUE))
  ), .SDcols = c(year_cols, "Total")]
  
  # bind
  nn_wide_with_totals <- rbind(nn_wide_mean, total_row, fill = TRUE)
  
  # optional: lock column order
  setcolorder(nn_wide_with_totals, c("scenario", year_cols, "Total"))
  
  # print and/or save
  print(nn_wide_with_totals)
  fwrite(nn_wide_with_totals, file.path(PLOT_DIR, "nn_mean_per_scenario_wide_with_totals.csv"))
  
  message("\nSaved NN tables to: ", PLOT_DIR)
} else {
  warning("No NN stats were collected (no quanti plots produced). Skipping NN overview tables.")
}

message("Done. Plots saved to: ", PLOT_DIR)
