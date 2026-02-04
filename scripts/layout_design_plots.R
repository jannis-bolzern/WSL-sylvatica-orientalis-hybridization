#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(terra)
  library(stringr)
  library(scales)
})

# ============================================================
# USER SETTINGS
# ============================================================

PROJECT_ROOT <- "."               # run from project root (recommended)
INPUT_ROOT   <- file.path(PROJECT_ROOT, "input_files")
RES_ROOT     <- file.path(PROJECT_ROOT, "results")

# Where to save plots:
PLOT_DIR <- file.path(PROJECT_ROOT, "plots_forester_briefing")
dir.create(PLOT_DIR, showWarnings = FALSE, recursive = TRUE)

# Scenarios + proportions to show
CONFIGS <- c("dispersed", "one_cluster", "multi_cluster", "transects")
PCTS    <- c(10, 25, 40)          # % Orientalis introduced
RUN_ID  <- 1                      # biological replicate r01
SHOW_REP <- 1                     # Nemo replicate to visualize on maps

# Times (generations/years) to plot adult P1 maps
TIMES <- c(100, 200, 300, 400, 500)

# Burn-in: use the burn-in output TXT (end of burn-in)
# If you know the exact file, set it here; otherwise we auto-detect.
BURNIN_TXT <- NA_character_

# Which stage is "adult" in your model?
ADULT_STAGE <- 3  # stage3

# ============================================================
# HELPERS: GRID + COORDS
# ============================================================

find_grid_shp <- function(input_root) {
  # Try common locations
  cand <- c(
    file.path(input_root, "grid", "Grid_4x4m_100x100m.shp"),
    file.path(input_root, "grid", "grid_4x4m_100x100m.shp"),
    file.path(input_root, "Grid_4x4m_100x100m.shp"),
    file.path(input_root, "grid_4x4m_100x100m.shp")
  )
  cand <- cand[file.exists(cand)]
  if (length(cand) > 0) return(cand[1])
  
  # fallback: search
  hits <- list.files(input_root, pattern = "\\.shp$", recursive = TRUE, full.names = TRUE)
  hits <- hits[grepl("4x4m_100x100m", hits)]
  if (length(hits) == 0) stop("Could not find grid shapefile under: ", input_root)
  hits[1]
}

make_coords_from_grid <- function(grid_shp) {
  g <- terra::vect(grid_shp)
  # patch id column name might differ
  pid_col <- intersect(names(g), c("patchID", "patch.ID", "patch_ID", "patchId"))
  if (length(pid_col) == 0) stop("Grid shapefile has no patchID-like column. Columns: ", paste(names(g), collapse=", "))
  pid_col <- pid_col[1]
  patchID <- g[[pid_col]]
  
  cent <- terra::centroids(g)
  xy <- as.data.frame(terra::crds(cent))
  setDT(xy)
  setnames(xy, c("x", "y"))
  
  coords <- data.table(patchID = patchID, x = xy$x, y = xy$y)
  
  # matrix-like plotting indices from patchID (assumes row-wise IDs 1..N^2)
  n_patches <- nrow(coords)
  N <- as.integer(round(sqrt(n_patches)))
  if (N*N != n_patches) warning("patch count is not a square; plotting grid indices may be off.")
  coords[, row := ((patchID - 1) %/% N) + 1]
  coords[, col := ((patchID - 1) %%  N) + 1]
  coords[, y_plot := N - row + 1]
  coords[, x_plot := col]
  coords[]
}

nearest_neighbor_dist <- function(coords_dt, present_patch_ids) {
  pts <- coords_dt[patchID %in% present_patch_ids, .(x, y)]
  if (nrow(pts) <= 1) return(NA_real_)
  d <- as.matrix(dist(pts))
  diag(d) <- Inf
  apply(d, 1, min)
}

# ============================================================
# HELPERS: READ BURNIN TXT -> ADULT COUNTS PER PATCH
# ============================================================

# Burn-in TXT has wide columns like a3.fem.p1 ... a3.fem.p625
extract_stage_fem_patch_counts <- function(txt_path, stage = 3, generation, replicate = 1) {
  stopifnot(file.exists(txt_path))
  hdr <- names(fread(txt_path, nrows = 0))
  
  # some outputs may use off.* instead of a0.*
  hdr2 <- gsub("^off\\.", "a0.", hdr)
  
  stage_pat <- paste0("^a", stage, "\\.fem\\.p\\d+$")
  stage_cols <- grep(stage_pat, hdr2, value = TRUE)
  if (length(stage_cols) == 0) {
    stop("No columns matching ", stage_pat, " in: ", txt_path)
  }
  
  cols_needed <- c("replicate", "generation", stage_cols)
  dt <- fread(txt_path, select = cols_needed, showProgress = FALSE)
  setnames(dt, gsub("^off\\.", "a0.", names(dt)))
  
  row <- dt[replicate == replicate & generation == generation]
  if (nrow(row) != 1) stop("Could not find exactly one row for replicate=", replicate, " generation=", generation)
  
  # parse patch numbers, ensure order
  cols <- grep(stage_pat, names(row), value = TRUE)
  pnum <- as.integer(sub(paste0("^a", stage, "\\.fem\\.p"), "", cols))
  ord <- order(pnum)
  vals <- as.numeric(unlist(row[, ..cols])[ord])
  
  data.table(patchID = pnum[ord], N_adults = vals)
}

# Find burnin txt if not provided
auto_find_burnin_txt <- function(res_root) {
  hits <- list.files(res_root, pattern = "burnin.*\\.txt$", recursive = TRUE, full.names = TRUE)
  # exclude bygen if you like
  hits <- hits[!grepl("_bygen\\.txt$", hits)]
  if (length(hits) == 0) stop("No burnin*.txt found under: ", res_root)
  # choose most recent
  hits[which.max(file.info(hits)$mtime)]
}

# ============================================================
# HELPERS: PLANTING LAYOUTS (where O is planted)
# We use the scenario-specific patch_init_stage_size_* file:
# stage1 > 0 indicates planted Orientalis seedlings in that patch.
# ============================================================

find_scenario_stage_file <- function(input_root, cfg, pct, run_id) {
  # Your files are typically like: patch_init_stage_size_<sim>.txt
  # sim looks like: dispersed_p10_r01
  sim <- sprintf("%s_p%02d_r%02d", cfg, pct, run_id)
  fn1 <- file.path(input_root, "patch_init_stage_size", paste0("patch_init_stage_size_", sim, ".txt"))
  fn2 <- file.path(input_root, "patch_init_stage_size", paste0(sim, ".txt"))
  fn3 <- file.path(input_root, "patch_init_stage_size", paste0("patch_init_stage_size_", sim, ".txt"))
  cand <- c(fn1, fn2, fn3)
  cand <- cand[file.exists(cand)]
  if (length(cand) == 0) stop("Cannot find scenario stage file for ", sim, " under input_files.")
  cand[1]
}

read_nemo_stage_matrix <- function(path, n_patches) {
  # Reads your Nemo matrix format: { {a,b,c,d} {..} }
  # Very simple parser: extract numbers by line.
  txt <- readLines(path, warn = FALSE)
  txt <- txt[grepl("\\{", txt)]
  # drop opening/closing braces lines
  txt <- txt[grepl("\\{.*\\}", txt)]
  rows <- lapply(txt, function(line) {
    nums <- unlist(strsplit(gsub("[\\{\\}]", "", line), ","))
    nums <- nums[nzchar(nums)]
    as.numeric(nums)
  })
  mat <- do.call(rbind, rows)
  if (nrow(mat) != n_patches) {
    stop("Stage matrix row count mismatch in ", path, ": got ", nrow(mat), " expected ", n_patches)
  }
  mat
}

# ============================================================
# HELPERS: QUANTI FILES -> mean adult P1 per patch (one replicate)
# ============================================================

parse_quanti_meta <- function(path) {
  nm <- basename(path)
  
  # expected: dispersed_p10_r01_k30_b0.03_100_01.quanti
  m <- str_match(nm, "^(.*)_p(\\d+)_r(\\d+)_k(\\d+)_b([0-9.]+)_([0-9]{3})_([0-9]+)\\.quanti$")
  if (any(is.na(m))) return(NULL)
  
  data.table(
    configuration = m[,2],
    pct = as.integer(m[,3]),
    run_id = as.integer(m[,4]),
    k = as.integer(m[,5]),
    b = as.numeric(m[,6]),
    gen = as.integer(m[,7]),
    rep = as.integer(m[,8]),
    path = path
  )
}

read_quanti_adult_patch_mean <- function(path, adult_stage = 3) {
  dt <- fread(path, showProgress = FALSE)
  
  # detect columns
  p1_col <- intersect(names(dt), c("P1", "p1"))
  if (length(p1_col) == 0) stop("No P1 column in quanti: ", path)
  p1_col <- p1_col[1]
  
  stage_col <- intersect(names(dt), c("stage", "Stage", "age_class", "ageClass", "age", "Age"))
  if (length(stage_col) == 0) stop("No stage/age column in quanti: ", path)
  stage_col <- stage_col[1]
  
  patch_col <- intersect(names(dt), c("patch", "patchID", "patch_id", "deme", "pop", "Patch"))
  if (length(patch_col) == 0) stop("No patch column in quanti: ", path)
  patch_col <- patch_col[1]
  
  # subset adults
  dt <- dt[get(stage_col) == adult_stage]
  if (nrow(dt) == 0) {
    return(data.table(patchID = integer(0), meanP1 = numeric(0)))
  }
  
  out <- dt[, .(meanP1 = mean(get(p1_col), na.rm = TRUE)), by = .(patchID = as.integer(get(patch_col)))]
  out[]
}

# ============================================================
# PLOTTING
# ============================================================

plot_adults_map <- function(coords, adults_dt, title) {
  df <- merge(coords, adults_dt, by = "patchID", all.x = TRUE)
  df[is.na(N_adults), N_adults := 0]
  ggplot(df, aes(x = x_plot, y = y_plot, fill = N_adults)) +
    geom_tile(color = "grey85", linewidth = 0.1) +
    coord_equal() +
    scale_fill_viridis_c(option = "C", direction = -1) +
    labs(title = title, x = "", y = "", fill = "Adults\n(stage3)") +
    theme_minimal(base_size = 12) +
    theme(panel.grid = element_blank())
}

plot_layout_O <- function(coords, O_patch_ids, title) {
  df <- copy(coords)
  df[, planted := patchID %in% O_patch_ids]
  ggplot(df, aes(x = x_plot, y = y_plot, fill = planted)) +
    geom_tile(color = "grey85", linewidth = 0.1) +
    coord_equal() +
    scale_fill_manual(values = c("FALSE" = "white", "TRUE" = "black")) +
    labs(title = title, x = "", y = "", fill = "Planted\nOrientalis") +
    theme_minimal(base_size = 12) +
    theme(panel.grid = element_blank())
}

plot_adult_P1_map <- function(coords, p1_dt, title) {
  df <- merge(coords, p1_dt, by = "patchID", all.x = TRUE)
  ggplot(df, aes(x = x_plot, y = y_plot, fill = meanP1)) +
    geom_tile(color = "grey85", linewidth = 0.1) +
    coord_equal() +
    scale_fill_gradient2(
      low = "#2b6cb0", mid = "white", high = "#c53030",
      midpoint = 0,
      na.value = "grey95"
    ) +
    labs(title = title, x = "", y = "", fill = "Mean adult P1") +
    theme_minimal(base_size = 12) +
    theme(panel.grid = element_blank())
}

# ============================================================
# MAIN
# ============================================================

grid_shp <- find_grid_shp(INPUT_ROOT)
coords <- make_coords_from_grid(grid_shp)
n_patches <- nrow(coords)

# ----------------------------
# 1) BURNIN adults map + NN distances
# ----------------------------
if (is.na(BURNIN_TXT)) BURNIN_TXT <- auto_find_burnin_txt(RES_ROOT)

message("Using burn-in txt: ", BURNIN_TXT)

# detect final generation available (take max in file)
burn_hdr <- fread(BURNIN_TXT, select = c("generation"), showProgress = FALSE)
BURNIN_END_GEN <- max(burn_hdr$generation, na.rm = TRUE)
rm(burn_hdr)

burn_adults <- extract_stage_fem_patch_counts(
  BURNIN_TXT,
  stage = ADULT_STAGE,
  generation = BURNIN_END_GEN,
  replicate = SHOW_REP
)

burn_present <- burn_adults[N_adults > 0, patchID]
burn_nn <- nearest_neighbor_dist(coords, burn_present)

# Save burn-in plots
p1 <- plot_adults_map(coords, burn_adults, sprintf("Burn-in baseline: adults (stage3) at gen %d (rep %d)", BURNIN_END_GEN, SHOW_REP))
ggsave(file.path(PLOT_DIR, "burnin_adults_map.png"), p1, width = 7, height = 6.5, dpi = 220)

nn_df <- data.table(nn_dist = burn_nn)
p2 <- ggplot(nn_df[!is.na(nn_dist)], aes(x = nn_dist)) +
  geom_histogram(bins = 25, color = "white") +
  labs(
    title = sprintf("Burn-in baseline: nearest-neighbor distance of adult patches (gen %d, rep %d)", BURNIN_END_GEN, SHOW_REP),
    x = "Distance (m)", y = "Count"
  ) +
  theme_minimal(base_size = 12)
ggsave(file.path(PLOT_DIR, "burnin_adults_nndist.png"), p2, width = 7, height = 4.5, dpi = 220)

burn_nn_summary <- nn_df[!is.na(nn_dist), .(
  n_adult_patches = .N,
  mean_m = mean(nn_dist),
  median_m = median(nn_dist),
  sd_m = sd(nn_dist),
  q05_m = quantile(nn_dist, 0.05),
  q95_m = quantile(nn_dist, 0.95)
)]
fwrite(burn_nn_summary, file.path(PLOT_DIR, "burnin_adults_nndist_summary.csv"))
message("Burn-in NN summary:\n", capture.output(print(burn_nn_summary)))

# ----------------------------
# 2) Planting layouts (where Orientalis seedlings are planted)
# ----------------------------
layout_panels <- list()
layout_key <- list()

for (cfg in CONFIGS) {
  for (pct in PCTS) {
    
    stage_file <- find_scenario_stage_file(INPUT_ROOT, cfg, pct, RUN_ID)
    mat <- read_nemo_stage_matrix(stage_file, n_patches = n_patches)
    
    # scheme is stage0..stage3 columns; planting uses stage1 seedlings
    planted_ids <- which(mat[,2] > 0)  # stage1 > 0
    
    title <- sprintf("%s | %d%% O", cfg, pct)
    layout_panels[[paste(cfg, pct, sep = "_")]] <- plot_layout_O(coords, planted_ids, title)
    layout_key[[paste(cfg, pct, sep = "_")]] <- data.table(configuration = cfg, pct = pct, planted_n = length(planted_ids))
  }
}

# Save one big faceted layout plot via patchwork if available; otherwise save individually
layout_info <- rbindlist(layout_key)
fwrite(layout_info, file.path(PLOT_DIR, "layout_planted_patch_counts.csv"))

if (requireNamespace("patchwork", quietly = TRUE)) {
  library(patchwork)
  # order panels config-major then pct
  ordered_names <- as.vector(outer(CONFIGS, PCTS, paste, sep = "_"))
  ordered_names <- ordered_names[ordered_names %in% names(layout_panels)]
  p_layout <- wrap_plots(layout_panels[ordered_names], ncol = length(PCTS)) +
    plot_annotation(title = sprintf("Planting designs (seedling patches) | r%02d", RUN_ID))
  ggsave(file.path(PLOT_DIR, "planting_layouts_4x3.png"), p_layout, width = 12, height = 10, dpi = 220)
} else {
  message("Package 'patchwork' not found; saving planting layouts individually.")
  for (nm in names(layout_panels)) {
    ggsave(file.path(PLOT_DIR, paste0("layout_", nm, ".png")), layout_panels[[nm]], width = 6.5, height = 6.2, dpi = 220)
  }
}

# ----------------------------
# 3) Adult P1 maps over time (one Nemo replicate)
# ----------------------------

# Find all quanti files for r01 only (biological run)
all_quanti <- list.files(RES_ROOT, pattern = "\\.quanti$", recursive = TRUE, full.names = TRUE)

meta_list <- lapply(all_quanti, parse_quanti_meta)
meta <- rbindlist(meta_list, fill = TRUE)
meta <- meta[!is.na(configuration)]

# Filter to our 4 configs, 3 pcts, run_id==RUN_ID, rep==SHOW_REP, gen in TIMES
meta <- meta[
  configuration %in% CONFIGS &
    pct %in% PCTS &
    run_id == RUN_ID &
    rep == SHOW_REP &
    gen %in% TIMES
]

if (nrow(meta) == 0) {
  stop("No quanti files found matching your structure under results/. Check paths and naming.")
}

# Build maps per time
for (tt in TIMES) {
  sub <- meta[gen == tt]
  panels <- list()
  
  for (cfg in CONFIGS) {
    for (pct in PCTS) {
      row <- sub[configuration == cfg & pct == pct]
      if (nrow(row) == 0) next
      
      # If multiple found (e.g. duplicates), take the first
      qfile <- row$path[1]
      p1_dt <- read_quanti_adult_patch_mean(qfile, adult_stage = ADULT_STAGE)
      
      title <- sprintf("%s | %d%% O", cfg, pct)
      panels[[paste(cfg, pct, sep = "_")]] <- plot_adult_P1_map(coords, p1_dt, title)
    }
  }
  
  if (requireNamespace("patchwork", quietly = TRUE)) {
    library(patchwork)
    ordered_names <- as.vector(outer(CONFIGS, PCTS, paste, sep = "_"))
    ordered_names <- ordered_names[ordered_names %in% names(panels)]
    
    p_all <- wrap_plots(panels[ordered_names], ncol = length(PCTS)) +
      plot_annotation(title = sprintf("Mean adult P1 per patch | gen %d | Nemo rep %02d | r%02d", tt, SHOW_REP, RUN_ID))
    
    ggsave(file.path(PLOT_DIR, sprintf("adult_P1_maps_gen%03d.png", tt)),
           p_all, width = 12, height = 10, dpi = 220)
  } else {
    for (nm in names(panels)) {
      ggsave(file.path(PLOT_DIR, sprintf("adultP1_gen%03d_%s.png", tt, nm)),
             panels[[nm]], width = 6.5, height = 6.2, dpi = 220)
    }
  }
}

# ----------------------------
# 4) Nearest-neighbor distances at final time (gen 500) for adults
# ----------------------------
final_gen <- max(TIMES)

nn_rows <- list()

for (cfg in CONFIGS) {
  for (pct in PCTS) {
    
    row <- meta[gen == final_gen & configuration == cfg & pct == pct]
    if (nrow(row) == 0) next
    qfile <- row$path[1]
    
    # mean adult P1 per patch, but for NN we just need presence of adults in patch
    qdt <- fread(qfile, showProgress = FALSE)
    stage_col <- intersect(names(qdt), c("stage","Stage","age_class","ageClass","age","Age"))[1]
    patch_col <- intersect(names(qdt), c("patch","patchID","patch_id","deme","pop","Patch"))[1]
    if (is.na(stage_col) || is.na(patch_col)) next
    
    adults <- qdt[get(stage_col) == ADULT_STAGE]
    present <- unique(as.integer(adults[[patch_col]]))
    nn <- nearest_neighbor_dist(coords, present)
    
    nn_rows[[paste(cfg, pct, sep="_")]] <- data.table(
      configuration = cfg,
      pct = pct,
      gen = final_gen,
      rep = SHOW_REP,
      n_adult_patches = length(present),
      mean_nn_m = if (all(is.na(nn))) NA_real_ else mean(nn, na.rm = TRUE),
      median_nn_m = if (all(is.na(nn))) NA_real_ else median(nn, na.rm = TRUE),
      sd_nn_m = if (all(is.na(nn))) NA_real_ else sd(nn, na.rm = TRUE)
    )
  }
}

nn_final <- rbindlist(nn_rows, fill = TRUE)
fwrite(nn_final, file.path(PLOT_DIR, sprintf("adult_nndist_summary_gen%03d.csv", final_gen)))

message("Done.")
message("Plots saved to: ", PLOT_DIR)
message("Key outputs:")
message(" - burnin_adults_map.png")
message(" - burnin_adults_nndist.png + burnin_adults_nndist_summary.csv")
message(" - planting_layouts_4x3.png (or per-panel PNGs)")
message(" - adult_P1_maps_gen100/200/300/400/500.png")
message(" - adult_nndist_summary_gen500.csv")
