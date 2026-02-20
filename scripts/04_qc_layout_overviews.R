#!/usr/bin/env Rscript

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

# ---- helpers ----
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
    geom_tile(aes(fill = patch_value), color = 'grey85', linewidth = 0.15) +
    coord_equal() +
    scale_fill_manual(values = c(S = 'grey75', O = '#c53030')) +
    labs(title = title, x = '', y = '', fill = NULL) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid = element_blank(),
      legend.position = 'none',
      plot.title = element_text(hjust = 0.5, size = 10),
      plot.background = element_rect(fill = 'white', color = NA),
      panel.background = element_rect(fill = 'white', color = NA)
    )
}

# ---- load inputs ----
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

# ---- render panels ----
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
    plot_layout_binary(dt, coords, rep_lab)
  })
  plist <- Filter(Negate(is.null), plist)
  if (length(plist) == 0) next

  main_title <- sprintf(
    '%s | requested %.0f%% (%.1f/%d) | used %d/%d = %.2f%%',
    cfg, 100 * p, p * n_patches, n_patches, 
    groups$n_orientalis[i], n_patches,
    100 * groups$n_orientalis[i] / n_patches
  )

  panel <- cowplot::plot_grid(plotlist = plist, nrow = 1, align = 'hv')
  final <- cowplot::plot_grid(
    cowplot::ggdraw() + cowplot::draw_label(main_title, fontface = 'bold', size = 14),
    panel,
    ncol = 1,
    rel_heights = c(0.18, 1)
  )

  out_png <- file.path(PLOT_DIR, paste0('overview_', key, '.png'))
  ggsave(out_png, final, width = 16, height = 3.8, dpi = 220, bg = 'white')
}

message('Saved layout overview panels to: ', PLOT_DIR)
