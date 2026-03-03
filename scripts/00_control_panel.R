#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# 00_control_panel.R
#
# Purpose:
#   Central configuration for the full simulation pipeline.
#
# Used by: 
#   01_burnin_inputs_and_ini.R, 03_run_inputs_inis_from_burnin.R,
#   04_qc_layout_overviews.R, 07_qc_spacial_outcomes.R
#
# Inputs:
#   Template INI path,
#   Nemo-age binary path
#
# Outputs:
#   Creates required folder structure; creates global constants for
#   downstream scripts.
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table)
  library(sf)
  library(terra)
  library(ggplot2)
  library(cowplot)
})

# Path to NEMO-AGE binary (symlink recommended, see docs)
NEMO_BIN <- "./nemoage"

# Template (must exist)
TEMPLATE_INI <- "input/ini_files/template/nemoage_template.ini"
stopifnot(file.exists(TEMPLATE_INI))

# Root folders

ROOT <- list(
  docs       = "docs",
  input      = "input",
  output     = "output",
  scripts    = "scripts",
  manuscript = "manuscript"
)

# Input structure

INPUT <- list(
  input_files = file.path(ROOT$input, "input_files"),
  ini_files   = file.path(ROOT$input, "ini_files")
)

# Subfolders inside input_files
IN_FILES <- list(
  grid     = file.path(INPUT$input_files, "grid"),
  cfg      = file.path(INPUT$input_files, "configs_txt"),
  quanti   = file.path(INPUT$input_files, "quanti_init_freq"),
  stage    = file.path(INPUT$input_files, "patch_init_stage_size"),
  disperse = file.path(INPUT$input_files, "disperse")
)

# Subfolders inside ini_files
INI <- list(
  burnin = file.path(INPUT$ini_files, "burnin"),
  run    = file.path(INPUT$ini_files, "run")
)

# Output structure

OUTPUT <- list(
  simulations = file.path(ROOT$output, "simulations"),
  logs        = file.path(ROOT$output, "logs"),
  analysis    = file.path(ROOT$output, "analysis"),
  qc          = file.path(ROOT$output, "qc")
)

# Scripts structure

SCRIPTS <- list(
  root      = ROOT$scripts,
  functions = file.path(ROOT$scripts, "functions"),
  manifests = file.path(ROOT$scripts, "manifests")
)


# Create all directories

ALL_DIRS <- unique(c(
  unlist(ROOT),
  unlist(INPUT),
  unlist(IN_FILES),
  unlist(INI),
  unlist(OUTPUT),
  unlist(SCRIPTS)
))

invisible(lapply(ALL_DIRS, dir.create, recursive = TRUE, showWarnings = FALSE))


# Landscape
GRID_RES <- 4      # meters (patch = 4x4 m)
EXTENT_M <- 100    # 100x100 m
N        <- EXTENT_M / GRID_RES  # 25
CRS      <- "EPSG:3035"
GRID_SHP <- file.path(IN_FILES$grid, "Grid_4x4m_100x100m.shp")

# Scenario design
PROP_LEVELS    <- c(0.10, 0.25, 0.40)
CONFIG_LEVELS  <- c("dispersed", "one_cluster", "multi_cluster", "transects")
N_LAYOUT_RUNS  <- 5  # "biological replicates" per (config x proportion)

# Planting scheme sylvatica burn-in: a small fraction of patches with one adult, plus uniform seedlings
BURNIN_ADULT_OCCUPANCY <- 0.20 # proportion of patches with adults
BURNIN_STAGE1_SEEDLINGS <- 5 # number of stage 1 seedlings in all patches

# Planting scheme for orientalis
PLANTING_SCHEME <- c(0, 20, 0, 0)  # stage0..stage3

# Burn-in + run settings
BURNIN_GENS <- 500
RUN_GENS    <- 1000
K_VALUE     <- 30
B_VALUE     <- 0.03

# write stats + quanti at generations (log times)
LOG_TIMES <- c(2, 10, 20, 30, 40, 50, 100, 150, 200, 300, 400, 600, 800, 1000)

# NEMO replicates
NEMO_REPS_BURNIN <- 1
NEMO_REPS_RUN    <- 20

# Dispersal (fixed)
# SEED
SEED_D_MEAN <- 5
SEED_B      <- 1.3
SEED_D_T    <- 50
# POLLEN
POLLEN_D_MEAN <- 30
POLLEN_B      <- 1
POLLEN_D_T    <- 100

# Layout controls
# Dispersed (Poisson-disc-like): larger -> more regular spacing
DISPERSED_STRENGTH <- 1.20

# One cluster: how "soft" the square edge is (0 = perfect square)
CLUSTER_JITTER <- 0.15

# Multi clusters: number of clusters (3 or 4 are typical)
MULTI_N_CLUSTERS <- 4

# Transects: approximate number of 1-patch-wide transects for min/max proportions
TRANSECTS_MIN <- 2
TRANSECTS_MAX <- 7

# Seed (deterministic layout generation)
SEED_BASE <- 9999

# Execution settings
RUN_PARALLEL <- TRUE        # TRUE = use GNU parallel
N_CORES <- "auto"           # "auto" or integer (e.g. 8)
HALT_ON_ERROR <- TRUE       # stop all runs if one fails
LOG_RUNS <- TRUE            # write logs per run

