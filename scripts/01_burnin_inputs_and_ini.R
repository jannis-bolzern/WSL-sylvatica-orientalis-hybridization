#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# 01_burnin_inputs_and_ini.R
#
# Purpose:
#   Prepare and write all files needed for the Sylvatica-only burn-in:
#   grid, dispersal matrices, burn-in init matrices, and burn-in INI.
#
# Dependencies:
#   sources 00_control_panel.R
#   sources project_helpers.R
#
# Inputs:
#   Nemo-age template INI; grid parameters; dispersal kernel parameters.
#
# Outputs:
#   - input_files/grid/*.shp
#   - input_files/disperse/*Seed*_d*.txt and *Pollen*_d*.txt
#   - input_files/quanti_init_freq/quanti_init_file_burnin_sylv.txt
#   - input_files/patch_init_stage_size/patch_init_stage_size_burnin_sylv.txt
#   - ini_files/burnin/*.ini
#   - scripts/02_run_burnin.sh
# ------------------------------------------------------------------------------

source("scripts/00_control_panel.R")
source("scripts/functions/project_helpers.R")

make_or_load_grid_strict <- function(shp_path, extent_m = 100, res = 4, crs = "EPSG:3035") {
  if (file.exists(shp_path)) return(vect(shp_path))

  r <- rast(extent = ext(0, extent_m, 0, extent_m), resolution = res, crs = crs)
  r[] <- 1:ncell(r)  # row-wise patchID in raster order

  grid_poly <- as.polygons(r, dissolve = FALSE)
  names(grid_poly)[names(grid_poly) == "lyr.1"] <- "patchID"
  writeVector(grid_poly, shp_path, filetype = "ESRI Shapefile", overwrite = TRUE)

  grid_poly
}

grid <- make_or_load_grid_strict(GRID_SHP, EXTENT_M, GRID_RES, CRS)
stopifnot(nrow(grid) == N * N)

cent <- centroids(grid)

coords <- as.data.frame(crds(cent))
names(coords) <- c("x", "y")
coords$patchID <- grid$patchID

# matrix-like plotting coords
coords$row <- ((coords$patchID - 1) %/% N) + 1
coords$col <- ((coords$patchID - 1) %%  N) + 1
coords$y_plot <- N - coords$row + 1
coords$x_plot <- coords$col

patch_ids <- coords$patchID
n_patches <- length(patch_ids)

dist_mat <- as.matrix(distance(cent))  # meters


exp_power_kernel <- function(d, b, x) {
  a <- d * exp(lgamma(2 / b) - lgamma(3 / b))
  b * exp(-(x^b / a^b)) / (2 * pi * a^2 * exp(lgamma(2 / b)))
}

build.reduced.dispersal.matrices <- function(num_patch, distance_matrix, d, b, d_thresh) {
  stopifnot(nrow(distance_matrix) == num_patch, ncol(distance_matrix) == num_patch)

  rate_full <- matrix(0, nrow = num_patch, ncol = num_patch)
  conn      <- matrix(NA_integer_, nrow = num_patch, ncol = num_patch)
  rate_red  <- matrix(NA_real_,    nrow = num_patch, ncol = num_patch)

  for (i in 1:num_patch) {
    rate_full[i, ] <- exp_power_kernel(d, b, distance_matrix[i, ])
    ord <- order(rate_full[i, ], decreasing = TRUE)

    conn[i, ]     <- ord
    rate_red[i, ] <- rate_full[i, ord]

    to_remove <- which(rate_red[i, ] < d_thresh)
    if (length(to_remove)) {
      rate_red[i, to_remove] <- NA
      conn[i, to_remove]     <- NA
    }

    s <- sum(rate_red[i, ], na.rm = TRUE)
    if (s > 0) rate_red[i, !is.na(rate_red[i, ])] <- rate_red[i, !is.na(rate_red[i, ])] / s
  }

  list(dispersal_matrix = rate_full, connectivity_matrix = conn, rate_matrix = rate_red)
}

# Ensure no empty rows: connectivity starts with focal patch; rate starts with 1 if empty.
write.red.dispersal.matrix <- function(connectivity_matrix, rate_matrix, conn_file, rate_file) {
  # connectivity
  cat("{", file = conn_file)
  for (i in 1:nrow(connectivity_matrix)) {
    cat("{", file = conn_file, append = TRUE)
    cat(i, file = conn_file, append = TRUE)
    if (!all(is.na(connectivity_matrix[i, ]))) {
      non_na <- connectivity_matrix[i, which(!is.na(connectivity_matrix[i, ]))]
      non_na <- non_na[non_na != i]
      if (length(non_na)) {
        cat(",", file = conn_file, append = TRUE)
        cat(non_na, sep = ",", file = conn_file, append = TRUE)
      }
    }
    cat("}\n", file = conn_file, append = TRUE)
  }
  cat("}\n", file = conn_file, append = TRUE)

  # rate
  cat("{", file = rate_file)
  for (i in 1:nrow(rate_matrix)) {
    cat("{", file = rate_file, append = TRUE)
    if (all(is.na(rate_matrix[i, ]))) {
      cat(1, file = rate_file, append = TRUE)
    } else {
      cat(rate_matrix[i, which(!is.na(rate_matrix[i, ]))], sep = ",", file = rate_file, append = TRUE)
    }
    cat("}\n", file = rate_file, append = TRUE)
  }
  cat("}\n", file = rate_file, append = TRUE)
}

# thresholds (rate at distance d_t)
seed_thresh   <- exp_power_kernel(SEED_D_MEAN,   SEED_B,   SEED_D_T)
pollen_thresh <- exp_power_kernel(POLLEN_D_MEAN, POLLEN_B, POLLEN_D_T)

seed_mats <- build.reduced.dispersal.matrices(n_patches, dist_mat, SEED_D_MEAN, SEED_B, seed_thresh)
pol_mats  <- build.reduced.dispersal.matrices(n_patches, dist_mat, POLLEN_D_MEAN, POLLEN_B, pollen_thresh)

seed_conn_file   <- file.path(IN_FILES$disperse, sprintf("Seed_connectivity_matrix_d%s.txt", SEED_D_MEAN))
seed_rate_file   <- file.path(IN_FILES$disperse, sprintf("Seed_rate_matrix_d%s.txt", SEED_D_MEAN))
pollen_conn_file <- file.path(IN_FILES$disperse, sprintf("Pollen_connectivity_matrix_d%s.txt", POLLEN_D_MEAN))
pollen_rate_file <- file.path(IN_FILES$disperse, sprintf("Pollen_rate_matrix_d%s.txt", POLLEN_D_MEAN))

write.red.dispersal.matrix(seed_mats$connectivity_matrix, seed_mats$rate_matrix, seed_conn_file, seed_rate_file)
write.red.dispersal.matrix(pol_mats$connectivity_matrix,  pol_mats$rate_matrix,  pollen_conn_file, pollen_rate_file)

message("Wrote dispersal matrices to: ", IN_FILES$disperse)


# sylvatica-only quanti init: all zeros
q_burnin <- matrix(0, nrow = n_patches, ncol = 1)
q_burnin_file <- file.path(IN_FILES$quanti, "quanti_init_file_burnin_sylv.txt")
write.matrix.nemo(q_burnin, q_burnin_file)

# burn-in stage init: a small fraction of patches with one adult, plus uniform seedlings
build_patch_init_stage_size_burnin <- function(n_patches, adult_occupancy = 0.20, stage1_seedlings = 5, seed = 1) {
  set.seed(seed)
  mat <- matrix(0, nrow = n_patches, ncol = 4)
  colnames(mat) <- c("stage0","stage1","stage2","stage3")

  adults <- rep(0, n_patches)
  idx <- sample(seq_len(n_patches), size = as.integer(round(adult_occupancy * n_patches)))
  adults[idx] <- 1

  mat[, "stage3"] <- adults
  mat[, "stage1"] <- stage1_seedlings
  mat
}

burnin_stage <- build_patch_init_stage_size_burnin(n_patches, adult_occupancy = BURNIN_ADULT_OCCUPANCY, stage1_seedlings = BURNIN_STAGE1_SEEDLINGS, seed = SEED_BASE)
burnin_stage_file <- file.path(IN_FILES$stage, "patch_init_stage_size_burnin_sylv.txt")
write.matrix.nemo(burnin_stage, burnin_stage_file)

message("Burn-in init files written:\n- ", q_burnin_file, "\n- ", burnin_stage_file)


burnin_lines <- read_ini_lines(TEMPLATE_INI)

# core
burnin_lines <- set_ini_param(burnin_lines, "replicates", NEMO_REPS_BURNIN)
burnin_lines <- set_ini_param(burnin_lines, "generations", BURNIN_GENS)

# outputs
burnin_root <- file.path(OUTPUT$simulations, "burnin")
dir.create(burnin_root, showWarnings = FALSE, recursive = TRUE)
burnin_name <- sprintf("burnin_k%d_b%.3g", K_VALUE, B_VALUE)
burnin_lines <- set_ini_param(burnin_lines, "root_dir", burnin_root)
burnin_lines <- set_ini_param(burnin_lines, "filename", burnin_name)
burnin_lines <- set_ini_param(burnin_lines, "logfile", paste0(burnin_name, "_log"))

# init
burnin_lines <- set_ini_param(burnin_lines, "quanti_init_freq", paste0("&", q_burnin_file))
burnin_lines <- set_ini_param(burnin_lines, "patch_init_stage_size", paste0("&", burnin_stage_file))

# K and competition (fixed)
burnin_lines <- set_ini_param(burnin_lines, "patch_nbfem", K_VALUE)
burnin_lines <- set_ini_param(burnin_lines, "regulation_by_competition", B_VALUE)

# logging
burnin_lines <- set_ini_param(burnin_lines, "stat_log_time",
                              paste0("{{", paste(LOG_TIMES, collapse = ","), "}}"))
# quanti output in burn-in too
burnin_lines <- set_ini_param(burnin_lines, "quanti_dir", "quanti")
burnin_lines <- set_ini_param(burnin_lines, "quanti_logtime",
                              paste0("{{", paste(LOG_TIMES, collapse = ","), "}}"))

# dispersal
burnin_lines <- set_ini_param(burnin_lines, "seed_disperse_connectivity_matrix", paste0("&", seed_conn_file))
burnin_lines <- set_ini_param(burnin_lines, "seed_disperse_reduced_matrix",       paste0("&", seed_rate_file))
burnin_lines <- set_ini_param(burnin_lines, "breed_disperse_connectivity_matrix_mal", paste0("&", pollen_conn_file))
burnin_lines <- set_ini_param(burnin_lines, "breed_disperse_reduced_matrix_mal",       paste0("&", pollen_rate_file))

burnin_ini <- file.path(INI$burnin, paste0(burnin_name, ".ini"))
writeLines(burnin_lines, burnin_ini)

# burnin output txt we will parse later
BURNIN_TXT <- file.path(burnin_root, paste0(burnin_name, ".txt"))

message("Wrote burn-in ini: ", burnin_ini)
message("Burn-in will write: ", BURNIN_TXT)


# Write bash helper to run the burn-in
burnin_ini <- file.path(INI$burnin, sprintf("burnin_k%i_b%0.2f.ini", K_VALUE, B_VALUE))
burnin_log <- file.path(OUTPUT$logs, sprintf("burnin_k%i_b%0.2f.log", K_VALUE, B_VALUE))
burnin_sh  <- file.path(ROOT$scripts, "02_run_burnin.sh")

cmd_line <- sprintf("%s %s", shQuote(NEMO_BIN), shQuote(burnin_ini))

bash_lines <- c(
  "#!/usr/bin/env bash",
  "set -euo pipefail",
  "",
  sprintf("if [[ ! -x %s ]]; then", shQuote(NEMO_BIN)),
  "  echo 'ERROR: NEMO_BIN not found/executable. Create a symlink in project root, e.g.:'",
  "  echo '  ln -sf /path/to/nemoage_binary ./nemoage && chmod +x ./nemoage'",
  "  exit 1",
  "fi",
  "",
  cmd_line,
  "",
  "echo 'Burn-in finished.'"
)

writeLines(bash_lines, burnin_sh)
Sys.chmod(burnin_sh, mode = "0755")

message("\nNext step:")
message("  Run the burn-in with: ", burnin_sh)
message("  Then continue with scripts/02_generate_run_inis_from_burnin.R\n")

