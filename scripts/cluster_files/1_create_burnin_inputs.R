  
  
  library(data.table)
  library(readr)
  
################################################################ set params 
 
 # Template (must exist)
TEMPLATE_INI <- "ini_files/templates/nemoage_template.ini"
stopifnot(file.exists(TEMPLATE_INI))

# Output folders
OUT <- list(
  input   = "input_files",
  ini     = "ini_files",
  results = "results",
  scripts = "scripts"
)

DIRS <- list(
  grid     = file.path(OUT$input, "grid"),
  cfg      = file.path(OUT$input, "configs_txt"),
  quanti   = file.path(OUT$input, "quanti_init_freq"),
  stage    = file.path(OUT$input, "patch_init_stage_size"),
  disperse = file.path(OUT$input, "disperse"),
  plots    = file.path(OUT$input, "plots"),
  ini_burnin = file.path(OUT$ini, "burnin"),
  ini_run    = file.path(OUT$ini, "run")
)

for (d in c(OUT, DIRS)) dir.create(d, showWarnings = FALSE, recursive = TRUE)

# Landscape
GRID_RES <- 4      # meters (patch = 4x4 m)
EXTENT_M <- 100    # 100x100 m
N        <- EXTENT_M / GRID_RES  # 25
CRS      <- "EPSG:3035"
GRID_SHP <- file.path(DIRS$grid, "Grid_4x4m_100x100m.shp")

# Scenario design
PROP_LEVELS    <- c(0.10, 0.25, 0.40)
CONFIG_LEVELS  <- c("dispersed", "one_cluster", "multi_cluster", "transects")
N_LAYOUT_RUNS  <- 5  # "biological replicates" per (config x proportion)

# Planting scheme for orientalis
PLANTING_SCHEME <- c(0, 20, 0, 0)  # stage0..stage3

# Burn-in + run settings
BURNIN_GENS <- 500
RUN_GENS    <- 500
K_VALUE     <- 30
B_VALUE     <- 0.03

# write stats + quanti every 50 generations
LOG_EVERY <- 50
LOG_TIMES <- c(2, seq(LOG_EVERY, RUN_GENS, by = LOG_EVERY))  # include generation 2

# NEMO replicates
NEMO_REPS_BURNIN <- 1
NEMO_REPS_RUN    <- 30

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

################################################################ grid (NO TERRA)

# grid parameters
N   <- EXTENT_M / GRID_RES          # 25
res <- GRID_RES                     # 4 m
stopifnot(N == as.integer(N))

# patch IDs (row-wise, raster order)
patch_ids <- 1:(N * N)

# row / column indices (raster-style)
row <- ((patch_ids - 1) %/% N) + 1
col <- ((patch_ids - 1) %%  N) + 1

# centroid coordinates (meters)
coords <- data.frame(
  patchID = patch_ids,
  row     = row,
  col     = col,
  x       = (col - 0.5) * res,
  y       = (row - 0.5) * res
)

# plotting coordinates (matrix-like, origin top-left)
coords$y_plot <- N - coords$row + 1
coords$x_plot <- coords$col

# basic checks
stopifnot(nrow(coords) == N * N)

# distance matrix (meters)
dist_mat <- as.matrix(dist(coords[, c("x", "y")]))

n_patches <- nrow(coords)

## CHECK 
coords[coords$patchID %in% c(1, N, N*(N-1)+1, N*N), ]


################################################################ nemo-writers
 
 # general NEMO matrix writer: {{row1}\n{row2}\n...}
write.matrix.nemo <- function(mat, outfile) {
  cat("{", file = outfile)
  for (i in seq_len(nrow(mat))) {
    cat("{", file = outfile, append = TRUE)
    cat(mat[i, ], sep = ",", file = outfile, append = TRUE)
    cat("}\n", file = outfile, append = TRUE)
  }
  cat("}\n", file = outfile, append = TRUE)
}

# patch_nbfem vector file: {{K1,K2,...,Kn}}
write_patchK_vector <- function(K_vec, outfile) {
  stopifnot(length(K_vec) > 0)
  cat("{{", file = outfile)
  cat(K_vec, sep = ",", file = outfile, append = TRUE)
  cat("}}\n", file = outfile, append = TRUE)
}

################################################################ dispersal
 
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

seed_conn_file   <- file.path(DIRS$disperse, sprintf("Seed_connectivity_matrix_d%s.txt", SEED_D_MEAN))
seed_rate_file   <- file.path(DIRS$disperse, sprintf("Seed_rate_matrix_d%s.txt", SEED_D_MEAN))
pollen_conn_file <- file.path(DIRS$disperse, sprintf("Pollen_connectivity_matrix_d%s.txt", POLLEN_D_MEAN))
pollen_rate_file <- file.path(DIRS$disperse, sprintf("Pollen_rate_matrix_d%s.txt", POLLEN_D_MEAN))

write.red.dispersal.matrix(seed_mats$connectivity_matrix, seed_mats$rate_matrix, seed_conn_file, seed_rate_file)
write.red.dispersal.matrix(pol_mats$connectivity_matrix,  pol_mats$rate_matrix,  pollen_conn_file, pollen_rate_file)

message("Wrote dispersal matrices to: ", DIRS$disperse)

 

################################################################ burnin-inputs

  # sylvatica-only quanti init: all zeros
q_burnin <- matrix(0, nrow = n_patches, ncol = 1)
q_burnin_file <- file.path(DIRS$quanti, "quanti_init_file_burnin_sylv.txt")
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

burnin_stage <- build_patch_init_stage_size_burnin(n_patches, adult_occupancy = 0.20, stage1_seedlings = 5, seed = SEED_BASE)
burnin_stage_file <- file.path(DIRS$stage, "patch_init_stage_size_burnin_sylv.txt")
write.matrix.nemo(burnin_stage, burnin_stage_file)

message("Burn-in init files written:\n- ", q_burnin_file, "\n- ", burnin_stage_file)

  
################################################################ ini-helpers
 
 read_ini_lines <- function(path) readLines(path, warn = FALSE)

set_ini_param <- function(lines, key, value) {
  rx <- paste0("^\\s*", key, "\\b")
  idx <- grep(rx, lines)

  newline <- paste(key, value)

  # key not present -> append
  if (length(idx) == 0) return(c(lines, newline))

  # replace first occurrence
  lines[idx[1]] <- newline

  # remove any additional occurrences
  if (length(idx) > 1) lines <- lines[-idx[-1]]

  lines
}

# drop lines matching regex
drop_ini_lines <- function(lines, regex) lines[!grepl(regex, lines)]

################################################################ write-burnin-ini
  
  burnin_lines <- read_ini_lines(TEMPLATE_INI)

# core
burnin_lines <- set_ini_param(burnin_lines, "replicates", NEMO_REPS_BURNIN)
burnin_lines <- set_ini_param(burnin_lines, "generations", BURNIN_GENS)

# outputs
burnin_root <- file.path(OUT$results, "burnin")
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

burnin_ini <- file.path(DIRS$ini_burnin, paste0(burnin_name, ".ini"))
writeLines(burnin_lines, burnin_ini)

# burnin output txt we will parse later
BURNIN_TXT <- file.path(burnin_root, paste0(burnin_name, ".txt"))

message("Wrote burn-in ini: ", burnin_ini)
message("Burn-in will write: ", BURNIN_TXT)
