library(data.table)
library(readr)

################################################################ set params 

# ---- Templates (must exist)
TEMPLATE_INI <- "ini_files/templates/nemoage_template.ini"
stopifnot(file.exists(TEMPLATE_INI))

# ---- Output folders
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

# ---- Landscape
GRID_RES <- 4      # meters (patch = 4x4 m)
EXTENT_M <- 100    # 100x100 m
N        <- EXTENT_M / GRID_RES  # 25
CRS      <- "EPSG:3035"
GRID_SHP <- file.path(DIRS$grid, "Grid_4x4m_100x100m.shp")

# ---- Scenario design
PROP_LEVELS    <- c(0.10, 0.25, 0.40)
CONFIG_LEVELS  <- c("dispersed", "one_cluster", "multi_cluster", "transects")
N_LAYOUT_RUNS  <- 5  # "biological replicates" per (config × proportion)

# ---- Planting scheme for Orientalis
PLANTING_SCHEME <- c(0, 20, 0, 0)  # stage0..stage3

# ---- Burn-in + run settings (fixed, but easy to change)
BURNIN_GENS <- 100
RUN_GENS    <- 1000
K_VALUE     <- 30
B_VALUE     <- 0.03

# write stats + quanti every 50 generations
LOG_EVERY <- 20
LOG_TIMES <- c(5,10,15,20,25,30,35,40,45,50,60,70,80,90,100, seq(LOG_EVERY, RUN_GENS, by = LOG_EVERY))  # include initial generations
#LOG_TIMES <- c(5,10,15,20,25,30,35,40,45,50,60,70,80,90,100,110,120,130,140,150,200,250,300,400,500,1000)

# NEMO replicates
NEMO_REPS_BURNIN <- 1
NEMO_REPS_RUN    <- 10

# ---- Dispersal (fixed)
# SEED
SEED_D_MEAN <- 5
SEED_B      <- 1.3
SEED_D_T    <- 20
# POLLEN
POLLEN_D_MEAN <- 30
POLLEN_B      <- 1
POLLEN_D_T    <- 100

# ---- Layout controls
# Dispersed (Poisson-disc-like): larger -> more regular spacing
DISPERSED_STRENGTH <- 1.20

# One cluster: how "soft" the square edge is (0 = perfect square by layers)
CLUSTER_JITTER <- 0.15   # small jitter on square distance ordering

# Multi clusters: number of clusters (3 or 4 are typical)
MULTI_N_CLUSTERS <- 4

# Transects: approximate number of 1-patch-wide transects for min/max proportions
TRANSECTS_MIN <- 2
TRANSECTS_MAX <- 6

# ---- Seed (deterministic layout generation)
SEED_BASE <- 04022026

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


################################################################  baseline-from-burnin

burnin_root <- file.path(OUT$results, "burnin")
burnin_name <- sprintf("burnin_k%d_b%.3g", K_VALUE, B_VALUE)
BURNIN_TXT <- file.path(burnin_root, paste0(burnin_name, ".txt"))


extract_baseline_stage_matrix <- function(txt_path, n_patches, gen, rep = 1) {
  stopifnot(file.exists(txt_path))

  hdr <- names(fread(txt_path, nrows = 0))
  hdr2 <- gsub("^off\\.", "a0.", hdr) # older outputs may use off.* for stage0

  stage_cols <- lapply(0:3, function(s) grep(paste0("^a", s, "\\.fem\\.p\\d+$"), hdr2, value = TRUE))
  if (any(lengths(stage_cols) == 0)) stop("Could not find per-patch female columns a0..a3 in: ", txt_path)

  cols_needed <- c("replicate", "generation", unlist(stage_cols))
  dt <- fread(txt_path, select = cols_needed, showProgress = FALSE)
  setnames(dt, gsub("^off\\.", "a0.", names(dt)))

  row <- dt[replicate == rep & generation == gen]
  if (nrow(row) != 1) stop("Could not find exactly one row for replicate=", rep, " generation=", gen)

  out <- matrix(0, nrow = n_patches, ncol = 4)
  colnames(out) <- paste0("stage", 0:3)

  for (s in 0:3) {
    cols <- grep(paste0("^a", s, "\\.fem\\.p\\d+$"), names(row), value = TRUE)
    pnum <- as.integer(sub(paste0("^a", s, "\\.fem\\.p"), "", cols))
    ord  <- order(pnum)
    vals <- as.numeric(unlist(row[, ..cols])[ord])
    out[pnum[ord], s + 1] <- vals
  }

  out
}

BASELINE_GEN <- BURNIN_GENS
BASELINE_REP <- 1

if (!file.exists(BURNIN_TXT)) {
  stop("Burn-in output not found yet: ", BURNIN_TXT, "\nRun burn-in first, then re-render or re-run this section.")
}

baseline_stage <- extract_baseline_stage_matrix(BURNIN_TXT, n_patches, gen = BASELINE_GEN, rep = BASELINE_REP)
baseline_stage_file <- file.path(DIRS$stage, sprintf("patch_init_stage_size_baseline.txt"))
write.matrix.nemo(baseline_stage, baseline_stage_file)

message("Wrote baseline stage init: ", baseline_stage_file)

################################################################  layout-generators

rank_by_square_layers <- function(rows, cols, center_r, center_c, jitter = 0, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  d <- pmax(abs(rows - center_r), abs(cols - center_c))
  if (jitter > 0) d <- d + runif(length(d), -jitter, jitter)

  order(d)
}

# 1) dispersed: Poisson-disc-like sequential selection with min-distance relaxation
select_orientalis_dispersed <- function(
  nO,
  patch_ids,
  dist_mat,
  base_spacing_m,
  seed = NULL,
  strength = 1.20,
  relax_step = 0.95,
  max_relax = 12
) {
  if (!is.null(seed)) set.seed(seed)
  if (nO <= 0) return(integer(0))
  if (nO >= length(patch_ids)) return(patch_ids)

  minDist <- base_spacing_m * strength

  for (attempt in seq_len(max_relax)) {
    chosen <- integer(0)

    for (cand in sample(patch_ids)) {
      if (!length(chosen) || min(dist_mat[cand, chosen]) >= minDist) {
        chosen <- c(chosen, cand)
      }

      if (length(chosen) >= nO) return(chosen[1:nO])
    }

    minDist <- minDist * relax_step
  }

  # fallback
  sample(patch_ids, nO)
}

# 2) one square-ish cluster around (near) center, with slight edge softness via jitter
select_orientalis_one_cluster <- function(nO, coords_df, seed = NULL, jitter = 0.15) {
  if (!is.null(seed)) set.seed(seed)
  if (nO <= 0) return(integer(0))
  if (nO >= nrow(coords_df)) return(coords_df$patchID)

  # random small shift around center to create different layout runs
  center_r <- round(N / 2) + sample(-2:2, 1)
  center_c <- round(N / 2) + sample(-2:2, 1)

  center_r <- max(1, min(N, center_r))
  center_c <- max(1, min(N, center_c))

  ord <- rank_by_square_layers(
    coords_df$row,
    coords_df$col,
    center_r,
    center_c,
    jitter = jitter,
    seed = seed
  )

  coords_df$patchID[ord][1:nO]
}

# 3) multiple square-ish clusters with roughly equal size, placed roughly evenly across the grid
select_orientalis_multi_cluster <- function(
  nO,
  coords_df,
  seed = NULL,
  n_centers = 4,
  jitter = 0.15
) {
  if (!is.null(seed)) set.seed(seed)
  if (nO <= 0) return(integer(0))
  if (nO >= nrow(coords_df)) return(coords_df$patchID)

  n_centers <- max(2, min(n_centers, 6))

  # choose center locations by partitioning the grid into n_centers regions
  if (n_centers == 3) {
    centers_rc <- rbind(
      c(round(N * 0.25), round(N * 0.25)),
      c(round(N * 0.75), round(N * 0.25)),
      c(round(N * 0.50), round(N * 0.75))
    )
  } else { # default 4
    centers_rc <- rbind(
      c(round(N * 0.25), round(N * 0.25)),
      c(round(N * 0.75), round(N * 0.25)),
      c(round(N * 0.25), round(N * 0.75)),
      c(round(N * 0.75), round(N * 0.75))
    )

    if (n_centers > 4) {
      extra <- replicate(
        n_centers - 4,
        c(sample(5:(N - 4), 1), sample(5:(N - 4), 1))
      )
      centers_rc <- rbind(centers_rc, t(extra))
    }
  }

  # jitter centers slightly for each run
  centers_rc <- centers_rc +
    matrix(sample(-2:2, nrow(centers_rc) * 2, replace = TRUE), ncol = 2)

  centers_rc[, 1] <- pmin(N, pmax(1, centers_rc[, 1]))
  centers_rc[, 2] <- pmin(N, pmax(1, centers_rc[, 2]))

  # sizes per cluster (near-equal)
  base  <- floor(nO / nrow(centers_rc))
  sizes <- rep(base, nrow(centers_rc))
  sizes[1:(nO - sum(sizes))] <- sizes[1:(nO - sum(sizes))] + 1

  chosen    <- integer(0)
  available <- coords_df$patchID

  for (k in seq_len(nrow(centers_rc))) {
    center_r <- centers_rc[k, 1]
    center_c <- centers_rc[k, 2]

    sub <- coords_df[match(available, coords_df$patchID), ]

    ord <- rank_by_square_layers(
      sub$row,
      sub$col,
      center_r,
      center_c,
      jitter = jitter,
      seed = seed + k
    )

    take <- sub$patchID[ord][1:min(sizes[k], length(ord))]

    chosen    <- c(chosen, take)
    available <- setdiff(available, take)

    if (length(chosen) >= nO) break
  }

  chosen <- unique(chosen)

  if (length(chosen) > nO) {
    chosen <- chosen[1:nO]
  }

  if (length(chosen) < nO) {
    chosen <- c(
      chosen,
      sample(setdiff(coords_df$patchID, chosen), nO - length(chosen))
    )
  }

  chosen
}

# 4) transects (hard edges): full-length straight columns, approx matching p
select_orientalis_transects <- function(
  nO,
  coords_df,
  prop_orientalis = NULL,
  seed = NULL,
  ...
) {
  if (!is.null(seed)) set.seed(seed)
  if (nO <= 0) return(integer(0))
  if (nO >= nrow(coords_df)) return(coords_df$patchID)

  n_full <- nO %/% N
  rem    <- nO %% N

  full_cols <- if (n_full > 0) sample(1:N, n_full, replace = FALSE) else integer(0)

  chosen <- coords_df$patchID[coords_df$col %in% full_cols]

  if (rem > 0) {
    remaining_cols <- setdiff(1:N, full_cols)
    partial_col    <- sample(remaining_cols, 1)

    # continuous partial transect: fill from top OR bottom
    from_top    <- sample(c(TRUE, FALSE), 1)
    partial_rows <- if (from_top) 1:rem else (N - rem + 1):N

    chosen <- c(
      chosen,
      coords_df$patchID[
        coords_df$col == partial_col &
          coords_df$row %in% partial_rows
      ]
    )
  }

  chosen
}

# master wrapper
select_orientalis <- function(
  nO,
  configuration,
  patch_ids,
  dist_mat,
  coords_df,
  prop_orientalis,
  seed = NULL
) {
  configuration <- match.arg(configuration, CONFIG_LEVELS)

  if (configuration == "dispersed") {
    spacing_m <- sqrt(length(patch_ids) / max(1, nO)) * GRID_RES

    return(
      select_orientalis_dispersed(
        nO,
        patch_ids,
        dist_mat,
        base_spacing_m = spacing_m,
        seed = seed,
        strength = DISPERSED_STRENGTH
      )
    )
  }

  if (configuration == "one_cluster") {
    return(
      select_orientalis_one_cluster(
        nO,
        coords_df,
        seed = seed,
        jitter = CLUSTER_JITTER
      )
    )
  }

  if (configuration == "multi_cluster") {
    return(
      select_orientalis_multi_cluster(
        nO,
        coords_df,
        seed = seed,
        n_centers = MULTI_N_CLUSTERS,
        jitter = CLUSTER_JITTER
      )
    )
  }

  if (configuration == "transects") {
    return(
      select_orientalis_transects(
        nO,
        coords_df,
        prop_orientalis = prop_orientalis,
        seed = seed,
        min_tr = TRANSECTS_MIN,
        max_tr = TRANSECTS_MAX
      )
    )
  }

  stop("Unknown configuration: ", configuration)
}



################################################################  scenario-writers (WITHOUT PLOTTING!)

scenario_id <- function(configuration, prop_orientalis, run_id) {
  sprintf("%s_p%02d_r%02d", configuration, round(100 * prop_orientalis), run_id)
}

build_config_table <- function(orientalis_ids, patch_ids) {
  dt <- data.table(patchID = patch_ids, patch_value = "S")
  dt[patchID %in% orientalis_ids, patch_value := "O"]
  dt[order(patchID)]
}

write_config_quarto_schema <- function(conf_dt, sim_name, outfile) {
  out <- copy(conf_dt)
  out[, simulation := sim_name]
  setcolorder(out, c("simulation", "patchID", "patch_value"))
  fwrite(out, outfile, sep = "\t")
}

# quanti init (one column): 1 in O patches, 0 otherwise
build_quanti_init <- function(conf_dt, n_patches) {
  mat <- matrix(0, nrow = n_patches, ncol = 1)
  mat[conf_dt$patchID, 1] <- ifelse(conf_dt$patch_value == "O", 1, 0)
  mat
}

# patch_init_stage_size for run: start from baseline, clear O patches, plant orientalis scheme
build_patch_init_stage_size_from_baseline <- function(conf_dt, baseline_stage_mat, scheme = c(0,20,0,0)) {
  stopifnot(nrow(baseline_stage_mat) == nrow(conf_dt))
  out <- baseline_stage_mat
  O <- conf_dt$patchID[conf_dt$patch_value == "O"]
  out[O, ] <- 0
  out[O, ] <- matrix(rep(scheme, each = length(O)), nrow = length(O), byrow = FALSE)
  out
}


################################################################   generate-scenarios

refill_sylvatica_to_baseline <- function(
  plant_mat,
  baseline_syl_totals,
  conf_dt,
  seed = NULL
) {

  if (!is.null(seed)) set.seed(seed)

  out <- plant_mat

  S_ids <- conf_dt$patchID[conf_dt$patch_value == "S"]

  # count ONLY sylvatica (i.e., only S patches)
  current_syl_totals <- colSums(out[S_ids, , drop = FALSE])

  deficits <- baseline_syl_totals - current_syl_totals

  for (s in seq_len(ncol(out))) {

    deficit <- deficits[s]

    if (deficit <= 0) next   # we never remove sylvatica

    add_to <- sample(S_ids, size = deficit, replace = TRUE)
    tab <- table(add_to)

    out[as.integer(names(tab)), s] <-
      out[as.integer(names(tab)), s] + as.integer(tab)
  }

  out
}

# tot number of syl from the burnin for each age class
baseline_syl_totals <- colSums(baseline_stage)
manifest <- data.table()
summary_list <- list()

for (p in PROP_LEVELS) {
  nO_target <- p * n_patches
  nO <- as.integer(round(nO_target))
  p_used <- nO / n_patches

  for (cfg in CONFIG_LEVELS) {
    for (r in seq_len(N_LAYOUT_RUNS)) {

      sim_name <- scenario_id(cfg, p, r)

      seed <- SEED_BASE + round(p * 1000) * 1000 + match(cfg, CONFIG_LEVELS) * 100 + r

      O_ids <- select_orientalis(
        nO = nO,
        configuration = cfg,
        patch_ids = patch_ids,
        dist_mat = dist_mat,
        coords_df = coords,
        prop_orientalis = p,
        seed = seed
      )

      conf <- build_config_table(O_ids, patch_ids)

      # configs_txt
      cfg_file <- file.path(DIRS$cfg, paste0(sim_name, ".txt"))
      write_config_quarto_schema(conf, sim_name, cfg_file)

      # quanti init
      q_mat <- build_quanti_init(conf, n_patches)
      q_file <- file.path(DIRS$quanti, paste0("quanti_init_file_", sim_name, ".txt"))
      write.matrix.nemo(q_mat, q_file)

      # run stage init (baseline + clearing + planting)
      plant_mat_raw <- build_patch_init_stage_size_from_baseline(conf, baseline_stage,scheme = PLANTING_SCHEME) ## CHANGED
      plant_mat <- refill_sylvatica_to_baseline(plant_mat_raw,baseline_syl_totals, conf, seed = seed + 999) ## CHANGED
      
      ## check
      S_ids <- conf$patchID[conf$patch_value == "S"]
      if (!all(colSums(plant_mat[S_ids, , drop = FALSE]) == baseline_syl_totals)) {
        stop("Sylvatica totals do not match baseline AFTER refill in ", sim_name)
      }
      
      ## summary table check 
      # identify patch types
      S_ids <- conf$patchID[conf$patch_value == "S"]
      O_ids <- conf$patchID[conf$patch_value == "O"]
      
      # count individuals per species per stage
      syl_counts <- colSums(plant_mat[S_ids, , drop = FALSE])
      ori_counts <- colSums(plant_mat[O_ids, , drop = FALSE])
      
      summary_list[[sim_name]] <- data.table(
        sim_name = sim_name,
        configuration = cfg,
        prop_orientalis = p,
        run_id = r,
      
        syl_stage0 = syl_counts[1],
        syl_stage1 = syl_counts[2],
        syl_stage2 = syl_counts[3],
        syl_stage3 = syl_counts[4],
      
        ori_stage0 = ori_counts[1],
        ori_stage1 = ori_counts[2],
        ori_stage2 = ori_counts[3],
        ori_stage3 = ori_counts[4]
      )


      stage_file <- file.path(DIRS$stage, paste0("patch_init_stage_size_", sim_name, ".txt"))
      write.matrix.nemo(plant_mat, stage_file)

      manifest <- rbind(
        manifest,
        data.table(
          sim_name = sim_name,
          configuration = cfg,
          prop_orientalis = p,
          prop_used = p_used,
          n_orientalis = nO,
          n_target = nO_target,
          run_id = r,
          seed = seed,
          cfg_file = cfg_file,
          quanti_file = q_file,
          stage_file = stage_file
        )
      )
    }
  }
}


# store the final numbers in a summary table
summary_dt <- rbindlist(summary_list)
summary_file <- file.path(OUT$input, "initial_population_summary.csv")
fwrite(summary_dt, summary_file)
message("Wrote summary file: ", summary_file)


manifest_file <- file.path(OUT$input, "manifest_scenarios.tsv")
fwrite(manifest, manifest_file, sep = "\t")
message("Generated ", nrow(manifest), " scenario layouts.")
message("Manifest: ", manifest_file)



################################################################################ 

# PLOT REMOVED

################################################################################ 



################################################################   add selection

selection_scenarios <- data.table(
  sel_type = c("neutral", "sel_E", "sel_O", "heterosis"),
  sel_opt  = c(NA, 1.0, -1.0, 0)
)[
  , .(sel_strength = c("low", "mid", "high")), by = .(sel_type, sel_opt)
]

# stronger sel on stage0, a bit milder on stage 1
selection_strength_map <- list(
  low  = c(40, 60, 120, 180),   # weak overall
  mid  = c(20, 30, 80, 120),   # moderate
  high = c(10, 15, 50, 80)      # strong early selection
)

# attach selection variance
selection_scenarios[, sel_var := Map(function(type, strength) {
  if (type == "neutral") return(NA)
  selection_strength_map[[strength]]
}, sel_type, sel_strength)]


# crete selection ID
selection_scenarios[, sel_id := ifelse(
  sel_type == "neutral",
  "neutral",
  paste0(sel_type, "_", sel_strength)
)]


format_selection_matrix <- function(var_vec) {
  var_vec <- unlist(var_vec)
  paste0("{{", paste(var_vec, collapse = "} {"), "}}")
}


add_selection_ini <- function(ini, opt, var) {

  # if (is.na(opt) || all(is.na(unlist(var)))) return(ini)

   if (is.na(opt) || is.null(var) || all(is.na(unlist(var)))) return(ini)
   
  ## 1. change LIFE CYCLE EVENTS
  ini <- set_ini_param(ini, "seed_disperse", "2")
  ini <- set_ini_param(ini, "regulation", "3")
  ini <- set_ini_param(ini, "viability_selection", "4")
  ini <- set_ini_param(ini, "save_stats", "5")
  ini <- set_ini_param(ini, "aging_multi", "6")
  ini <- set_ini_param(ini, "save_files", "7")
  
  sel_matrix <- format_selection_matrix(var)

  ## 2. add SELECTION block at the end
  selection_block <- c(
    "",
    "## ----------------------------",
    "## SELECTION",
    "## ----------------------------",
    "viability_selection        4",
    "selection_trait_dimension  1",
    "selection_trait            quant",
    "selection_model            quadratic",
    "selection_fitness_model    absolute",
    "selection_at_stage         {{0,1,2,3}}",
     paste0("selection_local_optima     {{", opt, "}}"),
     paste0("selection_matrix           ", sel_matrix),
     "selection_output",
     "selection_output_dir        fitness",
     paste0("selection_output_logtime  {{", paste(LOG_TIMES, collapse = ","), "}}")
    )
    
  c(ini, selection_block)
}


################################################################    write-run-inis


run_template <- read_ini_lines(TEMPLATE_INI)

# set constants
run_template <- set_ini_param(run_template, "replicates", NEMO_REPS_RUN)
run_template <- set_ini_param(run_template, "generations", RUN_GENS)

run_template <- set_ini_param(run_template, "patch_nbfem", K_VALUE)
run_template <- set_ini_param(run_template, "regulation_by_competition", B_VALUE)

run_template <- set_ini_param(run_template, "stat_log_time",
                              paste0("{{", paste(LOG_TIMES, collapse = ","), "}}"))
run_template <- set_ini_param(run_template, "quanti_dir", "quanti")
run_template <- set_ini_param(run_template, "quanti_logtime",
                              paste0("{{", paste(LOG_TIMES, collapse = ","), "}}"))

# dispersal
run_template <- set_ini_param(run_template, "seed_disperse_connectivity_matrix", paste0("&", seed_conn_file))
run_template <- set_ini_param(run_template, "seed_disperse_reduced_matrix",       paste0("&", seed_rate_file))
run_template <- set_ini_param(run_template, "breed_disperse_connectivity_matrix_mal", paste0("&", pollen_conn_file))
run_template <- set_ini_param(run_template, "breed_disperse_reduced_matrix_mal",       paste0("&", pollen_rate_file))

ini_manifest <- data.table()

for (s in seq_len(nrow(selection_scenarios))) {
  sel_id  <- selection_scenarios$sel_id[s]
  sel_opt <- selection_scenarios$sel_opt[s]
  sel_var <- selection_scenarios$sel_var[s]
  
  for (i in seq_len(nrow(manifest))) {
    sim  <- manifest$sim_name[i]
    cfg  <- manifest$configuration[i]
    p    <- manifest$prop_orientalis[i]
    rid  <- manifest$run_id[i]
    seed <- manifest$seed[i]
  
    # directory grouping
    run_root <- file.path(OUT$results, sel_id, cfg, sprintf("p%02d", round(100*p)), sprintf("r%02d", rid), "run")
    dir.create(run_root, showWarnings = FALSE, recursive = TRUE)
  
    ini <- run_template
    ini <- set_ini_param(ini, "root_dir", run_root)
    ini <- set_ini_param(ini, "filename", paste0(sim, "_", sel_id, "_k", K_VALUE, "_b", B_VALUE))
    ini <- set_ini_param(ini, "logfile", paste0(sim, "_", sel_id, "_k", K_VALUE, "_b", B_VALUE, "_log"))

    # init (baseline-derived stage matrix + scenario quanti init)
    ini <- set_ini_param(ini, "quanti_init_freq", paste0("&", manifest$quanti_file[i]))
    ini <- set_ini_param(ini, "patch_init_stage_size", paste0("&", manifest$stage_file[i]))
  
    # add selection if included
    if (!is.na(sel_opt)) {
      ini <- add_selection_ini(ini, sel_opt, sel_var)
    }

    
    ini_path <- file.path(DIRS$ini_run, paste0(sim,"_", sel_id, "_k", K_VALUE, "_b", B_VALUE, ".ini"))
    writeLines(ini, ini_path)
  
    ini_manifest <- rbind(
      ini_manifest,
      data.table(
        sim_name = sim,
        configuration = cfg,
        prop_orientalis = p,
        run_id = rid,
        selection = sel_id,
        ini = ini_path,
        run_root = run_root
      )
    )
  }
}


ini_manifest_file <- file.path(OUT$scripts, "ini_manifest.tsv")
fwrite(ini_manifest, ini_manifest_file, sep = "\t")
message("Wrote run inis to: ", DIRS$ini_run)
message("INI manifest: ", ini_manifest_file)


################################################################    write-run-all


cmds <- sprintf("./nemoage %s", ini_manifest$ini)

sh_all <- file.path(OUT$scripts, "run_all.sh")
writeLines(c("#!/usr/bin/env bash", "set -euo pipefail", "", cmds), sh_all)
Sys.chmod(sh_all, "0755")

message("Wrote: ", sh_all)
