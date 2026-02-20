# Project helper functions
# Do not edit function bodies here unless you want to change core logic.
# Prefer changing parameters in scripts/00_control_panel.R.

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

plot_grid_binary_gg <- function(conf_dt, coords_df, title = NULL) {
  df <- merge(coords_df, conf_dt, by = "patchID", all.x = TRUE)
  df$patch_value <- ifelse(is.na(df$patch_value), "S", df$patch_value)

  ggplot(df, aes(x = x_plot, y = y_plot, fill = patch_value)) +
    geom_tile(color = "grey85", linewidth = 0.1) +
    coord_equal() +
    labs(title = title, x = "", y = "", fill = "") +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid = element_blank(),
      legend.position = "none",
      plot.title = element_text(size = 10, hjust = 0.5))
}


# utility: square layer distance (Chebyshev) around a center in (row,col)
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
  nO, coords_df, prop_orientalis = NULL, seed = NULL,
  min_tr = NULL, max_tr = NULL, gap = 4L, ...
) {
  if (!is.null(seed)) set.seed(seed)
  if (nO <= 0) return(integer(0))
  if (nO >= nrow(coords_df)) return(coords_df$patchID)

  N <- max(coords_df$row)   # local grid size
  k <- 3L

  n_full <- nO %/% N
  rem    <- nO %% N

  # distribute FULL columns across 3 transects
  f <- rep(n_full %/% k, k)
  r <- n_full %% k
  if (r > 0) f[sample.int(k, r)] <- f[sample.int(k, r)] + 1L

  # choose which transect gets the ONE partial column (if any)
  w_used <- f
  t_rem  <- 0L
  if (rem > 0) {
    z <- which(f == 0L)
    t_rem <- if (length(z)) sample(z, 1) else sample.int(k, 1)
    w_used[t_rem] <- w_used[t_rem] + 1L
  }

  w <- pmax(1L, w_used)  # keep 3 visible blocks (some may be unused at very low nO)

  slack <- N - (sum(w) + 2L * gap)
  if (slack < 0) stop("Impossible: N too small for 3 transects with this gap and nO.")

  pad <- as.vector(rmultinom(1, slack, rep(1, 4)))
  s1 <- 1L + pad[1]
  s2 <- s1 + w[1] + gap + pad[2]
  s3 <- s2 + w[2] + gap + pad[3]
  starts <- c(s1, s2, s3)

  chosen <- integer(0)

  for (t in 1:k) {
    cols_block <- starts[t]:(starts[t] + w[t] - 1L)
    cols_use   <- if (w_used[t] > 0) cols_block[seq_len(w_used[t])] else integer(0)

    if (f[t] > 0) {
      chosen <- c(chosen, coords_df$patchID[coords_df$col %in% cols_use[1:f[t]]])
    }

    if (rem > 0 && t == t_rem) {
      pc   <- cols_use[f[t] + 1L]
      rows <- if (sample(c(TRUE, FALSE), 1)) 1:rem else (N - rem + 1L):N
      chosen <- c(chosen, coords_df$patchID[coords_df$col == pc & coords_df$row %in% rows])
    }
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
