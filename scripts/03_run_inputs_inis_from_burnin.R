#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# 03_run_inputs_inis_from_burnin.R
#
# Purpose:
#   Build scenario inputs and run INIs from a completed burn-in:
#   (i) extract baseline patch demography from burn-in .txt,
#   (ii) generate planting layouts (S/O patch maps),
#   (iii) create scenario init matrices (clear + plant on baseline),
#   (iv) write run INIs for neutral / sel_E / sel_O.
#
# Dependencies:
#   sources 00_control_panel.R
#   sources project_helpers.R
#   requires burn-in to be executed first
#
# Inputs:
#   - Burn-in demographic output (.txt) at baseline generation
#   - Nemo-age template INI; dispersal matrices from input_files/disperse/
#   - Parameters from control panel
#
# Outputs:
#   - input_files/configs_txt/<scenario>.txt
#   - input_files/quanti_init_freq/quanti_init_file_<scenario>.txt
#   - input_files/patch_init_stage_size/patch_init_stage_size_<scenario>.txt
#   - ini_files/run/*.ini (all scenario x selection combinations)
#   - manifests (scenario list + ini list)
#   - scripts/05_run_all.sh
#   - scripts/06_move_logs.sh 
# ------------------------------------------------------------------------------

source("scripts/00_control_panel.R")
source("scripts/functions/project_helpers.R")
source("scripts/01_burnin_inputs_and_ini.R")

# Recreate/load grid + distances (fast if grid already exists)
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


n_patches <- nrow(grid)

# Derive burn-in filenames (same convention as in 01 + the original QMD)
burnin_root <- file.path(OUTPUT$simulations, "burnin")
burnin_name <- sprintf("burnin_k%i_b%0.2f", K_VALUE, B_VALUE)
BURNIN_INI <- file.path(INI$burnin, sprintf("burnin_k%i_b%0.2f.ini", K_VALUE, B_VALUE))
BURNIN_TXT <- file.path(burnin_root, paste0(burnin_name, ".txt"))

if (!file.exists(BURNIN_TXT)) {
  stop(
    "Burn-in output not found: ", BURNIN_TXT, "\n",
    "Run: Rscript scripts/01_generate_burnin_inputs_and_inis.R\n",
    "Then: bash scripts/01_run_burnin.sh"
  )
}


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

baseline_stage <- extract_baseline_stage_matrix(BURNIN_TXT, n_patches, gen = BASELINE_GEN, rep = BASELINE_REP)
baseline_stage_file <- file.path(IN_FILES$stage, sprintf("patch_init_stage_size_baseline.txt"))
write.matrix.nemo(baseline_stage, baseline_stage_file)

message("Wrote baseline stage init: ", baseline_stage_file)


# function to keep total number of sylvatica per stage identical across all simulations with the same prop_orientalis 
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
      cfg_file <- file.path(IN_FILES$cfg, paste0(sim_name, ".txt"))
      write_config_quarto_schema(conf, sim_name, cfg_file)
      
      # quanti init
      q_mat <- build_quanti_init(conf, n_patches)
      q_file <- file.path(IN_FILES$quanti, paste0("quanti_init_file_", sim_name, ".txt"))
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
      
      # write final file
      stage_file <- file.path(IN_FILES$stage, paste0("patch_init_stage_size_", sim_name, ".txt"))
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
summary_file <- file.path(INPUT$input_files, "initial_population_summary.csv")
fwrite(summary_dt, summary_file)
message("Wrote summary file: ", summary_file)


manifest_file <- file.path(SCRIPTS$manifests, "manifest_scenarios.tsv")
fwrite(manifest, manifest_file, sep = "\t")
message("Generated ", nrow(manifest), " scenario layouts.")
message("Manifest: ", manifest_file)


selection_scenarios <- data.table(
  sel_id   = c("neutral", "sel_E", "sel_O"),
  sel_opt  = c(NA,  1.0, -1.0),   # NA = no selection
  sel_var  = c(NA, 20,  20)
)

add_selection_ini <- function(ini, opt, var) {

  ## 1. change LIFE CYCLE EVENTS
  ini <- set_ini_param(ini, "seed_disperse", "2")
  ini <- set_ini_param(ini, "viability_selection", "3")
  ini <- set_ini_param(ini, "regulation", "4")
  ini <- set_ini_param(ini, "save_stats", "5")
  ini <- set_ini_param(ini, "aging_multi", "6")
  ini <- set_ini_param(ini, "save_files", "7")

  ## 2. add SELECTION block at the end
  selection_block <- c(
    "",
    "## ----------------------------",
    "## SELECTION",
    "## ----------------------------",
    "viability_selection        3",
    "selection_trait_dimension  1",
    "selection_trait            quant",
    "selection_model            quadratic",
    "selection_fitness_model    absolute",
    "selection_at_stage         {{0}}",
    paste0("selection_local_optima     {{", opt, "}}"),
    paste0("selection_matrix           {{", var, "}}")
  )

  c(ini, selection_block)
}


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
    run_root <- file.path(OUTPUT$simulations, "run", sel_id, cfg, sprintf("p%02d", round(100*p)), sprintf("r%02d", rid))
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

    
    ini_path <- file.path(INI$run, paste0(sim,"_", sel_id, "_k", K_VALUE, "_b", B_VALUE, ".ini"))
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

ini_manifest_file <- file.path(SCRIPTS$manifests, "ini_manifest.tsv")
fwrite(ini_manifest, ini_manifest_file, sep = "\t")

message("Wrote run inis to: ", INI$run)
message("INI manifest: ", ini_manifest_file)

sh_all <- file.path(SCRIPTS$root, "05_run_all.sh")

if (RUN_PARALLEL) {
  
  cores <- if (identical(N_CORES, "auto")) '$(nproc)' else as.integer(N_CORES)
  halt  <- if (HALT_ON_ERROR) "--halt soon,fail=1" else ""
  
  bash_lines <- c(
    "#!/usr/bin/env bash",
    "set -euo pipefail",
    "",
    "command -v parallel >/dev/null 2>&1 || { echo 'GNU parallel not installed'; exit 1; }",
    "",
    sprintf(
      "parallel -j %s %s --bar '%s {}' ::: %s",
      cores,
      halt,
      NEMO_BIN,
      paste(shQuote(ini_manifest$ini), collapse = " ")
    )
  )
  
} else {
  
  cmds <- sprintf("%s %s", shQuote(NEMO_BIN), shQuote(ini_manifest$ini))
  
  bash_lines <- c(
    "#!/usr/bin/env bash",
    "set -euo pipefail",
    "",
    cmds,
    "",
  )
  
}

bash_lines <- c(
  bash_lines,
  "",
  "echo",
  "echo 'All simulations finished.'",
  "echo 'Move logs from root to /logs with scripts/06_move_logs.sh'",
  "echo"
)

writeLines(bash_lines, sh_all)
Sys.chmod(sh_all, "0755")

message("Wrote: ", sh_all)

move_script <- file.path(SCRIPTS$root, "06_move_logs.sh")

move_lines <- c(
  "#!/usr/bin/env bash",
  "set -euo pipefail",
  "",
  "echo 'Moving log files...'",
  "",
  "shopt -s nullglob",
  "logs=( *_log )",
  "",
  "if [ ${#logs[@]} -eq 0 ]; then",
  "  echo 'No *_log files found in root.'",
  "  exit 0",
  "fi",
  "",
  "date_dir=\"output/logs/$(date +%F)\"",
  "mkdir -p \"$date_dir\"",
  "",
  "for f in \"${logs[@]}\"; do",
  "  mv \"$f\" \"$date_dir/\"",
  "done",
  "",
  "echo \"Moved ${#logs[@]} log files to $date_dir\""
)

writeLines(move_lines, move_script)
Sys.chmod(move_script, "0755")

message("Wrote: ", move_script)

message("\nNext step:")
message("Run all scenarios with: ", file.path(sh_all))
message("After completion, move the logs from the root directory with: ", file.path(move_script))

