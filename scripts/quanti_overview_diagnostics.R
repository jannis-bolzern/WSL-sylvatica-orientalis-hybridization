#!/usr/bin/env Rscript
# Quanti overview diagnostics: selection × layout replicates


suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(stringr)
  library(scales)
  library(viridis)
})

# Project paths + global settings live here
source("scripts/00_control_panel.R")

# ---- Paths 
RES_ROOT <- file.path(OUTPUT$simulations, "run")
OUT_DIR  <- file.path(OUTPUT$analysis, "diagnostics")
SC_DIR   <- file.path(SCRIPTS$manifests)

dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# ---- Run controls
SEL_LEVELS <- c("neutral", "sel_E", "sel_O")
CFG_LEVELS <- c("dispersed", "one_cluster", "multi_cluster", "transects")
PCT_LEVELS <- c(10, 25, 40)
RUN_IDS    <- 1:5

P1_TOL          <- 0
GEN_FINAL       <- 1000
MAKE_TILE_PLOTS <- TRUE
P1_BIN_WIDTH    <- 0.1


## Helper functions

save_plot <- function(p, file, width = 11, height = 6.5, dpi = 220) {
  dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
  ggsave(file, p, width = width, height = height, dpi = dpi)
  invisible(file)
}

safe_slug <- function(x) {
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

# Parse metadata from output basenames
parse_meta_from_basename <- function(fname) {
  nm <- basename(fname)
  m <- str_match(
    nm,
    "^(.+?)_p(\\d+)_r(\\d+)(?:_(neutral|sel_E|sel_O))?_k(\\d+(?:\\.\\d+)?)_b(\\d+(?:\\.\\d+)?)(?:_|\\.|$)"
  )
  if (any(is.na(m))) return(NULL)
  
  data.table(
    configuration = m[, 2],
    pct = as.integer(m[, 3]),
    layout_run = as.integer(m[, 4]),
    selection = m[, 5],
    k = as.numeric(m[, 6]),
    b = as.numeric(m[, 7])
  )
}

# Suffix parser for quanti files: _<gen>_<rep>.quanti
parse_quanti_suffix <- function(fname) {
  nm <- basename(fname)
  m <- str_match(nm, "_(\\d{3,4})_(\\d+)\\.quanti$")
  if (any(is.na(m))) return(NULL)
  data.table(generation = as.integer(m[,2]), replicate = as.integer(m[,3]))
}

# Robust list + metadata filter
list_files_with_meta <- function(paths, keep_cfg, keep_pct, keep_run) {
  metas <- lapply(paths, parse_meta_from_basename)
  ok <- !vapply(metas, is.null, logical(1))
  paths <- paths[ok]
  metas <- metas[ok]
  meta_dt <- rbindlist(metas, use.names = TRUE, fill = TRUE)
  meta_dt[, path := paths]
  
  meta_dt <- meta_dt[
    configuration %in% keep_cfg &
      pct %in% keep_pct &
      layout_run %in% keep_run
  ]
  meta_dt[]
}

# Read minimal hybridization metrics at a single generation from each quanti file.
# Uses ALL stages
read_quanti_metrics_at_gen <- function(quanti_meta, gen_keep, tol = 1e-8) {
  if (nrow(quanti_meta) == 0) return(data.table())
  
  out <- vector("list", nrow(quanti_meta))
  j <- 0L
  
  for (i in seq_len(nrow(quanti_meta))) {
    f <- quanti_meta$path[i]
    suf <- parse_quanti_suffix(f)
    if (is.null(suf)) next
    if (!identical(as.integer(suf$generation), as.integer(gen_keep))) next
    
    dt <- fread(f, select = c("P1","stage"), showProgress = FALSE)
    if (nrow(dt) == 0) next
    if (!is.numeric(dt$P1)) dt[, P1 := as.numeric(P1)]
    
    N <- nrow(dt)
    
    j <- j + 1L
    out[[j]] <- data.table(
      configuration  = quanti_meta$configuration[i],
      ori_proportion = quanti_meta$pct[i] / 100,
      layout_run     = quanti_meta$layout_run[i],
      selection      = quanti_meta$selection[i],
      k              = quanti_meta$k[i],
      b              = quanti_meta$b[i],
      generation     = as.integer(gen_keep),
      replicate      = suf$replicate,
      N              = N,
      prop_F1        = mean(abs(dt$P1) <= tol, na.rm = TRUE),
      prop_hybrid    = mean(dt$P1 > (-1 + tol) & dt$P1 < (1 - tol), na.rm = TRUE),
      HI_mean        = mean(1 - abs(dt$P1), na.rm = TRUE)
    )
  }
  
  rbindlist(out[seq_len(j)], use.names = TRUE, fill = TRUE)
}

# prepare common factor levels
apply_overview_factors <- function(dt, cfg_levels, pct_levels_prop, run_ids) {
  if (nrow(dt) == 0) return(dt)
  
  if ("configuration" %in% names(dt)) {
    dt[, configuration := factor(configuration, levels = cfg_levels)]
  }
  if ("selection" %in% names(dt)) {
    dt[, selection := factor(selection, levels = SEL_LEVELS)]
  }
  if ("ori_proportion" %in% names(dt)) {
    dt[, ori_proportion := factor(ori_proportion, levels = pct_levels_prop)]
  }
  if ("layout_run" %in% names(dt)) {
    dt[, layout_run := factor(layout_run, levels = run_ids, labels = sprintf("r%02d", run_ids))]
  }
  
  # stable kb ordering
  if (!("kb" %in% names(dt)) && all(c("k","b") %in% names(dt))) {
    dt[, kb := paste0("k", k, " b", b)]
  }
  if ("kb" %in% names(dt) && all(c("k","b") %in% names(dt))) {
    kb_tab <- unique(dt[, .(k, b, kb)])
    setorder(kb_tab, k, b)
    dt[, kb := factor(kb, levels = kb_tab$kb)]
  }
  
  dt[]
}

# heatmaps per (configuration x kb)
plot_layout_heatmap <- function(layout_dt, value_col, title, fill_lab) {
  dt <- copy(layout_dt)
  dt[, value := get(value_col)]
  ggplot(dt, aes(x = ori_proportion, y = layout_run, fill = value)) +
    geom_tile(color = "white", linewidth = 0.25, na.rm = FALSE) +
    facet_grid(configuration ~ selection) +
    scale_fill_viridis_c(option = "C", limits = c(0, 1), oob = squish, na.value = "grey90") +
    labs(
      title = title,
      x = "Orientalis introduced (proportion)",
      y = "Layout replicate",
      fill = fill_lab
    ) +
    theme_bw(base_size = 11) +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.background = element_rect(fill = "grey95")
    )
}

# layout points + across-layout mean +- SD, per panel
plot_layout_points_and_sd <- function(layout_dt, summ_dt, y_col_layout, y_col_mean, y_col_sd, title, ylab) {
  ld <- copy(layout_dt)
  sm <- copy(summ_dt)
  
  ld[, y := get(y_col_layout)]
  sm[, `:=`(y_mean = get(y_col_mean), y_sd = get(y_col_sd))]
  
  sm[, `:=`(
    ymin = pmax(0, y_mean - y_sd),
    ymax = pmin(1, y_mean + y_sd)
  )]
  
  ggplot() +
    geom_point(
      data = ld,
      aes(x = ori_proportion, y = y),
      alpha = 0.35, size = 1.2,
      position = position_jitter(width = 0.12, height = 0)
    ) +
    geom_errorbar(
      data = sm,
      aes(x = ori_proportion, ymin = ymin, ymax = ymax),
      width = 0.15, linewidth = 0.45
    ) +
    geom_point(
      data = sm,
      aes(x = ori_proportion, y = y_mean),
      size = 2.0
    ) +
    facet_grid(configuration ~ selection) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(
      title = title,
      x = "Orientalis introduced (proportion)",
      y = ylab
    ) +
    theme_bw(base_size = 11) +
    theme(
      panel.grid.major.x = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.background = element_rect(fill = "grey95")
    )
}


## P1 distribution over time

make_p1_breaks <- function(bin_width = 0.1) {
  bw <- as.numeric(bin_width)
  br <- seq(-1, 1, by = bw)
  br <- round(br, 10)
  if (tail(br, 1) < 1) br <- c(br, 1)
  br
}


build_quanti_p1_bins <- function(quanti_meta, breaks, tol = 1e-8, keep_generations = NULL) {
  if (nrow(quanti_meta) == 0) return(data.table())
  
  bin_list <- list()
  bin_mids <- (breaks[-length(breaks)] + breaks[-1]) / 2
  
  for (i in seq_len(nrow(quanti_meta))) {
    f <- quanti_meta$path[i]
    suf <- parse_quanti_suffix(f)
    if (is.null(suf)) next
    if (!is.null(keep_generations) && !(suf$generation %in% keep_generations)) next
    
    dt <- fread(f, select = c("P1"), showProgress = FALSE)
    if (nrow(dt) == 0) next
    if (!is.numeric(dt$P1)) dt[, P1 := as.numeric(P1)]
    
    bw  <- breaks[2] - breaks[1]
    eps <- bw / 1e6
    
    p1r <- round(dt$P1, 8)
    p1r <- pmin(pmax(p1r, -1), 1)
    p1r[p1r == 1] <- 1 - eps 
    p1r <- p1r + eps
    
    dt[, bin_id := findInterval(p1r, breaks, rightmost.closed = TRUE, all.inside = TRUE)]
    dt[, bin_mid := bin_mids[bin_id]]
    
    bins <- dt[, .N, by = .(bin_mid)]
    bins[, prop := N / sum(N)]
    bins[, `:=`(
      configuration = quanti_meta$configuration[i],
      ori_proportion = quanti_meta$pct[i] / 100,
      layout_run = quanti_meta$layout_run[i],
      selection = quanti_meta$selection[i],
      k = quanti_meta$k[i],
      b = quanti_meta$b[i],
      generation = suf$generation,
      replicate = suf$replicate
    )]
    
    bin_list[[length(bin_list) + 1]] <- bins
  }
  
  rbindlist(bin_list, use.names = TRUE, fill = TRUE)
}

plot_p1_tile_over_time <- function(p1_bins, breaks, cfg_levels, pct_levels_prop, title_prefix) {
  if (nrow(p1_bins) == 0) return(NULL)
  
  p1_bins[, kb := paste0("k", k, " b", b)]
  kb_tab <- unique(p1_bins[, .(k, b, kb)])
  setorder(kb_tab, k, b)
  p1_bins[, kb := factor(kb, levels = kb_tab$kb)]
  
  # mean across internal replicates and across layout replicates
  p1_bins[, bin_mid := round(as.numeric(bin_mid), 2)]
  p1_mean <- p1_bins[, .(mean_prop = mean(prop, na.rm = TRUE)),
                     by = .(configuration, ori_proportion, kb, generation, bin_mid)]
  
  p1_mean[, configuration := factor(configuration, levels = cfg_levels)]
  p1_mean[, ori_proportion := factor(ori_proportion, levels = pct_levels_prop)]
  p1_mean[, gen_f := factor(generation, levels = sort(unique(generation)))]
  
  ggplot(p1_mean, aes(x = gen_f, y = bin_mid, fill = mean_prop)) +
    geom_tile() +
    facet_grid(ori_proportion ~ configuration) +
    scale_fill_viridis_c(option = "C", trans = "sqrt") +
    labs(
      title = title_prefix,
      x = "Generation",
      y = "P1 bin midpoint (−1 = sylvatica, 0 = hybrid, +1 = orientalis)",
      fill = "Mean\nproportion"
    ) +
    theme_bw(base_size = 11) +
    theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
}

## Run quanti overview

index <- data.table()

# Collect quanti metadata across selection modes
meta_list <- list()
gen_max_tbl <- data.table()

for (sel in SEL_LEVELS) {
  sel_root <- file.path(RES_ROOT, sel)
  cat("

### ", sel, "

", sep = "")
  
  if (!dir.exists(sel_root)) {
    cat("**Skipping** (folder not found): `", sel_root, "`

", sep = "")
    next
  }
  
  quanti_files_all <- list.files(sel_root, recursive = TRUE, full.names = TRUE, pattern = "\\.quanti$")
  cfg_rx <- paste0("(", paste(CFG_LEVELS, collapse = "|"), ")")
  quanti_files <- quanti_files_all[grepl(paste0("/", cfg_rx, "/p\\d+/r\\d+/run/(quanti|qunati)/"), quanti_files_all)]
  
  qm <- list_files_with_meta(quanti_files, CFG_LEVELS, PCT_LEVELS, RUN_IDS)
  if (nrow(qm) == 0) {
    cat("_No quanti files found for this selection mode._

")
    next
  }
  
  # if selection token missing in filenames, fill from folder name
  qm[is.na(selection) | selection == "", selection := sel]
  
  meta_list[[sel]] <- qm
  
  suf_sel <- rbindlist(lapply(qm$path, parse_quanti_suffix), use.names = TRUE, fill = TRUE)
  gen_max <- max(suf_sel$generation, na.rm = TRUE)
  
  gen_max_tbl <- rbind(gen_max_tbl, data.table(selection = sel, gen_max = gen_max), fill = TRUE)
  
  cat("Found **", nrow(qm), "** quanti files; max generation = **", gen_max, "**

", sep = "")
}

quanti_meta_all <- rbindlist(meta_list, use.names = TRUE, fill = TRUE)

if (nrow(quanti_meta_all) == 0) {
  cat("

---

**No quanti files found across the requested selections/configs/runs.**
")
} else {
  
  # Choose a shared GEN_FINAL across selections
  if (is.null(GEN_FINAL) || is.na(GEN_FINAL)) {
    GEN_FINAL <- min(gen_max_tbl$gen_max, na.rm = TRUE)
    cat("

Using shared GEN_FINAL = **", GEN_FINAL,
        "** (minimum of max generation across selections; override with GEN_FINAL)

", sep = "")
  } else {
    GEN_FINAL <- as.integer(GEN_FINAL)
    cat("

Using GEN_FINAL = **", GEN_FINAL, "** 

", sep = "")
  }
  
  tol <- as.numeric(P1_TOL)
  
  # output folder for combined overview plots/tables
  out_all <- file.path(OUT_DIR, "quanti_overview")
  dir.create(out_all, showWarnings = FALSE, recursive = TRUE)
  
  # 1) replicate-level (internal Nemo reps) metrics at GEN_FINAL
  hyb_prop <- read_quanti_metrics_at_gen(quanti_meta_all, gen_keep = GEN_FINAL, tol = tol)
  
  if (nrow(hyb_prop) == 0) {
    cat("**No quanti files at GEN_FINAL across selections.**

")
  } else {
    
    missing_sel <- setdiff(SEL_LEVELS, unique(hyb_prop$selection))
    if (length(missing_sel) > 0) {
      cat("Missing data at GEN_FINAL for: **", paste(missing_sel, collapse = ", "),
          "** (these panels will be empty)

", sep = "")
    }
    
    # 2) layout-run means (mean across internal replicates within each layout_run)
    hyb_layout <- hyb_prop[
      , .(
        N_mean = mean(N, na.rm = TRUE),
        prop_F1 = mean(prop_F1, na.rm = TRUE),
        prop_hybrid = mean(prop_hybrid, na.rm = TRUE),
        HI_mean = mean(HI_mean, na.rm = TRUE),
        n_internal_reps = .N
      ),
      by = .(selection, configuration, ori_proportion, k, b, generation, layout_run)
    ]
    
    # 3) across-layout summaries (mean +- SD across layout replicates)
    hyb_layout[, kb := paste0("k", k, " b", b)] 
    
    hyb_summ <- hyb_layout[
      , {
        m_h  <- mean(prop_hybrid, na.rm = TRUE)
        s_h  <- sd(prop_hybrid, na.rm = TRUE)
        m_hi <- mean(HI_mean, na.rm = TRUE)
        s_hi <- sd(HI_mean, na.rm = TRUE)
        list(
          mean_prop_hybrid_layout = m_h,
          sd_prop_hybrid_layout   = s_h,
          cv_prop_hybrid_layout   = if (is.finite(m_h) && m_h > 0) s_h / m_h else NA_real_,
          mean_HI_layout          = m_hi,
          sd_HI_layout            = s_hi,
          n_layouts               = .N
        )
      },
      by = .(selection, configuration, ori_proportion, k, b, kb, generation)
    ]
    
    # factors for stable plot layout
    hyb_layout_f <- apply_overview_factors(copy(hyb_layout), CFG_LEVELS, PCT_LEVELS / 100, RUN_IDS)
    hyb_summ_f   <- apply_overview_factors(copy(hyb_summ),   CFG_LEVELS, PCT_LEVELS / 100, RUN_IDS)
    
    # write tables (combined across selections)
    f_layout <- file.path(out_all, sprintf("layout_means_gen%d.tsv", GEN_FINAL))
    f_summ   <- file.path(out_all, sprintf("layout_summary_gen%d.tsv", GEN_FINAL))
    f_genmax <- file.path(out_all, "gen_max_by_selection.tsv")
    fwrite(hyb_layout, f_layout, sep = "	")
    fwrite(hyb_summ,   f_summ,   sep = "	")
    fwrite(gen_max_tbl, f_genmax, sep = "	")
    
    # fgacets: configuration x selection)
    p1 <- plot_layout_heatmap(
      hyb_layout_f,
      value_col = "prop_hybrid",
      title = sprintf("Hybrid proportion by layout replicate | gen %d (layout means; all stages)", GEN_FINAL),
      fill_lab = "Hybrid
proportion"
    )
    
    p2 <- plot_layout_heatmap(
      hyb_layout_f,
      value_col = "HI_mean",
      title = sprintf("Mean hybrid index by layout replicate | gen %d (layout means; all stages)", GEN_FINAL),
      fill_lab = "Mean
HI"
    )
    
    f_heat1 <- file.path(out_all, sprintf("overview_heatmap_prop_hybrid_gen%d.png", GEN_FINAL))
    f_heat2 <- file.path(out_all, sprintf("overview_heatmap_HI_mean_gen%d.png", GEN_FINAL))
    save_plot(p1, f_heat1, width = 13.5, height = 7.5)
    save_plot(p2, f_heat2, width = 13.5, height = 7.5)
    
    # layout points + across-layout sd
    p3 <- plot_layout_points_and_sd(
      layout_dt = hyb_layout_f,
      summ_dt   = hyb_summ_f,
      y_col_layout = "prop_hybrid",
      y_col_mean   = "mean_prop_hybrid_layout",
      y_col_sd     = "sd_prop_hybrid_layout",
      title = sprintf("Hybrid proportion: layout means + across-layout SD | gen %d", GEN_FINAL),
      ylab  = "Hybrid proportion (layout mean)"
    )
    
    p4 <- plot_layout_points_and_sd(
      layout_dt = hyb_layout_f,
      summ_dt   = hyb_summ_f,
      y_col_layout = "HI_mean",
      y_col_mean   = "mean_HI_layout",
      y_col_sd     = "sd_HI_layout",
      title = sprintf("Mean HI: layout means + across-layout SD | gen %d", GEN_FINAL),
      ylab  = "Mean HI (layout mean)"
    )
    
    f_sd1 <- file.path(out_all, sprintf("overview_layout_sd_prop_hybrid_gen%d.png", GEN_FINAL))
    f_sd2 <- file.path(out_all, sprintf("overview_layout_sd_HI_mean_gen%d.png", GEN_FINAL))
    save_plot(p3, f_sd1, width = 13.5, height = 7.5)
    save_plot(p4, f_sd2, width = 13.5, height = 7.5)
    
    index <- data.table(
      GEN_FINAL = GEN_FINAL,
      n_quanti_files = nrow(quanti_meta_all),
      n_metrics_rows = nrow(hyb_prop),
      out_dir = out_all
    )
    
    cat("
Wrote combined overview outputs to: `", out_all, "`

", sep = "")
    cat("- `", basename(f_heat1), "`
", sep = "")
    cat("- `", basename(f_heat2), "`
", sep = "")
    cat("- `", basename(f_sd1), "`
", sep = "")
    cat("- `", basename(f_sd2), "`
", sep = "")
    cat("- `", basename(f_layout), "`
", sep = "")
    cat("- `", basename(f_summ), "`
", sep = "")
    cat("- `", basename(f_genmax), "`

", sep = "")
  }
  
  # p1 distribution over time tile plots
  
  if (isTRUE(MAKE_TILE_PLOTS)) {
    brks <- make_p1_breaks(P1_BIN_WIDTH)
    
    for (sel in SEL_LEVELS) {
      qm <- meta_list[[sel]]
      if (is.null(qm) || nrow(qm) == 0) next
      
      suf_sel <- rbindlist(lapply(qm$path, parse_quanti_suffix), use.names = TRUE, fill = TRUE)
      gens_all <- sort(unique(suf_sel$generation))
      gens_all <- gens_all[!is.na(gens_all) & gens_all <= GEN_FINAL]
      if (length(gens_all) == 0) next
      
      p1_bins <- build_quanti_p1_bins(qm, breaks = brks, tol = tol, keep_generations = gens_all)
      p_tile <- plot_p1_tile_over_time(
        p1_bins,
        breaks = brks,
        cfg_levels = CFG_LEVELS,
        pct_levels_prop = PCT_LEVELS / 100,
        title_prefix = sprintf("P1 distribution over time (mean across reps; all stages) (%s) | up to gen %d", sel, GEN_FINAL)
      )
      
      if (!is.null(p_tile)) {
        f_tile <- file.path(out_all, sprintf("overview_P1_tile_over_time_%s_up_to_gen%d.png", sel, GEN_FINAL))
        save_plot(p_tile, f_tile, width = 12, height = 7)
      }
    }
  }
}

