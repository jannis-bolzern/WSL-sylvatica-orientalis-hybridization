library(data.table)

res_path <- "C:/Users/stefanin/Dropbox/WSL_PhD/Projects/Hybridization2/output/analysis"

## which configuration gives higher HI and higher W ?
# does mean HI differs among configurations, and does this depend on proportion of O introduced, selection scenario, age class, time (short vs long term)?

dt <- readRDS(file.path(res_path, "Quanti_fit_data.RDS"))

## for individuals in a population, keep the mean
dt_hi <- dt[,.( 
  N = .N,
  N_hybrid = sum(P1 > -1 & P1 < 1),
  prop_hybrids =  mean(P1 > -1 & P1 < 1), ## ==  sum(P1 > -1 & P1 < 1) / length(P1) (i.e. the proprotions)
  mean_HI = mean(1 - abs(P1)), ## average HI of individuals within the group
  mean_W = mean(W) 
),

by = .(configuration, Proportion_orientalis,selection,year,sim_id, age_class, cost) ]

gc()


## chec distirbution of HI 
hist(dt_hi$mean_HI)
hist(dt_hi$mean_W)
# bounded between 0 and 1 and skewed ---> logit transform?

# mean across age classes within replicate per year = one HI per replicate per year
## mean values for the population (across age classes)
dt_hi_pop <- dt_hi[, .(
  N = sum(N),
  N_hybrid = sum(N_hybrid),
  
  prop_hybrids = sum(N_hybrid) / sum(N),  
  
  mean_HI = weighted.mean(mean_HI, w = N), ## weighted for the population size
  mean_W  = weighted.mean(mean_W,  w = N)  ## weighted for the population size
  
),
by = .(configuration, Proportion_orientalis, selection, year, sim_id, cost)]


# function to run test on one subset
run_test <- function(dat, response_var) {
  
  formula <- as.formula(paste(response_var, "~ configuration"))
  
  aov_model <- aov(formula, data = dat)
  
  # tests (for the error to be numeric, otherwise doesn^t work)
  shapiro_p  <- tryCatch(
    shapiro.test(residuals(aov_model))$p.value,
    error = function(e) NA_real_
  )
  
  bartlett_p <- tryCatch(
    bartlett.test(formula, data = dat)$p.value,
    error = function(e) NA_real_
  )
  
  use_anova <- !is.na(shapiro_p) & !is.na(bartlett_p) &
    (shapiro_p > 0.05) & (bartlett_p > 0.05)
  
  if(use_anova) {
    test_used <- "ANOVA"
    p_value <- summary(aov_model)[[1]][["Pr(>F)"]][1]
  } else {
    test_used <- "Kruskal-Wallis"
    p_value <- kruskal.test(formula, data = dat)$p.value
  }
  
  return(list(
    test_used = test_used,
    p_value = p_value,
    shapiro_p = shapiro_p,
    bartlett_p = bartlett_p
  ))
}

############## run test on HI

res_HI <- dt_hi_pop[, {
  
  res <- run_test(.SD, "mean_HI")
  
  # also compute medians per configuration (VERY useful)
  med <- .SD[, .(median_HI = median(mean_HI)), by = configuration]
  
  list(
    n = .N,
    test_used = res$test_used,
    p_value = res$p_value,
    shapiro_p = res$shapiro_p,
    bartlett_p = res$bartlett_p,
    
    # optional summary
    max_median_HI = max(med$median_HI),
    best_config_HI = med[which.max(median_HI)]$configuration
  )
  
}, by = .(selection, Proportion_orientalis, year)]



############## run test on W

res_W <- dt_hi_pop[, {
  
  res <- run_test(.SD, "mean_W")
  
  med <- .SD[, .(median_W = median(mean_W)), by = configuration]
  
  list(
    n = .N,
    test_used = res$test_used,
    p_value = res$p_value,
    shapiro_p = res$shapiro_p,
    bartlett_p = res$bartlett_p,
    
    max_median_W = max(med$median_W),
    best_config_W = med[which.max(median_W)]$configuration
  )
  
}, by = .(selection, Proportion_orientalis, year)]

