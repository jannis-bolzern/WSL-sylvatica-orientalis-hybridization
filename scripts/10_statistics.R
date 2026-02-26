library(data.table)


########## 100 years #############
HI_summary <- hyb_prop_100[, .(
  mean_HI = mean(mean_HI),
  sd_HI   = sd(mean_HI)
), by = .(configuration,
          Proportion_orientalis,
          selection)]

# rank configuration from highest to lowest HI
HI_summary[order(selection, Proportion_orientalis, -mean_HI)]

# is the pattern consistent? 
HI_summary[, .SD[which.max(mean_HI)], by = .(selection,Proportion_orientalis )]


## compute overall means across proportions and selections
HI_summary[, .(overall_HI = mean(mean_HI)),by = configuration][order(-overall_HI)]


# linear model --?
model <- lm(mean_HI ~ configuration * Proportion_orientalis *selection,data = HI_summary)
summary(model)


## ANOVA ??


########## 1000 years ###########
HI_summary <- hyb_prop_1000[, .(
  mean_HI = mean(mean_HI),
  sd_HI   = sd(mean_HI)
), by = .(configuration,
          Proportion_orientalis,
          selection)]

# rank configuration from highest to lowest HI
HI_summary[order(selection, Proportion_orientalis, -mean_HI)]

# is the pattern consistent? 
HI_summary[, .SD[which.max(mean_HI)], by = .(selection,Proportion_orientalis )]


## compute overall means across proportions and selections
HI_summary[, .(overall_HI = mean(mean_HI)),by = configuration][order(-overall_HI)]
