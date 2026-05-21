# 0. import our defined fit_h2_regress and fit_h2_regress_boot functions
setwd("C:/Users/user/Box/Infant Motor R01/WU Only Infant Motor R01/MZ-DZ_Comparisons/MzDz_Twins/accel_heritability_2026_02_11/")
list.files()
source("./heritability_utils.R")



# 1. Packages to install/load ----
library(Matrix)    # for sparse/block diag construction
library(regress)   # fits REML with multiple user-supplied covariance matrices
library(tidyverse); library(ggpubr)


# 2. Format data and check distributions ----
list.files()

DATA <- read.csv("data_accelClean2026-03-11.csv", header=TRUE,
                 stringsAsFactors = TRUE)

colnames(DATA)

static_vars <- c(
  "family_id", "redcap_event_name", "final_twintype","zygosity", 
  "sexType", "include", "sex", "agemonths_6", "race")

dynamic_vars <- c(
  # "total_no_mvt_time", "total_mvt_time",
  "l_time", "r_time", "simultaneous_time",
  #"l_only_time", "r_only_time", 
  "l_magnitude", "r_magnitude", # "bilateral_magnitude", 
  "l_magnitude_sd", "r_magnitude_sd", 
  #"l_peak_magnitude", "r_peak_magnitude", 
  "l_jerk", "r_jerk", 
  "l_sd_freq", "r_sd_freq", 
  #"RAW_no_mvt_time", "RAW_mvt_time",  
  "RAW_l_time", "RAW_r_time", "RAW_simultaneous_time")
  #"RAW_l_only_time", "RAW_r_only_time")


summary(DATA$zygosity)

DATA <- DATA %>%
  filter(is.na(zygosity)==FALSE)

# 3. looping through outcome variables
results <- vector("list", length(dynamic_vars))
names(results) <- dynamic_vars

for (i in seq_along(dynamic_vars)) {
  y <- dynamic_vars[i]
  
  results[[i]] <- fit_h2_regress_boot(
    df = DATA,
    y_var = y,
    id_var = "record_id",
    family_var = "family_id",
    zygosity_var = "zygosity",
    conf_level = 0.95,
    mz_label = "MZ",
    dz_label = "DZ"
  )
}

# save after the loop runs (hopefully!)
save.image(file = "test_heritability.RData")
getwd()


results
str(results)

# Raw Variance Components and Jack-Knifed CIs ----------------------------------
result_df <- do.call(rbind, lapply(results, function(x) {
  data.frame(
    outcome    = x$outcome,
    component  = c("V_A", "V_C", "V_E"),
    estimate   = x$results$Estimate,
    std_error  = x$results$Std.Error,
    ci_lower   = x$results$CI_Lower,
    ci_upper   = x$results$CI_Upper,
    conf_level = x$conf_level
  )
}))

rownames(result_df) <- NULL  # optional: clean up auto-generated rownames

list.files()
write.csv(result_df, paste("./mixedModEstimates_RawVariances", Sys.Date(), ".csv", sep=""))


# Estimate Proportion of Variance and Jack-Knifed CIs --------------------------
prop_results <- do.call(rbind, lapply(results, function(x) {
  
  # Compute proportion from point estimates
  total_est <- sum(x$results$Estimate)
  prop_est  <- x$results$Estimate / total_est
  
  # Compute proportions across all jackknife replicates
  jack_totals <- rowSums(x$raw_jack)
  jack_props  <- x$raw_jack / jack_totals  # n_replicates x 3 matrix
  
  # Jackknife SE and CI for proportions
  n         <- nrow(jack_props)
  jack_mean <- colMeans(jack_props)
  
  # Explicitly compute squared deviations per column
  deviations <- sweep(jack_props, 2, jack_mean, "-")  # subtract column means
  jack_se    <- sqrt(((n - 1) / n) * colSums(deviations^2))
  
  z <- qnorm(1 - (1 - x$conf_level) / 2)
  
  data.frame(
    outcome    = x$outcome,
    component  = c("V_A", "V_C", "V_E"),
    prop       = prop_est,
    prop_se    = jack_se,
    prop_lower = prop_est - z * jack_se,
    prop_upper = prop_est + z * jack_se,
    conf_level = x$conf_level
  )
}))

rownames(prop_results) <- NULL
prop_results

list.files()
write.csv(prop_results, paste("./mixedModEstimates_ProportionVariances", Sys.Date(), ".csv", sep=""))




# Plots of estimates for Duration Variables ------------------------------------
duration_vars <- c("l_time", "r_time", "simultaneous_time",
                   "RAW_l_time", "RAW_r_time", "RAW_simultaneous_time")



duration_map <- tibble::tribble(
  ~base_var,            ~prop_var,               ~raw_var,
  #"total_no_mvt_time",  "total_no_mvt_time",     "RAW_no_mvt_time",
  #"total_mvt_time",     "total_mvt_time",        "RAW_mvt_time",
  "l_time",             "l_time",                "RAW_l_time",
  #"l_only_time",        "l_only_time",           "RAW_l_only_time",
  "r_time",             "r_time",                "RAW_r_time",
  #"r_only_time",        "r_only_time",           "RAW_r_only_time",
  "simultaneous_time",  "simultaneous_time",     "RAW_simultaneous_time"
)

results_duration <- result_df %>%
  filter(outcome %in% duration_vars) %>%
  left_join(
    duration_map %>% 
      pivot_longer(
        cols = c(prop_var, raw_var),
        names_to = "variableType",
        values_to = "outcome"
      ),
    by = "outcome"
  )

results_duration <- results_duration %>%
  mutate(
    variableType = recode(
      variableType,
      prop_var = "prop_time",
      raw_var  = "raw_time"
    )
  ) 


results_duration 

str(results_duration)
ggplot(data=results_duration %>%
         filter(component == "V_A"), 
       aes(x=base_var, y=estimate)) +
  geom_errorbar(aes(ymin=ci_lower, ymax=ci_upper, col=variableType), 
                width=0.2, position=position_dodge(width=0.5))+
  geom_point(aes(col=variableType), position=position_dodge(width=0.5)) +
  ggtitle("Estimated ACE model with Boot-CIs")+
  theme_bw()+
  theme(axis.text.y = element_text(size=10, color="black"),
        axis.text.x =element_text(size=10, color="black", angle=45, hjust=1),
        legend.text=element_text(size=10, color="black"),
        legend.title=element_text(size=10, face="bold"),
        axis.title=element_text(size=10, face="bold"),
        plot.title=element_text(size=12, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=10, face="bold"),
        legend.position = "bottom")

getwd()
ggsave(filename="./mixedEffects_ACEmod.jpeg",
       plot = last_plot(),
       path = "C:/Users/user/Box/Infant Motor R01/WU Only Infant Motor R01/MZ-DZ_Comparisons/MzDz_Twins/heritability_mixedModels/plots",
       width = 6,
       height = 4,
       units = "in",
       dpi = 300)



