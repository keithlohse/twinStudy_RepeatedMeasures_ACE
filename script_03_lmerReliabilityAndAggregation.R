# 1. Packages to install/load ----
library(tidyverse); library(ggpubr);
library(lme4); library(lmerTest)


# 2. Format data and check distributions ----
setwd("C:/Users/user/Box/Infant Motor R01/WU Only Infant Motor R01/MZ-DZ_Comparisons/MzDz_Twins/accel_heritability_2026_02_11")
list.files()

DATA <- read.csv("data_accelClean2026-03-11.csv", header=TRUE,
                 stringsAsFactors = TRUE)

colnames(DATA)

static_vars <- c(
  "family_id", "redcap_event_name", "final_twintype","zygosity", 
  "sexType", "include", "sex", "agemonths_6", "race")

dynamic_vars <- c(
  "total_no_mvt_time", "total_mvt_time",
  "l_time", "r_time", "simultaneous_time",
  "l_only_time", "r_only_time", 
  "l_magnitude", "r_magnitude", "bilateral_magnitude", 
  "l_magnitude_sd", "r_magnitude_sd", 
  # "l_peak_magnitude", "r_peak_magnitude",  # dropping peak magnitude from the analysis 04/02/2026
  "l_jerk", "r_jerk", 
  "l_sd_freq", "r_sd_freq", 
  "RAW_no_mvt_time", "RAW_mvt_time", "RAW_simultaneous_time", 
  "RAW_l_time", "RAW_r_time", "RAW_l_only_time", "RAW_r_only_time")

summary(DATA$zygosity)

#DATA <- DATA %>%
#  filter(is.na(zygosity)==FALSE)

# Scatter Plots of All Variables -----------------------------------------------
dynamic_vars

# test plot 
PLOT_DAT <- DATA %>%
  select(family_id, child_id, zygosity, chunk, eval(dynamic_vars[4])) %>%
  pivot_wider(names_from = child_id, values_from = eval(dynamic_vars[4]))

ggplot(PLOT_DAT, aes(x = `101`, y = `102`)) +
  ggtitle(paste(eval(dynamic_vars[4]), "by Chunk with Mean")) +
  #geom_path(aes(group = family_id), alpha=0.2, col="grey")+
  #stat_ellipse(aes(group = family_id), alpha=0.5, shape=3, col="grey")+
  geom_point(aes(group = family_id), alpha=0.5, col="grey", shape=3) +
  geom_point(data=PLOT_DAT %>% group_by(family_id) %>%
                 summarise(zygosity=zygosity[1],
                           `101` = mean(`101`, na.rm=TRUE),
                           `102` = mean(`102`, na.rm=TRUE)),
    color = "black", alpha=0.8, size = 1, shape = 16) +
  stat_smooth(data=PLOT_DAT %>% group_by(family_id) %>%
                summarise(zygosity=zygosity[1],
                          `101` = mean(`101`, na.rm=TRUE),
                          `102` = mean(`102`, na.rm=TRUE)),
              lty=2, col="blue", method = "lm", se = FALSE) +
  stat_cor(data=PLOT_DAT %>% group_by(family_id) %>%
             summarise(zygosity=zygosity[1],
                       `101` = mean(`101`, na.rm=TRUE),
                       `102` = mean(`102`, na.rm=TRUE)),
           aes(label = paste(after_stat(rr.label), sep = "~`,`~")),
           r.digits = 2) +
  facet_wrap(~zygosity) +
  theme_bw() + 
  theme(axis.text=element_text(size=10, color="black"),
        legend.text=element_text(size=10, color="black"),
        legend.title=element_text(size=10, face="bold"),
        axis.title=element_text(size=10, face="bold"),
        plot.title=element_text(size=12, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=10, face="bold"),
        legend.position = "none")


# longitudinal plot
# take a sample of ten families of each zygosity
set.seed(2)
mz_list <- sample(unique(factor(DATA[DATA$zygosity=="MZ",]$family)), size=10)
dz_list <- sample(unique(factor(DATA[DATA$zygosity=="DZ",]$family)), size=10)

LONG_DATA <- DATA %>%
  select(family_id, child_id, zygosity, chunk, eval(dynamic_vars[4])) %>%
  filter(family_id %in% mz_list | family_id %in% dz_list)

head(LONG_DATA)

ggplot(LONG_DATA, aes(x = chunk, y = .data[[dynamic_vars[4]]])) +
  ggtitle(paste(dynamic_vars[4], "by Chunk")) +
  geom_line(aes(col = zygosity, lty=as.factor(child_id)), alpha=0.5) +
  geom_point(aes(col = zygosity, shape=as.factor(child_id)), alpha=0.5) +
  facet_wrap(~family_id, ncol=5, scales="free") +
  theme_bw() + labs(color="Zygosity", lty="Twin", shape="Twin")+
  theme(axis.text=element_text(size=8, color="black"),
        legend.text=element_text(size=8, color="black"),
        legend.title=element_text(size=8, face="bold"),
        axis.title=element_text(size=8, face="bold"),
        plot.title=element_text(size=10, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_blank(),
        legend.position = "bottom")


# Looping through variables to create plots
getwd()
setwd("./plots/")

for (v in dynamic_vars) {
  message("Plotting Twin Plot: ", v)
  
  PLOT_DAT <- DATA %>%
    select(family_id, child_id, zygosity, chunk, all_of(v)) %>%
    pivot_wider(names_from = child_id, values_from = all_of(v))
  
  p <- ggplot(PLOT_DAT, aes(x = `101`, y = `102`)) +
    ggtitle(paste(eval(v), "by Chunk with Mean")) +
    #geom_path(aes(group = family_id), alpha=0.2, col="grey")+
    #stat_ellipse(aes(group = family_id), alpha=0.5, shape=3, col="grey")+
    geom_point(aes(group = family_id), alpha=0.5, col="grey", shape=3) +
    geom_point(data=PLOT_DAT %>% group_by(family_id) %>%
                 summarise(zygosity=zygosity[1],
                           `101` = mean(`101`, na.rm=TRUE),
                           `102` = mean(`102`, na.rm=TRUE)),
               color = "black", alpha=0.8, size = 1, shape = 16) +
    stat_smooth(data=PLOT_DAT %>% group_by(family_id) %>%
                  summarise(zygosity=zygosity[1],
                            `101` = mean(`101`, na.rm=TRUE),
                            `102` = mean(`102`, na.rm=TRUE)),
                lty=2, col="blue", method = "lm", se = FALSE) +
    stat_cor(data=PLOT_DAT %>% group_by(family_id) %>%
               summarise(zygosity=zygosity[1],
                         `101` = mean(`101`, na.rm=TRUE),
                         `102` = mean(`102`, na.rm=TRUE)),
             aes(label = paste(after_stat(rr.label), sep = "~`,`~")),
             r.digits = 2) +
    facet_wrap(~zygosity) +
    theme_bw() + 
    theme(axis.text=element_text(size=10, color="black"),
          legend.text=element_text(size=10, color="black"),
          legend.title=element_text(size=10, face="bold"),
          axis.title=element_text(size=10, face="bold"),
          plot.title=element_text(size=12, face="bold", hjust=0.5),
          panel.grid.minor = element_blank(),
          strip.text = element_text(size=10, face="bold"),
          legend.position = "none")
  
  ggsave(
    filename = paste0(v, "_twinCor_.jpeg", sep=""),
    plot = p,
    width = 6, height = 3, units = "in", dpi = 150
  )
  
  message("Plotting Longitudinal Plot: ", v)
  # Longitudinal Plot
  LONG_DATA <- DATA %>%
    select(family_id, child_id, zygosity, chunk, all_of(v)) %>%
    filter(family_id %in% mz_list | family_id %in% dz_list)
  
  head(LONG_DATA)
  
  p2<-ggplot(LONG_DATA, aes(x = chunk, y = .data[[v]])) +
    ggtitle(paste(v, "by Chunk")) +
    geom_line(aes(col = zygosity, lty=as.factor(child_id)), alpha=0.5) +
    geom_point(aes(col = zygosity, shape=as.factor(child_id)), alpha=0.5) +
    facet_wrap(~family_id, ncol=5) +
    theme_bw() + labs(color="Zygosity", lty="Twin", shape="Twin")+
    theme(axis.text=element_text(size=8, color="black"),
          legend.text=element_text(size=8, color="black"),
          legend.title=element_text(size=8, face="bold"),
          axis.title=element_text(size=8, face="bold"),
          plot.title=element_text(size=10, face="bold", hjust=0.5),
          panel.grid.minor = element_blank(),
          strip.text = element_blank(),
          legend.position = "bottom")
  
  ggsave(
    filename = paste0(v, "_longitudinal.jpeg", sep=""),
    plot = p2,
    width = 6, height = 5, units = "in", dpi = 150
  )
  
}

getwd()
setwd("C:/Users/user/Box/Infant Motor R01/WU Only Infant Motor R01/MZ-DZ_Comparisons/MzDz_Twins/accel_heritability_2026_02_11")







# Distributions by Chunk and Twin Type -----------------------------------------
dynamic_vars

cbPalette <- c("#555555", "#56B4E9","#E69F00", "#009E73", 
               "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
               "#999933", "#882255", "#661100", "#6699CC")

# test plot 
PLOT_DAT <- DATA %>%
  select(family_id, child_id, zygosity, chunk, eval(dynamic_vars[4]))

ggplot(PLOT_DAT, aes_string(x = eval(dynamic_vars[4]))) +
  ggtitle(paste("Distribution of ", eval(dynamic_vars[4]))) +
  geom_density(aes(fill=as.factor(chunk)), alpha=0.2)+
  facet_wrap(~zygosity) +
  theme_bw() + labs(fill="2-hr Chunk")+
  scale_fill_manual(values=cbPalette)+
  theme(axis.text=element_text(size=10, color="black"),
        legend.text=element_text(size=10, color="black"),
        legend.title=element_text(size=10, face="bold"),
        axis.title=element_text(size=10, face="bold"),
        plot.title=element_text(size=12, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=10, face="bold"),
        legend.position = "bottom")



getwd()
setwd("./plots/")

for (v in dynamic_vars) {
  message("Plotting: ", v)
  
  PLOT_DAT <- DATA %>%
    select(family_id, child_id, zygosity, chunk, any_of(v))
  
  p <- ggplot(PLOT_DAT, aes_string(x = eval(v))) +
    ggtitle(paste("Distribution of ", eval(v))) +
    geom_density(aes(fill=as.factor(chunk)), alpha=0.2)+
    facet_wrap(~zygosity) +
    theme_bw() + labs(fill="2-hr Chunk")+
    scale_fill_manual(values=cbPalette)+
    theme(axis.text=element_text(size=10, color="black"),
          legend.text=element_text(size=10, color="black"),
          legend.title=element_text(size=10, face="bold"),
          axis.title=element_text(size=10, face="bold"),
          plot.title=element_text(size=12, face="bold", hjust=0.5),
          panel.grid.minor = element_blank(),
          strip.text = element_text(size=10, face="bold"),
          legend.position = "bottom")
  
  ggsave(
    filename = paste0(v, "_distirbution_.jpeg", sep=""),
    plot = p,
    width = 6, height = 3.5, units = "in", dpi = 150
  )
}

getwd()
setwd("C:/Users/user/Box/Infant Motor R01/WU Only Infant Motor R01/MZ-DZ_Comparisons/MzDz_Twins/accel_heritability_2026_02_11")




# Calculating ICCs for reliability and family variance -------------------------
head(DATA)

# Example model for family variance
fam_dz <- lmer(r_time~1+(1|family_id)+(1|record_id), data=DATA %>% filter(zygosity=="DZ"), REML=FALSE)
fam_mz <- lmer(r_time~1+(1|family_id)+(1|record_id), data=DATA %>% filter(zygosity=="MZ"), REML=FALSE)

vc_dz <- as.data.frame(VarCorr(fam_dz))
vc_mz <- as.data.frame(VarCorr(fam_mz))

sigma_twin <- vc_dz$vcov[vc_dz$grp == "record_id"] 
sigma_family <- vc_dz$vcov[vc_dz$grp == "family_id"]
sigma_residual  <- vc_dz$vcov[vc_dz$grp == "Residual"]

# ICC for reliability of chunks within a kid:
(sigma_family + sigma_twin) / (sigma_family + sigma_twin + sigma_residual)

# ICC for variance explained by family (equivalent to ICCs in Falconer's formula)
sigma_family / (sigma_family + sigma_twin)


twin_list <- levels(DATA$zygosity)
twin_list
dynamic_vars

zygosity <- NULL
variable <- NULL
#K <- NULL
N <- NULL
ICC_reliabilty <- NULL
ICC_family <- NULL

k <- 0
for (i in twin_list) {
  print(i)
  
  temp <- DATA[DATA$zygosity==eval(i),]
  
  for (v in dynamic_vars) {
    k=k+1
    print(v)
    
    form <- paste(eval(v),"~1+(1|family_id)+(1|record_id)", sep="")
    mod <- lmer(form, data=temp, REML=FALSE)
    
    vc <- as.data.frame(VarCorr(mod))
    
    sigma_twin <- vc$vcov[vc$grp == "record_id"] 
    sigma_family <- vc$vcov[vc$grp == "family_id"]
    sigma_residual  <- vc$vcov[vc$grp == "Residual"]
    
    # Variance explained by kid compared to total variance
    ICC_rel <- (sigma_family + sigma_twin) / (sigma_family + sigma_twin + sigma_residual)
    print(ICC_rel)
    
    # Variance explained by family compared to individual variance
    ICC_fam <- sigma_family / (sigma_family + sigma_twin)
    print(ICC_fam)
    
    zygosity[k] <- eval(i)
    variable[k] <- eval(v)
    N[k] <- length(unique(temp$record_id))[1]
    ICC_reliabilty[k] <- round(ICC_rel, 4)
    ICC_family[k] <- round(ICC_fam, 4)
  }
}

ICCS <- data.frame(zygosity, variable, N, ICC_reliabilty, ICC_family)
head(ICCS)


ICCS <- ICCS %>%
  pivot_wider(names_from="zygosity", values_from = N:ICC_family, names_sep="_") 

ICCS

getwd()
write.csv(ICCS, paste("icclme4Estimates", Sys.Date(), ".csv", sep=""))





