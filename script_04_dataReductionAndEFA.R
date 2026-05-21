# 1. Packages to install/load ----
library("irr"); library("psych"); library("tidyverse"); 
library("ggpubr"); library("corrplot"); library("factoextra")

# 2. Format data and check distributions ----
setwd("C:/Users/user/Box/Infant Motor R01/WU Only Infant Motor R01/MZ-DZ_Comparisons/MzDz_Twins/accel_heritability_2026_02_11")
list.files()

DATA <- read.csv("data_accelClean2026-03-11.csv", header=TRUE,
                 stringsAsFactors = TRUE)

colnames(DATA)

# static_vars <- c(
#   "family_id", "redcap_event_name", "final_twintype","zygosity", 
#   "sexType", "include", "sex", "agemonths_6", "race")

# reduce the total accel variables down to the 16 variables we are interested in
dynamic_vars <- c(
  "l_time", "r_time", "simultaneous_time",
  "RAW_l_time", "RAW_r_time", "RAW_simultaneous_time", 
  "l_magnitude", "r_magnitude",
  #"l_peak_magnitude", "r_peak_magnitude", # peak magnitude removed from analysis 2026 04 02
  "l_magnitude_sd", "r_magnitude_sd", 
  "l_jerk", "r_jerk",
  "l_sd_freq", "r_sd_freq")



ACCEL_DATA <- DATA %>% select(record_id, family_id, child_id, 
                              zygosity, chunk, any_of(dynamic_vars))
colnames(ACCEL_DATA)

# Aggregating the data down to one observation per person
ACCEL_DATA <- ACCEL_DATA %>% 
  group_by(record_id) %>%
  summarize(
    family_id = family_id[1], 
    child_id = child_id[1],
    zygosity = zygosity[1],
    across(all_of(dynamic_vars), mean, na.rm = TRUE))

head(ACCEL_DATA)


# Creating a matrix of scaled data ---------------------------------------------
SCALED <- scale(ACCEL_DATA %>% 
                  dplyr::select(l_time:r_sd_freq) %>%
                  # drop the raw variables because they too strongly correlated
                  # with the proportion variables, which are of greater interest
                  dplyr::select(-RAW_l_time, -RAW_r_time, -RAW_simultaneous_time))


# Checking for Collinearity between Variables ----------------------------------
cor_mat <- cor(SCALED)
cor_mat

det(cor_mat)

cortest.bartlett(cor_mat, n=204)

KMO(cor_mat)

smc(cor_mat)


# Correlalograms ----
jpeg("./EFA/unclustered_correlogram.jpeg", 
     width = 6, height = 6, units = 'in', res = 150)
corrplot(cor_mat, method="ellipse", type="lower", tl.cex=0.75, cl.cex = 0.75)
dev.off()

jpeg("./EFA/clustered_correlogram.jpeg", 
     width = 6, height = 6, units = 'in', res = 150)
corrplot(cor_mat, method="ellipse", type="lower", order="hclust", tl.cex=0.75, cl.cex = 0.75)
dev.off()





# Supplemental Figure i: correlations between accel measures ----
library(GGally)

jpeg("./EFA/supplementalFigure_i.png", 
     width = 16, height = 14, units = 'in', res = 150)

cbPalette <- c("#56B4E9", "#E69F00",  "#009E73", "#999999",
               "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
               "#999933", "#882255", "#661100", "#6699CC")


ggpairs(ACCEL_DATA %>%
          select(zygosity, 
                 any_of(dynamic_vars), 
                 # dropping the highly colinear RAW times
                 -RAW_l_time, -RAW_r_time, -RAW_simultaneous_time),
        columns=2:12,
        mapping = ggplot2::aes(col=zygosity, fill=zygosity, alpha=0.5),
        lower = list(continuous = wrap("points", shape=21)),
        upper = list(continuous = wrap("cor", size = 3)),
        diag = list(continuous = "densityDiag", discrete = "barDiag", na = "naDiag"))+
  theme_bw()+scale_color_manual(values=cbPalette)+
  scale_fill_manual(values=cbPalette)

dev.off()



# Testing Multivariate Normality ----
# Compute mean vector and covariance matrix
mean_vec <- colMeans(SCALED)  # Means of the columns
cov_mat <- cov(SCALED)        # Covariance matrix

# Compute Mahalanobis distances
mdist <- mahalanobis(SCALED, mean_vec, cov_mat)

# Define degrees of freedom
p <- ncol(SCALED)  # Number of variables

# Compute theoretical quantiles from Chi-square distribution
qq_chi2 <- qchisq(ppoints(nrow(SCALED)), df = p)


# Q-Q Plot for Mahalanobis distances
qqplot(qq_chi2, sort(mdist),
       main = "Q-Q Plot of Mahalanobis Distances",
       xlab = "Theoretical Quantiles (Chi-Square)",
       ylab = "Mahalanobis Distances",
       pch = 21, col = "black")

# Add a reference line
abline(0, 1, col = "blue", lwd = 2)
# The data look fairly non-multivariate normal

# Compute Mardia’s test manually
library(MVN)
# Perform Mardia's test for multivariate normality
mardia_result <- MVN::mvn(data = SCALED, mvn_test = "mardia")

# Print the test results
print(mardia_result)






# EFA itself (restricted subset of variables) ----------------------------------
parallel <- fa.parallel(SCALED, n.iter=2000, fm="minres", fa="both")
jpeg("./EFA/parallel_EFA_PCA_sim.jpeg", 
     width = 5, height = 4, units = 'in', res = 150)
fa.parallel(SCALED, n.iter=2000, fm="minres", fa="both")
dev.off()


str(SCALED)
# 3 Factors No Rotation
VSS(SCALED, n=5, rotate="none", fm="minres")
FACTORS_NONE <- fa(SCALED, 3, rotate="none", fm="minres")
FACTORS_NONE
fa.diagram(FACTORS_NONE)

# 3 Factors Varimax
VSS(SCALED, n=5, rotate="varimax", fm="minres")
FACTORS_VARIMAX <- fa(SCALED, 3, rotate="varimax", fm="minres")
FACTORS_VARIMAX
fa.diagram(FACTORS_VARIMAX, sort=TRUE, cut=0, digits=2, main="Model 2", cex=0.75)

# 3 Factors Promax
VSS(SCALED, n=5, rotate="promax", fm="minres")
FACTORS_PROMAX <- fa(SCALED, 3, rotate="promax", fm="minres")
FACTORS_PROMAX
fa.diagram(FACTORS_PROMAX, sort=TRUE, cut=0, digits=2, main="Model 2", cex=0.75)

# 3 Factor Oblimin
VSS(SCALED, n=5, rotate="oblimin", fm="minres")
FACTORS_OBLIMIN3 <- fa(SCALED, 3, rotate="oblimin", fm="minres")
sink("./EFA/output_3factor_oblimin.txt")
FACTORS_OBLIMIN3
sink()

jpeg("./EFA/EFA_oblimin3.jpeg", 
     width = 8, height = 8, units = 'in', res = 150)
fa.diagram(FACTORS_OBLIMIN3, sort=TRUE, cut=0, digits=2, main="Model 2", cex=0.75)
dev.off()



# Saving FA values ----
factor.scores(SCALED, FACTORS_OBLIMIN3)
factor.scores(SCALED, FACTORS_OBLIMIN3)$scores
factor.scores(SCALED, FACTORS_OBLIMIN3)$scores[,"MR1"]

head(ACCEL_DATA)
ACCEL_DATA <- ACCEL_DATA %>%
  mutate(duration = factor.scores(SCALED, FACTORS_OBLIMIN3)$scores[,"MR1"],
         vigor = factor.scores(SCALED, FACTORS_OBLIMIN3)$scores[,"MR3"],
         variability = factor.scores(SCALED, FACTORS_OBLIMIN3)$scores[,"MR2"],
  )

# Exporting the data with factor scores ----------------------------------------
write.csv(ACCEL_DATA, 
          paste("./data_accelMeansWithFactorScores",
          Sys.Date(), ".csv", sep=""))


# Calculating Cronbach's Alpha for each factor ---------------------------------
# Factor 1 - Duration
psych::alpha(SCALED[, c("l_time", "r_time", "simultaneous_time")])

# Factor 2 - Vigor 
psych::alpha(SCALED[, c(
                 "l_magnitude_sd",
                 "r_magnitude_sd",
                 "l_magnitude",
                 "r_magnitude")])

# Factor 3 - Variance
psych::alpha(SCALED[, c("l_sd_freq",
                 "r_sd_freq",
                 "l_jerk",
                 "r_jerk")])



# Plot Figure 1B: Factor Scores by Twin Type -----------------------------------
colnames(ACCEL_DATA)

library(ggpubr);library(patchwork)

PLOT_DAT <- ACCEL_DATA %>% select(family_id, child_id, zygosity,
                                  duration, vigor, variability) %>%
  pivot_longer(cols=duration:variability, names_to = "variable",  values_to = "value") %>%
  pivot_wider(names_from = child_id, values_from = value) %>%
  mutate(zygosity = fct_relevel(zygosity, "MZ", "DZ"))

str(PLOT_DAT)

# f1_sol3
a<-ggplot(data=PLOT_DAT %>% filter(variable=="duration"), 
          aes(x=`101`, y=`102`)) +
  ggtitle(label = "F1 - 'Duration'")+
  geom_point(aes(col=zygosity), shape=16)+
  stat_smooth(aes(lty=zygosity), col="black", method="lm", se=FALSE)+
  stat_cor(data=PLOT_DAT %>% 
             filter(variable=="duration")%>%
             group_by(family_id) %>%
             summarise(zygosity=zygosity[1],
                       `101` = mean(`101`, na.rm=TRUE),
                       `102` = mean(`102`, na.rm=TRUE)),
           aes(label = paste(after_stat(rr.label), sep = "~`,`~")),
           r.digits = 2) +
  scale_color_manual(values=c("grey40", "grey60")) +
  scale_x_continuous(name = "Twin 1", limits=c(-3,3)) +
  scale_y_continuous(name = "Twin 2", limits=c(-3,3)) +
  facet_wrap(~zygosity) +
  theme_bw()+
  theme(axis.text.x =element_text(size=12, color="black", angle=45),
        axis.text.y =element_text(size=12, color="black"),
        legend.text=element_text(size=12, color="black"),
        legend.title=element_text(size=12, face="bold"),
        #axis.title=element_blank(),
        plot.title=element_text(size=12, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=12, face="bold"),
        legend.position = "none")

ggsave(filename="./f1_duration.jpeg",
       plot = a,
       width = 6,
       height = 3,
       units = "in",
       dpi = 300)

# f2_sol3
b<-ggplot(data=PLOT_DAT %>% filter(variable=="vigor"), 
          aes(x=`101`, y=`102`)) +
  ggtitle(label = "F2 - 'Vigor'")+
  geom_point(aes(col=zygosity), shape=16)+
  stat_smooth(aes(lty=zygosity), col="black", method="lm", se=FALSE)+
  stat_cor(data=PLOT_DAT %>% 
             filter(variable=="vigor")%>%
             group_by(family_id) %>%
             summarise(zygosity=zygosity[1],
                       `101` = mean(`101`, na.rm=TRUE),
                       `102` = mean(`102`, na.rm=TRUE)),
           aes(label = paste(after_stat(rr.label), sep = "~`,`~")),
           r.digits = 2) +
  scale_color_manual(values=c("grey40", "grey60")) +
  scale_x_continuous(name = "Twin 1", limits=c(-3,3)) +
  scale_y_continuous(name = "Twin 2", limits=c(-3,3)) +
  facet_wrap(~zygosity) +
  theme_bw()+
  theme(axis.text.x =element_text(size=12, color="black", angle=45),
        axis.text.y =element_text(size=12, color="black"),
        legend.text=element_text(size=12, color="black"),
        legend.title=element_text(size=12, face="bold"),
        #axis.title=element_blank(),
        plot.title=element_text(size=12, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=12, face="bold"),
        legend.position = "none")

ggsave(filename="./f2_vigor.jpeg",
       plot = b,
       width = 6,
       height = 3,
       units = "in",
       dpi = 300)



# f3_sol3
c<-ggplot(data=PLOT_DAT %>% filter(variable=="variability"), 
          aes(x=`101`, y=`102`)) +
  ggtitle(label = "F3 - 'Variability'")+
  geom_point(aes(col=zygosity), shape=16)+
  stat_smooth(aes(lty=zygosity), col="black", method="lm", se=FALSE)+
  stat_cor(data=PLOT_DAT %>% 
             filter(variable=="variability")%>%
             group_by(family_id) %>%
             summarise(zygosity=zygosity[1],
                       `101` = mean(`101`, na.rm=TRUE),
                       `102` = mean(`102`, na.rm=TRUE)),
           aes(label = paste(after_stat(rr.label), sep = "~`,`~")),
           r.digits = 2) +
  scale_color_manual(values=c("grey40", "grey60")) +
  scale_x_continuous(name = "Twin 1", limits=c(-3,3)) +
  scale_y_continuous(name = "Twin 2", limits=c(-3,3)) +
  facet_wrap(~zygosity) +
  theme_bw()+
  theme(axis.text.x =element_text(size=12, color="black", angle=45),
        axis.text.y =element_text(size=12, color="black"),
        legend.text=element_text(size=12, color="black"),
        legend.title=element_text(size=12, face="bold"),
        #axis.title=element_blank(),
        plot.title=element_text(size=12, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=12, face="bold"),
        legend.position = "none")

ggsave(filename="./f3_variability.jpeg",
       plot = c,
       width = 6,
       height = 3,
       units = "in",
       dpi = 300)


(a/b/c)


ggsave(filename="./figure1_B.jpeg",
       plot = last_plot(),
       width = 6,
       height = 9,
       units = "in",
       dpi = 600)




# Correlations with ASQ Gross and Fine Motor Scores at 6-Months ----------------
head(ACCEL_DATA)
head(DATA)

ACCEL_DATA <- merge(
  x = ACCEL_DATA,
  y = DATA %>% 
    select(record_id, family_id,  
           agestage_grossscore, agestage_finescore) %>%
    group_by(record_id) %>%
    slice(1),
  by = c("record_id", "family_id")
)

head(ACCEL_DATA)


# Partial correlations from mixed model with random-effect of family ----------
# Append ages and stages into var_list
accel_list <- c("duration", "vigor", "variability")
var_list <- c("agestage_finescore", "agestage_grossscore")


# mixed model Random effect of family
library(lme4); library(lmerTest)
summary(lmer(scale(agestage_grossscore)~scale(duration)+(1|family_id),
             data=ACCEL_DATA, REML=TRUE))

# Looping through to plot adjusted associations
COR <- c(NULL)
VAR1 <- c(NULL)
VAR2 <- c(NULL)

ADJ_BETA <- c(NULL)
ADJ_SE <- c(NULL)
ADJ_P <- c(NULL)

setwd("C:/Users/user/Box/Infant Motor R01/WU Only Infant Motor R01/MZ-DZ_Comparisons/MzDz_Twins/accel_heritability_2026_02_11/asd_associations")

k=1
for(v in var_list) {
  print(v)
  
  for(a in accel_list) {
    print(a)
    
    # create temporary plotting dataframe with family means
    PLOT_DATA <- ACCEL_DATA %>%
      dplyr::group_by(family_id) %>%
      dplyr::mutate(
        accel = .data[[a]],
        asd = .data[[v]],
        accel_family_mean = mean(.data[[a]], na.rm = TRUE),
        asd_family_mean = mean(.data[[v]], na.rm = TRUE)
      ) %>%
      dplyr::ungroup()
    
    jpeg(paste("scatter_", v, "_", a, ".jpeg", sep=""), 
         width = 4, height = 3, units = 'in', res = 150)
    
    # plot showing within and between family effects ----
    print(
      ggplot(PLOT_DATA,
             aes(x = accel, y = asd)) +
        
        # connect twins
        geom_line(aes(group = family_id), alpha = 0.5, color = "grey40") +
        
        # individual twins
        geom_point(color = "grey40", alpha = 0.5) +
        
        # overall regression
        geom_smooth(method = "lm", se = FALSE, color = "black", lty = 2) +
        
        # family mean points
        geom_point(
          data = PLOT_DATA %>% group_by(family_id) %>% slice(1),
          aes(x = accel_family_mean, y = asd_family_mean),
          fill = "orange3",
          shape=21,
          size = 2
        ) +
        
        # between-family regression
        geom_smooth(
          data = PLOT_DATA %>% group_by(family_id) %>% slice(1),
          aes(x = accel_family_mean, y = asd_family_mean),
          method = "lm",
          se = FALSE,
          color = "orange3"
        ) +
        
        scale_x_continuous(name = a) +
        scale_y_continuous(name = v) +
        
        theme_bw() +
        theme(axis.text.x = element_text(size = 10),
              legend.text = element_text(size = 8, color = "black"),
              legend.title = element_text(size = 8, face = "bold"),
              axis.title = element_text(size = 10, face = "bold"),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank(),
              strip.text = element_text(size = 10, face = "bold"),
              legend.position = "none")
    )
    
    # print(ggplot(data=ACCEL_DATA, aes_string(y=eval(a), x=eval(v))) + 
    #         geom_point(col="black", alpha=0.5)+
    #         stat_smooth(method="lm", se=FALSE, col="black", lty=2)+
    #         scale_x_continuous(name = eval(v)) +
    #         scale_y_continuous(name = eval(a)) +
    #         theme_bw()+
    #         theme(axis.text.x = element_text(size=10),
    #               legend.text=element_text(size=8, color="black"),
    #               legend.title=element_text(size=8, face="bold"),
    #               axis.title=element_text(size=10, face="bold"),
    #               #plot.title=element_text(size=14, face="bold", hjust=0.5),
    #               panel.grid.minor = element_blank(),
    #               panel.grid.major = element_blank(),
    #               strip.text = element_text(size=10, face="bold"),
    #               legend.position = "none")
    # )
    
    dev.off()
    
    VAR1[k] <- eval(v)
    VAR2[k] <- eval(a)
    COR[k] <- cor(ACCEL_DATA[,eval(v)], ACCEL_DATA[,eval(a)], use="complete", method="pearson")
    
    mixed_formula <- paste("scale(", eval(v), ")~scale(", eval(a), ")+(1|family_id)")
    
    mixed_mod<-summary(lmer(mixed_formula,
                            data=ACCEL_DATA, REML=TRUE))$coefficients
    
    ADJ_BETA[k] <- mixed_mod[2]
    ADJ_SE[k] <- mixed_mod[4]
    ADJ_P[k] <- mixed_mod[10]
    
    k=k+1
  }
} 


ASQ_RESULTS <- data.frame(VAR1, VAR2, COR,
                          ADJ_BETA, ADJ_SE, ADJ_P)

write.csv(ASQ_RESULTS, paste("./accel_to_ASQ_cors", Sys.Date(), ".csv", sep=""))

ASQ_RESULTS

ASQ_RESULTS <- ASQ_RESULTS %>% 
  mutate(VAR1=fct_recode(VAR1, 
                         `Fine Motor`="agestage_finescore",
                         `Gross Motor` = "agestage_grossscore"),
         VAR2=fct_recode(VAR2, 
                         Duration ="duration",
                         Vigor = "vigor",
                         Variability = "variability"),
         VAR2 = fct_relevel(VAR2,
                            "Duration",
                            "Vigor",
                            "Variability"
                            )
         )


ggplot(data=ASQ_RESULTS, aes(x=VAR2, y=ADJ_BETA)) +
  geom_hline(aes(yintercept=0), col="black", lty=2)+
  geom_point(aes(col=VAR1), shape=16, size=1.5,
             position = position_dodge(width=0.5))+
  geom_errorbar(aes(ymin=ADJ_BETA-2*ADJ_SE,
                    ymax=ADJ_BETA+2*ADJ_SE,
                    col=VAR1), width=0.2,
                position = position_dodge(width=0.5))+
  scale_x_discrete(name = "6-Month\n Accelerometry Variable", limits=rev) +
  scale_y_continuous(name = "Adjusted Correlations from Mixed Effect Model", limits=c(-0.5, 0.5)) +
  scale_color_manual(values=c("black", "grey60"))+
  coord_flip() +
  theme_bw()+
  labs(color="6-Month ASQ Measures")+
  theme(axis.text=element_text(size=10, color="black"),
        legend.text=element_text(size=10, color="black"),
        legend.title=element_text(size=10, face="bold"),
        axis.title=element_text(size=10, face="bold"),
        plot.title=element_text(size=12, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=10, face="bold"),
        legend.position = "bottom")

setwd("C:/Users/user/Box/Infant Motor R01/WU Only Infant Motor R01/MZ-DZ_Comparisons/MzDz_Twins/accel_heritability_2026_02_11/")
ggsave(
  filename="./figure2A.jpeg",
  plot = last_plot(),
  width = 5,
  height = 3,
  units = "in",
  dpi = 300
)



