
# 1. Packages to install/load ----
library(tidyverse)


# 2. Format data and check distributions ----
setwd("C:/Users/user/Box/Infant Motor R01/WU Only Infant Motor R01/MZ-DZ_Comparisons/MzDz_Twins/accel_heritability_2026_02_11/")
list.files()

# Read in unlabelled data - has colnames we want from REDCAP
NAMES <- read.csv("InfantMotorDevelopme-ZygosityAndScoresRep_DATA_2026-02-11_1247.csv",
                 header=TRUE, stringsAsFactors = TRUE, na.strings = c("", "NA"))
# make sure to add na.strings = "" because REDCAP does not indicate NAs.
# also need to include "NA" because that string appears to have been entered
# for some entropy values

# Read in labelled data - codes string variables as factors in REDCAP
DATA <- read.csv("InfantMotorDevelopme-ZygosityAndScoresRep_DATA_LABELS_2026-02-11_1247.csv",
                 header = TRUE, stringsAsFactors = TRUE, na.strings = c("", "NA"))
# Check that both data frames are the same size
# also need to include "NA" because that string appears to have been entered
# for some entropy values


# Replace labelled colnames with (shorter) unlabelled colnames
colnames(DATA) <- colnames(NAMES)

str(DATA)
colnames(DATA)
DATA[DATA$record_id=="EU1061101",]
DATA[DATA$record_id=="EU1061102",]
DATA[DATA$record_id=="WU1067101",]
DATA[DATA$record_id=="WU1067102",]


demo_vars <- c("include",
               "sex", "agemonths_6", "ethnicity", "race", "final_twintype", "height_6", "weight_6",
               "household_earnings_6", "caregiver1_edu_6", "caregiver2_edu_6", 
               "agestage_comscore_4", "agestage_grossscore_4", 
               "agestage_finescore_4", "agestage_probsolvscore_4", "agestage_persocscore_4",
               "agestage_comscore_6", "agestage_grossscore_6", "agestage_finescore_6", 
               "agestage_probsolvscore_6", "agestage_persocscore_6", 
               "agestage_comscore_8", "agestage_grossscore_8", "agestage_finescore_8",
               "agestage_probsolvscore_8", "agestage_persocscore_8")

# STROBE FLOW: Number of unique IDs in RED CAP minus "test" cases ----
unique(DATA$record_id)
length(unique(DATA$record_id))-6
length(unique(DATA %>% mutate(family_id = factor(str_sub(record_id, 1, 6))) %>% pull(family_id)))-6

# Filter out just the ACCEL data and zygosity information
DATA <- DATA %>% 
  filter(is.na(redcap_repeat_instrument) == TRUE) %>% # we need to remove extra rows for eye tracking coding
  select(record_id, redcap_event_name, any_of(demo_vars))%>%
  group_by(record_id) %>%
  fill(any_of(demo_vars), .direction="updown") %>% # fill in all missing values of include by subject
  ungroup() %>%
  filter(include == "Yes") %>%
  mutate(record_id = factor(record_id)) 

str(DATA)

# STROBE FLOW: Recalculate for Include = "no" removed ----
length(unique(DATA$record_id))

DATA[DATA$record_id=="EU1061101",]
DATA[DATA$record_id=="WU1067101",]

length(unique(DATA %>% mutate(family_id = factor(str_sub(record_id, 1, 6))) %>% pull(family_id)))


# Import Chunked Accelerometry Data from Box ---- 
list.files()
ACCEL <- read.csv("data_chunkAGGREGATED2026-02-11.csv",
                  header = TRUE, stringsAsFactors = TRUE, na.strings = c("", "NA")) %>%
  select(-X)

str(ACCEL)

# Prune down record_ids to match
length(unique(DATA$record_id))
length(unique(ACCEL$record_id))

summary(DATA$redcap_event_name)
summary(ACCEL$redcap_event_name) # I need to replace the redcap event name from 
# the ACCEL data

ACCEL <- ACCEL %>% 
  mutate(redcap_event_name = factor(ifelse(redcap_event_name=="enrollment__6_mont_arm_1",
                                    "Enrollment & 6 Months", NA))) %>%
  filter(record_id %in% unique(DATA$record_id)) # remove anyone who was include =="no" in main dataset


# Merging Demographic data from REDCAP with ACCEL data from Box ----
ACCEL <- merge(x=ACCEL,
               y=DATA,
               by = c("record_id", "redcap_event_name"),
               all.x = TRUE # keep only those participants with IDs from ACCEL
               ) %>%
  arrange(record_id, chunk)

# STROBE FLOW: Participants with processed 6-month accel data ----
length(unique(ACCEL$record_id))
length(unique(ACCEL$family_id))


# Confirm coding of REDCAP event name
summary(ACCEL$redcap_event_name)

colnames(ACCEL)
ACCEL <- ACCEL %>% 
  # fill in family level variables with missing data
  group_by(family_id) %>%
  fill(any_of(c("ethnicity", "race", "final_twintype",
                "household_earnings_6", "caregiver1_edu_6", "caregiver2_edu_6")), 
       .direction="updown") %>%
  ungroup() %>%
  mutate(
    final_twintype = trimws(as.character(final_twintype)),
    
    zygosity = case_when(
      grepl("Monozygotic", final_twintype) ~ "MZ",
      grepl("Dizygotic",  final_twintype) ~ "DZ",
      TRUE                                ~ NA_character_
    ),
    
    sexType = case_when(
      grepl("Different sex Dizygotic", final_twintype) ~ "Different",
      grepl("Like-sex Dizygotic",       final_twintype) ~ "Same",
      TRUE                                              ~ NA_character_
    ),
    
    zygosity = factor(zygosity, levels = c("MZ", "DZ")),
    sexType  = factor(sexType,  levels = c("Same", "Different")),
    family_id = factor(str_sub(record_id, 1, 6))
  ) %>%
  relocate(record_id, family_id, child_id, redcap_event_name, final_twintype, zygosity, sexType, any_of(demo_vars))
  


# Adding in transformations of the duration variables --------------------------
colnames(ACCEL)
# total_no_mvt_time"      "total_mvt_time"         "l_time"                 "l_only_time"           
# "r_time"                 "r_only_time"           "simultaneous_time"
# are all expressed as active proportions of the total waking time (i.e., wear_time)
# as such, they capture variance from two domains active time and wake time
# to unconfound this, I will create "RAW_" versions of each variable below

ACCEL <- ACCEL %>%
  mutate(
    RAW_no_mvt_time = round((total_no_mvt_time*wear_time)/60 ,3),
    RAW_mvt_time = round((total_mvt_time*wear_time)/60 ,3),
    RAW_simultaneous_time = round((simultaneous_time*wear_time)/60 ,3),
    RAW_l_time = round((l_time*wear_time)/60 ,3), 
    RAW_r_time = round((r_time*wear_time)/60 ,3), 
    RAW_l_only_time = round((l_only_time*wear_time)/60 ,3), 
    RAW_r_only_time = round((r_only_time*wear_time)/60 ,3)
  ) 

plot(x=ACCEL$l_time, y=ACCEL$r_time)
plot(x=ACCEL$RAW_l_time, y=ACCEL$RAW_r_time)
plot(x=ACCEL$l_time, y=ACCEL$RAW_l_time)
plot(x=ACCEL$r_time, y=ACCEL$RAW_r_time)


# Check the Cross Tabs to ensure we have ~9 chunks for every subject (Spanning Day 1 and Day 2) 
# for every subject:redcap event
xtabs(~ACCEL$record_id)
xtabs(~ACCEL$family_id)
xtabs(~ACCEL$record_id+ACCEL$redcap_event_name)
xtabs(~ACCEL$family_id+ACCEL$child_id)


# Filtering to Complete Data/Twin Pairs ----------------------------------------
MISSING <- ACCEL %>%
  group_by(family_id, child_id) %>%
  summarize(count=n()) %>%
  pivot_wider(names_from = child_id,
              values_from = count) %>%
  filter(is.na(`101`)|is.na(`102`)) # shows families with no observations for at least one kid

# STROBE FLOW: Missing twin pairs
MISSING

length(unique(ACCEL$record_id))
length(unique(ACCEL$family_id))

# unknown twin type
ACCEL %>% 
  select(record_id, family_id, child_id, final_twintype) %>%
  filter(is.na(final_twintype)==TRUE) %>%
  group_by(record_id) %>%
  slice(1)

# like-sex unknown
ACCEL %>% 
  select(record_id, family_id, child_id, final_twintype) %>%
  filter(final_twintype == "Like-sex Unknown") %>%
  group_by(record_id) %>%
  slice(1)

ACCEL <- ACCEL %>% 
  filter(!family_id %in% unique(MISSING$family_id)) %>% # Remove families with missing twins from accel data
  filter(is.na(final_twintype)==FALSE) %>% # Remove families with missing Twin Type information
  filter(final_twintype != "Like-sex Unknown") %>% # remove families for whom twin type is unknown
  mutate(record_id = factor(record_id),
         family_id = factor(family_id))


# STROBE FLOW: Remaining twin pairs with complete accel data after exclusions
length(unique(ACCEL$record_id))
length(unique(ACCEL$family_id))

xtabs(~ACCEL$record_id)
xtabs(~ACCEL$family_id)
xtabs(~ACCEL$record_id+ACCEL$redcap_event_name)
xtabs(~ACCEL$family_id+ACCEL$child_id)




# Merging the ages and stages 4, 6, 8 data into a single variable  -------------
# data could have been collected a 4, 6, or 8 month depending on availability
# kids were assessed with the age appropriate instrument
# for our purposes, we will merge these into one variable

ACCEL <- ACCEL %>%
  mutate(
    agestage_comscore = coalesce(agestage_comscore_4, agestage_comscore_6, agestage_comscore_8),
    agestage_grossscore = coalesce(agestage_grossscore_4, agestage_grossscore_6, agestage_grossscore_8),
    agestage_finescore = coalesce(agestage_finescore_4, agestage_finescore_6, agestage_finescore_8),
    agestage_probsolvscore = coalesce(agestage_probsolvscore_4, agestage_probsolvscore_6, agestage_probsolvscore_8),
    agestage_persocscore = coalesce(agestage_persocscore_4, agestage_persocscore_6, agestage_persocscore_8)
    )


# Outputting Demographic and Summary Statistics --------------------------------
str(ACCEL)
colnames(ACCEL)

# define accel vars
accel_vars<-c("file_id", "chunk", 
              "wear_time", "total_no_mvt_time", "total_mvt_time", "l_time",
              "r_time", "simultaneous_time", "l_only_time", "r_only_time", 
              "l_magnitude", "r_magnitude", "bilateral_magnitude", "l_magnitude_sd",
              "r_magnitude_sd", "l_peak_magnitude", "r_peak_magnitude", "l_jerk",
              "r_jerk", "l_sd_freq", "r_sd_freq", "RAW_no_mvt_time",
              "RAW_mvt_time", "RAW_simultaneous_time",  "RAW_l_time", "RAW_r_time",
              "RAW_l_only_time", "RAW_r_only_time")

# define helper functions

create_summary_table <- function(data, unique_cutoff = 0.95) {
  
  map_dfr(names(data), function(var) {
    
    x <- data[[var]]
    
    # ------------------------------------
    # 1. EXCLUDE ID-LIKE VARIABLES
    # ------------------------------------
    nonmiss_n <- sum(!is.na(x))
    
    # If completely missing, just return a row describing missingness
    if (nonmiss_n == 0) {
      return(
        tibble(
          variable   = var,
          type       = class(x)[1],
          n          = 0,
          percent    = 0,
          mean       = NA_real_,
          sd         = NA_real_,
          median     = NA_real_,
          p25        = NA_real_,
          p75        = NA_real_,
          missing_n  = length(x),
          n_levels   = ifelse(is.factor(x), nlevels(x), NA_integer_)
        )
      )
    }
    
    prop_unique <- length(unique(na.omit(x))) / nonmiss_n
    
    if (prop_unique > unique_cutoff) {
      return(NULL)
    }
    
    # ------------------------------------
    # 2. CATEGORICAL VARIABLES
    # ------------------------------------
    if (is.factor(x) || is.character(x)) {
      
      if (is.character(x)) x <- factor(x)
      
      total_n <- length(x)
      missing_n <- sum(is.na(x))
      
      level_table <- table(x, useNA = "no")
      
      tibble(
        variable = var,
        level = names(level_table),
        n = as.numeric(level_table),
        percent = round(100 * as.numeric(level_table) / total_n, 2),
        mean = NA_real_,
        sd = NA_real_,
        median = NA_real_,
        p25 = NA_real_,
        p75 = NA_real_,
        missing_n = missing_n
      )
      
      # ------------------------------------
      # 3. CONTINUOUS VARIABLES
      # ------------------------------------
    } else if (is.numeric(x) || is.integer(x)) {
      
      tibble(
        variable = var,
        level = NA_character_,
        n = sum(!is.na(x)),
        percent = NA_real_,
        mean = mean(x, na.rm = TRUE),
        sd = sd(x, na.rm = TRUE),
        median = median(x, na.rm = TRUE),
        p25 = quantile(x, 0.25, na.rm = TRUE),
        p75 = quantile(x, 0.75, na.rm = TRUE),
        missing_n = sum(is.na(x))
      )
      
    } else {
      NULL
    }
  })
}



# Kid Level Demographic data
DEMO <- ACCEL %>% 
  select(!any_of(accel_vars)) %>%
  group_by(record_id) %>%
  slice(1) %>%
  ungroup()

str(DEMO)
kid_summary_overall <- create_summary_table(DEMO %>% select(final_twintype:agestage_persocscore))

# some kids appear to be missing values for the include variable?
DEMO %>% select(record_id, include) %>% filter(is.na(include)==TRUE)

kid_summary_MZ <- create_summary_table(DEMO %>% 
                                         select(final_twintype:agestage_persocscore) %>%
                                       filter(zygosity=="MZ"))

kid_summary_DZ <- create_summary_table(DEMO %>% 
                                         select(final_twintype:agestage_persocscore) %>%
                                         filter(zygosity=="DZ"))

write.csv(kid_summary_overall, paste("./data_SummaryOverall", Sys.Date(), ".csv", sep=""))
write.csv(kid_summary_MZ, paste("./data_SummaryMZonly", Sys.Date(), ".csv", sep=""))
write.csv(kid_summary_DZ, paste("./data_SummaryDZonly", Sys.Date(), ".csv", sep=""))




# Export Final Clean Accel Data ------------------------------------------------
write.csv(ACCEL, paste("./data_accelClean", Sys.Date(), ".csv", sep=""))




# Aggregating the Down to One observation per person for descriptive analyses ----
dynamic_vars <- c(
  "l_time", "r_time", "simultaneous_time",
  "RAW_l_time", "RAW_r_time", "RAW_simultaneous_time", 
  "l_magnitude", "r_magnitude",
  "l_peak_magnitude", "r_peak_magnitude", 
  "l_magnitude_sd", "r_magnitude_sd", 
  "l_jerk", "r_jerk",
  "l_sd_freq", "r_sd_freq")

colnames(ACCEL)
ACCEL_DATA <- ACCEL %>% 
  group_by(record_id) %>%
  summarize(
    family_id = family_id[1],
    zygosity = zygosity[1],
    sex = sex[1],
    sexType = sexType[1],
    across(all_of(dynamic_vars), mean, na.rm = TRUE))



# Tests of Statistically Signficant Differences for Table 1 --------------------
library(lme4); library(lmerTest)

colnames(DEMO)

sink("descriptiveComparisons_Table2.txt")
summary(lmer(agemonths_6~zygosity+(1|family_id), data=DEMO, REML=TRUE))
summary(lmer(height_6~zygosity+(1|family_id), data=DEMO, REML=TRUE))
summary(lmer(weight_6~zygosity+(1|family_id), data=DEMO, REML=TRUE))

fisher.test(DEMO$zygosity, DEMO$race)
fisher.test(DEMO$zygosity, DEMO$ethnicity)
fisher.test(DEMO$zygosity, DEMO$caregiver1_edu_6)
fisher.test(DEMO$zygosity, DEMO$caregiver2_edu_6)
fisher.test(DEMO$zygosity, DEMO$household_earnings_6, simulate.p.value=TRUE)
fisher.test(DEMO$zygosity, DEMO$sex)


print("agestage_comscore")
summary(lmer(agestage_comscore~zygosity+(1|family_id), data=DEMO, REML=TRUE))
print("agestage_grossscore")
summary(lmer(agestage_grossscore~zygosity+(1|family_id), data=DEMO, REML=TRUE))
print("agestage_finescore")
summary(lmer(agestage_finescore~zygosity+(1|family_id), data=DEMO, REML=TRUE))
print("agestage_probsolvscore")
summary(lmer(agestage_probsolvscore~zygosity+(1|family_id), data=DEMO, REML=TRUE))
print("agestage_persocscore")
summary(lmer(agestage_persocscore~zygosity+(1|family_id), data=DEMO, REML=TRUE))
sink()


# Supplemental Table i: Differences between Twin Pairs -------------------------
colnames(ACCEL_DATA)

library(effectsize)


sink("./supplemental_table_i.txt")
print("+---- Left Time ----+")
model <- lmer(l_time~zygosity+(1|family_id), data=ACCEL_DATA, REML=TRUE)
summary(model)
print("+---- MZ Mean and SD ----+")
mean(ACCEL_DATA[ACCEL_DATA$zygosity=="MZ",]$l_time)
sd(ACCEL_DATA[ACCEL_DATA$zygosity=="MZ",]$l_time)
print("+---- DZ Mean and SD ----+")
mean(ACCEL_DATA[ACCEL_DATA$zygosity=="DZ",]$l_time)
sd(ACCEL_DATA[ACCEL_DATA$zygosity=="DZ",]$l_time)
print("---- Cohen's D -----")
# Extract components
b         <- fixef(model)["zygosityDZ"]       # -0.01843
var_rand  <- as.numeric(VarCorr(model)$family_id)  # 0.002270
var_resid <- sigma(model)^2                         # 0.002398
sd_total  <- sqrt(var_rand + var_resid)

cohens_d  <- b / sd_total
cohens_d

print("+---- Right Time ----+")
model<-lmer(r_time~zygosity+(1|family_id), data=ACCEL_DATA, REML=TRUE)
summary(model)
print("+---- MZ Mean and SD ----+")
mean(ACCEL_DATA[ACCEL_DATA$zygosity=="MZ",]$r_time)
sd(ACCEL_DATA[ACCEL_DATA$zygosity=="MZ",]$r_time)
print("+---- DZ Mean and SD ----+")
mean(ACCEL_DATA[ACCEL_DATA$zygosity=="DZ",]$r_time)
sd(ACCEL_DATA[ACCEL_DATA$zygosity=="DZ",]$r_time)
print("---- Cohen's D -----")
# Extract components
b         <- fixef(model)["zygosityDZ"]       # -0.028
var_rand  <- as.numeric(VarCorr(model)$family_id)  # 0.002435
var_resid <- sigma(model)^2                         # 0.002847
sd_total  <- sqrt(var_rand + var_resid)

cohens_d  <- b / sd_total
cohens_d


print("+---- Simultaneous Time ----+")
model <- lmer(simultaneous_time~zygosity+(1|family_id), data=ACCEL_DATA, REML=TRUE)
summary(model)
print("+---- MZ Mean and SD ----+")
mean(ACCEL_DATA[ACCEL_DATA$zygosity=="MZ",]$simultaneous_time)
sd(ACCEL_DATA[ACCEL_DATA$zygosity=="MZ",]$simultaneous_time)
print("+---- DZ Mean and SD ----+")
mean(ACCEL_DATA[ACCEL_DATA$zygosity=="DZ",]$simultaneous_time)
sd(ACCEL_DATA[ACCEL_DATA$zygosity=="DZ",]$simultaneous_time)
print("---- Cohen's D -----")
# Extract components
b         <- fixef(model)["zygosityDZ"]       
var_rand  <- as.numeric(VarCorr(model)$family_id)  
var_resid <- sigma(model)^2                         
sd_total  <- sqrt(var_rand + var_resid)

cohens_d  <- b / sd_total
cohens_d


print("+---- RAW Left Time ----+")
model<-lmer(RAW_l_time~zygosity+(1|family_id), data=ACCEL_DATA, REML=TRUE)
summary(model)
print("+---- MZ Mean and SD ----+")
mean(ACCEL_DATA[ACCEL_DATA$zygosity=="MZ",]$RAW_l_time)
sd(ACCEL_DATA[ACCEL_DATA$zygosity=="MZ",]$RAW_l_time)
print("+---- DZ Mean and SD ----+")
mean(ACCEL_DATA[ACCEL_DATA$zygosity=="DZ",]$RAW_l_time)
sd(ACCEL_DATA[ACCEL_DATA$zygosity=="DZ",]$RAW_l_time)
print("---- Cohen's D -----")
# Extract components
b         <- fixef(model)["zygosityDZ"]       
var_rand  <- as.numeric(VarCorr(model)$family_id)  
var_resid <- sigma(model)^2                         
sd_total  <- sqrt(var_rand + var_resid)

cohens_d  <- b / sd_total
cohens_d

print("+---- RAW Right Time ----+")
model <- lmer(RAW_r_time~zygosity+(1|family_id), data=ACCEL_DATA, REML=TRUE)
summary(model)
print("+---- MZ Mean and SD ----+")
mean(ACCEL_DATA[ACCEL_DATA$zygosity=="MZ",]$RAW_r_time)
sd(ACCEL_DATA[ACCEL_DATA$zygosity=="MZ",]$RAW_r_time)
print("+---- DZ Mean and SD ----+")
mean(ACCEL_DATA[ACCEL_DATA$zygosity=="DZ",]$RAW_r_time)
sd(ACCEL_DATA[ACCEL_DATA$zygosity=="DZ",]$RAW_r_time)
print("---- Cohen's D -----")
# Extract components
b         <- fixef(model)["zygosityDZ"]       # -0.01843
var_rand  <- as.numeric(VarCorr(model)$family_id)  # 0.002270
var_resid <- sigma(model)^2                         # 0.002398
sd_total  <- sqrt(var_rand + var_resid)

cohens_d  <- b / sd_total
cohens_d

print("+---- RAW Simultaneous Time ----+")
model <-lmer(RAW_simultaneous_time~zygosity+(1|family_id), data=ACCEL_DATA, REML=TRUE)
summary(model)
print("+---- MZ Mean and SD ----+")
mean(ACCEL_DATA[ACCEL_DATA$zygosity=="MZ",]$RAW_simultaneous_time)
sd(ACCEL_DATA[ACCEL_DATA$zygosity=="MZ",]$RAW_simultaneous_time)
print("+---- DZ Mean and SD ----+")
mean(ACCEL_DATA[ACCEL_DATA$zygosity=="DZ",]$RAW_simultaneous_time)
sd(ACCEL_DATA[ACCEL_DATA$zygosity=="DZ",]$RAW_simultaneous_time)
print("---- Cohen's D -----")
# Extract components
b         <- fixef(model)["zygosityDZ"]       
var_rand  <- as.numeric(VarCorr(model)$family_id)  
var_resid <- sigma(model)^2                         
sd_total  <- sqrt(var_rand + var_resid)

cohens_d  <- b / sd_total
cohens_d

print("+---- Left Magnitude ----+")
model <-lmer(l_magnitude~zygosity+(1|family_id), data=ACCEL_DATA, REML=TRUE) 
summary(model)
print("+---- MZ Mean and SD ----+")
mean(ACCEL_DATA[ACCEL_DATA$zygosity=="MZ",]$l_magnitude)
sd(ACCEL_DATA[ACCEL_DATA$zygosity=="MZ",]$l_magnitude)
print("+---- DZ Mean and SD ----+")
mean(ACCEL_DATA[ACCEL_DATA$zygosity=="DZ",]$l_magnitude)
sd(ACCEL_DATA[ACCEL_DATA$zygosity=="DZ",]$l_magnitude)
print("---- Cohen's D -----")
# Extract components
b         <- fixef(model)["zygosityDZ"]       
var_rand  <- as.numeric(VarCorr(model)$family_id)  
var_resid <- sigma(model)^2                         
sd_total  <- sqrt(var_rand + var_resid)

cohens_d  <- b / sd_total
cohens_d

print("+---- Right Magnitude ----+")
model <- lmer(r_magnitude~zygosity+(1|family_id), data=ACCEL_DATA, REML=TRUE)
summary(model)
print("+---- MZ Mean and SD ----+")
mean(ACCEL_DATA[ACCEL_DATA$zygosity=="MZ",]$r_magnitude)
sd(ACCEL_DATA[ACCEL_DATA$zygosity=="MZ",]$r_magnitude)
print("+---- DZ Mean and SD ----+")
mean(ACCEL_DATA[ACCEL_DATA$zygosity=="DZ",]$r_magnitude)
sd(ACCEL_DATA[ACCEL_DATA$zygosity=="DZ",]$r_magnitude)
print("---- Cohen's D -----")
# Extract components
b         <- fixef(model)["zygosityDZ"]      
var_rand  <- as.numeric(VarCorr(model)$family_id)  
var_resid <- sigma(model)^2                         
sd_total  <- sqrt(var_rand + var_resid)

cohens_d  <- b / sd_total
cohens_d

print("+---- Left Magnitude SD ----+")
model <- lmer(l_magnitude_sd~zygosity+(1|family_id), data=ACCEL_DATA, REML=TRUE)
summary(model)
print("+---- MZ Mean and SD ----+")
mean(ACCEL_DATA[ACCEL_DATA$zygosity=="MZ",]$l_magnitude_sd)
sd(ACCEL_DATA[ACCEL_DATA$zygosity=="MZ",]$l_magnitude_sd)
print("+---- DZ Mean and SD ----+")
mean(ACCEL_DATA[ACCEL_DATA$zygosity=="DZ",]$l_magnitude_sd)
sd(ACCEL_DATA[ACCEL_DATA$zygosity=="DZ",]$l_magnitude_sd)
print("---- Cohen's D -----")
# Extract components
b         <- fixef(model)["zygosityDZ"]       
var_rand  <- as.numeric(VarCorr(model)$family_id)  
var_resid <- sigma(model)^2                         
sd_total  <- sqrt(var_rand + var_resid)

cohens_d  <- b / sd_total
cohens_d

print("+---- Right Magnitude SD ----+")
model <-lmer(r_magnitude_sd~zygosity+(1|family_id), data=ACCEL_DATA, REML=TRUE) 
summary(model)
print("+---- MZ Mean and SD ----+")
mean(ACCEL_DATA[ACCEL_DATA$zygosity=="MZ",]$r_magnitude_sd)
sd(ACCEL_DATA[ACCEL_DATA$zygosity=="MZ",]$r_magnitude_sd)
print("+---- DZ Mean and SD ----+")
mean(ACCEL_DATA[ACCEL_DATA$zygosity=="DZ",]$r_magnitude_sd)
sd(ACCEL_DATA[ACCEL_DATA$zygosity=="DZ",]$r_magnitude_sd)
print("---- Cohen's D -----")
# Extract components
b         <- fixef(model)["zygosityDZ"]      
var_rand  <- as.numeric(VarCorr(model)$family_id)  
var_resid <- sigma(model)^2                         
sd_total  <- sqrt(var_rand + var_resid)

cohens_d  <- b / sd_total
cohens_d


print("+---- Left Jerk ----+")
model <- lmer(l_jerk~zygosity+(1|family_id), data=ACCEL_DATA, REML=TRUE)
summary(model)
print("+---- MZ Mean and SD ----+")
mean(ACCEL_DATA[ACCEL_DATA$zygosity=="MZ",]$l_jerk)
sd(ACCEL_DATA[ACCEL_DATA$zygosity=="MZ",]$l_jerk)
print("+---- DZ Mean and SD ----+")
mean(ACCEL_DATA[ACCEL_DATA$zygosity=="DZ",]$l_jerk)
sd(ACCEL_DATA[ACCEL_DATA$zygosity=="DZ",]$l_jerk)
print("---- Cohen's D -----")
# Extract components
b         <- fixef(model)["zygosityDZ"]      
var_rand  <- as.numeric(VarCorr(model)$family_id)  
var_resid <- sigma(model)^2                         
sd_total  <- sqrt(var_rand + var_resid)

cohens_d  <- b / sd_total
cohens_d

print("+---- Right Jerk ----+")
model <- lmer(r_jerk~zygosity+(1|family_id), data=ACCEL_DATA, REML=TRUE)
summary(model)
print("+---- MZ Mean and SD ----+")
mean(ACCEL_DATA[ACCEL_DATA$zygosity=="MZ",]$r_jerk)
sd(ACCEL_DATA[ACCEL_DATA$zygosity=="MZ",]$r_jerk)
print("+---- DZ Mean and SD ----+")
mean(ACCEL_DATA[ACCEL_DATA$zygosity=="DZ",]$r_jerk)
sd(ACCEL_DATA[ACCEL_DATA$zygosity=="DZ",]$r_jerk)
print("---- Cohen's D -----")
# Extract components
b         <- fixef(model)["zygosityDZ"]       
var_rand  <- as.numeric(VarCorr(model)$family_id)  
var_resid <- sigma(model)^2                         
sd_total  <- sqrt(var_rand + var_resid)

cohens_d  <- b / sd_total
cohens_d

print("+---- Left SD Frequency ----+")
model <- lmer(l_sd_freq~zygosity+(1|family_id), data=ACCEL_DATA, REML=TRUE) 
summary(model)
print("+---- MZ Mean and SD ----+")
mean(ACCEL_DATA[ACCEL_DATA$zygosity=="MZ",]$l_sd_freq)
sd(ACCEL_DATA[ACCEL_DATA$zygosity=="MZ",]$l_sd_freq)
print("+---- DZ Mean and SD ----+")
mean(ACCEL_DATA[ACCEL_DATA$zygosity=="DZ",]$l_sd_freq)
sd(ACCEL_DATA[ACCEL_DATA$zygosity=="DZ",]$l_sd_freq)
print("---- Cohen's D -----")
# Extract components
b         <- fixef(model)["zygosityDZ"]      
var_rand  <- as.numeric(VarCorr(model)$family_id)  
var_resid <- sigma(model)^2                         
sd_total  <- sqrt(var_rand + var_resid)

cohens_d  <- b / sd_total
cohens_d

print("+---- Right SD Frequency ----+")
model <- lmer(r_sd_freq~zygosity+(1|family_id), data=ACCEL_DATA, REML=TRUE)
summary(model)
print("+---- MZ Mean and SD ----+")
mean(ACCEL_DATA[ACCEL_DATA$zygosity=="MZ",]$r_sd_freq)
sd(ACCEL_DATA[ACCEL_DATA$zygosity=="MZ",]$r_sd_freq)
print("+---- DZ Mean and SD ----+")
mean(ACCEL_DATA[ACCEL_DATA$zygosity=="DZ",]$r_sd_freq)
sd(ACCEL_DATA[ACCEL_DATA$zygosity=="DZ",]$r_sd_freq)
print("---- Cohen's D -----")
# Extract components
b         <- fixef(model)["zygosityDZ"]      
var_rand  <- as.numeric(VarCorr(model)$family_id)  
var_resid <- sigma(model)^2                         
sd_total  <- sqrt(var_rand + var_resid)

cohens_d  <- b / sd_total
cohens_d


print("+---- FDR Corrected P-Values across all tests ----+")
# FDR corrected p-values for 14 tests (peak magnitude dropped from analysis)
p.adjust(c(0.146, 0.357, 0.0637, 0.583, 0.391, 0.272, 0.342, 0.583,
           0.0597, 0.0166, 0.052, 0.00463, 0.207, 0.0743),
         method="fdr")


sink()





# Supplemental Table ii --------------------------------------------------------
str(ACCEL_DATA)

sink("supplementalTableii_sexComparisons.txt")
print("+---- Left Time ----+")
model <- lmer(l_time~sex+(1|family_id), data=ACCEL_DATA, REML=TRUE)
summary(model)
print("+---- Female Mean and SD ----+")
mean(ACCEL_DATA[ACCEL_DATA$sex=="Female",]$l_time, na.rm=TRUE)
sd(ACCEL_DATA[ACCEL_DATA$sex=="Female",]$l_time, na.rm=TRUE)
print("+---- DZ Mean and SD ----+")
mean(ACCEL_DATA[ACCEL_DATA$sex=="Male",]$l_time, na.rm=TRUE)
sd(ACCEL_DATA[ACCEL_DATA$sex=="Male",]$l_time, na.rm=TRUE)
print("---- Cohen's D -----")
# Extract components
b         <- fixef(model)["sexMale"]      
var_rand  <- as.numeric(VarCorr(model)$family_id)  
var_resid <- sigma(model)^2                         
sd_total  <- sqrt(var_rand + var_resid)

cohens_d  <- b / sd_total
cohens_d



print("+---- Right Time ----+")
model<-lmer(r_time~sex+(1|family_id), data=ACCEL_DATA, REML=TRUE)
summary(model)
print("+---- Female Mean and SD ----+")
mean(ACCEL_DATA[ACCEL_DATA$sex=="Female",]$r_time, na.rm=TRUE)
sd(ACCEL_DATA[ACCEL_DATA$sex=="Female",]$r_time, na.rm=TRUE)
print("+---- DZ Mean and SD ----+")
mean(ACCEL_DATA[ACCEL_DATA$sex=="Male",]$r_time, na.rm=TRUE)
sd(ACCEL_DATA[ACCEL_DATA$sex=="Male",]$r_time, na.rm=TRUE)
print("---- Cohen's D -----")
# Extract components
b         <- fixef(model)["sexMale"]      
var_rand  <- as.numeric(VarCorr(model)$family_id)  
var_resid <- sigma(model)^2                         
sd_total  <- sqrt(var_rand + var_resid)

cohens_d  <- b / sd_total
cohens_d




print("+---- Simultaneous Time ----+")
model <-lmer(simultaneous_time~sex+(1|family_id), data=ACCEL_DATA, REML=TRUE) 
summary(model)
print("+---- Female Mean and SD ----+")
mean(ACCEL_DATA[ACCEL_DATA$sex=="Female",]$simultaneous_time, na.rm=TRUE)
sd(ACCEL_DATA[ACCEL_DATA$sex=="Female",]$simultaneous_time, na.rm=TRUE)
print("+---- DZ Mean and SD ----+")
mean(ACCEL_DATA[ACCEL_DATA$sex=="Male",]$simultaneous_time, na.rm=TRUE)
sd(ACCEL_DATA[ACCEL_DATA$sex=="Male",]$simultaneous_time, na.rm=TRUE)
print("---- Cohen's D -----")
# Extract components
b         <- fixef(model)["sexMale"]      
var_rand  <- as.numeric(VarCorr(model)$family_id)  
var_resid <- sigma(model)^2                         
sd_total  <- sqrt(var_rand + var_resid)

cohens_d  <- b / sd_total
cohens_d



print("+---- RAW Left Time ----+")
model<-lmer(RAW_l_time~sex+(1|family_id), data=ACCEL_DATA, REML=TRUE)
summary(model)
print("+---- Female Mean and SD ----+")
mean(ACCEL_DATA[ACCEL_DATA$sex=="Female",]$RAW_l_time, na.rm=TRUE)
sd(ACCEL_DATA[ACCEL_DATA$sex=="Female",]$RAW_l_time, na.rm=TRUE)
print("+---- DZ Mean and SD ----+")
mean(ACCEL_DATA[ACCEL_DATA$sex=="Male",]$RAW_l_time, na.rm=TRUE)
sd(ACCEL_DATA[ACCEL_DATA$sex=="Male",]$RAW_l_time, na.rm=TRUE)
print("---- Cohen's D -----")
# Extract components
b         <- fixef(model)["sexMale"]      
var_rand  <- as.numeric(VarCorr(model)$family_id)  
var_resid <- sigma(model)^2                         
sd_total  <- sqrt(var_rand + var_resid)

cohens_d  <- b / sd_total
cohens_d


print("+---- RAW Right Time ----+")
model<-lmer(RAW_r_time~sex+(1|family_id), data=ACCEL_DATA, REML=TRUE)
summary(model)
print("+---- Female Mean and SD ----+")
mean(ACCEL_DATA[ACCEL_DATA$sex=="Female",]$RAW_r_time, na.rm=TRUE)
sd(ACCEL_DATA[ACCEL_DATA$sex=="Female",]$RAW_r_time, na.rm=TRUE)
print("+---- DZ Mean and SD ----+")
mean(ACCEL_DATA[ACCEL_DATA$sex=="Male",]$RAW_r_time, na.rm=TRUE)
sd(ACCEL_DATA[ACCEL_DATA$sex=="Male",]$RAW_r_time, na.rm=TRUE)
print("---- Cohen's D -----")
# Extract components
b         <- fixef(model)["sexMale"]      
var_rand  <- as.numeric(VarCorr(model)$family_id)  
var_resid <- sigma(model)^2                         
sd_total  <- sqrt(var_rand + var_resid)

cohens_d  <- b / sd_total
cohens_d



print("+---- RAW Simultaneous Time ----+")
model<-lmer(RAW_simultaneous_time~sex+(1|family_id), data=ACCEL_DATA, REML=TRUE)
summary(model)
print("+---- Female Mean and SD ----+")
mean(ACCEL_DATA[ACCEL_DATA$sex=="Female",]$RAW_simultaneous_time, na.rm=TRUE)
sd(ACCEL_DATA[ACCEL_DATA$sex=="Female",]$RAW_simultaneous_time, na.rm=TRUE)
print("+---- DZ Mean and SD ----+")
mean(ACCEL_DATA[ACCEL_DATA$sex=="Male",]$RAW_simultaneous_time, na.rm=TRUE)
sd(ACCEL_DATA[ACCEL_DATA$sex=="Male",]$RAW_simultaneous_time, na.rm=TRUE)
print("---- Cohen's D -----")
# Extract components
b         <- fixef(model)["sexMale"]      
var_rand  <- as.numeric(VarCorr(model)$family_id)  
var_resid <- sigma(model)^2                         
sd_total  <- sqrt(var_rand + var_resid)

cohens_d  <- b / sd_total
cohens_d




print("+---- Left Magnitude ----+")
model <- lmer(l_magnitude~sex+(1|family_id), data=ACCEL_DATA, REML=TRUE)
summary(model)
print("+---- Female Mean and SD ----+")
mean(ACCEL_DATA[ACCEL_DATA$sex=="Female",]$l_magnitude, na.rm=TRUE)
sd(ACCEL_DATA[ACCEL_DATA$sex=="Female",]$l_magnitude, na.rm=TRUE)
print("+---- DZ Mean and SD ----+")
mean(ACCEL_DATA[ACCEL_DATA$sex=="Male",]$l_magnitude, na.rm=TRUE)
sd(ACCEL_DATA[ACCEL_DATA$sex=="Male",]$l_magnitude, na.rm=TRUE)
print("---- Cohen's D -----")
# Extract components
b         <- fixef(model)["sexMale"]      
var_rand  <- as.numeric(VarCorr(model)$family_id)  
var_resid <- sigma(model)^2                         
sd_total  <- sqrt(var_rand + var_resid)

cohens_d  <- b / sd_total
cohens_d



print("+---- Right Magnitude ----+")
model <- lmer(r_magnitude~sex+(1|family_id), data=ACCEL_DATA, REML=TRUE)
summary(model)
print("+---- Female Mean and SD ----+")
mean(ACCEL_DATA[ACCEL_DATA$sex=="Female",]$r_magnitude, na.rm=TRUE)
sd(ACCEL_DATA[ACCEL_DATA$sex=="Female",]$r_magnitude, na.rm=TRUE)
print("+---- DZ Mean and SD ----+")
mean(ACCEL_DATA[ACCEL_DATA$sex=="Male",]$r_magnitude, na.rm=TRUE)
sd(ACCEL_DATA[ACCEL_DATA$sex=="Male",]$r_magnitude, na.rm=TRUE)
print("---- Cohen's D -----")
# Extract components
b         <- fixef(model)["sexMale"]      
var_rand  <- as.numeric(VarCorr(model)$family_id)  
var_resid <- sigma(model)^2                         
sd_total  <- sqrt(var_rand + var_resid)

cohens_d  <- b / sd_total
cohens_d




print("+---- Left Magnitude SD ----+")
model <- lmer(l_magnitude_sd~sex+(1|family_id), data=ACCEL_DATA, REML=TRUE)
summary(model)
print("+---- Female Mean and SD ----+")
mean(ACCEL_DATA[ACCEL_DATA$sex=="Female",]$l_magnitude_sd, na.rm=TRUE)
sd(ACCEL_DATA[ACCEL_DATA$sex=="Female",]$l_magnitude_sd, na.rm=TRUE)
print("+---- DZ Mean and SD ----+")
mean(ACCEL_DATA[ACCEL_DATA$sex=="Male",]$l_magnitude_sd, na.rm=TRUE)
sd(ACCEL_DATA[ACCEL_DATA$sex=="Male",]$l_magnitude_sd, na.rm=TRUE)
print("---- Cohen's D -----")
# Extract components
b         <- fixef(model)["sexMale"]      
var_rand  <- as.numeric(VarCorr(model)$family_id)  
var_resid <- sigma(model)^2                         
sd_total  <- sqrt(var_rand + var_resid)

cohens_d  <- b / sd_total
cohens_d


print("+---- Right Magnitude SD ----+")
model <- lmer(r_magnitude_sd~sex+(1|family_id), data=ACCEL_DATA, REML=TRUE)
summary(model)
print("+---- Female Mean and SD ----+")
mean(ACCEL_DATA[ACCEL_DATA$sex=="Female",]$r_magnitude_sd, na.rm=TRUE)
sd(ACCEL_DATA[ACCEL_DATA$sex=="Female",]$r_magnitude_sd, na.rm=TRUE)
print("+---- DZ Mean and SD ----+")
mean(ACCEL_DATA[ACCEL_DATA$sex=="Male",]$r_magnitude_sd, na.rm=TRUE)
sd(ACCEL_DATA[ACCEL_DATA$sex=="Male",]$r_magnitude_sd, na.rm=TRUE)
print("---- Cohen's D -----")
# Extract components
b         <- fixef(model)["sexMale"]      
var_rand  <- as.numeric(VarCorr(model)$family_id)  
var_resid <- sigma(model)^2                         
sd_total  <- sqrt(var_rand + var_resid)

cohens_d  <- b / sd_total
cohens_d



print("+---- Left Jerk ----+")
model <- lmer(l_jerk~sex+(1|family_id), data=ACCEL_DATA, REML=TRUE)
summary(model)
print("+---- Female Mean and SD ----+")
mean(ACCEL_DATA[ACCEL_DATA$sex=="Female",]$l_jerk, na.rm=TRUE)
sd(ACCEL_DATA[ACCEL_DATA$sex=="Female",]$l_jerk, na.rm=TRUE)
print("+---- DZ Mean and SD ----+")
mean(ACCEL_DATA[ACCEL_DATA$sex=="Male",]$l_jerk, na.rm=TRUE)
sd(ACCEL_DATA[ACCEL_DATA$sex=="Male",]$l_jerk, na.rm=TRUE)
print("---- Cohen's D -----")
# Extract components
b         <- fixef(model)["sexMale"]      
var_rand  <- as.numeric(VarCorr(model)$family_id)  
var_resid <- sigma(model)^2                         
sd_total  <- sqrt(var_rand + var_resid)

cohens_d  <- b / sd_total
cohens_d


print("+---- Right Jerk ----+")
model <- lmer(r_jerk~sex+(1|family_id), data=ACCEL_DATA, REML=TRUE)
summary(model)
print("+---- Female Mean and SD ----+")
mean(ACCEL_DATA[ACCEL_DATA$sex=="Female",]$r_jerk, na.rm=TRUE)
sd(ACCEL_DATA[ACCEL_DATA$sex=="Female",]$r_jerk, na.rm=TRUE)
print("+---- DZ Mean and SD ----+")
mean(ACCEL_DATA[ACCEL_DATA$sex=="Male",]$r_jerk, na.rm=TRUE)
sd(ACCEL_DATA[ACCEL_DATA$sex=="Male",]$r_jerk, na.rm=TRUE)
print("---- Cohen's D -----")
# Extract components
b         <- fixef(model)["sexMale"]      
var_rand  <- as.numeric(VarCorr(model)$family_id)  
var_resid <- sigma(model)^2                         
sd_total  <- sqrt(var_rand + var_resid)

cohens_d  <- b / sd_total
cohens_d


print("+---- Left SD Frequency ----+")
model <- lmer(l_sd_freq~sex+(1|family_id), data=ACCEL_DATA, REML=TRUE) 
summary(model)
print("+---- Female Mean and SD ----+")
mean(ACCEL_DATA[ACCEL_DATA$sex=="Female",]$l_sd_freq, na.rm=TRUE)
sd(ACCEL_DATA[ACCEL_DATA$sex=="Female",]$l_sd_freq, na.rm=TRUE)
print("+---- DZ Mean and SD ----+")
mean(ACCEL_DATA[ACCEL_DATA$sex=="Male",]$l_sd_freq, na.rm=TRUE)
sd(ACCEL_DATA[ACCEL_DATA$sex=="Male",]$l_sd_freq, na.rm=TRUE)
print("---- Cohen's D -----")
# Extract components
b         <- fixef(model)["sexMale"]      
var_rand  <- as.numeric(VarCorr(model)$family_id)  
var_resid <- sigma(model)^2                         
sd_total  <- sqrt(var_rand + var_resid)

cohens_d  <- b / sd_total
cohens_d


print("+---- Right SD Frequency ----+")
model <- lmer(r_sd_freq~sex+(1|family_id), data=ACCEL_DATA, REML=TRUE)
summary(model)
print("+---- Female Mean and SD ----+")
mean(ACCEL_DATA[ACCEL_DATA$sex=="Female",]$r_sd_freq, na.rm=TRUE)
sd(ACCEL_DATA[ACCEL_DATA$sex=="Female",]$r_sd_freq, na.rm=TRUE)
print("+---- DZ Mean and SD ----+")
mean(ACCEL_DATA[ACCEL_DATA$sex=="Male",]$r_sd_freq, na.rm=TRUE)
sd(ACCEL_DATA[ACCEL_DATA$sex=="Male",]$r_sd_freq, na.rm=TRUE)
print("---- Cohen's D -----")
# Extract components
b         <- fixef(model)["sexMale"]      
var_rand  <- as.numeric(VarCorr(model)$family_id)  
var_resid <- sigma(model)^2                         
sd_total  <- sqrt(var_rand + var_resid)

cohens_d  <- b / sd_total
cohens_d



print("+---- FDR Corrected P-Values across all tests ----+")
# FDR corrected p-values for 14 tests (peak magnitude dropped from analysis)
p.adjust(c(0.00549, 0.0579, 0.0141, 0.0663, 0.176, 0.0617, 0.054, 0.236, 
           0.681, 0.358, 0.00276, 0.0122, 0.000473, 0.00798),
         method="fdr")


sink()








# Supplemental Table iii -------------------------------------------------------
str(ACCEL_DATA)
DZ_ONLY <- ACCEL_DATA %>% filter(zygosity=="DZ")

sink("supplementalTableiii_sexTypeComparisons.txt")
print("+---- Left Time ----+")
model <- lmer(l_time~sexType+(1|family_id), data=DZ_ONLY, REML=TRUE)
summary(model)
print("+---- Same Sex Mean and SD ----+")
mean(DZ_ONLY[DZ_ONLY$sexType=="Same",]$l_time, na.rm=TRUE)
sd(DZ_ONLY[DZ_ONLY$sexType=="Same",]$l_time, na.rm=TRUE)
print("+---- Different Sex Mean and SD ----+")
mean(DZ_ONLY[DZ_ONLY$sexType=="Different",]$l_time, na.rm=TRUE)
sd(DZ_ONLY[DZ_ONLY$sexType=="Different",]$l_time, na.rm=TRUE)
print("---- Cohen's D -----")
# Extract components
b         <- fixef(model)["sexTypeDifferent"]      
var_rand  <- as.numeric(VarCorr(model)$family_id)  
var_resid <- sigma(model)^2                         
sd_total  <- sqrt(var_rand + var_resid)

cohens_d  <- b / sd_total
cohens_d



print("+---- Right Time ----+")
model<-lmer(r_time~sexType+(1|family_id), data=DZ_ONLY, REML=TRUE)
summary(model)
print("+---- Same Sex Mean and SD ----+")
mean(DZ_ONLY[DZ_ONLY$sexType=="Same",]$r_time, na.rm=TRUE)
sd(DZ_ONLY[DZ_ONLY$sexType=="Same",]$r_time, na.rm=TRUE)
print("+---- Different Sex Mean and SD ----+")
mean(DZ_ONLY[DZ_ONLY$sexType=="Different",]$r_time, na.rm=TRUE)
sd(DZ_ONLY[DZ_ONLY$sexType=="Different",]$r_time, na.rm=TRUE)
print("---- Cohen's D -----")
# Extract components
b         <- fixef(model)["sexTypeDifferent"]      
var_rand  <- as.numeric(VarCorr(model)$family_id)  
var_resid <- sigma(model)^2                         
sd_total  <- sqrt(var_rand + var_resid)

cohens_d  <- b / sd_total
cohens_d



print("+---- Simultaneous Time ----+")
model <- lmer(simultaneous_time~sexType+(1|family_id), data=DZ_ONLY, REML=TRUE)
summary(model)
print("+---- Same Sex Mean and SD ----+")
mean(DZ_ONLY[DZ_ONLY$sexType=="Same",]$simultaneous_time, na.rm=TRUE)
sd(DZ_ONLY[DZ_ONLY$sexType=="Same",]$simultaneous_time, na.rm=TRUE)
print("+---- Different Sex Mean and SD ----+")
mean(DZ_ONLY[DZ_ONLY$sexType=="Different",]$simultaneous_time, na.rm=TRUE)
sd(DZ_ONLY[DZ_ONLY$sexType=="Different",]$simultaneous_time, na.rm=TRUE)
print("---- Cohen's D -----")
# Extract components
b         <- fixef(model)["sexTypeDifferent"]      
var_rand  <- as.numeric(VarCorr(model)$family_id)  
var_resid <- sigma(model)^2                         
sd_total  <- sqrt(var_rand + var_resid)

cohens_d  <- b / sd_total
cohens_d


print("+---- RAW Left Time ----+")
model <-lmer(RAW_l_time~sexType+(1|family_id), data=DZ_ONLY, REML=TRUE) 
summary(model)
print("+---- Same Sex Mean and SD ----+")
mean(DZ_ONLY[DZ_ONLY$sexType=="Same",]$RAW_l_time, na.rm=TRUE)
sd(DZ_ONLY[DZ_ONLY$sexType=="Same",]$RAW_l_time, na.rm=TRUE)
print("+---- Different Sex Mean and SD ----+")
mean(DZ_ONLY[DZ_ONLY$sexType=="Different",]$RAW_l_time, na.rm=TRUE)
sd(DZ_ONLY[DZ_ONLY$sexType=="Different",]$RAW_l_time, na.rm=TRUE)
print("---- Cohen's D -----")
# Extract components
b         <- fixef(model)["sexTypeDifferent"]      
var_rand  <- as.numeric(VarCorr(model)$family_id)  
var_resid <- sigma(model)^2                         
sd_total  <- sqrt(var_rand + var_resid)

cohens_d  <- b / sd_total
cohens_d



print("+---- RAW Right Time ----+")
model <- lmer(RAW_r_time~sexType+(1|family_id), data=DZ_ONLY, REML=TRUE)
summary(model)
print("+---- Same Sex Mean and SD ----+")
mean(DZ_ONLY[DZ_ONLY$sexType=="Same",]$RAW_r_time, na.rm=TRUE)
sd(DZ_ONLY[DZ_ONLY$sexType=="Same",]$RAW_r_time, na.rm=TRUE)
print("+---- Different Sex Mean and SD ----+")
mean(DZ_ONLY[DZ_ONLY$sexType=="Different",]$RAW_r_time, na.rm=TRUE)
sd(DZ_ONLY[DZ_ONLY$sexType=="Different",]$RAW_r_time, na.rm=TRUE)
print("---- Cohen's D -----")
# Extract components
b         <- fixef(model)["sexTypeDifferent"]      
var_rand  <- as.numeric(VarCorr(model)$family_id)  
var_resid <- sigma(model)^2                         
sd_total  <- sqrt(var_rand + var_resid)

cohens_d  <- b / sd_total
cohens_d



print("+---- RAW Simultaneous Time ----+")
model <-lmer(RAW_simultaneous_time~sexType+(1|family_id), data=DZ_ONLY, REML=TRUE) 
summary(model)
print("+---- Same Sex Mean and SD ----+")
mean(DZ_ONLY[DZ_ONLY$sexType=="Same",]$RAW_simultaneous_time, na.rm=TRUE)
sd(DZ_ONLY[DZ_ONLY$sexType=="Same",]$RAW_simultaneous_time, na.rm=TRUE)
print("+---- Different Sex Mean and SD ----+")
mean(DZ_ONLY[DZ_ONLY$sexType=="Different",]$RAW_simultaneous_time, na.rm=TRUE)
sd(DZ_ONLY[DZ_ONLY$sexType=="Different",]$RAW_simultaneous_time, na.rm=TRUE)
print("---- Cohen's D -----")
# Extract components
b         <- fixef(model)["sexTypeDifferent"]      
var_rand  <- as.numeric(VarCorr(model)$family_id)  
var_resid <- sigma(model)^2                         
sd_total  <- sqrt(var_rand + var_resid)

cohens_d  <- b / sd_total
cohens_d


print("+---- Left Magnitude ----+")
model <- lmer(l_magnitude~sexType+(1|family_id), data=DZ_ONLY, REML=TRUE)
summary(model)
print("+---- Same Sex Mean and SD ----+")
mean(DZ_ONLY[DZ_ONLY$sexType=="Same",]$l_magnitude, na.rm=TRUE)
sd(DZ_ONLY[DZ_ONLY$sexType=="Same",]$l_magnitude, na.rm=TRUE)
print("+---- Different Sex Mean and SD ----+")
mean(DZ_ONLY[DZ_ONLY$sexType=="Different",]$l_magnitude, na.rm=TRUE)
sd(DZ_ONLY[DZ_ONLY$sexType=="Different",]$l_magnitude, na.rm=TRUE)
print("---- Cohen's D -----")
# Extract components
b         <- fixef(model)["sexTypeDifferent"]      
var_rand  <- as.numeric(VarCorr(model)$family_id)  
var_resid <- sigma(model)^2                         
sd_total  <- sqrt(var_rand + var_resid)

cohens_d  <- b / sd_total
cohens_d




print("+---- Right Magnitude ----+")
model <- lmer(r_magnitude~sexType+(1|family_id), data=DZ_ONLY, REML=TRUE)
summary(model)
print("+---- Same Sex Mean and SD ----+")
mean(DZ_ONLY[DZ_ONLY$sexType=="Same",]$r_magnitude, na.rm=TRUE)
sd(DZ_ONLY[DZ_ONLY$sexType=="Same",]$r_magnitude, na.rm=TRUE)
print("+---- Different Sex Mean and SD ----+")
mean(DZ_ONLY[DZ_ONLY$sexType=="Different",]$r_magnitude, na.rm=TRUE)
sd(DZ_ONLY[DZ_ONLY$sexType=="Different",]$r_magnitude, na.rm=TRUE)
print("---- Cohen's D -----")
# Extract components
b         <- fixef(model)["sexTypeDifferent"]      
var_rand  <- as.numeric(VarCorr(model)$family_id)  
var_resid <- sigma(model)^2                         
sd_total  <- sqrt(var_rand + var_resid)

cohens_d  <- b / sd_total
cohens_d




print("+---- Left Magnitude SD ----+")
model <- lmer(l_magnitude_sd~sexType+(1|family_id), data=DZ_ONLY, REML=TRUE)
summary(model)
print("+---- Same Sex Mean and SD ----+")
mean(DZ_ONLY[DZ_ONLY$sexType=="Same",]$l_magnitude_sd, na.rm=TRUE)
sd(DZ_ONLY[DZ_ONLY$sexType=="Same",]$l_magnitude_sd, na.rm=TRUE)
print("+---- Different Sex Mean and SD ----+")
mean(DZ_ONLY[DZ_ONLY$sexType=="Different",]$l_magnitude_sd, na.rm=TRUE)
sd(DZ_ONLY[DZ_ONLY$sexType=="Different",]$l_magnitude_sd, na.rm=TRUE)
print("---- Cohen's D -----")
# Extract components
b         <- fixef(model)["sexTypeDifferent"]      
var_rand  <- as.numeric(VarCorr(model)$family_id)  
var_resid <- sigma(model)^2                         
sd_total  <- sqrt(var_rand + var_resid)

cohens_d  <- b / sd_total
cohens_d




print("+---- Right Magnitude SD ----+")
model <- lmer(r_magnitude_sd~sexType+(1|family_id), data=DZ_ONLY, REML=TRUE)
summary(model)
print("+---- Same Sex Mean and SD ----+")
mean(DZ_ONLY[DZ_ONLY$sexType=="Same",]$r_magnitude_sd, na.rm=TRUE)
sd(DZ_ONLY[DZ_ONLY$sexType=="Same",]$r_magnitude_sd, na.rm=TRUE)
print("+---- Different Sex Mean and SD ----+")
mean(DZ_ONLY[DZ_ONLY$sexType=="Different",]$r_magnitude_sd, na.rm=TRUE)
sd(DZ_ONLY[DZ_ONLY$sexType=="Different",]$r_magnitude_sd, na.rm=TRUE)
print("---- Cohen's D -----")
# Extract components
b         <- fixef(model)["sexTypeDifferent"]      
var_rand  <- as.numeric(VarCorr(model)$family_id)  
var_resid <- sigma(model)^2                         
sd_total  <- sqrt(var_rand + var_resid)

cohens_d  <- b / sd_total
cohens_d




print("+---- Left Jerk ----+")
model <- lmer(l_jerk~sexType+(1|family_id), data=DZ_ONLY, REML=TRUE)
summary(model)
print("+---- Same Sex Mean and SD ----+")
mean(DZ_ONLY[DZ_ONLY$sexType=="Same",]$l_jerk, na.rm=TRUE)
sd(DZ_ONLY[DZ_ONLY$sexType=="Same",]$l_jerk, na.rm=TRUE)
print("+---- Different Sex Mean and SD ----+")
mean(DZ_ONLY[DZ_ONLY$sexType=="Different",]$l_jerk, na.rm=TRUE)
sd(DZ_ONLY[DZ_ONLY$sexType=="Different",]$l_jerk, na.rm=TRUE)
print("---- Cohen's D -----")
# Extract components
b         <- fixef(model)["sexTypeDifferent"]      
var_rand  <- as.numeric(VarCorr(model)$family_id)  
var_resid <- sigma(model)^2                         
sd_total  <- sqrt(var_rand + var_resid)

cohens_d  <- b / sd_total
cohens_d




print("+---- Right Jerk ----+")
model <- lmer(r_jerk~sexType+(1|family_id), data=DZ_ONLY, REML=TRUE)
summary(model)
print("+---- Same Sex Mean and SD ----+")
mean(DZ_ONLY[DZ_ONLY$sexType=="Same",]$r_jerk, na.rm=TRUE)
sd(DZ_ONLY[DZ_ONLY$sexType=="Same",]$r_jerk, na.rm=TRUE)
print("+---- Different Sex Mean and SD ----+")
mean(DZ_ONLY[DZ_ONLY$sexType=="Different",]$r_jerk, na.rm=TRUE)
sd(DZ_ONLY[DZ_ONLY$sexType=="Different",]$r_jerk, na.rm=TRUE)
print("---- Cohen's D -----")
# Extract components
b         <- fixef(model)["sexTypeDifferent"]      
var_rand  <- as.numeric(VarCorr(model)$family_id)  
var_resid <- sigma(model)^2                         
sd_total  <- sqrt(var_rand + var_resid)

cohens_d  <- b / sd_total
cohens_d




print("+---- Left SD Frequency ----+")
model <- lmer(l_sd_freq~sexType+(1|family_id), data=DZ_ONLY, REML=TRUE)
summary(model)
print("+---- Same Sex Mean and SD ----+")
mean(DZ_ONLY[DZ_ONLY$sexType=="Same",]$l_sd_freq, na.rm=TRUE)
sd(DZ_ONLY[DZ_ONLY$sexType=="Same",]$l_sd_freq, na.rm=TRUE)
print("+---- Different Sex Mean and SD ----+")
mean(DZ_ONLY[DZ_ONLY$sexType=="Different",]$l_sd_freq, na.rm=TRUE)
sd(DZ_ONLY[DZ_ONLY$sexType=="Different",]$l_sd_freq, na.rm=TRUE)
print("---- Cohen's D -----")
# Extract components
b         <- fixef(model)["sexTypeDifferent"]      
var_rand  <- as.numeric(VarCorr(model)$family_id)  
var_resid <- sigma(model)^2                         
sd_total  <- sqrt(var_rand + var_resid)

cohens_d  <- b / sd_total
cohens_d




print("+---- Right SD Frequency ----+")
model <-lmer(r_sd_freq~sexType+(1|family_id), data=DZ_ONLY, REML=TRUE) 
summary(model)
print("+---- Same Sex Mean and SD ----+")
mean(DZ_ONLY[DZ_ONLY$sexType=="Same",]$r_sd_freq, na.rm=TRUE)
sd(DZ_ONLY[DZ_ONLY$sexType=="Same",]$r_sd_freq, na.rm=TRUE)
print("+---- Different Sex Mean and SD ----+")
mean(DZ_ONLY[DZ_ONLY$sexType=="Different",]$r_sd_freq, na.rm=TRUE)
sd(DZ_ONLY[DZ_ONLY$sexType=="Different",]$r_sd_freq, na.rm=TRUE)
print("---- Cohen's D -----")
# Extract components
b         <- fixef(model)["sexTypeDifferent"]      
var_rand  <- as.numeric(VarCorr(model)$family_id)  
var_resid <- sigma(model)^2                         
sd_total  <- sqrt(var_rand + var_resid)

cohens_d  <- b / sd_total
cohens_d


print("+---- FDR Corrected P-Values across all tests ----+")
# FDR corrected p-values for 14 tests (peak magnitude dropped from analysis)
p.adjust(c(0.497, 0.376, 0.297, 0.270, 0.239, 0.200, 0.906, 0.647,
           0.991, 0.737, 0.450, 0.845, 0.880, 0.558),
         method="fdr")

sink()


