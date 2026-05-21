# Title: Merging Accelerometry Data
# Author: Keith Lohse, PhD Pstat
# Date: 2026-02-11

library("tidyverse")

# Set the primary working directory to the ProcessedData/Aim folder that you want
# to aggregate. E.g., here is my "dummy" GitHub folder with a similar file 
# structure: 
# setwd("~/GitHub/accelerometry_R01")
setwd("C:/Users/user/Box/Infant Motor R01/WU Only Infant Motor R01/ProcessedData/Aim1/")


# Replacing the loop below with a recursive search
# this helps prevent Box Drive from timing out/updating mid search
# I confirmed this gives us identical output to the nested-loops structure
# I was using previously (commented out below). 
main_dir <- getwd()

# Get all files recursively with full paths
# this takes a minute or two...
all_files <- list.files(
  path = main_dir,
  pattern = "ChunkedOutput\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

all_files <- all_files[grepl("/Output/", all_files)]

ALL_DATA <- vector("list", length(all_files))

for (i in seq_along(all_files)) {
  
  cat("Reading:", basename(all_files[i]), "\n")
  
  # Copy from Box to local temp file
  temp_local <- tempfile(fileext = ".csv")
  file.copy(all_files[i], temp_local, overwrite = TRUE)
  
  current_data <- read.csv(temp_local, header = TRUE)
  current_data$file_id <- basename(all_files[i])
  
  unlink(temp_local)
  
  ALL_DATA[[i]] <- current_data
}

MASTER <- bind_rows(ALL_DATA)




# # Set your directory first and then run:
# main_dir <- getwd()
# # Looking at the different levels of the data
# # Main Directory
# list.files()
# # Subject Level
# list.files("./WU1001101/")
# # Time Point Level
# list.files("./WU1001101/6 months/")
# # Output files at each time point
# list.files("./WU1001101/6 months/Output/")
# 
# # 1. List of Subject Folders ----
# # We want to take the Day 1 outputs and the Day 2 outputs and merge them together
# list.files(main_dir)
# SUBJECT_LIST <-list.files(main_dir)
# # See full subject list
# SUBJECT_LIST
# 
# # Prune the non-folders out of the list:
# dir.exists(SUBJECT_LIST)
# SUBJECT_LIST <- SUBJECT_LIST[dir.exists(SUBJECT_LIST)==TRUE]
# # Check the names in the pruned list:
# SUBJECT_LIST
# # Check the sums to make sure the appropriate number of subjects have been selected
# sum(dir.exists(SUBJECT_LIST))
# 
# # The accelerometer fell off for one participant and they don't have usable Day 2 data
# #SUBJECT_LIST <- SUBJECT_LIST[SUBJECT_LIST != "EU1047102"]
# 
# 
# # 2. Get available folders inside of each subject folder: ----
# # Inside of each folder, create a list of available folders
# 
# # The for-loop below will loop through each level of the directory by
# # appending the next folder name to the parent directory: e.g.:
# list.files(paste(main_dir, "/", SUBJECT_LIST[1], sep=""))
# 
# 
# for(s in SUBJECT_LIST) {
#   sub_id <- s
#   sub_dir <- paste(main_dir, "/",s,"/", sep="")
#   list.files(sub_dir)
#   print(s)
#   
#   # Inside of each subject
#   TIME_LIST <- list.files(sub_dir)
#   
#   for(t in TIME_LIST) {
#     time_dir <- paste(sub_dir,t,"/Output/", sep="")
#     print(t)
#     #print(list.files(time_dir))
#     
#     FILES_LIST <- list.files(time_dir, pattern="csv")
#     FILES_LIST <- FILES_LIST[str_sub(FILES_LIST, start=-17, end=-1)=="ChunkedOutput.csv"]
#     
#     for (file in FILES_LIST){
#       file_dir <- paste(time_dir, file, sep="")
#       print(file)
#        
#       # if the MASTER data set doesn't exist, create it
#       if (!exists("MASTER")){
#        MASTER <- read.csv(file_dir, header=TRUE, sep=",")
#        MASTER$file_id <- factor(file)
#          
#       } else {        
#        # Create the temporary data file:
#        temp_dataset <-read.csv(file_dir, header=TRUE, sep=",")
#        temp_dataset$file_id <- factor(file)
#        colnames(MASTER) <- colnames(temp_dataset)
#        
#        MASTER<-rbind(MASTER, temp_dataset) 
#        # Remove or "empty" the temporary data set
#        rm(temp_dataset) 
#    }
#   }
#  }
# }

# Confirm the data look correct
head(MASTER)
nrow(MASTER)
ncol(MASTER)

# Add a Day variable based on the file_id
# str_split break the file name up based on "_" characters
# map_chr transforms the input by by applying a function to each element of a list
# factor converts the resulting string vector into a factor
# MASTER$day <- factor(map_chr(str_split(MASTER$file_id, "_"), 2))


# Add a family_id variable based on the file_id
# str_split break the file name up based on "_" characters
# map_chr transforms the input by by applying a function to each element of a list
# factor converts the resulting string vector into a factor
str_sub(map_chr(str_split(MASTER$file_id, "_"), 1), 1, 6)
MASTER$family_id <- str_sub(map_chr(str_split(MASTER$file_id, "_"), 1), 1, 6)

str_sub(map_chr(str_split(MASTER$file_id, "_"), 1), -3, -1)
MASTER$child_id <- str_sub(map_chr(str_split(MASTER$file_id, "_"), 1), -3, -1)


head(MASTER)
MASTER <- MASTER %>% relocate(file_id, record_id, family_id, child_id, 
                    redcap_event_name, .before=chunk)

# Set the working directory to where you want to save the output file:
setwd("C:/Users/user/Box/Infant Motor R01/WU Only Infant Motor R01/MZ-DZ_Comparisons/MzDz_Twins/accel_heritability_2026_02_11/")
write.csv(MASTER, paste("./data_chunkAGGREGATED", Sys.Date(), ".csv", sep=""))


write.csv(MASTER %>%
            group_by(record_id) %>%
            slice(1) %>%
            pull(record_id), paste("./data_chunkUniqueIDs", Sys.Date(), ".csv", sep=""))

