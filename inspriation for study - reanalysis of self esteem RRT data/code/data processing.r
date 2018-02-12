# process data

# author: Ian Hussey (ian.hussey@ugent.be)
# license: GPLv3+
 

# Dependencies ------------------------------------------------------------


library(plyr)
library(tidyverse)
library(data.table)
library(schoRsch)
library(psych)


# Data acquisition and cleaning -------------------------------------------


## Set the working directory
setwd("/Users/Ian/Dropbox/Work/Projects/Suicide and self-harm/RRT self-esteem CC/Master data/RRTs/")

# Read all files with the .iqdat extension
files <- list.files(pattern = "\\.csv$")  

# Read these files sequentially into a single data frame
input_df <- dplyr::tbl_df(plyr::rbind.fill(lapply(files, data.table::fread, header = TRUE)))  # tbl_df() requires dplyr, rbind.fill() requires plyr, fread requires data.table

setwd("/Users/Ian/Dropbox/Work/Projects/Suicide and self-harm/RRT self-esteem CC/Processed data/Inducer trials/")

# Make some variable names more transparent
trimmed_df <- 
  input_df %>%
  filter(trialtype == 0,  # inducer trials only
         stimulusFile == "stimuliBlock1.xlsx",
         stimulus == "Goed" |  # Good
           stimulus == "Juist" |  # just
           stimulus == "Correct" |  # correct
           stimulus == "Exact" |  # exact
           stimulus == "In orde" |  # alright
           stimulus == "Mis" |  # wrong
           stimulus == "Onjuist" |  # false
           stimulus == "Incorrect" |  # incorrect
           stimulus == "Verkeerd" |  # wrong
           stimulus == "Fout") %>%  # inducer blocks only
  dplyr::select(stimulusFile,
                stimulus,
                requiredResponse.corr,
                requiredResponse.rt,
                participant,
                gender,
                age,
                condition) %>%
  dplyr::rename(accuracy = requiredResponse.corr,
                rt = requiredResponse.rt) %>%
  rowwise() %>%
  dplyr::mutate(rt = round(rt*1000, 0), 
                stimulus_category = ifelse(stimulus == "Goed" |  # Good
                                             stimulus == "Juist" |  # just
                                             stimulus == "Correct" |  # correct
                                             stimulus == "Exact" |  # exact
                                             stimulus == "In orde",  # alright
                                           "true",
                                           ifelse(stimulus == "Mis" |  # wrong
                                                    stimulus == "Onjuist" |  # false
                                                    stimulus == "Incorrect" |  # incorrect
                                                    stimulus == "Verkeerd" |  # wrong
                                                    stimulus == "Fout",  # error
                                                  "false", NA))) %>%
  ungroup()

trimmed_df %>% write.csv(file = "processed data - inducer trials.csv", row.names = FALSE)


# demographics and parameters  --------------------------------------------


demographics_df <-
  trimmed_df %>%
  tibble::rownames_to_column() %>%
  dplyr::group_by(participant) %>%
  dplyr::filter(rowname == min(rowname)) %>%
  ungroup() %>%
  select(participant, age, gender) %>%
  mutate(gender = ifelse(gender == "M", "m",
                         ifelse(gender == FALSE, "f", gender)))

# factor variables
gender_counts <- demographics_df %>% count(gender)

# all ps (after exclusions)
age_distribution <-
  demographics_df %>%
  dplyr::select(age) %>%
  psych::describe(fast = TRUE,  # subset of descriptive stats
                  ranges = FALSE,
                  trim = 0) %>%
  dplyr::select(-vars, -se)


sink("analysis/descriptive statistics.txt")
cat("\n Gender counts \n")
gender_counts
cat("\n age \n")
age_distribution
sink()

