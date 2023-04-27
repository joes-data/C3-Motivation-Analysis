# !!!ALWAYS PULL BEFORE START WORKING!!!

# Little Git Hub Guide
#' 0. First before push anything: Pull everything. Click on Git --> Pull
#' 1. Click on Git
#' 2. Mark file you have been working on
#' 3. Click on commit and make your comment (in pop up window, commit message)
#' 4. Click on push

# Preparations for analyses -----------------------------------------------

### Set working directory
# To location where your .csv data file is (Save this script in same folder!)
# Use RStudio menu: Session -> Set Working Directory -> To Source File Location

# My working directory:
setwd("./data") #we use relational paths now, so everyone can work on it. Important everybody needs
# a sub folder called data where the data is stored

### Read in data
# Several options:
# 1) Use point-and-click menu under Environment (Import Dataset -> From Text (base) - follow instructions in menu)
# 2) Or use this piece of code (only works if data file is in same location as working directory)
# Important: Check whether the name is correct!!
data <- read.csv("FinalData_Expra_Group2_Motivation_2023.csv", stringsAsFactors = FALSE)

prelim <- read.csv("PreliminaryData_Expra_Group2_Motivation_2023.csv", stringsAsFactors = FALSE)

### Load helpful packages
# Extend this list with whatever packages you need
library(tidyverse)
library(dplyr)

## How many participants in our data set?
n_distinct(data$VPN) # 116 participants (on April 27)

## How many participants per condition?
data %>% group_by(condition) %>% summarise(n = n_distinct(VPN))

# 40 control, 41 economy, 35 environment (on April 27)


# 1) Check exclusion criteria ---------------------------------------------

## Exclusion: Seriousness Check (1 participant)
table(data$quality)

data_wo_quality <- data %>% filter(quality == "serious")

## Exclusion: Manipulation check
data_manipulated <- data_wo_quality %>% filter(Manipulation_Question =="yes")

## Exclusion: Attention check
# Subset for every condition and then for the correct answer scheme.
environment_attention <- subset(data_manipulated, condition == "pro_environment") %>%
  subset(AC_environment_true_preserve == TRUE & AC_environment_true_harmony == TRUE &
           AC_environment_false_events == FALSE & AC_environment_false_neighbor == FALSE)


economy_attention <- subset(data_manipulated, condition == "pro_economy") %>%
  subset(AC_economy_false_international == FALSE & AC_economy_false_neighbor == FALSE &
           AC_economy_true_crisis == TRUE & AC_economy_true_modern == TRUE)


control_attention <- subset(data_manipulated, condition == "control") %>%
  subset(AC_education_false_events == FALSE & AC_education_false_neighbor == FALSE &
           AC_education_true_quality == TRUE & AC_education_true_access == TRUE)

# merge datasets per condition to one big data frame
included_data <- rbind(control_attention, economy_attention, environment_attention)

## Exclusion: Did not take part in experiment

## Final data summary
# How many participants in our data set?
n_distinct(included_data$VPN) # 76 participants (on April 27)

# How many participants per condition?
included_data %>% group_by(condition) %>% summarise(n = n_distinct(VPN))

# 23 control, 29 economy, 24 environment (on April 27)


# 2) Inspect socio-demographics -------------------------------------------


# 3) Recode and aggregate scales ------------------------------------------


# 4) Analyze pre-registered hypotheses ------------------------------------


