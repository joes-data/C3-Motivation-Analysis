# Test if you see this your piece of code GitHub worked

# Preparations for analyses -----------------------------------------------

### Set working directory
# To location where your .csv data file is (Save this script in same folder!)
# Use RStudio menu: Session -> Set Working Directory -> To Source File Location

# My working directory:
setwd("C:/Users/Administrator/bwSyncShare/Lehre/Expra FSS2023/Group 2 - Motivation/Analyse")

### Read in data
# Several options:
# 1) Use point-and-click menu under Environment (Import Dataset -> From Text (base) - follow instructions in menu)
# 2) Or use this piece of code (only works if data file is in same location as working directory)
# Important: Check whether the name is correct!!
data <- read.csv("PreliminaryData_Expra_Group2_Motivatio_2023.csv", stringsAsFactors = FALSE)

### Load helpful packages
# Extend this list with whatever packages you need
library(tidyverse)


# 1) Check exclusion criteria ---------------------------------------------


# 2) Inspect socio-demographics -------------------------------------------


# 3) Recode and aggregate scales ------------------------------------------


# 4) Analyze pre-registered hypotheses ------------------------------------

