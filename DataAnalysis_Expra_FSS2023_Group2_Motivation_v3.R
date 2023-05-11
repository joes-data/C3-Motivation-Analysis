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
#setwd("C:/Users/Administrator/bwSyncShare/Lehre/Expra FSS2023/Group 2 - Motivation/Analyse")
setwd("./data")

### Read in data
# Several options:
# 1) Use point-and-click menu under Environment (Import Dataset -> From Text (base) - follow instructions in menu)
# 2) Or use this piece of code (only works if data file is in same location as working directory)
# Important: Check whether the name is correct!!
data <- read.csv("FinalData_Expra_Group2_Motivation_2023.csv", stringsAsFactors = FALSE)

### Load helpful packages
# Extend this list with whatever packages you need
library(tidyverse)
library(afex)
library(emmeans)
library(ggplot2)

## How many participants in our data set?
n_distinct(data$VPN) # 116 participants

## How many participants per condition?
data %>% group_by(condition) %>% summarise(n = n_distinct(VPN))

# 40 control, 41 economy, 35 environment 


# 1) Check exclusion criteria ---------------------------------------------

## Exclusion: Seriousness Check (2 participant)
table(data$quality)

data_wo_quality <- data %>% filter(quality == "serious")

## Exclusion: Manipulation Check 2 - political campaign question (21 participants)
table(data_wo_quality$Manipulation_Question)

data_wo_MC2 <- data_wo_quality %>% filter(Manipulation_Question == "yes")

## Exclusion: Manipulation Check 1 - remembered campaign goal (47 participants)
data_wo_MC1 <- data_wo_MC2 %>% 
  # create new variable that codes whether MC1 was answered correctly
  mutate(MC1_correct = case_when(
    condition == "control" & political_attitude_mc == "Neutral-education" ~ "correct",
    condition == "pro_economy" & political_attitude_mc == "Pro-economy" ~ "correct",
    condition == "pro_environment" & political_attitude_mc == "Pro-environment" ~ "correct",
    TRUE ~ "incorrect"
  )) %>%
  filter(MC1_correct == "correct")

## Exclusion: Attention Check (6 participants)
data_wo_AC <- data_wo_MC1 %>%
  # create new variable that codes whether MC1 was answered correctly
  mutate(AC_correct = case_when(
    condition == "control" & AC_education_true_access & AC_education_true_quality & 
      !AC_education_false_events & !AC_education_false_neighbor ~ "correct",
    condition == "pro_economy" & AC_economy_true_crisis & AC_economy_true_modern & 
      !AC_economy_false_international & !AC_economy_false_neighbor ~ "correct",
    condition == "pro_environment" & AC_environment_true_harmony & AC_environment_true_preserve &
      !AC_environment_false_events & !AC_environment_false_neighbor~ "correct",
    TRUE ~ "incorrect"
  )) %>%
  filter(AC_correct == "correct")

## Final data set with pre-registered exclusion criteria
data_exclusion_prereg <- data_wo_AC %>%
  # clean data set: remove variables used for exclusion of participants
  select(-c(AC_environment_true_preserve:AC_economy_true_modern, political_attitude_mc, 
            Manipulation_Question, quality, MC1_correct, AC_correct))

# Sample size
n_distinct(data_exclusion_prereg$VPN) # 40 participants

# Sample size per condition
data_exclusion_prereg %>% group_by(condition) %>% summarise(n = n_distinct(VPN))

# 7 control, 11 economy, 22 environment 
  
## Final data set without exclusion due to manipulation checks
data_exclusion_MC <- data_wo_quality %>%
  # create new variable that codes whether MC1 was answered correctly
  mutate(MC1_correct = case_when(
    condition == "control" & political_attitude_mc == "Neutral-education" ~ "correct",
    condition == "pro_economy" & political_attitude_mc == "Pro-economy" ~ "correct",
    condition == "pro_environment" & political_attitude_mc == "Pro-environment" ~ "correct",
    TRUE ~ "incorrect"
  ))  %>% 
  # Answered that political campaign should not influence political decisions
  filter(Manipulation_Question == "yes") %>%
  # create new variable that codes if Attention Check was answered correctly
  mutate(AC_correct = case_when(
    condition == "control" & AC_education_true_access & AC_education_true_quality & 
      !AC_education_false_events & !AC_education_false_neighbor ~ "correct",
    condition == "pro_economy" & AC_economy_true_crisis & AC_economy_true_modern & 
      !AC_economy_false_international & !AC_economy_false_neighbor ~ "correct",
    condition == "pro_environment" & AC_environment_true_harmony & AC_environment_true_preserve &
      !AC_environment_false_events & !AC_environment_false_neighbor~ "correct",
    TRUE ~ "incorrect"
  )) %>%
  filter(AC_correct == "correct") %>%
  # remove those who used a smartphone.
  filter(device != "Smartphone") %>%
  # exclude not_english speaking
  mutate(language_r_exclude = case_when(
    language == "fluent_german" ~ "fluent_english",
    language == "german" ~ "english",
    language == "not_german" ~ "not_english"
  )) %>%
  filter(language_r_exclude != "not_english") %>%
  # clean data set: remove variables used for exclusion of participants
  select(-c(AC_environment_true_preserve:AC_economy_true_modern, political_attitude_mc, AC_correct, language_r_exclude))

# Sample size
n_distinct(data_exclusion_MC$VPN) # 53 participants

# Sample size per condition
data_exclusion_MC %>% group_by(condition) %>% summarise(n = n_distinct(VPN))

# 15 control, 21 economy, 17 environment 

# 2) Inspect socio-demographics -------------------------------------------

## Disclaimer: We will continue with the larger data set and deal with MC-issues later


## Recoding of open text-field answers

# variables that could be recoded
table(data_exclusion_MC$culture)
table(data_exclusion_MC$culture_country)
table(data_exclusion_MC$disruption_text)
table(data_exclusion_MC$device_other)
table(data_exclusion_MC$other_comments)
table(data_exclusion_MC$language)

# new data set: data_socDemo
data_socDemoc <- data_exclusion_MC %>%
  # recode culture variable
  mutate(culture_r = case_when(
    culture == "‚I do not want to answer‘" ~ "no_answer",
    culture == "i do not want to answer" ~ "no_answer",
    culture == "I do not want to answer " ~ "no_answer",
    culture == "I don't know" ~ "no_answer",
    culture == "Nerd culture" ~ "no_answer",
    culture == "Deutsch" ~ "german",
    culture == "deutsch" ~ "german",
    culture == "german" ~ "german",
    culture == "German" ~ "german",
    culture == "German " ~ "german",
    culture == "german culture" ~ "german",
    culture == "German culture" ~ "german",
    culture == "German culture" ~ "german",
    culture == "German culture " ~ "german",
    culture == "German left sided/green" ~ "german",
    culture == "German, also interested in Latin American culture" ~ "german",
    culture == "germany" ~ "german",
    culture == "Germany" ~ "german",
    culture == "gernan" ~ "german",
    culture == "german/middle european culture " ~ "german/european",
    culture == "German/European culture" ~ "german/european",
    culture == "german, french" ~ "german/french",
    culture == "European" ~ "european",
    culture == "European Culture" ~ "european",
    culture == "european" ~ "european",
    culture == "of Central European culture" ~ "european",
    culture == "western culture" ~ "western",
    culture == "Western culture" ~ "western",
    culture == "Western/European" ~ "western/european",
    culture == "American" ~ "american",
    culture == "American " ~ "american",
    culture == "North American" ~ "american",
    culture == "Bavaria" ~ "bavarian",
    culture == "bavarian" ~ "bavarian",
    culture == "Bavarian" ~ "bavarian",
    culture == "bulgarian" ~ "bulgarian",
    culture == "Chinese" ~ "chinese",
    culture == "Estonian" ~ "estonian",
    culture == "France" ~ "french",
    culture == "France, Portugal" ~ "french/portugese",
    culture == "Japanese, French" ~ "japanese/french",
    culture == "Mediteran" ~ "mediterranean",
    culture == "turkish" ~ "turkish")) %>%
  # recode culture_country variable
  mutate(culture_country_r = case_when(
    culture_country == "Australia" ~ "australia",
    culture_country == "Canada" ~ "canada",
    culture_country == "Bulgaria " ~ "bulgaria",
    culture_country == "Estonia" ~ "estonia",
    culture_country == "Germany" ~ "germany",
    culture_country == "Germany " ~ "germany",
    culture_country == "germany" ~ "germany",
    culture_country == "German" ~ "germany",
    culture_country == "Deutschland" ~ "germany",
    culture_country == "Turkey" ~ "turkey",
    culture_country == "United States " ~ "US",
    culture_country == "United States" ~ "US",
    culture_country == "I do not want to answer" ~ "no_answer")) %>%
  # recode device_other variable
  mutate(device_other_r = case_when(
    device_other == "Tablet but in the Browser mode which is similar to a laptop/computer" ~ "Tablet"
  )) %>%
  # recode language variable
  mutate(language_r = case_when(
    language == "fluent_german" ~ "fluent_english",
    language == "german" ~ "english",
    language == "not_german" ~ "not_english"
  ))

## Distribution of participants' age
data_socDemoc %>%
  group_by(VPN) %>%
  filter(num_searched_cue == 1) %>%
  group_by(age) %>%
  summarize(n = n()) %>%
  mutate(relFreq = n/sum(n))

## Distribution of participants' gender
data_socDemoc %>%
  group_by(VPN) %>%
  filter(num_searched_cue == 1) %>%
  group_by(gender) %>%
  summarize(n = n()) %>%
  mutate(relFreq = n/sum(n))


## Distribution of language comprehension
data_socDemoc %>%
  group_by(VPN) %>%
  filter(num_searched_cue == 1) %>%
  group_by(language_r) %>%
  summarize(n = n()) %>%
  mutate(relFreq = n/sum(n))

## Distribution of participants' culture

# self-identified
data_socDemoc %>%
  group_by(VPN) %>%
  filter(num_searched_cue == 1) %>%
  group_by(culture_r) %>%
  summarize(n = n()) %>%
  mutate(relFreq = n/sum(n))

# grew-up in country
data_socDemoc %>%
  group_by(VPN) %>%
  filter(num_searched_cue == 1) %>%
  group_by(culture_country_r) %>%
  summarize(n = n()) %>%
  mutate(relFreq = n/sum(n))


## Distribution of participants' technical answers

# disruptions during study
data_socDemoc %>%
  group_by(VPN) %>%
  filter(num_searched_cue == 1) %>%
  group_by(disruption) %>%
  summarize(n = n()) %>%
  mutate(relFreq = n/sum(n))

table(data_socDemoc$disruption_text)

# used device
data_socDemoc %>%
  group_by(VPN) %>%
  filter(num_searched_cue == 1) %>%
  group_by(device) %>%
  summarize(n = n()) %>%
  mutate(relFreq = n/sum(n))

table(data_socDemoc$device_other_r)

# comments on study
table(data_socDemoc$other_comments)

# 3) Recode and aggregate scales ------------------------------------------

## I did not notice that you stated to only use 4 items in the pre-registration 
## for now, I recode and sum all of them - this can always be changed

data_scales <- data_socDemoc %>%
  # recode items
  mutate(HEXACO_Con_3R_r = case_when(HEXACO_Con_3R == 1 ~ 5,
                               HEXACO_Con_3R == 2 ~ 4,
                               HEXACO_Con_3R == 3 ~ 3,
                               HEXACO_Con_3R == 4 ~ 2,
                               HEXACO_Con_3R == 5 ~ 1),
         HEXACO_Con_4R_r = case_when(HEXACO_Con_4R == 1 ~ 5,
                               HEXACO_Con_4R == 2 ~ 4,
                               HEXACO_Con_4R == 3 ~ 3,
                               HEXACO_Con_4R == 4 ~ 2,
                               HEXACO_Con_4R == 5 ~ 1),
         HEXACO_Con_5R_r = case_when(HEXACO_Con_5R == 1 ~ 5,
                               HEXACO_Con_5R == 2 ~ 4,
                               HEXACO_Con_5R == 3 ~ 3,
                               HEXACO_Con_5R == 4 ~ 2,
                               HEXACO_Con_5R == 5 ~ 1),
         HEXACO_Con_6R_r = case_when(HEXACO_Con_6R == 1 ~ 5,
                               HEXACO_Con_6R == 2 ~ 4,
                               HEXACO_Con_6R == 3 ~ 3,
                               HEXACO_Con_6R == 4 ~ 2,
                               HEXACO_Con_6R == 5 ~ 1),
         HEXACO_Con_8R_r = case_when(HEXACO_Con_8R == 1 ~ 5,
                               HEXACO_Con_8R == 2 ~ 4,
                               HEXACO_Con_8R == 3 ~ 3,
                               HEXACO_Con_8R == 4 ~ 2,
                               HEXACO_Con_8R == 5 ~ 1),
         HEXACO_Con_10R_r = case_when(HEXACO_Con_10R == 1 ~ 5,
                                HEXACO_Con_10R == 2 ~ 4,
                                HEXACO_Con_10R == 3 ~ 3,
                                HEXACO_Con_10R == 4 ~ 2,
                                HEXACO_Con_10R == 5 ~ 1),
         HEXACO_Op_1R_r = case_when(HEXACO_Op_1R == 1 ~ 5,
                                     HEXACO_Op_1R == 2 ~ 4,
                                     HEXACO_Op_1R == 3 ~ 3,
                                     HEXACO_Op_1R == 4 ~ 2,
                                     HEXACO_Op_1R == 5 ~ 1),
         HEXACO_Op_4R_r = case_when(HEXACO_Op_4R == 1 ~ 5,
                                     HEXACO_Op_4R == 2 ~ 4,
                                     HEXACO_Op_4R == 3 ~ 3,
                                     HEXACO_Op_4R == 4 ~ 2,
                                     HEXACO_Op_4R == 5 ~ 1),
         HEXACO_Op_6R_r = case_when(HEXACO_Op_6R == 1 ~ 5,
                                     HEXACO_Op_6R == 2 ~ 4,
                                     HEXACO_Op_6R == 3 ~ 3,
                                     HEXACO_Op_6R == 4 ~ 2,
                                     HEXACO_Op_6R == 5 ~ 1),
         HEXACO_Op_9R_r = case_when(HEXACO_Op_9R == 1 ~ 5,
                                     HEXACO_Op_9R == 2 ~ 4,
                                     HEXACO_Op_9R == 3 ~ 3,
                                     HEXACO_Op_9R == 4 ~ 2,
                                     HEXACO_Op_9R == 5 ~ 1),
         HEXACO_Op_10R_r = case_when(HEXACO_Op_10R == 1 ~ 5,
                                     HEXACO_Op_10R == 2 ~ 4,
                                     HEXACO_Op_10R == 3 ~ 3,
                                     HEXACO_Op_10R == 4 ~ 2,
                                     HEXACO_Op_10R == 5 ~ 1)) %>%
  # sum HonHum items to sum score
  rowwise() %>%
  mutate(sum_Con = sum(HEXACO_Con_1, HEXACO_Con_2, HEXACO_Con_3R_r, HEXACO_Con_4R_r, HEXACO_Con_5R_r, 
                       HEXACO_Con_6R_r, HEXACO_Con_7, HEXACO_Con_8R_r, HEXACO_Con_9, HEXACO_Con_10R_r),
         sum_Op = sum(HEXACO_Op_1R_r, HEXACO_Op_2, HEXACO_Op_3, HEXACO_Op_4R_r, HEXACO_Op_5, 
                      HEXACO_Op_6R_r, HEXACO_Op_7, HEXACO_Op_8, HEXACO_Op_9R_r, HEXACO_Op_10R_r),
         sum_Preserve = sum(Scale_Preserve_1, Scale_Preserve_2, Scale_Preserve_3)) %>%
  ungroup() %>%
  # for a cleaner data set: remove individual data columns
  select(-c(HEXACO_Con_1:Scale_Preserve_3, HEXACO_Con_3R_r:HEXACO_Op_10R_r))


# 4) Analyze pre-registered hypotheses ------------------------------------

## (H1) Rated likelihood of building mine follows this pattern: pro-economy > control > pro-environment

# Prep data set
data_H1 <- data_scales %>%
  # keep only one row of data per participant
  filter(num_searched_cue == 1) %>%
  # reduce number of variables
  select(VPN, condition, likelihood_decision)
  
# Run analysis: ANOVA with contrasts (use 'aov_ez' function from afex package)
data_H1$condition <- as.factor(data_H1$condition) #CHECK when changing data set (levels: control pro_economy pro_environment)
contrast_H1 <- c(0, 1, -1)

#create reference grid
refGrid_1 <- emmeans(aov_ez(id = "VPN", data = data_H1, dv = "likelihood_decision", 
                          between = c("condition")), specs = c("condition"))
between_contrast_1 <- contrast(refGrid_1, list(contrast_H1))

# one-factor ANOVA without planned contrasts
between <- aov_ez(id = "VPN", dv = "likelihood_decision", data = data_H1, between = "condition")
summary(between)
between$Anova

## (H2) Probability of searching pro-economy information follows this pattern: pro-economy > control > pro-environment

# Prep data set 
data_H2 <- data_scales %>%
  # recode the source variable
  mutate(source_recoded = case_when(
    source == "Cabrera, Advisor for Finance" ~ "pro_economy",
    source == "Diaz, Professor for Political Economy" ~ "pro_economy",
    source == "Arlacon, Expert for Nature Preservation" ~ "pro_environment",
    source == "Espinoza, Minister for Environment" ~ "pro_environment"
  )) %>%
  # count number of searches per source_r
  group_by(VPN, condition, source_recoded) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  complete(VPN, source_recoded, fill = list(n = 0)) %>%
  group_by(VPN) %>%
  fill(condition, .direction = "downup") %>%
  # calculate probability of searching for pro_economy source
  mutate(probability_source = n/sum(n)) %>%
  # keep only pro_economy source probability
  filter(source_recoded == "pro_economy") %>%
  select(VPN, condition, probability_source)

# Run analysis: ANOVA with contrasts
data_H2$condition <- as.factor(data_H2$condition) #CHECK when changing data set (levels: control pro_economy pro_environment)
contrast_H2 <- c(0, 1, -1)

#create reference grid
refGrid_2 <- emmeans(aov_ez(id = "VPN", data = data_H2, dv = "probability_source", 
                            between = c("condition")), specs = c("condition"))
between_contrast_2 <- contrast(refGrid_2, list(contrast_H2))

## (H3) Rating of neutral information as speaking for building the mine follows this pattern: pro-economy > control > pro-environment

# Prep data set 
data_H3 <- data_scales %>%
  # keep only val == neutral
  filter(val == "neutral") %>%
  # summarise by calculating the mean of eval_info
  group_by(VPN, condition) %>%
  summarise(mean_eval_info = mean(eval_info))

# Run analysis: ANOVA with contrasts
data_H3$condition <- as.factor(data_H3$condition) #CHECK when changing data set (levels: control pro_economy pro_environment)
contrast_H3 <- c(0, 1, -1)

#create reference grid
refGrid_3 <- emmeans(aov_ez(id = "VPN", data = data_H3, dv = "mean_eval_info", 
                            between = c("condition")), specs = c("condition"))
between_contrast_3 <- contrast(refGrid_3, list(contrast_H3))

## (H4a) Rating of information at t-1 predicts rating of case at t
## (H4b)Relationship of ratings is moderated by motivation manipulation

# Prep data set


# Run analysis: linear mixed model(s)


# 5) Exploratory analyses -------------------------------------------------

## 1) Exploration of failed manipulation checks (sub-group analyses)

## 2) Influence of covariates

# MODERATOR

# gender (t-test: male vs. female on likelihood decision)

# political attitude (Regression: predictor political attitude on likelihood decision and probability source [data_H2])

# MEDIATOR

# Consciousness (Regression on likelihood_decision)

# Tendency in preserving things (Regression on likelihood_decision)

# PLOTS
# Paste Prereqs here, e.g.: color or other theme related issues
color_condition <- c("#CD853F", "steelblue", "darkgreen")

## (H1) Rated likelihood of building mine follows this pattern: pro-economy > control > pro-environment
plot_data_1 <- data_H1 %>%
  group_by(condition)%>%
  summarize(m = mean(likelihood_decision))

#barplot
ggplot(data = plot_data_1, mapping = aes(x = condition, y = m, fill = condition))+
  geom_bar(stat = "identity")+
  ylab("mean")+
  xlab("condition")+
  ggtitle("Likelihood to build the mine")+
  theme_minimal()+
  scale_fill_manual(values = color_condition)

#boxplot
ggplot(data = data_H1, mapping = aes(x = condition, y = likelihood_decision, fill = condition))+
  geom_boxplot()+
  ggtitle("Likelihood to build the mine")+
  theme_minimal()+
  scale_fill_manual(values = color_condition)

## (H2) Probability of searching pro-economy information follows this pattern: pro-economy > control > pro-environment
plot_data_2 <- data_H2 %>%
  group_by(condition)%>%
  summarize(m = mean(probability_source))

#barplot
ggplot(data = plot_data_2, mapping = aes(x = condition, y = m, fill = condition))+
  geom_bar(stat = "identity")+
  ylab("mean")+
  xlab("condition")+
  ggtitle("Percentage of opened economy related claims")+
  theme_minimal()+
  scale_fill_manual(values = color_condition)

#boxplot
ggplot(data = data_H2, mapping = aes(x = condition, y = probability_source, fill = condition))+
  geom_boxplot()+
  ggtitle("Percentage of opened economy related claims")+
  theme_minimal()+
  scale_fill_manual(values = color_condition)

## (H3) Rating of neutral information as speaking for building the mine follows this pattern: pro-economy > control > pro-environment
plot_data_3 <- data_H3 %>%
  group_by(condition)%>%
  summarize(m = mean(mean_eval_info))

#barplot
ggplot(data = plot_data_3, mapping = aes(x = condition, y = m, fill = condition))+
  geom_bar(stat = "identity")+
  ylab("mean")+
  xlab("condition")+
  ggtitle("Evaluation of tendency to build the mine of ambiguous arguments")+
  theme_minimal()+
  scale_fill_manual(values = color_condition)

#boxplot
ggplot(data = data_H3, mapping = aes(x = condition, y = mean_eval_info, fill = condition))+
  geom_boxplot()+
  ggtitle("Evaluation of tendency to build the mine of ambiguous arguments")+
  theme_minimal()+
  scale_fill_manual(values = color_condition)
