library(GGally)
library(ggfortify)
library(ISLR)
library(MASS)
library(klaR)
library(kknn)
library(discrim)
library(tidyverse)
library(tidymodels)
library(glmnet)
library(leaps)
library(vip)
library(gam)
library(rpart)
library(ranger)
library(xgboost)
library(kernlab)
library(baguette)
rm(list=ls())


### Data import & wrangling ###
subjecta_concentrating_1 <- read.csv("https://raw.githubusercontent.com/jordan-bird/eeg-feature-generation/master/dataset/original_data/subjecta-concentrating-1.csv") %>%  as_tibble()
subjecta_concentrating_2 <- read.csv("https://raw.githubusercontent.com/jordan-bird/eeg-feature-generation/master/dataset/original_data/subjecta-concentrating-2.csv") %>%  as_tibble()
subjectb_concentrating_1 <- read.csv("https://raw.githubusercontent.com/jordan-bird/eeg-feature-generation/master/dataset/original_data/subjectb-concentrating-1.csv") %>%  as_tibble()
subjectb_concentrating_2 <- read.csv("https://raw.githubusercontent.com/jordan-bird/eeg-feature-generation/master/dataset/original_data/subjectb-concentrating-2.csv") %>%  as_tibble()
subjectc_concentrating_1 <- read.csv("https://raw.githubusercontent.com/jordan-bird/eeg-feature-generation/master/dataset/original_data/subjectc-concentrating-1.csv") %>%  as_tibble()
subjectc_concentrating_2 <- read.csv("https://raw.githubusercontent.com/jordan-bird/eeg-feature-generation/master/dataset/original_data/subjectc-concentrating-2.csv") %>%  as_tibble()
subjectd_concentrating_1 <- read.csv("https://raw.githubusercontent.com/jordan-bird/eeg-feature-generation/master/dataset/original_data/subjectd-concentrating-1.csv") %>%  as_tibble()
subjectd_concentrating_2 <- read.csv("https://raw.githubusercontent.com/jordan-bird/eeg-feature-generation/master/dataset/original_data/subjectd-concentrating-2.csv") %>%  as_tibble()

subjecta_concentrating <- full_join(subjecta_concentrating_1, subjecta_concentrating_2) %>% mutate(state = "concentrating")
subjectb_concentrating <- full_join(subjectb_concentrating_1, subjectb_concentrating_2) %>% mutate(state = "concentrating")
subjectc_concentrating <- full_join(subjectc_concentrating_1, subjectc_concentrating_2) %>% mutate(state = "concentrating")
subjectd_concentrating <- full_join(subjectd_concentrating_1, subjectd_concentrating_2) %>% mutate(state = "concentrating")
all_concentrating <- rbind(subjecta_concentrating, subjectb_concentrating, subjectc_concentrating, subjectd_concentrating) %>% dplyr::select(-"timestamps", -"Right.AUX")

subjecta_neutral_1 <- read.csv("https://raw.githubusercontent.com/jordan-bird/eeg-feature-generation/master/dataset/original_data/subjecta-neutral-1.csv") %>%  as_tibble()
subjecta_neutral_2 <- read.csv("https://raw.githubusercontent.com/jordan-bird/eeg-feature-generation/master/dataset/original_data/subjecta-neutral-2.csv") %>%  as_tibble()
subjectb_neutral_1 <- read.csv("https://raw.githubusercontent.com/jordan-bird/eeg-feature-generation/master/dataset/original_data/subjectb-neutral-1.csv") %>%  as_tibble()
subjectb_neutral_2 <- read.csv("https://raw.githubusercontent.com/jordan-bird/eeg-feature-generation/master/dataset/original_data/subjectb-neutral-2.csv") %>%  as_tibble()
subjectc_neutral_1 <- read.csv("https://raw.githubusercontent.com/jordan-bird/eeg-feature-generation/master/dataset/original_data/subjectc-neutral-1.csv") %>%  as_tibble()
subjectc_neutral_2 <- read.csv("https://raw.githubusercontent.com/jordan-bird/eeg-feature-generation/master/dataset/original_data/subjectc-neutral-2.csv") %>%  as_tibble()
subjectd_neutral_1 <- read.csv("https://raw.githubusercontent.com/jordan-bird/eeg-feature-generation/master/dataset/original_data/subjectd-neutral-1.csv") %>%  as_tibble()
subjectd_neutral_2 <- read.csv("https://raw.githubusercontent.com/jordan-bird/eeg-feature-generation/master/dataset/original_data/subjectd-neutral-2.csv") %>%  as_tibble()

subjecta_neutral <- full_join(subjecta_neutral_1, subjecta_neutral_2) %>% mutate(state = "neutral")
subjectb_neutral <- full_join(subjectb_neutral_1, subjectb_neutral_2) %>% mutate(state = "neutral")
subjectc_neutral <- full_join(subjectc_neutral_1, subjectc_neutral_2) %>% mutate(state = "neutral")
subjectd_neutral <- full_join(subjectd_neutral_1, subjectd_neutral_2) %>% mutate(state = "neutral")
all_neutral <- rbind(subjecta_neutral, subjectb_neutral, subjectc_neutral, subjectd_neutral) %>% dplyr::select(-"timestamps", -"Right.AUX")

subjecta_relaxed_1 <- read.csv("https://raw.githubusercontent.com/jordan-bird/eeg-feature-generation/master/dataset/original_data/subjecta-relaxed-1.csv") %>%  as_tibble()
subjecta_relaxed_2 <- read.csv("https://raw.githubusercontent.com/jordan-bird/eeg-feature-generation/master/dataset/original_data/subjecta-relaxed-2.csv") %>%  as_tibble()
subjectb_relaxed_1 <- read.csv("https://raw.githubusercontent.com/jordan-bird/eeg-feature-generation/master/dataset/original_data/subjectb-relaxed-1.csv") %>%  as_tibble()
subjectb_relaxed_2 <- read.csv("https://raw.githubusercontent.com/jordan-bird/eeg-feature-generation/master/dataset/original_data/subjectb-relaxed-2.csv") %>%  as_tibble()
subjectc_relaxed_1 <- read.csv("https://raw.githubusercontent.com/jordan-bird/eeg-feature-generation/master/dataset/original_data/subjectc-relaxed-1.csv") %>%  as_tibble()
subjectc_relaxed_2 <- read.csv("https://raw.githubusercontent.com/jordan-bird/eeg-feature-generation/master/dataset/original_data/subjectc-relaxed-2.csv") %>%  as_tibble()
subjectd_relaxed_1 <- read.csv("https://raw.githubusercontent.com/jordan-bird/eeg-feature-generation/master/dataset/original_data/subjectd-relaxed-1.csv") %>%  as_tibble()
subjectd_relaxed_2 <- read.csv("https://raw.githubusercontent.com/jordan-bird/eeg-feature-generation/master/dataset/original_data/subjectd-relaxed-2.csv") %>%  as_tibble()

subjecta_relaxed <- full_join(subjecta_relaxed_1, subjecta_relaxed_2) %>% mutate(state = "relaxed")
subjectb_relaxed <- full_join(subjectb_relaxed_1, subjectb_relaxed_2) %>% mutate(state = "relaxed")
subjectc_relaxed <- full_join(subjectc_relaxed_1, subjectc_relaxed_2) %>% mutate(state = "relaxed")
subjectd_relaxed <- full_join(subjectd_relaxed_1, subjectd_relaxed_2) %>% mutate(state = "relaxed")
all_relaxed <- rbind(subjecta_relaxed, subjectb_relaxed, subjectc_relaxed, subjectd_relaxed) %>% dplyr::select(-"timestamps", -"Right.AUX")

# Bind By Subject
subjecta_all <- rbind(subjecta_concentrating, subjecta_neutral, subjecta_relaxed) %>% dplyr::select(-"timestamps", -"Right.AUX")
subjectb_all <- rbind(subjectb_concentrating, subjectb_neutral, subjectb_relaxed) %>% dplyr::select(-"timestamps", -"Right.AUX")
subjectc_all <- rbind(subjectc_concentrating, subjectc_neutral, subjectc_relaxed) %>% dplyr::select(-"timestamps", -"Right.AUX")
subjectd_all <- rbind(subjectd_concentrating, subjectd_neutral, subjectd_relaxed) %>% dplyr::select(-"timestamps", -"Right.AUX")

# Bind ALL - Final Dataset:
eeg_dataset <- rbind(all_concentrating, all_neutral, all_relaxed)

rows <- sample(nrow(eeg_dataset))
d <- eeg_dataset[rows, ]

### END ###

### Data splitting & resampling ###
set.seed(42)
s <- initial_split(d)
train <- training(s)
test <- testing(s)
cv <- vfold_cv(train)

boost_tree_rec <-
  recipe(state ~ ., data = eeg_dataset) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_zv(all_predictors(), -all_outcomes()) %>% 
  step_normalize(all_predictors(), -all_outcomes()) %>%
  step_center(all_predictors(), -all_outcomes())

set.seed(99242352)
boost_tree <- boost_tree(
  mode = "classification",
  mtry = 5, #no need to tune this param, makes the computation heavy
  trees = tune(),
  min_n = tune(),
  tree_depth = tune(),
  learn_rate = tune(),
  loss_reduction = tune(),
  sample_size = tune(),
  stop_iter = tune()
)
  set_engine("xgboost", times = 3)


boost_tune <- boost_tree %>% tune_grid(resamples = cv, grid = 8)

best_boost_tree <- boost_tune %>% select_best("roc_auc")

best_boost_tree