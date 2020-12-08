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
b <- bootstraps(d) 

eeg_recipe <- recipe(state ~., data = train) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_zv(all_predictors(), -all_outcomes()) %>% 
  step_normalize(all_predictors(), -all_outcomes()) %>%
  step_center(all_predictors(), -all_outcomes())
rf_prep <- prep(eeg_recipe)
juice(rf_prep)
### END ###


### UNTUNED MODELS ###
model_mnr_untuned <- multinom_reg(penalty = 0, mixture = 0) %>% set_engine("glmnet")
workflow_mnr_untuned <- workflow() %>% add_recipe(eeg_recipe) %>% add_model(model_mnr_untuned)
workflow_mnr_untuned %>% fit(d) %>% tidy()

model_dt_untuned <- decision_tree() %>% set_engine("rpart") %>% set_mode("classification")
workflow_dt_untuned <- workflow() %>% add_recipe(eeg_recipe) %>% add_model(model_dt_untuned)
workflow_dt_untuned_results <- workflow_dt_untuned %>% last_fit(s)
workflow_dt_untuned_results %>% collect_metrics() 

model_rf_untuned <- rand_forest() %>% set_engine("ranger") %>% set_mode("classification")
workflow_rf_untuned <- workflow() %>% add_recipe(eeg_recipe) %>% add_model(model_rf_untuned)
workflow_rf_untuned_results <- workflow_rf_untuned %>% last_fit(s)
workflow_rf_untuned_results %>% collect_metrics() 

# Missing bagging, svm

### END ###

### TUNED MODELS # 

model_rf_tuned <- rand_forest(trees = 100, min_n = tune()) %>% set_mode("classification") %>% set_engine("ranger")
workflow_rf_tuned <- workflow() %>% add_recipe(eeg_recipe) %>% add_model(model_rf_tuned)
tuned_rf <- tune_grid(workflow_rf_tuned, resamples = cv, grid = 20)

model_dt_tuned <- decision_tree(cost_complexity = tune()) %>% set_engine("rpart") %>% set_mode("classification")
workflow_dt_tuned <- workflow() %>% add_recipe(eeg_recipe) %>% add_model(model_dt_tuned)
grid_dt_tuning <- grid_regular(tree_depth(), levels = 10)
tuned_dt <- workflow_dt_tuned %>% tune_grid(resamples = cv, grid = 10)
tuned_dt %>% collect_metrics() # lets you see the resulting metrics
tuned_dt %>% select_best("roc_auc") # the outcome of this is the best result by RMSE metric

workflow_dt_tuned <- workflow_dt_tuned %>% finalize_workflow(tuned_dt %>% select_best("roc_auc"))
workflow_dt_tuned %>% last_fit(s) %>% collect_metrics()

roc_vals <- metric_set(roc_auc)
ctrl <- control_grid(verbose = FALSE)
recipe_res <-
  model_rf_tuned %>% 
  tune_grid(
    eeg_recipe,
    resamples = cv,
    metrics = roc_vals,
    control = ctrl,
    grid = 20
  )


#BAG ONE#

#Grid-Tuned using baguette's "bagger()" function
##Bagugette's bagger() fits different models to bootstrap sample, not just decision trees##
###Here we feed the bagger() specific parameters, except we also feed it a list of base models###
####bagger() uses Principal Component Analysis to reduce dimensions####

nt_rec <-
  recipe(state ~ ., data = eeg_dataset) %>%
  step_pca(all_predictors()) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_zv(all_predictors(), -all_outcomes()) %>% 
  step_normalize(all_predictors(), -all_outcomes()) %>%
  step_center(all_predictors(), -all_outcomes())

ctrl <- control_bag(var_imp = TRUE)

set.seed(7687)
nt_pca_bag <- bagger(nt_rec, data = eeg_dataset, base_model = c("CART", "MARS", "C5.0"), #CART is decision trees, MARS is multivariate regression, C5.0 is decision trees
                       times = tune(), control = ctrl)

tuned_bagger <- nt_pca_bag %>% tune_grid(resamples = cv, grid = 12)

best_bagger <- tuned_bagger %>% select_best("roc_auc")

best_bagger

#BAG TWO#

bag_tree_rec <-
  recipe(state ~ ., data = eeg_dataset) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_zv(all_predictors(), -all_outcomes()) %>% 
  step_normalize(all_predictors(), -all_outcomes()) %>%
  step_center(all_predictors(), -all_outcomes())

set.seed(992352)
bag_tree <- bag_tree(bag_tree_rec, tree_depth = tune(), cost_complexity = tune(), min_n = tune(), class_cost = tune()) %>%
  set_mode("classification") %>%
  set_engine("rpart", times = 3)


tree_bagger <- bag_tree %>% tune_grid(resamples = cv, grid = 12)

best_bagger_tree <- tree_bagger %>% select_best("roc_auc")

best_bagger_tree


# tune_res
# 
# tune_res %>%
#   collect_metrics() %>%
#   filter(.metric == "roc_auc") %>%
#   select(mean, min_n, mtry) %>%
#   pivot_longer(min_n:mtry,
#                values_to = "value",
#                names_to = "parameter"
#   ) %>%
#   ggplot(aes(value, mean, color = parameter)) +
#   geom_point(show.legend = FALSE) +
#   facet_wrap(~parameter, scales = "free_x") +
#   labs(x = NULL, y = "AUC")

# eeg_rs <- bootstraps(eeg_dataset, times = 30)
# roc_vals <- metric_set(roc_auc)
# ctrl <- control_grid(verbose = FALSE)
# 
# recipe_res <-
#   rfc_mod %>% 
#   tune_grid(
#     eeg_rec,
#     resamples = eeg_rs,
#     metrics = roc_vals,
#     control = ctrl
#   )
# 
# tunable(recipe_res)
# tune_args(recipe_res)
# show_best(recipe_res, metric = "roc_auc")
