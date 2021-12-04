# Run RF GP
# Return predictions for each prediction object from each simulation

####################################################
# Front Matter
####################################################

rm(list = ls())
library(data.table)
library(dplyr)
library(ggplot2)
library(tictoc)
library(magrittr)
library(doParallel)
library(logger)
library(caret)
source("rf_sim_source.R")

# start clock
tic(msg="Total wall time")

# read clean data
training_set <- fread("../data/Training Set/training_set.csv")
prediction_set <- fread("../data/Prediction Set/prediction_set.csv")

# convert response to factor
training_set$label <- training_set$label %>% as.factor # 0 is quasar

# load command line argument for run number
args <- commandArgs(trailingOnly = TRUE) # use: qsub run_gp_rf.PBS -v RUN_NUM=4,B=500
run_num <- args[1]
B <- args[2] %>% as.numeric # number of simulations

log_info("Finished loading")

####################################################
# Tune RF mtry
####################################################

# control <- trainControl(method = "repeatedcv", number = 5, repeats = 2, search = "grid")
# tunegrid <- expand.grid(.mtry = c(1:6))
# metric <- "Accuracy"
# rf_gridsearch <- train(label ~ ug+gr+ri+iz+zs1+s1s2, data = training_set, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
# print(rf_gridsearch)
# plot(rf_gridsearch)

# mtry=2

####################################################
# Run Simulations
####################################################
# (1) Perturb train, and prediction
# (2) Fit model using 10-fold CV
# (3) Repeat B times
# RF Output: Preds on perturbed unlabeled set, CV CM, CV completeness+efficiency
# Note: ~20s/sim for rf

# set up cluster
n_cores <- 20
cl <- parallel::makeCluster(n_cores)
doParallel::registerDoParallel(cl)

# run simulations
log_info("Starting simulation.")
tic(msg = "Simulation time")
sim_results <- run_rf_simulations(train = training_set, new_data = prediction_set, B = B)
toc()

parallel::stopCluster(cl)
log_info("Finished simulation")

####################################################
# Save results
####################################################

file_name <- paste0("rf_results/rf_results", run_num, ".RData")
save(sim_results, file = file_name)
log_info("Saved data")

# stop clock
toc()

# (new) test set and prediction set
# each run B=1000, p=20, 6gb, ~1hr/run


