## This script runs RF once on unperturbed data (traditional method)
# RF tuned with 10-fold CV
# Saves probabilities and predictions on prediction set

##########################
# Front Matter
##########################

rm(list = ls())
library(data.table)
library(dplyr)
library(tictoc)
library(magrittr)
library(logger)
library(caret)

training_set <- fread("Training Set/training_set.csv")
prediction_set <- fread("Prediction Set/prediction_set.csv")
log_info("Finished loading")

# convert response to factor
training_set$label <- training_set$label %>% as.factor # 0 is quasar

##########################
# Tune Random Forest
##########################
set.seed(314159)
#set.seed(123)

########## custom summary function for CV folds ##########
MySummary  <- function(data, lev = NULL, model = NULL){ # see ?defaultSummary
  a1 <- defaultSummary(data, lev, model)
  b1 <- twoClassSummary(data, lev, model)
  c1 <- prSummary(data, lev, model)
  out <- c(a1, b1, c1)
  out}

# define folds for stratified CV
folds <- 10
metric <- c('Accuracy')
ntree <- 50 # 20 gives worse comp+eff

cvIndex <- createFolds(training_set$label, k = folds, returnTrain = T) # default is stratified
control <- trainControl(index = cvIndex,
                        method = 'cv',
                        number = folds)
#,
#summaryFunction = MySummary)

tic(msg = "Finished RF tuning in")
rf_fit <- train(label ~ ug + gr + ri + iz + zs1 + s1s2,
                data = training_set,
                method = "rf",
                metric = metric,
                ntree = ntree,
                trControl = control,
                tuneGrid = data.frame(mtry=2),   # 1:6 for tuning
                allowParallel = T)
toc() #13:06

##########################
# Calculate metrics
##########################

print("CV metrics on training set is: ")
print(rf_fit$results)


avg_cm <- confusionMatrix(rf_fit, norm = "overall") # "average" gives counts
avg_cm_table <- avg_cm$table/100
print("Confusion matrix:")
print(avg_cm_table)
# Old tables:
#             Reference
# Prediction       0            1
#      0        0.02608      0.00368
#      1        0.00570      0.96454

# Counts:
#             Reference
# Prediction      0           1
#      0        16935      2392
#      1        3705       626407


completeness <- rf_fit$results$Recall # average across all folds
efficiency <- rf_fit$results$Precision

print(paste("Completeness: ", completeness))
print(paste("Efficiency: ", efficiency))

# EQUIVALENTLY:
completeness <- avg_cm$table[1,1]/sum(avg_cm$table[,1])
print("Completeness:")
print(completeness) # 0.8230136

efficiency <- avg_cm$table[1,1]/sum(avg_cm$table[1,])
print("Efficiency:")
print(efficiency) # 0.8858931

# plot(rf_fit)
# print(rf_fit)
# rf_fit$bestTune

log_info("Finished tuning/fitting")

##########################
# Predict on unlabeled set
##########################


# RF probabilities
probs_q_prediction <- predict(rf_fit, newdata = prediction_set, type = "prob")$'0'
# Predictions from probabilities
preds_prediction <- ifelse(probs_q_prediction >= 0.5, 0, 1) # 0 is quasar

# save results
prediction_preds <- data.frame(preds = preds_prediction, probs_q = probs_q_prediction) # 0 is quasar
write.csv(prediction_preds, "Prediction Set/quasar_catalog_prediction_rf.csv", row.names = F)


