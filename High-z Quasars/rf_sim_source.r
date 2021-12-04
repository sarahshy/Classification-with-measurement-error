####################################################
# Front Matter
####################################################

# (loaded in foreach)
# library(foreach)
# library(ranger)

####################################################
# Perturbing functions
####################################################

perturb_data <- function(data){
  # prep column names
  col.names <- c("ug", "gr", "ri", "iz", "zs1", "s1s2")
  err.names <- c("ug_err", "gr_err", "ri_err", "iz_err", "zs1_err", "s1s2_err")
  
  # separate magnitudes from errors
  col.df <- data %>% select(all_of(col.names))
  err.df <- data %>% select(all_of(err.names))*sqrt(2) # sqrt(2*Variance) = sqrt(2)*SD
  
  # add random perturbations
  pert <- mapply(FUN = function(mag, error, i, j){ mag + rnorm(1, 0, error) },
                 col.df,
                 err.df) %>% as.data.frame
  
  # attach class column
  pert$label <- data$label
  return(pert)
}

####################################################
# Fit Model
####################################################

########## custom summary function for CV folds ##########
MySummary  <- function(data, lev = NULL, model = NULL){ # see ?defaultSummary
  a1 <- defaultSummary(data, lev, model)
  b1 <- twoClassSummary(data, lev, model)
  c1 <- prSummary(data, lev, model)
  out <- c(a1, b1, c1)
  out}

########## Random Forest ##########

# Fit RF using CV
# Return new data predictions
rf_metrics <- function(train, new_data){
  # define folds for stratified CV
  folds <- 10
  metric <- c('F') # doesn't actually matter, not tuning here
  ntree <- 50 # same results as 20
  cvIndex <- createFolds(train$label, k = folds, returnTrain = T) # default is stratified
  control <- trainControl(index = cvIndex,
                          method = 'cv',
                          number = folds,
                          summaryFunction = MySummary) # returns average across folds
  
  # run CV RF
  rf_fit <- train(label ~ ug + gr + ri + iz + zs1 + s1s2,
                  data = train,
                  method = "rf",
                  metric = metric,
                  ntree = ntree,
                  trControl = control,
                  tuneGrid = data.frame(mtry=2))

  # extract metrics (cm, completeness, efficiency)
  cm <- confusionMatrix(rf_fit, norm = "average")$table
  completeness <- rf_fit$results$Recall # average across all folds
  efficiency <- rf_fit$results$Precision

  
  # predict on new data
  new_data_preds <- predict(rf_fit, newdata = new_data) # save as character bc lower memory usage

  return(list(new_data_preds = new_data_preds,          # 0 is quasar
              cm = as.vector(cm),                       # TP/FN/FP/TN
              completeness = completeness,
              efficiency = efficiency))
}


####################################################
# Perturbation simulation in parallel
####################################################

run_rf_simulations <- function(train, new_data, B = 500){
  
  log_info("Entering parallel loop")
  output_list <- foreach(i=1:B,
                         .export=c("perturb_data", "rf_metrics", "MySummary"),
                         .packages=c("dplyr", "caret", "logger"),
                         .combine = "c") %dopar% {
                           pert_train <- perturb_data(train)
                           pert_new_data <- perturb_data(new_data)
                           
                           # fit RF and save metrics
                           metrics <- rf_metrics(train = pert_train, new_data = pert_new_data)
                           
                           return(metrics)
                           } # each iteration returned as a list
  
  log_info("Exited parallel loop")
  
  # combine lists
  keys <- unique(names(output_list))
  output <- lapply(as.list(keys), FUN = function(key){
    output_list[grep(key, names(output_list))] %>%
      unlist(use.names = F) %>% matrix(ncol = B, byrow = F)
  }) %>% setNames(keys)
  
  # convert one dim matrix to vector
  output$completeness <- output$completeness %>% as.vector
  output$efficiency <- output$efficiency %>% as.vector
  
  log_info("Combined parallel output")
  
  return(output)
}



