#-------------------------------------------------
# Load libraries
#-------------------------------------------------

library(mvtnorm)
library(e1071)
library(tidyverse)
library(randomForest)
library(cowplot)
library(svMisc)
library(caret)

#-------------------------------------------------
# Base data and noisifying
#-------------------------------------------------

# known noise function
noise_sigma_1 <- function(data){ (abs(data$x1)/2) %>% sqrt } # sd of noise (known ME) in x1
noise_sigma_2 <- function(data){ (abs(data$x2)/2) %>% sqrt } # noise in x2
# off-diagonals assumed 0 e.g. c(abs(x1)/2, 0, 0, abs(x2)/2). Otherwise, input matrix into rmvnorm
  
# create true data
make_base_data <- function(n=200, mean_1=c(0,0), mean_2=c(0,0), sigma_1, sigma_2) {
  class1 <- rmvnorm(ceiling(n/2), mean = mean_1, sigma = sigma_1)
  class2 <- rmvnorm(floor(n/2), mean = mean_2, sigma = sigma_2)
  
  data <- rbind(cbind(class1, 1), cbind(class2, 0)) %>% as_tibble
  colnames(data) <- c("x1", "x2", "label")
  data <- data %>% mutate(label = as.factor(label))
  
  return(data)
}

# add noise to base data
noisify_data <- function(data) {
  noise_sigma_1 <- noise_sigma_1(data)
  noise_sigma_2 <- noise_sigma_2(data)
  data_noisy <- data

  noise_x1 <- rnorm(nrow(data), 0, noise_sigma_1)
  noise_x2 <- rnorm(nrow(data), 0, noise_sigma_2)
  noise <- cbind(noise_x1, noise_x2)
  data_noisy[,c("x1", "x2")] <- data_noisy[,c("x1", "x2")] + noise

  return(data_noisy)
}

# perturb data
perturb_data <- function(data, double_error_sd=FALSE) {
  noise_sigma_1 <- noise_sigma_1(data) * (double_error_sd+1)
  noise_sigma_2 <- noise_sigma_2(data) * (double_error_sd+1)
  data_noisy <- data
  
  noise_x1 <- rnorm(nrow(data), 0, sqrt(2)*noise_sigma_1)
  noise_x2 <- rnorm(nrow(data), 0, sqrt(2)*noise_sigma_2)
  noise <- cbind(noise_x1, noise_x2)
  data_noisy[,c("x1", "x2")] <- data_noisy[,c("x1", "x2")] + noise
  
  return(data_noisy)
}

#-------------------------------------------------
# Plotting functions
#-------------------------------------------------

plot_data <- function(data) {
  ggplot(data) +
    geom_point(aes(x = x1, y = x2, fill = label), size = 1.2, shape = 21, stroke = 0) +
    scale_fill_manual(name = "Class label", values = c("#f1cc11", "#225588")) +
    labs(x = expression(Feature~1), y = expression(Feature~2)) +
    theme_bw()
}

plot_errors <- function(true, noisy){
  delta_noise <- data.frame(x1 = true$x1, delta_x1 = noisy$x1 - data$x1,
                            x2 = true$x2, delta_x2 = noisy$x2 - data$x2)
  p3 <- ggplot(delta_noise, aes(x = x1, y = delta_x1)) + geom_point(size = 0.8, alpha = 0.4) +
    ylim(-6, 6) +
    theme_bw() +
    labs(x = expression(Feature~1), y = "Noise added") +
    theme(text = element_text(family = "CMU Serif"),
          axis.title.x = element_text(family = "CMU Serif"))
  p4 <- ggplot(delta_noise, aes(x = x2, y = delta_x2)) + geom_point(size = 0.8, alpha = 0.4) +
    theme_bw() +
    ylim(-6, 6) +
    labs(x = expression(Feature~2), y = "Noise added") +
    theme(text = element_text(family = "CMU Serif"),
          axis.title.x = element_text(family = "CMU Serif"))
  
  plot_grid(p3, p4)
}

plot_data_with_error <- function(data) {
  noise_sigma_1 <- noise_sigma_1(data)
  noise_sigma_2 <- noise_sigma_2(data)
  
  plot_data(data) +
    geom_errorbar(aes(x = x1,
                      ymin = x2 - noise_sigma_2, # 1 sd below
                      ymax = x2 + noise_sigma_2, # 1 sd above
                      color = label)
                  , alpha = 0.2) +
    geom_errorbarh(aes(y = x2,
                       xmin = x1 - noise_sigma_1,
                       xmax = x1 + noise_sigma_1,
                       color = label),
                   alpha = 0.2) +
    scale_color_manual(name = "Class label", values = c("#f1cc11", "#225588"))
}

plot_data_with_decision_boundaries <- function(data, results, alpha = 0.1) {
  intercepts <- -results$theta_2^-1 * results$theta_0
  slopes <- -results$theta_2^-1 * results$theta_1
  
  plot_data(data) +
    geom_abline(intercept = intercepts, slope = slopes, alpha = alpha, color = "#999999")
    
}

plot_predictions_with_decision_boundaries <- function(data, results, alpha = 0.1) {
  intercepts <- -results$theta_2^-1 * results$theta_0
  slopes <- -results$theta_2^-1 * results$theta_1
  data$prediction <- results$preds
  
  ggplot(data) +
    geom_point(aes(x = x1, y = x2, fill = prediction), size = 1.2, shape = 21, stroke = 0) +
    scale_fill_manual(name = "Class label", values = c("#f1cc11", "#225588")) +
    labs(x = expression(Feature~1), y = expression(Feature~2)) +
    theme_bw() +
    geom_abline(intercept = intercepts, slope = slopes, alpha = alpha, color = "#999999")
  
}

# for SVM
plot_data_with_decision_boundaries_and_soft_classifications <- function(data, results, alpha_error = 0.2, alpha_boundary = 0.03) {
  intercepts <- -results$metrics$theta_2^-1 * results$metrics$theta_0
  slopes <- -results$metrics$theta_2^-1 * results$metrics$theta_1
  data$probs <- results$preds %>% apply(2, as.numeric) %>% colMeans
  
  noise_sigma_1 <- noise_sigma_1(data)
  noise_sigma_2 <- noise_sigma_2(data)
  
  plt <- ggplot(data) +
    geom_abline(intercept = intercepts, slope = slopes, alpha = alpha_boundary, color = "#777777") +
    geom_errorbar(aes(x = x1,
                      ymin = x2 - noise_sigma_2, # 1 sd below
                      ymax = x2 + noise_sigma_2), # 1 sd above
                  alpha = alpha_error, color = "#9F9F9F") +
    geom_errorbarh(aes(y = x2,
                       xmin = x1 - noise_sigma_1,
                       xmax = x1 + noise_sigma_1),
                   alpha = alpha_error, color = "#9F9F9F") +
    geom_point(aes(x = x1, y = x2, color = probs), size = 0.5) +
    scale_color_gradientn(colors = c("#f1cc11", "#99cc11", "#337755", "#225588"),
                          #values = c(0, 0.3, 0.9, 1), # original values
                          values = c(0, 0.35, 0.75, 1), # values to match rf
                          breaks = seq(0, 1, 0.25),
                          limits = c(0, 1),
                          #name = expression(italic(hat(p)["i,0"]))) +
                          name = expression(P(Class~1))) +
    theme_bw() +
    labs(x = expression(Feature~1), y = expression(Feature~2))

  return(plt)
}

# for RF
plot_data_with_soft_classifications <- function(data, results, alpha = 0.2) {
  data$probs <- results
  
  noise_sigma_1 <- noise_sigma_1(data)
  noise_sigma_2 <- noise_sigma_2(data)
  
  plt <- ggplot(data) +
    geom_errorbar(aes(x = x1,
                      ymin = x2 - noise_sigma_2, # 1 sd below
                      ymax = x2 + noise_sigma_2), # 1 sd above
                  alpha = alpha, color = "#9F9F9F") +
    geom_errorbarh(aes(y = x2,
                       xmin = x1 - noise_sigma_1,
                       xmax = x1 + noise_sigma_1),
                   alpha = alpha, color = "#9F9F9F") +
    geom_point(aes(x = x1, y = x2, color = probs), size = 0.5) +
    scale_color_gradientn(colors = c("#f1cc11", "#99cc11", "#337755", "#225588"),
                          values = c(0, 0.35, 0.75, 1),
                          breaks = seq(0, 1, 0.25),
                          limits = c(0, 1),
                          #name = expression(italic(hat(p)["i,0"]))) +
                          name = expression(P(Class~1))) +
    theme_bw() +
    labs(x = expression(Feature~1), y = expression(Feature~2))
  
  return(plt)
}


#-------------------------------------------------
# Fit classifier
#-------------------------------------------------

######### Linear SVM #########

# betas: coefs for scaled data
# thetas: coefs for unscaled data


# use 10-fold CV
svm_metrics <- function(data) {
  # tune uses 10-fold CV by default
  tune_par <- tune(svm, label ~., data = data, ranges = list(cost = c(2^c(seq(-7, 2, by = 2))))) # tune cost
  opt_cost <- tune_par$best.parameters$cost
  
  svm_result <- svm(label ~ ., data = data, kernel = "linear", cost = opt_cost)

  coefs <- coef(svm_result)
  
  centers <- svm_result$x.scale$`scaled:center` # centering and scaling attributes
  scales <- svm_result$x.scale$`scaled:scale`
  
  preds <- predict(svm_result, newdata = data)
  
  output <- list(cv_accuracy = 1-tune_par$best.performance, # CV accuracy
                  # separating hyperplane relative to scaled data
                  beta_0 = coefs[1],
                  beta_1 = coefs[2],
                  beta_2 = coefs[3],
                  # separating hyperplane relative to original (unscaled) data
                  theta_0 = coefs[1] - coefs[2]/scales[1]*centers[1] - coefs[3]/scales[2]*centers[2],
                  theta_1 = coefs[2]/scales[1],
                  theta_2 = coefs[3]/scales[2]
  )
  output <- c(output, preds = list(preds))
  return(output)
}

# Gaussian perturbation of SVM
# m: number of simulations
run_svm_simulations <- function(data, m = 500) { 
  metrics <- NULL
  preds <- data.frame(matrix(NA, nrow = m, ncol = nrow(data)))
  
  for(i in 1:m){
    obs <- perturb_data(data)
    obs_metrics <- svm_metrics(obs)
    metrics <- bind_rows(metrics, obs_metrics[-8])
    
    preds[i,] <- obs_metrics$preds
  }
  output <- list(metrics = metrics)
  output <- c(output, preds = list(preds))

  return(output)
}


######### Random Forest #########

rf_metrics <- function(data, ntree=100){
  # tune RF
  control <- trainControl(method = 'cv',
                          number = 10,
                          savePredictions = 'final', # hold-out set predictions
                          classProbs = T)
  tunegrid <- expand.grid(.mtry=(1:2))
  
  rf_result <- train(label ~.,
                     data = data,
                     method = 'rf',
                     metric = 'Accuracy',
                     ntree = ntree,
                     trControl = control,
                     tuneGrid = tunegrid)

  output <- list(cv_accuracy = max(rf_result$results$Accuracy),
                 probs = rf_result$pred$one[order(rf_result$pred$rowIndex)]) # P(Class = 1)
  
  return(output)
}

run_rf_simulations <- function(data, m=500, ntree=100){ 
  cv_accuracy <- c()
  probs <- data.frame(matrix(NA, nrow = m, ncol = nrow(data)))

  for(i in 1:m){
    obs <- perturb_data(data)
    obs_metrics <- rf_metrics(obs, ntree = ntree, probabilities = probabilities, predictions = predictions) # metrics for single new observation
    cv_accuracy <- c(cv_accuracy, obs_metrics$cv_accuracy) # add accuracy
    
      probs[i,] <- obs_metrics$probs # 1 col/obs, 1 row per sim round
  }
  output <- list(cv_accuracy = cv_accuracy,
                 probs = list(probs))
  return(output)
}

#-------------------------------------------------
# Helper functions for interpreting SVM results
#-------------------------------------------------

get_svm_intercept <- function(results){
  -results$theta_2^-1 * results$theta_0
}
  
get_svm_slope <- function(results){
  -results$theta_2^-1 * results$theta_1
}


