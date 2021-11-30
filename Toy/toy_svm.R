#-------------------------------------------------
# Front Matter
#-------------------------------------------------

rm(list = ls())
library(cowplot)
library(ggpubr)
library(tictoc)
library(HDInterval)
library(magrittr)
library(extrafont)

source("Toys/New Toy/me_toy_source.R")

#-------------------------------------------------
# Create heteroscedastic data
#-------------------------------------------------

set.seed(160)

mean_1 <- c(5, 5) # mean of class 1
mean_2 <- c(2, 2) # mean of class 2

sigma_1 <- matrix(c(3, -.5, -.5, 2), nrow = 2) # true X1 VARIANCE
sigma_2 <- matrix(c(4, -1, -1, 1), nrow = 2) # true X2 variance

data <- make_base_data(n = 200, mean_1, mean_2, sigma_1, sigma_2)

# create "observed" data
# add noise with covariance matrix c(abs(x1)/2, 0, 0, abs(x2)/2)
noisy <- noisify_data(data)


# rearrange data for plotting purposes
ord <- order(data$label)

#-------------------------------------------------
# Visualizing the data
#-------------------------------------------------

# true data
p1 <- plot_data(data[ord,]) + xlim(-8, 15) + ylim(-2, 13) +
  theme(text = element_text(family = "CMU Serif"),
        axis.title = element_text(family = "CMU Serif"),
        # axis.title = element_text(family = "CMU Serif Italic"),
        panel.grid.minor = element_blank())

# observed data
p2 <- plot_data(noisy[ord,]) + xlim(-8, 15) + ylim(-2, 13) +
  theme(text = element_text(family = "CMU Serif"),
        axis.title = element_text(family = "CMU Serif"),
        panel.grid.minor = element_blank())

# observed data with error bars
p3 <- plot_data_with_error(noisy[ord,]) + xlim(-8, 15) + ylim(-2, 13) +
  theme(text = element_text(family = "CMU Serif"),
        axis.title = element_text(family = "CMU Serif"),
        panel.grid.minor = element_blank())

ggarrange(p1, p2, p3, nrow = 1, common.legend = T, legend = "right")

# visualize noise added
plot_errors(data, noisy)

#-------------------------------------------------
# Tune and fit single SVM on true and observed data
#-------------------------------------------------

set.seed(285)

# tune
cost_seq <- c(2^c(seq(-7, 2, by = 1)))
tune_par_true <- tune(svm, label ~., data = data, ranges = list(cost = cost_seq)) # true data
tune_par_noise <- tune(svm, label ~., data = noisy, ranges = list(cost = cost_seq)) # observed data

(opt_cost_true <- tune_par_true$best.parameters$cost)
(opt_cost_noise <- tune_par_noise$best.parameters$cost)
tune_par_true$best.performance # cv accuracy of best model
tune_par_noise$best.performance
# tune_par_true$performances # cv accuracy of all models
# tune_par_noise$performances

# fit svm 
svm_true <- svm_metrics(data) # on true data
svm_measured <- svm_metrics(noisy) # on observed data

#-------------------------------------------------
# Plot boundaries
#-------------------------------------------------

p_true <- plot_data_with_decision_boundaries(data, svm_true, alpha = 1) + xlim(-8, 15) + ylim(-2, 13) +
  theme(text = element_text(family = "CMU Serif"),
        axis.title = element_text(family = "CMU Serif"),
        panel.grid.minor = element_blank())
p_noise <- plot_data_with_decision_boundaries(noisy, svm_measured, alpha = 1) + xlim(-8, 15) + ylim(-2, 13) +
  theme(text = element_text(family = "CMU Serif"),
        axis.title = element_text(family = "CMU Serif"),
        panel.grid.minor = element_blank())

plot_grid(p_true, p_noise)

#-------------------------------------------------
# SVM with Gaussian perturbation
#-------------------------------------------------

# For each simulation:
#   (1) perturb data
#   (2) tune SVM hyperparameters
#   (3) fit SVM

set.seed(728)
tic()
sim_results_noisy <- run_svm_simulations(noisy, m = 500) # 500 perturbations, 145s
toc()

# calculate probabilitiess and predictions
sim_results_noisy$probs <- apply(sim_results_noisy$preds, 2, FUN = function(column) { mean(as.numeric(column)) })
sim_results_noisy$final_preds <- ifelse(sim_results_noisy$probs > 0.5, 1, 0)

#-------------------------------------------------
# Compare methods visually
#-------------------------------------------------

# Gaussian perturbation method
p_pert <- plot_data_with_decision_boundaries_and_soft_classifications(noisy, sim_results_noisy) +
  xlim(-8, 15) + ylim(-2, 13) +
  theme(text = element_text(family = "CMU Serif"),
        axis.title = element_text(family = "CMU Serif"),
        panel.grid.minor = element_blank())

p_pert
plt1 <- ggarrange(p_pert, legend = "right")

# traditional method
p_noise_preds <- plot_predictions_with_decision_boundaries(noisy, svm_measured, alpha = 1) + xlim(-8, 15) + ylim(-2, 13) +
  theme(text = element_text(family = "CMU Serif"),
        axis.title = element_text(family = "CMU Serif"),
        panel.grid.minor = element_blank())
plt2 <- ggarrange(p_noise_preds, legend = "none")

ggarrange(plt2, plt1, widths = c(26, 37))

#-------------------------------------------------
# Unlabeled observations
#-------------------------------------------------

cb_palette <- c("#FF0000", "#0000FF")

point1 <- c(7,7)
point2 <- c(2.2, 3.9)
new_points <- rbind(point1, point2) %>% data.frame
names(new_points) <- c("x1", "x2")

new_points$sigma1 <- noise_sigma_1(new_points)
new_points$sigma2 <- noise_sigma_2(new_points)

# for double error on 7,7
new_points[1,3:4] <- new_points[1,3:4]*2

# plot observed data and highlight unlabeled objects
p3 + 
  geom_point(data = new_points, aes(x = new_points[,1], y = new_points[,2], shape = rownames(new_points), col = rownames(new_points)), size = 3, stroke = 1, col = cb_palette) +
  geom_point(data = new_points, aes(x = new_points[,1], y = new_points[,2]), col = c("#225588", "#f1cc11"), size = 0.6) +
  scale_shape_manual(name = "", labels = c("Object 1", "Object 2"), values = c(1,2)) +
  guides(shape = F, col = F) +
  theme(text = element_text(family = "CMU Serif"),
        axis.title = element_text(family = "CMU Serif")) +
  geom_errorbar(data = new_points, aes(x = x1,
                                       ymin = x2 - sigma2, # 1 sd below
                                       ymax = x2 + sigma2), # 1 sd above
                width = 0,
                color = cb_palette) +
  geom_errorbarh(data = new_points, aes(y = x2,
                                        xmin = x1 - sigma1,
                                        xmax = x1 + sigma1),
                 height = 0,
                 #alpha = 0.5,
                 color = cb_palette)

#-------------------------------------------------
# Compare methods
#-------------------------------------------------

# accuracy
svm_true$cv_accuracy
svm_measured$cv_accuracy

pert_accuracies <- sim_results_noisy$metrics$cv_accuracy
hdi(pert_accuracies, 0.6826895) # 1sd, 68% ci
mean(pert_accuracies)
sd(pert_accuracies)

# intercept
get_svm_intercept(svm_true)
get_svm_intercept(svm_measured)

pert_intercepts <- get_svm_intercept(sim_results_noisy$metrics)
hdi(pert_intercepts, 0.6826895)
mean(pert_intercepts)
sd(pert_intercepts)

# slope
get_svm_slope(svm_true)
get_svm_slope(svm_measured)

pert_slopes <- get_svm_slope(sim_results_noisy$metrics)
hdi(pert_slopes, 0.6826895)
mean(pert_slopes)
sd(pert_slopes)

#-------------------------------------------------
# Plot posterior predictive distributions
#-------------------------------------------------

hist_acc <- ggplot() + geom_histogram(aes(x=pert_accuracies), bins = 30, fill = "gray", col = "black", boundary = 0) +
  geom_vline(xintercept = svm_measured$cv_accuracy, linetype = "dashed", col = "blue") +
  labs(x = "", y = "") +
  theme_minimal()  +
  theme(text = element_text(size = 13, family = "Times", face = "bold"),
        panel.grid.minor.y = element_blank()) +
  scale_x_continuous(breaks = seq(0, 1, 0.5), limits = c(0,1)) +
  scale_y_continuous(breaks = c(0, 100, 200))# 200x125

hist_intercept <- ggplot() + geom_histogram(aes(x=pert_intercepts), bins = 30, fill = "gray", col = "black", boundary = 0) +
  geom_vline(xintercept = get_svm_intercept(svm_measured), linetype = "dashed", col = "blue") +
  labs(x = "", y = "") +
  theme_minimal() +
  theme(text = element_text(size = 13, family = "Times", face = "bold"),
        panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = c(0,25,50), limits = c(0,55))

hist_slope <- ggplot() + geom_histogram(aes(x=pert_slopes), bins = 30, fill = "gray", col = "black", boundary = 0) +
  geom_vline(xintercept = get_svm_slope(svm_measured), linetype = "dashed", col = "blue") +
  labs(x = "", y = "") +
  theme_minimal() +
  theme(text = element_text(size = 13, family = "Times", face = "bold"),
        panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks = c(-1.5, -1, -0.5)) +
  scale_y_continuous(breaks = c(0,25,50), limits = c(0,70))


