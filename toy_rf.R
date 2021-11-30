#-------------------------------------------------
# Front Matter
#-------------------------------------------------

rm(list = ls())
library(cowplot)
library(tictoc)
library(HDInterval)
library(stargazer)
library(magrittr)
library(extrafont)
library(ggpubr)

source("Toys/New Toy/me_toy_source.R")

#-------------------------------------------------
# Create heteroscedastic data
#-------------------------------------------------

set.seed(160) # same seed as SVM

mean_1 <- c(5, 5) # mean of class 1
mean_2 <- c(2, 2) # mean of class 2

sigma_1 <- matrix(c(3, -.5, -.5, 2), nrow = 2) # true X1 VARIANCE
sigma_2 <- matrix(c(4, -1, -1, 1), nrow = 2) # true X2 variance

data <- make_base_data(n = 200, mean_1, mean_2, sigma_1, sigma_2)
data  %<>% mutate(label = ifelse(label == 0, "zero", "one"), .keep = "unused")

# create "observed" data
# add noise with covariance matrix c(abs(x1)/2, 0, 0, abs(x2)/2)
noisy <- noisify_data(data)

#-------------------------------------------------
# Tune RF and fit
#-------------------------------------------------

# true data
set.seed(31)
rf_true <- rf_metrics(data)

# observed data
set.seed(13)
rf_observed <- rf_metrics(noisy)

#-------------------------------------------------
# RF with Gaussian Perturbation
#-------------------------------------------------

set.seed(728)

tic()
sim_results_noisy <- run_rf_simulations(noisy, m = 500, ntree = 100)
toc()

#-------------------------------------------------
# Comparing accuracies
#-------------------------------------------------

rf_true$cv_accuracy
rf_observed$cv_accuracy
hdi(sim_results_noisy$cv_accuracy, 0.6826895) # hpd credible interval
mean(sim_results_noisy$cv_accuracy)
sd(sim_results_noisy$cv_accuracy)

#-------------------------------------------------
# Histogram of accuracies
#-------------------------------------------------

ggplot() + geom_histogram(aes(x = sim_results_noisy$cv_accuracy),
                          bins = 30, fill = "grey", col = "black", boundary = 0) +
  theme_minimal() +
  theme(text = element_text(family = "Times", face = "bold"),
        panel.grid.minor = element_blank()) +
  labs(x = '', y = '') +
  scale_y_continuous(breaks = c(0, 100, 200)) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25))


#-------------------------------------------------
# Plot predicted probabilities
#-------------------------------------------------

# RF predictions without measurement error (standard method)
p_noise <- plot_data_with_soft_classifications(noisy, rf_observed$probs, alpha = 0) +
  theme(text = element_text(family = "CMU Serif"),
        axis.title = element_text(family = "CMU Serif"),
        panel.grid.minor = element_blank())

# RF predictions with measurement error (proposed method)
p_pert <- plot_data_with_soft_classifications(noisy, sim_results_noisy$probs %>% colMeans) +
  theme(text = element_text(family = "CMU Serif"),
        axis.title = element_text(family = "CMU Serif"),
        panel.grid.minor = element_blank())

p1 <- ggarrange(p_noise, legend = "none")
p2 <- ggarrange(p_pert, legend = "right")
ggarrange(p1, p2, widths = c(14, 20), common.legend = T)


