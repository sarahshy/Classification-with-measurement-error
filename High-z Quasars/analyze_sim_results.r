########################################
# Front Matter
########################################

rm(list = ls())
library(dplyr)
library(ggplot2)
library(data.table)

# load simulation results (sim_results)
load("/storage/work/s/sxs1825/Gaussian_Perturbation/Random_Forest/rf_results/rf_results50.RData")

########################################
# Completeness and efficiency
########################################

mean(sim_results$completeness) # 0.8206827
sd(sim_results$completeness) # 0.001000917

mean(sim_results$efficiency) # 0.884411
sd(sim_results$efficiency) # 0.0008599711

########################################
# Confusion Matrix on labeled set
########################################

# TP/FN/FP/TN

10*rowSums(sim_results$cm) / 500 # cm avg, 10* because cm only from one fold, should have used "overall". Same as adding up all CMs across folds
10*apply(sim_results$cm, 1, sd) # cm sd

accuracies <- apply(sim_results$cm, 2, FUN = function(cm_values){ (cm_values[1]+cm_values[4]) / sum(cm_values)})

# completeness
hist_comp <- ggplot() + geom_histogram(aes(x=sim_results$completeness), bins = 30, fill = "gray", col = "black", boundary = 0) +
  geom_vline(xintercept = 0.8230136, linetype = "dashed", col = "blue") +
  labs(x = "", y = "") +
  theme_minimal()  +
  theme(text = element_text(size = 13, family = "Times", face = "bold"),
        panel.grid.minor.y = element_blank()) +
  scale_y_continuous(breaks = c(0, 25, 50), limits = c(0,60))# 200x125

# efficiency
hist_eff <- ggplot() + geom_histogram(aes(x=sim_results$efficiency), bins = 30, fill = "gray", col = "black", boundary = 0) +
  geom_vline(xintercept = 0.8858931, linetype = "dashed", col = "blue") +
  labs(x = "", y = "") +
  theme_minimal()  +
  theme(text = element_text(size = 13, family = "Times", face = "bold"),
        panel.grid.minor.y = element_blank()) +
  scale_x_continuous(breaks = c(0.882, 0.884, 0.886)) +
  scale_y_continuous(breaks = c(0, 25, 50), limits = c(0,60))# 200x125

ggsave("../new plots/astro_efficiency_ppd.png", plot = hist_eff, units = "cm", width = 4.8, height = 3, dpi = 400)

########################################
# Predictions on unlabeled set
########################################

# create gprf quasar catalog
# prediction_vote <- apply(sim_results$new_data_preds, 1, function(row) {sum(row == "0")}) # num of quasar votes
# prediction_vote <- ifelse(prediction_vote >= 0.5*500, 0, 1) # 0 if quasar

prediction_vote <- read.csv("../data/Prediction Set/quasar_catalog_prediction_gprf.csv")

table(prediction_vote) # 1,853,331 and 9,637
#write.csv(prediction_vote, "../data/Prediction Set/quasar_catalog_prediction_gprf.csv", row.names = F)


# compare catalogs
trad_prediction <- read.csv("../data/Prediction Set/quasar_catalog_prediction_rf.csv")
table(trad_prediction$preds)

# overlap
table(prediction_vote$x, trad_prediction$preds)


########################################
# Color-color diagrams
########################################

# load all predictions
prediction_vote <- read.csv("../data/Prediction Set/quasar_catalog_prediction_gprf.csv")
trad_prediction <- read.csv("../data/Prediction Set/quasar_catalog_prediction_rf.csv")
prediction_set <- fread("../data/Prediction Set/prediction_set.csv")

cc_preds <- data.frame(prediction_set[,1:6], gp_vote = prediction_vote$x, trad_pred = trad_prediction$preds) # 0 is quasar

# filter down to quasars
pred_quasars <- cc_preds %>% filter(V2 == 0)
trad_quasars <- cc_preds %>% filter(V3 == 0)

# load true training labels
training_set <- fread("../data/Training Set/training_set.csv")
true_quasars <- training_set %>% filter(label == 0)
true_non_quasars <- training_set %>% filter(label == 1)


# yellow is true
# blue is traditional
# red is with measurement error

library(cowplot)

#colors <- c("#0072B2", "#CC0000", "#f1c232")
colors <- c("#0072B2", "#CC0000", "#eda000")
colors <- c("#0072B2", "#CC0000", "#00a40e")

p1 <- ggplot(data=NULL) +
  geom_density_2d(data = pred_quasars, mapping = aes(x = ug, y = gr), col = colors[1], alpha = 0.8) +
  geom_density_2d(data = trad_quasars, mapping = aes(x = ug, y = gr), col = colors[2], alpha = 0.8) +
  geom_density_2d(data = true_quasars, mapping = aes(x = ug, y = gr), col = colors[3], alpha = 0.8) +
  labs(x = "u - g", y = "g - r") +
  theme_minimal() +
  theme(legend.position = "none", text = element_text(family = "Times"), axis.title=element_text(family="Times", face = "italic"), panel.grid.minor=element_blank())
  
p1

p2 <- ggplot(data=NULL) +
  geom_density_2d(data = pred_quasars, mapping = aes(x = gr, y = ri), col = colors[1], alpha = 0.8) +
  geom_density_2d(data = trad_quasars, mapping = aes(x = gr, y = ri), col = colors[2], alpha = 0.8) +
  geom_density_2d(data = true_quasars, mapping = aes(x = gr, y = ri), col = colors[3], alpha = 0.8) +
  labs(x = "g - r", y = "r - i") +
  theme_minimal() +
  theme(legend.position = "none", text = element_text(family = "Times"), axis.title=element_text(family="Times", face = "italic"), panel.grid.minor=element_blank())


p3 <- ggplot(data=NULL) +
  geom_density_2d(data = pred_quasars, mapping = aes(x = ri, y = iz), col = colors[1], alpha = 0.8) +
  geom_density_2d(data = trad_quasars, mapping = aes(x = ri, y = iz), col = colors[2], alpha = 0.8) +
  geom_density_2d(data = true_quasars, mapping = aes(x = ri, y = iz), col = colors[3], alpha = 0.8) +
  labs(x = "r - i", y = "i - z") +
  theme_minimal() +
  theme(legend.position = "none", text = element_text(family = "Times"), axis.title=element_text(family="Times", face = "italic"), panel.grid.minor=element_blank())

p4 <- ggplot(data=NULL) +
  geom_density_2d(data = pred_quasars, mapping = aes(x = iz, y = zs1), col = colors[1], alpha = 0.8) +
  geom_density_2d(data = trad_quasars, mapping = aes(x = iz, y = zs1), col = colors[2], alpha = 0.8) +
  geom_density_2d(data = true_quasars, mapping = aes(x = iz, y = zs1), col = colors[3], alpha = 0.8) +
  labs(x = "i - z", y = "z - s1") +
  theme_minimal() +
  theme(legend.position = "none", text = element_text(family = "Times"), axis.title=element_text(family="Times", face = "italic"), panel.grid.minor=element_blank())

p5 <- ggplot(data=NULL) +
  geom_density_2d(data = pred_quasars, mapping = aes(x = zs1, y = s1s2), col = colors[1], alpha = 0.8) +
  geom_density_2d(data = trad_quasars, mapping = aes(x = zs1, y = s1s2), col = colors[2], alpha = 0.8) +
  geom_density_2d(data = true_quasars, mapping = aes(x = zs1, y = s1s2), col = colors[3], alpha = 0.8) +
  labs(x = "z - s1", y = "s1 - s2") +
  theme_minimal() +
  theme(legend.position = "none", text = element_text(family = "Times"), axis.title=element_text(family="Times", face = "italic"), panel.grid.minor=element_blank())


# extract legend
legend <- get_legend(
  ggplot(data=NULL) +
    geom_density_2d(data = pred_quasars, mapping = aes(x = zs1, y = s1s2, col = colors[1])) +
    geom_density_2d(data = trad_quasars, mapping = aes(x = zs1, y = s1s2, col = colors[2])) +
    geom_density_2d(data = true_quasars, mapping = aes(x = zs1, y = s1s2, col = colors[3])) +
    labs(col = "") +
    scale_color_manual(values = colors, labels = c("With measurement error", "Without measurement error", "True quasars")) +
    theme(text = element_text(family = "Times")) +
    theme_minimal()
)

# top row plots
top <- plot_grid(p1, p2, p3, nrow = 1)

# bottom row plots+legend
bottom <- plot_grid(p4, p5, legend, ncol = 3)

# create some space to the left of the legend
plot_grid(top, bottom, ncol = 1) # 750x500

ggsave("../Plots/color_color_quasars_green.png", units = "cm", width = 21, height = 14, dpi = 700)








colors <- c("#0072B2", "#CC0000", "#f1c232", "pink")

p1 <- ggplot(data=NULL) +
  geom_density_2d(data = pred_quasars, mapping = aes(x = ug, y = gr), col = colors[1], alpha = 0.8) +
  geom_density_2d(data = trad_quasars, mapping = aes(x = ug, y = gr), col = colors[2], alpha = 0.8) +
  geom_density_2d(data = true_quasars, mapping = aes(x = ug, y = gr), col = colors[3], alpha = 0.8) +
  geom_density_2d(data = true_non_quasars, mapping = aes(x = ug, y = gr), col = colors[4], alpha = 0.8) +
  labs(x = "u - g", y = "g - r") +
  theme_minimal() +
  theme(legend.position = "none", text = element_text(family = "Times"), axis.title=element_text(family="Times", face = "italic"), panel.grid.minor=element_blank())

p2 <- ggplot(data=NULL) +
  geom_density_2d(data = pred_quasars, mapping = aes(x = gr, y = ri), col = colors[1], alpha = 0.8) +
  geom_density_2d(data = trad_quasars, mapping = aes(x = gr, y = ri), col = colors[2], alpha = 0.8) +
  geom_density_2d(data = true_quasars, mapping = aes(x = gr, y = ri), col = colors[3], alpha = 0.8) +
  geom_density_2d(data = true_non_quasars, mapping = aes(x = gr, y = ri), col = colors[4], alpha = 0.8) +
  labs(x = "g - r", y = "r - i") +
  theme_minimal() +
  theme(legend.position = "none", text = element_text(family = "Times"), axis.title=element_text(family="Times", face = "italic"), panel.grid.minor=element_blank())


p3 <- ggplot(data=NULL) +
  geom_density_2d(data = pred_quasars, mapping = aes(x = ri, y = iz), col = colors[1], alpha = 0.8) +
  geom_density_2d(data = trad_quasars, mapping = aes(x = ri, y = iz), col = colors[2], alpha = 0.8) +
  geom_density_2d(data = true_quasars, mapping = aes(x = ri, y = iz), col = colors[3], alpha = 0.8) +
  geom_density_2d(data = true_non_quasars, mapping = aes(x = ri, y = iz), col = colors[4], alpha = 0.8) +
  labs(x = "r - i", y = "i - z") +
  theme_minimal() +
  theme(legend.position = "none", text = element_text(family = "Times"), axis.title=element_text(family="Times", face = "italic"), panel.grid.minor=element_blank())

p4 <- ggplot(data=NULL) +
  geom_density_2d(data = pred_quasars, mapping = aes(x = iz, y = zs1), col = colors[1], alpha = 0.8) +
  geom_density_2d(data = trad_quasars, mapping = aes(x = iz, y = zs1), col = colors[2], alpha = 0.8) +
  geom_density_2d(data = true_quasars, mapping = aes(x = iz, y = zs1), col = colors[3], alpha = 0.8) +
  geom_density_2d(data = true_non_quasars, mapping = aes(x = iz, y = zs1), col = colors[4], alpha = 0.8) +
  labs(x = "i - z", y = "z - s1") +
  theme_minimal() +
  theme(legend.position = "none", text = element_text(family = "Times"), axis.title=element_text(family="Times", face = "italic"), panel.grid.minor=element_blank())

p5 <- ggplot(data=NULL) +
  geom_density_2d(data = pred_quasars, mapping = aes(x = zs1, y = s1s2), col = colors[1], alpha = 0.8) +
  geom_density_2d(data = trad_quasars, mapping = aes(x = zs1, y = s1s2), col = colors[2], alpha = 0.8) +
  geom_density_2d(data = true_quasars, mapping = aes(x = zs1, y = s1s2), col = colors[3], alpha = 0.8) +
  geom_density_2d(data = true_non_quasars, mapping = aes(x = zs1, y = s1s2), col = colors[4], alpha = 0.8) +
  labs(x = "z - s1", y = "s1 - s2") +
  theme_minimal() +
  theme(legend.position = "none", text = element_text(family = "Times"), axis.title=element_text(family="Times", face = "italic"), panel.grid.minor=element_blank())

# extract legend
legend <- get_legend(
  ggplot(data=NULL) +
    geom_density_2d(data = pred_quasars, mapping = aes(x = zs1, y = s1s2, col = colors[1])) +
    geom_density_2d(data = trad_quasars, mapping = aes(x = zs1, y = s1s2, col = colors[2])) +
    geom_density_2d(data = true_quasars, mapping = aes(x = zs1, y = s1s2, col = colors[3])) +
    geom_density_2d(data = true_non_quasars, mapping = aes(x = ri, y = iz, col = colors[4])) +
    labs(col = "") +
    scale_color_manual(values = colors, labels = c("With measurement error", "Without measurement error", "True quasars", "True AE")) +
    theme(text = element_text(family = "Times")) +
    theme_minimal()
)

# top row plots
top <- plot_grid(p1, p2, p3, nrow = 1)

# bottom row plots+legend
bottom <- plot_grid(p4, p5, legend, ncol = 3)

# create some space to the left of the legend
plot_grid(top, bottom, ncol = 1) # 750x500

ggsave("../Plots/color_color_grid.png", units = "cm", width = 21, height = 14, dpi = 700)

