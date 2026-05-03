# ==============================================================================
# FILE: 09_improved_dgp_plots.R
# PURPOSE: Generate polished DGP plots and a LaTeX wrapper for slides
# ==============================================================================

library(ggplot2)
library(dplyr)
library(gridExtra)

# 1. SETUP
source("01_functions/01_dgp.R")

# Ensure the output folder exists
if(!dir.exists("03_output")) dir.create("03_output")

# 2. GENERATE HIGH-QUALITY DATA FOR PLOTTING
get_true_lines <- function() {
  df_all <- data.frame()
  
  # Loop through 3 cases to generate data
  for (case in 1:3) {
    temp_dat <- generate_rdd_data(case = case, N = 1000, tau = 1)
    
    # Assign descriptive labels for the plots
    if (case == 1) lab <- "Case 1: Knife-Edge (Symmetric)"
    if (case == 2) lab <- "Case 2: Moderate Asymmetry"
    if (case == 3) lab <- "Case 3: S-Shape (Stress Test)"
    
    temp_dat$Label <- lab
    df_all <- rbind(df_all, temp_dat)
  }
  return(df_all)
}

data_all <- get_true_lines()

# 3. PLOT A: "THE TRUTH" (Geometry Only)
plot_truth <- ggplot(data_all, aes(x = X, y = Y)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE, 
              aes(group = (X > 0)), color = "#D55E00", linewidth = 1.2) +
  facet_wrap(~Label, scales = "free_y") +
  theme_minimal() +
  labs(title = "The Geometry of the 3 Cases (True Functions)",
       subtitle = "Red lines show the underlying signal without noise",
       x = "Running Variable (X)", y = "Outcome (Y)") +
  theme(strip.text = element_text(face = "bold", size = 12),
        panel.grid.minor = element_blank())

ggsave("03_output/plot_dgp_truth_only.pdf", plot_truth, width = 10, height = 7)

# 4. PLOT B: "THE REALITY" (Scatter + Truth)
plot_clean_scatter <- ggplot(data_all, aes(x = X, y = Y)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  geom_point(alpha = 0.15, size = 0.8, color = "gray30") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE, 
              aes(group = (X > 0)), color = "#D55E00", linewidth = 1.2) +
  facet_wrap(~Label, scales = "free_y") +
  coord_cartesian(xlim = c(-0.8, 0.8)) + 
  theme_light() +
  labs(title = "Simulated Data with True DGP Overlay",
       subtitle = "Gray dots = Single realization of noise (N=1000). Red Line = True Expectation.",
       x = "Running Variable (X)", y = "Outcome (Y)") +
  theme(strip.text = element_text(face = "bold", size = 11, color = "black"),
        strip.background = element_rect(fill = "gray90"),
        panel.grid.minor = element_blank())

ggsave("03_output/plot_dgp_clean_scatter.pdf", plot_clean_scatter, width = 10, height = 7)

cat("\nDone! Plots saved as PDFs in 03_output/.\n")