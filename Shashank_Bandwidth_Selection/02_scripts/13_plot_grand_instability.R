# ==============================================================================
# FILE: 13_plot_grand_instability.R
# PURPOSE: Generate the 4-Panel "Instability Trap" Visualization
# ==============================================================================

library(ggplot2)
library(dplyr)

# 1. LOAD DGP FUNCTIONS
# We need the standard DGP (Cases 1-3) and the Complex DGP
source("01_functions/01_dgp.R")

# Define the Complex DGP locally to ensure consistency with the plotting logic
generate_complex_dgp <- function(N, cutoff = 0, tau = 5) {
  X <- runif(N, -1, 1)
  Z <- 0.3*X + rnorm(N, sd = 1)
  T <- ifelse(X >= cutoff, 1, 0)
  # High noise (sd=1) and covariates included
  Y <- tau * T + 2*X + 3*X^2 + 1.5*Z + rnorm(N, sd = 1)
  data.frame(X = X, Y = Y, Type = "Case 4: Complex DGP (High Noise)")
}

# 2. GENERATE DATA (N=100 for visual clarity)
set.seed(42) # Fixed seed for reproducible noise
N_plot <- 100

# Generate Case 1: Linear
d1 <- generate_rdd_data(N = N_plot, tau = 5, case = 1, sigma_eps = 1)
d1$Type <- "Case 1: Linear (Baseline)"

# Generate Case 2: Quadratic
d2 <- generate_rdd_data(N = N_plot, tau = 5, case = 2, sigma_eps = 1)
d2$Type <- "Case 2: Quadratic (Standard)"

# Generate Case 3: Structural Break
d3 <- generate_rdd_data(N = N_plot, tau = 5, case = 3, sigma_eps = 1)
d3$Type <- "Case 3: Structural Break (Stress Test)"

# Generate Case 4: Complex
d4 <- generate_complex_dgp(N = N_plot, tau = 5)
d4$W <- ifelse(d4$X >= 0, 1, 0) # Align column structure

# Combine all into one dataframe
df_all <- rbind(d1[, c("X","Y","Type")], 
                d2[, c("X","Y","Type")], 
                d3[, c("X","Y","Type")], 
                d4[, c("X","Y","Type")])

# Set Factor Order for 2x2 grid layout
df_all$Type <- factor(df_all$Type, levels = c(
  "Case 1: Linear (Baseline)", "Case 2: Quadratic (Standard)",
  "Case 3: Structural Break (Stress Test)", "Case 4: Complex DGP (High Noise)"
))

# 3. DEFINE TRUE SIGNAL (No Noise)
x_grid <- seq(-1, 1, length.out = 200)

# Truth for Case 1
y1 <- ifelse(x_grid < 0, 2 + x_grid + 0.5*x_grid^2, 2 + x_grid + 0.5*x_grid^2 + 5)
t1 <- data.frame(X=x_grid, Y=y1, Type="Case 1: Linear (Baseline)")

# Truth for Case 2
y2 <- ifelse(x_grid < 0, 2 + x_grid + 2*x_grid^2, 2 + x_grid + 4*x_grid^2 + 5)
t2 <- data.frame(X=x_grid, Y=y2, Type="Case 2: Quadratic (Standard)")

# Truth for Case 3
y3 <- ifelse(x_grid < 0, 2 + x_grid - 6*x_grid^2, 2 + x_grid + 6*x_grid^2 + 5)
t3 <- data.frame(X=x_grid, Y=y3, Type="Case 3: Structural Break (Stress Test)")

# Truth for Case 4 (E[Z]=0)
y4 <- ifelse(x_grid < 0, 2*x_grid + 3*x_grid^2, 2*x_grid + 3*x_grid^2 + 5)
t4 <- data.frame(X=x_grid, Y=y4, Type="Case 4: Complex DGP (High Noise)")

truth_all <- rbind(t1, t2, t3, t4)
truth_all$Type <- factor(truth_all$Type, levels = levels(df_all$Type))

# 4. PLOT GENERATION
p <- ggplot() +
  # Noisy Data (Grey Dots)
  geom_point(data = df_all, aes(x = X, y = Y), color = "grey70", alpha = 0.6, size = 1.5) +
  
  # True Signal (Dark Red Line)
  geom_line(data = truth_all, aes(x = X, y = Y), color = "darkred", size = 1.2) +
  
  # Cutoff Line
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  
  # Faceting
  facet_wrap(~Type, scales = "free_y", ncol = 2) +
  
  # Styling
  theme_minimal() +
  labs(
    title = "The Instability Trap: Comparing 4 Geometries (N=100)",
    subtitle = "Red Line = True Signal | Grey Dots = Noisy Data\nNotice how the noise in all cases (especially Case 4) obscures the red line.",
    x = "Running Variable (X)",
    y = "Outcome (Y)"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank()
  )

# 5. SAVE OUTPUT
if(!dir.exists("03_output")) dir.create("03_output")
ggsave("03_output/plot_grand_instability.pdf", plot = p, width = 10, height = 8)

cat("Success: 4-Panel plot saved to 03_output/plot_grand_instability.pdf\n")