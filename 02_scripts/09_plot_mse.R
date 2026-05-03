# ==============================================================================
# FILE: 09_plot_mse.R
# PURPOSE: Visualize the Bias-Variance Tradeoff (The "U-Shape" Curve)
# ==============================================================================

library(ggplot2)
library(dplyr)

# Check if results exist (Assuming this is run after 07_run_simulation.R)
if (!exists("results_all")) {
  warning("Warning: 'results_all' object not found in memory. Ensure simulations ran.")
}

if(!dir.exists("03_output")) dir.create("03_output")

# Function to plot a single case
plot_mse_curve <- function(res_fixed, case_name, filename) {
  
  # Extract Data
  h_vals <- as.numeric(gsub("h=", "", colnames(res_fixed)))
  mse_vals <- as.numeric(res_fixed["mse", ])
  bias_sq_vals <- as.numeric(res_fixed["bias", ])^2
  var_vals <- as.numeric(res_fixed["var", ])
  
  plot_data <- data.frame(
    h = h_vals,
    MSE = mse_vals,
    BiasSq = bias_sq_vals,
    Variance = var_vals
  )
  
  # Plot
  p <- ggplot(plot_data, aes(x = h)) +
    geom_line(aes(y = MSE, color = "Total MSE"), linewidth = 1.2) +
    geom_line(aes(y = Variance, color = "Variance"), linetype = "dashed") +
    geom_line(aes(y = BiasSq, color = "Bias^2"), linetype = "dotted") +
    geom_point(aes(y = MSE), size = 3) +
    labs(
      title = paste("Bias-Variance Tradeoff:", case_name),
      subtitle = "Dashed=Variance (Noise), Dotted=Bias^2 (Curvature), Solid=Total MSE",
      y = "Error Metrics",
      x = "Bandwidth (h)"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    scale_color_manual(values = c("Total MSE" = "black", "Variance" = "blue", "Bias^2" = "red"))
  
  ggsave(filename, plot = p, width = 8, height = 6)
  cat(paste("Saved:", filename, "\n"))
}

# Generate Plots for Case 1 and Case 3 (The best examples)
if (exists("results_all")) {
  if (!is.null(results_all$case1)) {
    plot_mse_curve(results_all$case1$fixed, "Case 1 (Symmetric)", "03_output/plot_mse_case1.pdf")
  }
  if (!is.null(results_all$case3)) {
    plot_mse_curve(results_all$case3$fixed, "Case 3 (Stress Test)", "03_output/plot_mse_case3.pdf")
  }
}