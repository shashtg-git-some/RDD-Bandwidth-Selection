# ==============================================================================
# FILE: 09_plot_kernel.R
# PURPOSE: Visualize Triangular Kernel Weights
# ==============================================================================

library(ggplot2)

if(!dir.exists("03_output")) dir.create("03_output")

cat("Generating Kernel Weight Illustration...\n")

# Define range
x <- seq(-1.5, 1.5, length.out = 300)
h <- 1.0

# Calculate Triangular Weights
w <- ifelse(abs(x) <= h, (1 - abs(x)/h), 0)
df <- data.frame(x = x, weight = w)

p <- ggplot(df, aes(x = x, y = weight)) +
  geom_area(fill = "skyblue", alpha = 0.5) +
  geom_line(color = "blue", linewidth = 1.2) +
  geom_vline(xintercept = c(-1, 1), linetype = "dashed") +
  labs(title = "Triangular Kernel Weights",
       subtitle = "Visualizing how data is weighted within bandwidth h=1.0",
       x = "Distance from Cutoff (X)", y = "Weight applied to Regression") +
  theme_classic() +
  coord_cartesian(ylim = c(0, 1.1))

ggsave("03_output/plot_kernel_weights.pdf", plot = p, width = 6, height = 4)
cat("Saved: 03_output/plot_kernel_weights.pdf\n")