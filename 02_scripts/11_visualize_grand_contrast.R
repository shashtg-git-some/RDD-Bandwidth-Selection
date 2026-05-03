# ==============================================================================
# FILE: 11_plot_grand_contrast.R
# PURPOSE: Visual comparison of Lennart's "Smooth" DGP vs Your "Tricky" DGP
# ==============================================================================

library(ggplot2)
library(gridExtra)

# 1. Define the True Functions (No Noise)
x_seq <- seq(-1, 1, length.out = 500)

# --- Lennart's Geometry (Smooth Quadratic) ---
y_lennart <- ifelse(x_seq < 0, 
                    2*x_seq + 3*x_seq^2 + 10,   
                    2*x_seq + 3*x_seq^2 + 10 + 5) 
df_lennart <- data.frame(X = x_seq, Y = y_lennart, Type = "Lennart's DGP (Smooth)")

# --- Your Case 3 Geometry (The Stress Test) ---
y_yours <- ifelse(x_seq < 0, 
                  2 + x_seq - 1.5 * x_seq^2,        # Left (Frown)
                  2 + x_seq + 1.5 * x_seq^2 + 1)    # Right (Smile + Shift)

df_yours <- data.frame(X = x_seq, Y = y_yours, Type = "Your Case 3 (Stress Test)")

# Combine
df_all <- rbind(df_lennart, df_yours)

# 2. Create the Comparison Plot
plot_contrast <- ggplot(df_all, aes(x = X, y = Y)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_line(color = "darkred", size = 1.5) +
  facet_wrap(~Type, scales = "free_y") +
  theme_minimal() +
  labs(title = "Why CV Fails vs. Succeeds: A Geometric Comparison",
       x = "Running Variable (X)", y = "True Outcome (Y)") +
  theme(strip.text = element_text(face = "bold", size = 14),
        plot.title = element_text(face = "bold", size = 16))

# 3. Save
if(!dir.exists("03_output")) dir.create("03_output")
ggsave("03_output/plot_dgp_contrast.pdf", plot_contrast, width = 10, height = 6)

cat("Created comparison plot: 03_output/plot_dgp_contrast.pdf\n")