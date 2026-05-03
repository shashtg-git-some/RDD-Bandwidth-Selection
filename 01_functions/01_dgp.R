# ==============================================================================
# FILE: 01_dgp.R
# PURPOSE: Functions to generate synthetic RDD datasets.
# ==============================================================================

generate_rdd_data <- function(N, tau, case, cutoff=0, sigma_eps=1) {
  
  # Parameter Recall
  if (!exists("cutoff")) cutoff <- 0
  if (!exists("sigma_eps")) sigma_eps <- 1
  
  X <- runif(N, -1, 1)
  eps <- rnorm(N, 0, sigma_eps)
  
  # --- CASE 1: Knife-Edge ---
  if (case == 1) {
    # Symmetric curvature (Assumption 3.6 fails)
    # This remains gentle/flatish, which is fine for the baseline.
    m <- 2 + X + 0.5 * X^2
  }
  
  # --- CASE 2: Moderate Asymmetry ---
  if (case == 2) {
    # Now you will visually see the curve "bend" differently on each side.
    m <- ifelse(X < 0, 
                2 + X + 2.0 * X^2, 
                2 + X + 4.0 * X^2)
  }
  
  # --- CASE 3: Severe Asymmetry / Stress Test ---
  if (case == 3) {
    # This makes the "S" very sharp. A linear ruler will fail hard here.
    # Left side curves DOWN sharply, Right side curves UP sharply.
    m <- ifelse(X < 0, 
                2 + X - 6.0 * X^2, 
                2 + X + 6.0 * X^2)
  }
  
  W <- as.numeric(X >= cutoff)
  Y <- m + tau * W + eps
  
  return(data.frame(X = X, Y = Y, W = W))
}