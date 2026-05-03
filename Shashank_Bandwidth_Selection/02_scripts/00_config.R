# ==============================================================================
# FILE: 00_config.R
# PURPOSE: Global configuration, parameters, and libraries.
# ==============================================================================

rm(list = ls())
set.seed(123)

# 1. Simulation Settings
S <- 500              # Number of Monte Carlo simulations
N_small <- 1000       # Finite sample size (High Variance context)

# 2. RDD Parameters
tau_true <- 1         # True treatment effect
cutoff <- 0           # Threshold
sigma_eps <- 1        # Noise SD

# 3. Bandwidth Grids
bandwidths_fixed <- c(0.05, 0.2, 0.4, 0.6, 0.8)
bandwidths_cv_grid <- seq(0.1, 1.0, by = 0.05)

# 4. Constants
CK <- 3.4375          # IK constant for Triangular Kernel

cat(" [INFO] Configuration loaded successfully.\n")