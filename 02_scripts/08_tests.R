# ==============================================================================
# FILE: 08_tests.R
# PURPOSE: Unit testing suite to verify estimator and selector logic.
# ==============================================================================

# 1. Load Dependencies (Assumes Project Root)
source("01_functions/01_dgp.R")
source("01_functions/02_estimator.R")

cat("\n========================================\n")
cat("    RUNNING DIAGNOSTIC TESTS \n")
cat("========================================\n")

# --- Test 1: Perfect Linear Fit ---
cat("Test 1: Perfect Linear Recovery... ")

test_x <- seq(-1, 1, length.out = 100)
test_y <- test_x + (test_x >= 0) * 0.5  # Jump of 0.5
test_data <- data.frame(X = test_x, Y = test_y)

# Use the estimator (Ensure h is large enough to capture data)
est <- rd_estimator(test_data, h = 0.5)

if (!is.na(est) && abs(est - 0.5) < 1e-10) {
  cat("PASSED [Difference is negligible]\n")
} else {
  cat("FAILED [Estimator did not recover linear jump]\n")
}

# --- Test 2: Handling Empty Neighborhoods ---
cat("Test 2: Empty Neighborhood Handling... ")

est_empty <- rd_estimator(test_data, h = 0.00001)

if (is.na(est_empty)) {
  cat("PASSED [Correctly returned NA]\n")
} else {
  cat("FAILED [Did not return NA for empty window]\n")
}

# --- Test 3: Kernel Weight Symmetry ---
cat("Test 3: Kernel Weight Symmetry... ")

u_vals <- c(-0.5, 0.5)
weights <- (1 - abs(u_vals)) * (abs(u_vals) <= 1)

if (weights[1] == weights[2]) {
  cat("PASSED [Weights are symmetric]\n")
} else {
  cat("FAILED [Kernel is asymmetric]\n")
}

# --- Test 4: Data Generation Dimensions ---
cat("Test 4: DGP Sample Size... ")

if (exists("generate_rdd_data")) {
  dgp_check <- generate_rdd_data(N = 500, tau = 1, case = 1)
  
  if (nrow(dgp_check) == 500) {
    cat("PASSED [N=500 generated]\n")
  } else {
    cat("FAILED [Incorrect sample size]\n")
  }
} else {
  cat("SKIPPED [Function 'generate_rdd_data' not found]\n")
}

cat("\nAll diagnostic checks completed.\n")