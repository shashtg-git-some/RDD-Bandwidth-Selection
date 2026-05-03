# ==============================================================================
# FILE: 03_bandwidth_ik.R
# PURPOSE: Imbens-Kalyanaraman (2012) Optimal Bandwidth Selector.
# ==============================================================================

IK_bandwidth <- function(X, Y, cutoff = 0) {
  
  N <- length(X)
  
  # Triangular Kernel Constant (Imbens & Kalyanaraman default)
  CK <- 3.4375
  
  # --- Step 1: Pilot Estimation ---
  h1 <- 1.84 * sd(X) * N^(-1/5)
  
  # Safety Loop for sparse data
  idx <- abs(X - cutoff) <= h1
  Nh <- sum(idx)
  
  if (Nh < 10) {
    h1 <- h1 * 2
    idx <- abs(X - cutoff) <= h1
    Nh <- sum(idx)
  }
  
  f_hat <- Nh / (2 * N * h1)
  
  if (Nh > 5) {
    sigma2_hat <- var(residuals(lm(Y[idx] ~ X[idx])))
  } else {
    sigma2_hat <- var(Y)
  }
  
  # --- Step 2: Curvature Estimation ---
  q25 <- quantile(X, 0.25); q75 <- quantile(X, 0.75)
  trim_idx <- (X >= q25) & (X <= q75)
  
  X_trim <- X[trim_idx]; Y_trim <- Y[trim_idx]
  D_trim <- as.numeric(X_trim >= cutoff)
  
  # Global Cubic Fit
  cubic_fit <- lm(Y_trim ~ D_trim + I(X_trim - cutoff) + I((X_trim - cutoff)^2) + I((X_trim - cutoff)^3))
  m3_hat <- 6 * coef(cubic_fit)[4] # 4th coef is cubic term usually
  if(is.na(m3_hat)) m3_hat <- 0
  
  denom_h2 <- max(m3_hat^2, 0.01, na.rm = TRUE)
  h2 <- 3.56 * (sigma2_hat / (f_hat * denom_h2))^(1/7) * N^(-1/7)
  
  # Estimate 2nd Derivatives (m'')
  idx_R <- (X >= cutoff) & (X <= cutoff + h2)
  fit_R <- lm(Y[idx_R] ~ I(X[idx_R]-cutoff) + I((X[idx_R]-cutoff)^2))
  m2_plus <- 2 * coef(fit_R)[3]
  
  idx_L <- (X < cutoff) & (X >= cutoff - h2)
  fit_L <- lm(Y[idx_L] ~ I(X[idx_L]-cutoff) + I((X[idx_L]-cutoff)^2))
  m2_minus <- 2 * coef(fit_L)[3]
  
  # --- Step 3: Regularization & Final Calculation ---
  r_plus  <- 720 * sigma2_hat / (sum(idx_R) * h2^4)
  r_minus <- 720 * sigma2_hat / (sum(idx_L) * h2^4)
  
  curvature_term <- (m2_plus - m2_minus)^2 + r_plus + r_minus
  if(is.na(curvature_term)) curvature_term <- 0.1
  
  h_opt <- CK * (2 * sigma2_hat / (f_hat * curvature_term))^(1/5) * N^(-1/5)
  return(as.numeric(h_opt))
}