# ==============================================================================
# FILE: 02_estimator.R
# PURPOSE: Local Linear Regression with Triangular Kernel.
# ==============================================================================

rd_estimator <- function(data, h, cutoff = 0) {
  
  X <- data$X
  Y <- data$Y
  
  # 1. Subset Data within bandwidth
  idx <- abs(X - cutoff) <= h
  Xh <- X[idx]
  Yh <- Y[idx]
  
  # Safety Check: Need points on both sides
  if (sum(Xh < cutoff) < 5 || sum(Xh >= cutoff) < 5) return(NA)
  
  # 2. Calculate Triangular Weights
  # Weight decays linearly from 1 (at cutoff) to 0 (at bandwidth edge)
  weights <- (1 - abs(Xh - cutoff) / h)
  
  # 3. Local Linear Regression (Weighted)
  # We fit separate regressions for Left and Right sides
  dat_local <- data.frame(Y = Yh, X = Xh, W = weights)
  
  # Left Side
  fit_L <- tryCatch(
    lm(Y ~ X, data = subset(dat_local, X < cutoff), weights = W),
    error = function(e) return(NA)
  )
  
  # Right Side
  fit_R <- tryCatch(
    lm(Y ~ X, data = subset(dat_local, X >= cutoff), weights = W),
    error = function(e) return(NA)
  )
  
  if (!is.list(fit_L) || !is.list(fit_R)) return(NA)
  
  # 4. Predict at Cutoff
  mu_minus <- predict(fit_L, newdata = data.frame(X = cutoff))
  mu_plus  <- predict(fit_R, newdata = data.frame(X = cutoff))
  
  return(as.numeric(mu_plus - mu_minus))
}