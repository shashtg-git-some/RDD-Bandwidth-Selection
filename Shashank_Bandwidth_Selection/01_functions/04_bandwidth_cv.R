# ==============================================================================
# FILE: 04_bandwidth_cv.R
# PURPOSE: Ludwig-Miller Cross-Validation Bandwidth Selector.
# ==============================================================================

CV_bandwidth <- function(data, bandwidth_grid, cutoff = 0, eval_band = 0.8, min_obs = 10) {
  
  X <- data$X; Y <- data$Y
  
  # Restrict to evaluation window (Median Band)
  threshold <- quantile(abs(X - cutoff), eval_band)
  eval_indices <- which(abs(X - cutoff) <= threshold)
  
  cv_errors <- numeric(length(bandwidth_grid))
  
  for (j in seq_along(bandwidth_grid)) {
    h <- bandwidth_grid[j]
    sq_errors <- numeric(length(eval_indices))
    valid_count <- 0
    
    # Leave-One-Out Loop
    for (k in seq_along(eval_indices)) {
      i <- eval_indices[k]
      xi <- X[i]; yi <- Y[i]
      
      # Determine neighbors (Same-Side Rule)
      if (xi < cutoff) {
        idx <- (X < cutoff) & (abs(X - xi) <= h)
      } else {
        idx <- (X >= cutoff) & (abs(X - xi) <= h)
      }
      
      idx[i] <- FALSE # Leave out the current point
      
      if (sum(idx) < min_obs) {
        sq_errors[k] <- NA; next
      }
      
      # WEIGHTED Local Linear Prediction (Triangular Kernel)
      # We calculate weights relative to the target point 'xi'
      dists <- abs(X[idx] - xi)
      weights <- (1 - dists / h)
      
      # Fit model
      fit <- lm(Y[idx] ~ X[idx], weights = weights)
      y_hat <- predict(fit, newdata = data.frame(X = xi))
      
      sq_errors[k] <- (yi - y_hat)^2
      valid_count <- valid_count + 1
    }
    
    # Penalize if too many points failed
    if (valid_count < length(eval_indices) * 0.5) {
      cv_errors[j] <- Inf
    } else {
      cv_errors[j] <- mean(sq_errors, na.rm = TRUE)
    }
  }
  
  h_opt <- bandwidth_grid[which.min(cv_errors)]
  return(list(h_opt = h_opt, cv_errors = cv_errors))
}