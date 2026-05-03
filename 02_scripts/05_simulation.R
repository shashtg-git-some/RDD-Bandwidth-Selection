# ==============================================================================
# FILE: 05_simulation.R
# PURPOSE: Monte Carlo Engine.
# ==============================================================================

simulate_case <- function(case, N) {
  
  # Parameter Recall
  if (!exists("S")) S <- 100
  if (!exists("bandwidths_fixed")) bandwidths_fixed <- c(0.2, 0.4)
  if (!exists("bandwidths_cv_grid")) bandwidths_cv_grid <- seq(0.1, 1.0, 0.1)
  
  # Setup Storage
  tau_fixed_mat <- matrix(NA, nrow = S, ncol = length(bandwidths_fixed))
  tau_IK <- numeric(S); h_IK <- numeric(S)
  tau_CV <- numeric(S); h_CV <- numeric(S)
  
  for (s in 1:S) {
    data <- generate_rdd_data(N, tau_true, case)
    
    # A. Fixed Bandwidths
    for (j in seq_along(bandwidths_fixed)) {
      tau_fixed_mat[s, j] <- rd_estimator(data, bandwidths_fixed[j])
    }
    
    # B. IK Selector
    h_temp <- IK_bandwidth(data$X, data$Y, cutoff)
    if (!is.na(h_temp)) {
      h_IK[s] <- h_temp
      tau_IK[s] <- rd_estimator(data, h_IK[s])
    }
    
    # C. CV Selector
    cv_res <- CV_bandwidth(data, bandwidths_cv_grid, cutoff, eval_band = 0.8)
    h_CV[s] <- cv_res$h_opt
    tau_CV[s] <- rd_estimator(data, h_CV[s])
    
    if (s %% (S/10) == 0) cat(sprintf("Case %d: Sim %d/%d...\n", case, s, S))
  }
  
  # Aggregate Stats
  results_fixed <- matrix(NA, nrow = 3, ncol = length(bandwidths_fixed))
  rownames(results_fixed) <- c("bias", "var", "mse")
  colnames(results_fixed) <- paste0("h=", bandwidths_fixed)
  
  results_fixed["bias", ] <- colMeans(tau_fixed_mat - tau_true, na.rm = TRUE)
  results_fixed["var", ]  <- apply(tau_fixed_mat, 2, var, na.rm = TRUE)
  results_fixed["mse", ]  <- colMeans((tau_fixed_mat - tau_true)^2, na.rm = TRUE)
  
  stats_IK <- c(bias = mean(tau_IK - tau_true, na.rm=T), var = var(tau_IK, na.rm=T), mse = mean((tau_IK - tau_true)^2, na.rm=T), h_avg = mean(h_IK, na.rm=T))
  stats_CV <- c(bias = mean(tau_CV - tau_true, na.rm=T), var = var(tau_CV, na.rm=T), mse = mean((tau_CV - tau_true)^2, na.rm=T), h_avg = mean(h_CV, na.rm=T))
  
  return(list(fixed = results_fixed, IK = stats_IK, CV = stats_CV))
}