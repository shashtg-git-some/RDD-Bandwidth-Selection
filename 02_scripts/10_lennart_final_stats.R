# ==============================================================================
# FILE: 10_run_lennart_sims.R
# PURPOSE: Calculate stats for Lennart's DGP and save to 03_output/
# ==============================================================================

# 1. LOAD YOUR ALGORITHMS (From 01_functions)
source("01_functions/03_bandwidth_ik.R")
source("01_functions/04_bandwidth_cv.R")

# 2. DEFINE ROBUST ESTIMATOR (Crash-Proof)
rd_estimator <- function(data, h) {
  data_sub <- subset(data, abs(X) <= h)
  if (sum(data_sub$X < 0) < 5 || sum(data_sub$X >= 0) < 5) return(NA)
  w <- (1 - abs(data_sub$X) / h)
  fit_left  <- lm(Y ~ X, data = subset(data_sub, X < 0), weights = subset(w, data_sub$X < 0))
  fit_right <- lm(Y ~ X, data = subset(data_sub, X >= 0), weights = subset(w, data_sub$X >= 0))
  mu_minus <- predict(fit_left, newdata = data.frame(X = 0))
  mu_plus  <- predict(fit_right, newdata = data.frame(X = 0))
  return(mu_plus - mu_minus)
}

# 3. LENNART'S DGP
generate_complex_srd <- function(N, cutoff = 0, tau = 5, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  X <- runif(N, -1, 1)
  Z <- 0.3*X + rnorm(N, sd = 1)
  T <- ifelse(X >= cutoff, 1, 0)
  Y <- tau * T + 2*X + 3*X^2 + 1.5*Z + rnorm(N, sd = 1)
  data.frame(X = X, Y = Y, T = T)
}

# 4. SIMULATION RUNNER
get_final_stats <- function(N_val, n_sims = 200) {
  cat(paste0("\nRunning ", n_sims, " Simulations for N = ", N_val, "...\n"))
  
  results <- data.frame(h_ik = numeric(n_sims), tau_ik = numeric(n_sims),
                        h_cv = numeric(n_sims), tau_cv = numeric(n_sims))
  
  # Define Grid for CV
  cv_grid <- seq(0.1, 1.5, by = 0.1)
  
  for(i in 1:n_sims) {
    dat <- generate_complex_srd(N = N_val, seed = 123 + i)
    
    # --- IK ---
    h_ik <- IK_bandwidth(dat$X, dat$Y) 
    results$h_ik[i] <- h_ik
    results$tau_ik[i] <- rd_estimator(dat, h_ik)
    
    # --- CV ---
    cv_res <- CV_bandwidth(dat, bandwidth_grid = cv_grid)
    h_cv <- cv_res$h_opt  
    
    results$h_cv[i] <- h_cv
    results$tau_cv[i] <- rd_estimator(dat, h_cv)
    
    if(i %% 50 == 0) cat(paste0(i, ".."))
  }
  
  # Helper to calculate metrics
  calc_metrics <- function(ests, h_vals) {
    valid_ests <- ests[!is.na(ests)]
    valid_h    <- h_vals[!is.na(ests)]
    
    bias <- mean(valid_ests) - 5
    var  <- var(valid_ests)
    mse  <- bias^2 + var
    h_avg <- mean(valid_h)
    
    return(c(h_avg, bias, var, mse))
  }
  
  stats_ik <- calc_metrics(results$tau_ik, results$h_ik)
  stats_cv <- calc_metrics(results$tau_cv, results$h_cv)
  
  return(list(ik = stats_ik, cv = stats_cv))
}

# 5. EXECUTE SIMULATIONS
res_100  <- get_final_stats(100, n_sims = 200)
res_2000 <- get_final_stats(2000, n_sims = 200)

# 6. FORMAT DATA FOR EXPORT
final_df <- data.frame(
  "Sample Size" = c("Small (N=100)", "Small (N=100)", "Large (N=2000)", "Large (N=2000)"),
  "Algorithm"   = c("IK", "CV", "IK", "CV"),
  "Avg Bandwidth" = sprintf("%.4f", c(res_100$ik[1], res_100$cv[1], res_2000$ik[1], res_2000$cv[1])),
  "Bias"        = sprintf("%.4f", c(res_100$ik[2], res_100$cv[2], res_2000$ik[2], res_2000$cv[2])),
  "Var"         = sprintf("%.4f", c(res_100$ik[3], res_100$cv[3], res_2000$ik[3], res_2000$cv[3])),
  "MSE"         = sprintf("%.4f", c(res_100$ik[4], res_100$cv[4], res_2000$ik[4], res_2000$cv[4]))
)

# 7. SAVE TO LATEX FILE
if(!dir.exists("03_output")) dir.create("03_output")
file_path <- "03_output/lennart_final_stats.tex"

cat("\\begin{table}[h]\n", file = file_path)
cat("\\centering\n", file = file_path, append = TRUE)
cat("\\caption{Final Comparison for Quadratic DGP (Lennart's Case)}\n", file = file_path, append = TRUE)
cat("\\begin{tabular}{llcccc}\n", file = file_path, append = TRUE)
cat("\\hline\n", file = file_path, append = TRUE)
cat("Sample & Algorithm & Avg $h$ & Bias & Var & MSE \\\\\n", file = file_path, append = TRUE)
cat("\\hline\n", file = file_path, append = TRUE)

for (i in 1:nrow(final_df)) {
  row_str <- paste(final_df[i, ], collapse = " & ")
  cat(paste0(row_str, " \\\\\n"), file = file_path, append = TRUE)
  if (i == 2) cat("\\hline\n", file = file_path, append = TRUE) 
}

cat("\\hline\n", file = file_path, append = TRUE)
cat("\\end{tabular}\n", file = file_path, append = TRUE)
cat("\\label{tab:lennart_final}\n", file = file_path, append = TRUE)
cat("\\end{table}\n", file = file_path, append = TRUE)

cat("\n\nSUCCESS: Table saved to '03_output/lennart_final_stats.tex'\n")
print(final_df)