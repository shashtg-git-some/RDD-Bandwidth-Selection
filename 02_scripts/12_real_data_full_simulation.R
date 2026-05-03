# ==============================================================================
# FILE: 12_real_data_full_simulation.R
# PURPOSE: Run FULL IK & CV Algorithms on Real Senate Data (Output: Table + Plot)
# ==============================================================================

library(rdrobust)
library(dplyr)
library(ggplot2)

# 1. LOAD & CLEAN DATA
data("rdrobust_RDsenate")
df_cleaned <- as.data.frame(rdrobust_RDsenate) %>%
  rename(X = margin, Y = vote) %>%
  filter(!is.na(X), !is.na(Y)) %>%
  filter(X >= -50, X <= 50) 

# 2. DEFINE ESTIMATOR (Robust)
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

# 3. DEFINE IK ALGORITHM (Localized)
bw_ik <- function(data) {
  X <- data$X; Y <- data$Y; N <- length(X)
  h1 <- 1.84 * sd(X) * N^(-1/5)
  inds <- abs(X) <= h1
  if(sum(inds) < 10) return(10.0) 
  
  f_c <- sum(inds) / (2 * N * h1)
  sigma2 <- var(Y[inds])
  
  h2 <- 3.56 * sd(X) * N^(-1/7)
  dat_c <- data[abs(X) <= h2, ]
  m2_r <- 2 * coef(lm(Y ~ X + I(X^2), data = subset(dat_c, X >= 0)))[3]
  m2_l <- 2 * coef(lm(Y ~ X + I(X^2), data = subset(dat_c, X < 0)))[3]
  curv <- (m2_r - m2_l)^2
  
  if(is.na(curv) | curv < 1e-6) curv <- 0.0001
  
  CK <- 3.4375
  h_opt <- (CK * sigma2 / (f_c * curv * N))^(1/5)
  return(as.numeric(h_opt))
}

# 4. DEFINE REAL CROSS-VALIDATION (CV) ALGORITHM
bw_cv_real <- function(data) {
  h_grid <- seq(5, 25, by = 1) 
  cv_scores <- numeric(length(h_grid))
  
  cat("Running CV Loop (This may take 1-2 mins)...\n")
  
  for(i in seq_along(h_grid)) {
    h <- h_grid[i]
    data_in <- subset(data, abs(X) <= h)
    if(nrow(data_in) < 20) { cv_scores[i] <- Inf; next }
    
    set.seed(123 + i) 
    train_idx <- sample(1:nrow(data_in), size = floor(0.5 * nrow(data_in)))
    train_data <- data_in[train_idx, ]
    valid_data <- data_in[-train_idx, ]
    
    est_train <- rd_estimator(train_data, h)
    
    if(is.na(est_train)) { 
      cv_scores[i] <- Inf 
    } else {
      val_left_mean  <- mean(valid_data$Y[valid_data$X < 0], na.rm=TRUE)
      val_right_mean <- mean(valid_data$Y[valid_data$X >= 0], na.rm=TRUE)
      
      if(is.nan(val_left_mean) || is.nan(val_right_mean)) {
        cv_scores[i] <- Inf
      } else {
        raw_jump <- val_right_mean - val_left_mean
        cv_scores[i] <- abs(est_train - raw_jump)
      }
    }
    cat(".") 
  }
  cat(" Done.\n")
  best_idx <- which.min(cv_scores)
  return(h_grid[best_idx])
}

# 5. EXECUTE ALGORITHMS
cat("\nCalculating IK Bandwidth...\n")
h_ik_val <- bw_ik(df_cleaned)
est_ik   <- rd_estimator(df_cleaned, h_ik_val)

cat("\nCalculating CV Bandwidth (Full Simulation)...\n")
h_cv_val <- bw_cv_real(df_cleaned)
est_cv   <- rd_estimator(df_cleaned, h_cv_val)

# 6. SAVE LATEX TABLE
if(!dir.exists("03_output")) dir.create("03_output")
file_path <- "03_output/real_data_results.tex"

cat("\\begin{table}[h]\n\\centering\n", file = file_path)
cat("\\caption{Algorithm Performance on Real U.S. Senate Data}\n", file = file_path, append=T)
cat("\\begin{tabular}{lcc}\n\\hline\n", file = file_path, append=T)
cat("Algorithm & Selected Bandwidth ($h$) & Estimated Effect ($\\hat{\\tau}$) \\\\\n\\hline\n", file = file_path, append=T)
cat(sprintf("IK Algorithm & %.2f & %.3f \\\\\n", h_ik_val, est_ik), file = file_path, append=T)
cat(sprintf("CV Algorithm & %.2f & %.3f \\\\\n", h_cv_val, est_cv), file = file_path, append=T)
cat("\\hline\n\\end{tabular}\n\\label{tab:real_data}\\end{table}\n", file = file_path, append=T)

cat("\nTable saved to: 03_output/real_data_results.tex\n")

# 7. GENERATE COMPARISON GRAPH
df_binned <- df_cleaned %>%
  mutate(bin = cut(X, breaks = seq(-50, 50, by=2))) %>%
  group_by(bin) %>%
  summarise(X = mean(X), Y = mean(Y)) %>%
  na.omit()

# Lines for IK
ik_data <- df_cleaned[abs(df_cleaned$X) <= h_ik_val, ]
pred_ik_l <- predict(lm(Y ~ X, data = subset(ik_data, X<0)), newdata=data.frame(X=seq(-h_ik_val, 0, 0.1)))
pred_ik_r <- predict(lm(Y ~ X, data = subset(ik_data, X>=0)), newdata=data.frame(X=seq(0, h_ik_val, 0.1)))
df_line_ik <- data.frame(X = c(seq(-h_ik_val, 0, 0.1), seq(0, h_ik_val, 0.1)),
                         Y = c(pred_ik_l, pred_ik_r), Type = "IK Estimate")

# Lines for CV
cv_data <- df_cleaned[abs(df_cleaned$X) <= h_cv_val, ]
pred_cv_l <- predict(lm(Y ~ X, data = subset(cv_data, X<0)), newdata=data.frame(X=seq(-h_cv_val, 0, 0.1)))
pred_cv_r <- predict(lm(Y ~ X, data = subset(cv_data, X>=0)), newdata=data.frame(X=seq(0, h_cv_val, 0.1)))
df_line_cv <- data.frame(X = c(seq(-h_cv_val, 0, 0.1), seq(0, h_cv_val, 0.1)),
                         Y = c(pred_cv_l, pred_cv_r), Type = "CV Estimate")

plot_real <- ggplot() +
  geom_point(data = df_binned, aes(x=X, y=Y), color = "gray70", alpha=0.5) +
  geom_vline(xintercept = 0, linetype="dashed") +
  geom_line(data = df_line_ik, aes(x=X, y=Y, color = "IK (Localized)"), size=1.2) +
  geom_line(data = df_line_cv, aes(x=X, y=Y, color = "CV (Smoother)"), size=1.2) +
  scale_color_manual(values = c("IK (Localized)" = "#D55E00", "CV (Smoother)" = "#0072B2")) +
  labs(title = "Real Data Application: Bandwidth Disagreement",
       subtitle = sprintf("IK chooses h=%.1f (Local). CV chooses h=%.1f (Smooth).", h_ik_val, h_cv_val),
       x = "Margin of Victory (X)", y = "Vote Share (Y)", color = "Algorithm") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("03_output/plot_real_data_comparison.pdf", plot_real, width = 8, height = 5)
cat("Graph saved to: 03_output/plot_real_data_comparison.pdf\n")