# ==============================================================================
# FILE: 07_run_simulation.R
# PURPOSE: Master execution script (3 Cases Only).
# ==============================================================================

# 1. Load all modules
# NOTE: Paths assume working directory is the Project Root
source("02_scripts/00_config.R")
source("01_functions/01_dgp.R")
source("01_functions/02_estimator.R")
source("01_functions/03_bandwidth_ik.R")
source("01_functions/04_bandwidth_cv.R")
source("02_scripts/05_simulation.R")
source("02_scripts/06_latex_output.R")

# 2. Create output directory
if (!dir.exists("03_output")) dir.create("03_output")
results_all <- list()

cat("Starting Simulations (3 Cases Strategy)... \n")
cat("Note: Script will SKIP cases that are already saved in '03_output/'.\n")

# 3. Run The 3 Cases
for (case_num in 1:3) {
  
  # --- SMART RESUME CHECK ---
  target_file <- paste0("03_output/case", case_num, ".tex")
  
  if (file.exists(target_file)) {
    cat(paste0("\n--- Found ", target_file, ". SKIPPING Case ", case_num, ". ---\n"))
    next # Skip to the next case
  }
  
  # --- RUN SIMULATION ---
  cat(paste0("\n--- Processing Case ", case_num, " ---\n"))
  res <- simulate_case(case = case_num, N = N_small)
  
  # Store results
  results_all[[paste0("case", case_num)]] <- res
  
  # Save to LaTeX
  save_to_latex(
    res, 
    filename = target_file, 
    caption = paste("Case", case_num, "Results"),
    label = paste0("tab:case", case_num)
  )
}

cat("\n========================================\n")
cat(" ALL SIMULATIONS COMPLETED SUCCESSFULLY \n")
cat("========================================\n")