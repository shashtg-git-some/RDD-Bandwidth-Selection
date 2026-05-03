# ==============================================================================
# FILE: 06_latex_output.R
# PURPOSE: Helper functions to export result matrices to professional LaTeX tables.
# ==============================================================================

#' Save Results to LaTeX (Professional Version)
save_to_latex <- function(res_list, filename, caption, label) {
  
  # Extract components
  fixed <- res_list$fixed
  ik    <- res_list$IK
  cv    <- res_list$CV
  
  # Create File Connection
  sink(filename)
  
  # 1. Table Environment (Force Top [t!])
  cat("\\begin{table}[t!]\n")
  cat("\\centering\n")
  cat(paste0("\\caption{", caption, "}\n"))
  cat(paste0("\\label{", label, "}\n"))
  
  # 2. Column Setup
  # Structure: Metric | 5 Fixed Cols | 2 Selector Cols
  cat("\\begin{tabular}{l ccccc cc}\n")
  cat("\\toprule\n")
  
  # 3. Header Grouping
  cat("& \\multicolumn{5}{c}{\\textbf{Fixed Bandwidths}} & \\multicolumn{2}{c}{\\textbf{Selectors}} \\\\\n")
  cat("\\cmidrule(lr){2-6} \\cmidrule(lr){7-8}\n")
  
  # 4. Column Names
  h_labels <- paste0("$h=", gsub("h=", "", colnames(fixed)), "$")
  header <- paste(c("Metric", h_labels, "IK", "CV"), collapse = " & ")
  cat(paste0(header, " \\\\ \n"))
  cat("\\midrule\n")
  
  # 5. Data Rows
  fmt <- function(x) sprintf("%.4f", x)
  
  # Bias
  row_bias <- c(fmt(fixed["bias", ]), fmt(ik["bias"]), fmt(cv["bias"]))
  cat(paste("Bias", paste(row_bias, collapse = " & "), sep = " & "))
  cat(" \\\\ \n")
  
  # Variance
  row_var <- c(fmt(fixed["var", ]), fmt(ik["var"]), fmt(cv["var"]))
  cat(paste("Var", paste(row_var, collapse = " & "), sep = " & "))
  cat(" \\\\ \n")
  
  # MSE
  row_mse <- c(fmt(fixed["mse", ]), fmt(ik["mse"]), fmt(cv["mse"]))
  cat(paste("MSE", paste(row_mse, collapse = " & "), sep = " & "))
  cat(" \\\\ \n")
  
  cat("\\addlinespace\n")
  
  # Avg Bandwidth Row
  fmt_h <- function(x) sprintf("%.3f", x)
  fixed_h_vals <- as.numeric(gsub("h=", "", colnames(fixed)))
  row_h <- c(
    as.character(fixed_h_vals), 
    fmt_h(ik["h_avg"]),
    fmt_h(cv["h_avg"])
  )
  
  cat(paste("Avg $h$", paste(row_h, collapse = " & "), sep = " & "))
  cat(" \\\\ \n")
  
  # 6. Bottom and Close
  cat("\\bottomrule\n")
  cat("\\end{tabular}\n")
  cat("\\end{table}\n")
  
  sink() # Close file
  cat(paste("Saved professional LaTeX table to:", filename, "\n"))
}