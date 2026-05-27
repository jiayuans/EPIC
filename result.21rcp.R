###########################################################################
## Summarize simulation results: 1 random change point model
## Calculate convergence flag, bias, MSE, and 95% coverage probability
###########################################################################
setwd("/Users/Shared/Windows/UCHealth/RA/Project/EPIC-CF/Analysis_Jiayuan/EPIC_Sim_Results/JM21rcp_052725")

###########################################################################
# 1. Read csv files
###########################################################################
files <- list.files(pattern = "^result_21rcp\\.[0-9]+\\.csv$")

file_id <- as.numeric(sub("^result_21rcp\\.([0-9]+)\\.csv$", "\\1", files))
files <- files[order(file_id)]

data_frames <- lapply(files, read.csv)

I <- length(data_frames)
I

###########################################################################
# 2. True values
# Adjust these if your true values are different
###########################################################################
truth <- c(
  B1        = -0.15,
  B2        = 0.15,
  cp1       = 5,
  c0        = -2,
  c1        = 0.1,
  c2        = 0.15,
  c3        = -0.1,
  u.tau.inv = 0.04,
  b0        = -2,
  b1        = 0.2,
  a         = 1.8,
  ga        = 0.2,
  ga1       = -0.05,
  w.tau.inv = 0.04,
  cp1mu     = 5,
  cp1var    = 1,
  u         = 0,
  w         = 0
)

###########################################################################
# 3. Row map in each CSV
# Assumption:
#   col 2 = lower 95% CI
#   col 4 = upper 95% CI
#   col 5 = posterior mean
#   col 12 = Rhat / convergence diagnostic
###########################################################################
row_map <- list(
  B1        = 1,
  B2        = 2,
  cp1       = 3:402,
  c0        = 403,
  c1        = 404,
  c2        = 405,
  c3        = 406,
  u.tau.inv = 407,
  b0        = 408,
  b1        = 409,
  a         = 410,
  ga        = 411,
  ga1       = 412,
  w.tau.inv = 413,
  cp1mu     = 414,
  cp1var    = 415,
  u         = 416:815,
  w         = 1216:1615
)

###########################################################################
# 4. Extract posterior mean and credible interval
###########################################################################
extract_param <- function(df, rows) {
  c(
    mean = mean(df[rows, 5], na.rm = TRUE),
    lb   = mean(df[rows, 2], na.rm = TRUE),
    ub   = mean(df[rows, 4], na.rm = TRUE)
  )
}

###########################################################################
# 5. Build matrices
###########################################################################
param_names <- names(row_map)

mean_mat <- matrix(NA, I, length(param_names), dimnames = list(NULL, param_names))
lb_mat   <- matrix(NA, I, length(param_names), dimnames = list(NULL, param_names))
ub_mat   <- matrix(NA, I, length(param_names), dimnames = list(NULL, param_names))

Flag <- rep(NA, I)

for (i in seq_len(I)) {
  
  df <- data_frames[[i]]
  
  # Same convergence rule as your original 1rcp code
  Flag[i] <- ifelse(max(df[, 12], na.rm = TRUE) < 1.3, 1, 0)
  
  for (p in param_names) {
    tmp <- extract_param(df, row_map[[p]])
    mean_mat[i, p] <- tmp["mean"]
    lb_mat[i, p]   <- tmp["lb"]
    ub_mat[i, p]   <- tmp["ub"]
  }
}

###########################################################################
# 6. Metric function
###########################################################################
calc_metrics <- function(est, lb, ub, true) {
  true <- unname(true)
  
  c(
    Mean  = mean(est, na.rm = TRUE),
    Truth = true,
    Bias  = mean(est - true, na.rm = TRUE),
    MSE   = mean((est - true)^2, na.rm = TRUE),
    CP95  = mean(lb <= true & ub >= true, na.rm = TRUE)
  )
}

###########################################################################
# 7. Summary tables
###########################################################################
params <- intersect(names(truth), colnames(mean_mat))

# All runs
summary_all <- do.call(rbind, lapply(params, function(p) {
  calc_metrics(mean_mat[, p], lb_mat[, p], ub_mat[, p], truth[p])
}))

summary_all <- data.frame(
  Parameter = params,
  summary_all,
  row.names = NULL
)

# Converged runs only
idx <- Flag == 1
sum(idx)

summary_conv <- do.call(rbind, lapply(params, function(p) {
  calc_metrics(mean_mat[idx, p], lb_mat[idx, p], ub_mat[idx, p], truth[p])
}))

summary_conv <- data.frame(
  Parameter = params,
  summary_conv,
  row.names = NULL
)

###########################################################################
# 8. Output
###########################################################################
summary_all[, -1]  <- round(summary_all[, -1], 2)
summary_conv[, -1] <- round(summary_conv[, -1], 2)

cat("\n================ ALL RUNS ================\n")
print(summary_all)

cat("\n=========== FLAG = 1 ONLY ================\n")
print(summary_conv)

cat("\n=========== FLAG TABLE ================\n")
print(table(Flag))