###########################################################################
## Summarize simulation results: convergence flag, bias, MSE, 95% CP
## Cleaner version
###########################################################################
setwd("/Users/Shared/Windows/UCHealth/RA/Project/EPIC-CF/Analysis_Jiayuan/EPIC_Sim_Results/JM2rcpc_033025")

###########################################################################
# 1. Read csv files
###########################################################################
files <- list.files(pattern = "^result_2rcpc\\.[0-9]+\\.csv$")

file_id <- as.numeric(sub("^result_2rcpc\\.([0-9]+)\\.csv$", "\\1", files))
files <- files[order(file_id)]

data_frames <- lapply(files, read.csv)

I <- length(data_frames)
I

###########################################################################
# 2. True values
# Adjust these if your output stores variance vs precision differently
###########################################################################
truth <- c(
  cp1      = 5,                    # average subject-specific cp1
  cp2      = 5 + (14/23) * (21.45 - 5),   # E[cp2]
  c0       = -2,
  c1       = 0.1,
  c2       = 0.15,
  c3       = 0.1,
  c4       = -0.1,
  u.tau.inv = 0.2^2,               
  b0       = -2,
  b1       = 0.2,
  a        = 1.8,
  ga       = 0.2,
  ga1      = -0.05,
  ga2      = -0.02,
  w.tau.inv = 1,                   # if this is residual variance; change if precision
  cp1mu    = 5,
  cp1var   = 1,                    # cp1.sd = 1, so variance = 1
  u        = 0,
  v        = 0,
  w        = 0
)

###########################################################################
# 3. Row map in each CSV
# Assumption:
#   col 2 = lower CI
#   col 4 = upper CI
#   col 5 = posterior mean
# Change if your file structure is different
###########################################################################
row_map <- list(
  B1        = 1,
  B2        = 2,
  B3        = 3,
  cp1       = 4:403,
  cp2       = 404:803,
  c0        = 804,
  c1        = 805,
  c2        = 806,
  c3        = 807,
  c4        = 808,
  u.tau.inv = 809,
  b0        = 810,
  b1        = 811,
  a         = 812,
  ga        = 813,
  ga1       = 814,
  ga2       = 815,
  w.tau.inv = 816,
  cp1mu     = 817,
  cp1var    = 818,
  u         = 1219:1618,
  v         = 1619:2018,
  w         = 2019:2418
)

###########################################################################
# 4. Extract function
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
  
  # same convergence rule as your Program #2
  Flag[i] <- ifelse(max(df[, 12], na.rm = TRUE) < 1.5, 1, 0)
  
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
# Only summarize parameters with known truths
###########################################################################
params <- intersect(names(truth), colnames(mean_mat))

# --- all runs
summary_all <- do.call(rbind, lapply(params, function(p) {
  calc_metrics(mean_mat[, p], lb_mat[, p], ub_mat[, p], truth[p])
}))
summary_all <- data.frame(Parameter = params, summary_all)

# --- converged only
idx <- Flag == 1
sum(idx)

summary_conv <- do.call(rbind, lapply(params, function(p) {
  calc_metrics(mean_mat[idx, p], lb_mat[idx, p], ub_mat[idx, p], truth[p])
}))
summary_conv <- data.frame(Parameter = params, summary_conv)

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