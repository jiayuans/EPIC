###########################################################################
## Summarize simulation results: convergence flag, bias, MSE, 95% CP
## Cleaner version
###########################################################################
setwd("/Users/Shared/Windows/UCHealth/RA/Project/EPIC-CF/Analysis_Jiayuan/EPIC_Sim_Results/mixnewJM6_032225")

## ----------------------------
## 1. Read files
## ----------------------------
files <- list.files(pattern = "^mixJM\\.newresult6\\.[0-9]+\\.csv$")

file_id <- as.numeric(sub("^mixJM\\.newresult6\\.([0-9]+)\\.csv$", "\\1", files))
files <- files[order(file_id)]

data_frames <- lapply(files, read.csv)
I <- length(data_frames)
I

## ----------------------------
## 2. True values
## Adjust these to your simulation truth
## ----------------------------
truth <- c(
  B1 = 0.0,
  B2 = 0.6,
  cp1 = 14,          # if you want cp1.mu instead, change below
  c10 = -3.3,
  c20 = -2.6,
  c1 = 0.3,
  c2 = 0.3,
  c3 = -0.05,
  pi1 = 0.55,
  pi2 = 0.45,
  pi1r = 0.6,
  pi2r = 0.4,
  u1.tau.inv = 1/4,       # if row stores variance; adjust if needed
  u2.tau.inv = 1/4,
  cp1.mu = 14,
  cp1.tau = 1/(1.5^2),
  b10 = -4,
  b20 = -2,
  b1 = 0.2,
  b2 = 0.3,
  a1 = 1.8,
  a2 = 1.6,
  ga10 = 0.7,
  ga20 = -0.2,
  ga11 = -0.2,
  w1.tau.inv = 1/11.11111111,
  w2.tau.inv = 1/11.11111111
)

## ----------------------------
## 3. Row map in each CSV
## Column assumption:
##   col 3 = Lower95
##   col 4 = Upper95
##   col 5 = Mean
## Adjust row numbers if needed
## ----------------------------
row_map <- list(
  B1 = 1,
  B2 = 2,
  c10 = 3,
  c20 = 4,
  c1 = 5,
  c2 = 6,
  c3 = 7,
  cp1 = 8:607,      # subject-specific cp1 rows
  pi1 = 608,
  pi2 = 609,
  pi1r = 610,
  pi2r = 611,
  u1.tau.inv = 612,
  u2.tau.inv = 613,
  cp1.mu = 616,
  cp1.tau = 617,
  b10 = 619,
  b20 = 620,
  b1 = 621,
  b2 = 622,
  a1 = 623,
  a2 = 624,
  ga10 = 625,
  ga20 = 626,
  ga11 = 627,
  w1.tau.inv = 630,
  w2.tau.inv = 631
)

## ----------------------------
## 4. Extract function
## ----------------------------
extract_param <- function(df, rows) {
  c(
    mean = mean(df[rows, 5], na.rm = TRUE),
    lb   = mean(df[rows, 2], na.rm = TRUE),
    ub   = mean(df[rows, 4], na.rm = TRUE)
  )
}

## ----------------------------
## 5. Build matrices
## ----------------------------
param_names <- names(row_map)

mean_mat <- matrix(NA, I, length(param_names), dimnames = list(NULL, param_names))
lb_mat   <- matrix(NA, I, length(param_names), dimnames = list(NULL, param_names))
ub_mat   <- matrix(NA, I, length(param_names), dimnames = list(NULL, param_names))

Flag <- rep(NA, I)

for (i in seq_len(I)) {
  df <- data_frames[[i]]
  
  Flag[i] <- ifelse(max(df[,12], na.rm = TRUE) < 1.3, 1, 0)
  
  for (p in param_names) {
    tmp <- extract_param(df, row_map[[p]])
    mean_mat[i,p] <- tmp["mean"]
    lb_mat[i,p]   <- tmp["lb"]
    ub_mat[i,p]   <- tmp["ub"]
  }
}

## ----------------------------
## 6. Metric function
## ----------------------------
calc_metrics <- function(est, lb, ub, true) {
  c(Truth = true,
    Mean = mean(est, na.rm = TRUE),
    Bias = mean(est - true, na.rm = TRUE),
    MSE  = mean((est - true)^2, na.rm = TRUE),
    CP95 = mean(lb <= true & ub >= true, na.rm = TRUE)
  )
}

## ----------------------------
## 7. Summary tables
## ----------------------------
params <- intersect(names(truth), colnames(mean_mat))

## --- all runs
summary_all <- do.call(rbind, lapply(params, function(p) {
  calc_metrics(mean_mat[,p], lb_mat[,p], ub_mat[,p], truth[p])
}))

summary_all <- data.frame(Parameter = params, summary_all)

## --- converged only
idx <- Flag == 1
sum(idx)

summary_conv <- do.call(rbind, lapply(params, function(p) {
  calc_metrics(mean_mat[idx,p], lb_mat[idx,p], ub_mat[idx,p], truth[p])
}))

summary_conv <- data.frame(Parameter = params, summary_conv)

## ----------------------------
## 8. Output
## ----------------------------
summary_all[, -1]  <- round(summary_all[, -1], 3)
summary_conv[, -1] <- round(summary_conv[, -1], 3)

cat("\n================ ALL RUNS ================\n")
print(summary_all)

cat("\n=========== FLAG = 1 ONLY ================\n")
print(summary_conv)
