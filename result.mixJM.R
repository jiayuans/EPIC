
###########################################################################
## JM9 Simulation: First 200 Datasets
## Metrics: Mean, Truth, Bias, MSE, CP95
###########################################################################
setwd("/Users/Shared/Windows/UCHealth/RA/Project/EPIC-CF/Analysis_Jiayuan/EPIC_Sim_Results/mixnewJM9_101726")

## 1. Read first 200 available simulation datasets
files <- list.files(pattern="^mixJM\\.newresult9\\.[0-9]+\\.csv$")
file_id <- as.numeric(sub("^mixJM\\.newresult9\\.([0-9]+)\\.csv$", "\\1", files))
ord <- order(file_id)
files <- head(files[ord], 200)
file_id <- head(file_id[ord], 200)

data_frames <- lapply(files, read.csv, check.names=FALSE)
I <- length(data_frames)
if (I < 200) warning("Only ", I, " simulation files are available.")

cat("Number of simulations:", I, "\n")
cat("Simulation ID range:", min(file_id), "-", max(file_id), "\n")

## 2. True values
truth <- c(
  B1=-0.3, B2=0.3, B3=0.9,
  c10=-3.3, c20=-2.6, c1=0.3, c2=0.3, c3=0.3, c4=-0.05,
  pi1=0.4, pi2=0.6, pi1r=0.4, pi2r=0.6,
  u1.tau.inv=0.25, u2.tau.inv=0.25,
  cp1.mu=9, cp1.tau=1,
  b10=-3.5, b20=-1, b1=0.2, b2=0.3,
  a1=2.5, a2=0.5, ga10=0.5, ga20=-0.1,
  ga11=-0.2, ga12=0.1,
  w1.tau.inv=0.04, w2.tau.inv=0.04
)

## 3. Row indices
row_map <- c(
  B1=1, B2=2, B3=3, c10=4, c20=5,
  c1=6, c2=7, c3=8, c4=9,
  pi1=610, pi2=611, pi1r=612, pi2r=613,
  u1.tau.inv=614, u2.tau.inv=615,
  cp1.mu=616, cp1.tau=617,
  b10=618, b20=619, b1=620, b2=621,
  a1=622, a2=623, ga10=624, ga20=625,
  ga11=626, ga12=627,
  w1.tau.inv=628, w2.tau.inv=629
)

## 4. Identify posterior summary columns
cn <- colnames(data_frames[[1]])
find_col <- function(pattern) {
  ind <- grep(pattern, cn, ignore.case=TRUE, perl=TRUE)
  if (length(ind)!=1) stop(paste("Cannot identify column:",pattern))
  ind
}
mean_col <- 5
lb_col <- find_col("2\\.5%|lower|l95|q2\\.5")
ub_col <- find_col("97\\.5%|upper|u95|q97\\.5")
rhat_col <- 12
cat("Mean:", cn[mean_col], " Lower:", cn[lb_col],
    " Upper:", cn[ub_col], " Rhat:", cn[rhat_col], "\n")

## 5. Extract estimates and convergence flags
params <- names(row_map)
mean_mat <- matrix(NA_real_,I,length(params),dimnames=list(NULL,params))
lb_mat <- ub_mat <- mean_mat
Flag <- rep(0L,I)

for (i in seq_len(I)) {
  df <- data_frames[[i]]
  rhat <- suppressWarnings(as.numeric(df[[rhat_col]]))
  Flag[i] <- as.integer(any(is.finite(rhat)) &&
                          max(rhat,na.rm=TRUE)<1.3)
  for (p in params) {
    r <- row_map[p]
    mean_mat[i,p] <- as.numeric(df[r,mean_col])
    lb_mat[i,p] <- as.numeric(df[r,lb_col])
    ub_mat[i,p] <- as.numeric(df[r,ub_col])
  }
}

## 6. Calculate metrics
calc_metrics <- function(est,lb,ub,true) {
  valid_est <- is.finite(est)
  valid_ci <- is.finite(lb) & is.finite(ub) & lb<=ub
  c(
    Mean=if(any(valid_est)) mean(est[valid_est]) else NA_real_,
    Truth=unname(true),
    Bias=if(any(valid_est)) mean(est[valid_est]-true) else NA_real_,
    MSE=if(any(valid_est)) mean((est[valid_est]-true)^2) else NA_real_,
    CP95=if(any(valid_ci)) mean(lb[valid_ci]<=true &
                                  ub[valid_ci]>=true) else NA_real_
  )
}

## 7. Summarize all and converged simulations
make_summary <- function(idx) {
  result <- do.call(rbind,lapply(params,function(p)
    calc_metrics(mean_mat[idx,p],lb_mat[idx,p],ub_mat[idx,p],truth[p])))
  data.frame(Parameter=params,round(result,3),row.names=NULL,check.names=FALSE)
}
summary_all <- make_summary(rep(TRUE,I))
summary_conv <- make_summary(Flag==1)

## 8. Print results
cat("\n========== ALL 200 SIMULATIONS ==========\n")
print(summary_all,row.names=FALSE)
cat("\n========== CONVERGED SIMULATIONS ==========\n")
print(summary_conv,row.names=FALSE)
cat("\n========== CONVERGENCE ==========\n")
print(table(factor(Flag,levels=0:1)))
cat("Convergence rate:",round(mean(Flag==1)*100,2),"%\n")

## 9. Export
write.csv(summary_all,"JM9_metrics_first200_all.csv",row.names=FALSE)
write.csv(summary_conv,"JM9_metrics_first200_converged.csv",row.names=FALSE)
write.csv(data.frame(Simulation=file_id,Flag=Flag),
          "JM9_convergence_first200.csv",row.names=FALSE)






###########################################################################
## Non-Mixture JM12: First 200 Simulations
## Metrics: Mean, Truth, Bias, MSE, CP95
###########################################################################
setwd("/Users/Shared/Windows/UCHealth/RA/Project/EPIC-CF/Analysis_Jiayuan/EPIC_Sim_Results/nonmixnewJM12_101726")

## 1. Read first 200 available simulations
files <- list.files(pattern="^nonmixJM\\.newresult12\\.[0-9]+\\.csv$")
file_id <- as.numeric(sub("^nonmixJM\\.newresult12\\.([0-9]+)\\.csv$", "\\1", files))
ord <- order(file_id)
files <- head(files[ord], 200)
file_id <- head(file_id[ord], 200)
if (length(files) < 200) warning("Only ", length(files), " files available.")
if (length(files) == 0) stop("No simulation files found.")
data_frames <- lapply(files, read.csv, check.names=FALSE)
I <- length(data_frames)
cat("Number of simulations:", I, "\n")
cat("Simulation ID range:", min(file_id), "-", max(file_id), "\n")

## 2. True values
truth <- c(
  B1=-0.3, B2=0.3, B3=0.9,
  c0=-3.3, c1=0.3, c2=0.3, c3=0.3, c4=-0.05,
  u.tau.inv=0.25, cp1.mu=9, cp1.tau=1,
  b0=-3.5, b=0.2, a=2.5,
  ga10=0.5, ga11=-0.2, ga12=0.1,
  w.tau.inv=0.04
)

## 3. Row indices from original JM12 extraction
row_map <- c(
  B1=1, B2=2, B3=3, c0=4,
  c1=5, c2=6, c3=7, c4=8,
  u.tau.inv=1809, cp1.mu=1810, cp1.tau=1811,
  b0=1812, b=1813, a=1814,
  ga10=1815, ga11=1816, ga12=1817,
  w.tau.inv=1818
)

## 4. Identify posterior summary columns
cn <- colnames(data_frames[[1]])
find_col <- function(pattern) {
  ind <- grep(pattern, cn, ignore.case=TRUE, perl=TRUE)
  if (length(ind)!=1) stop(paste("Cannot identify column:",pattern))
  ind
}
mean_col <- 5
lb_col <- find_col("2\\.5%|lower|l95|q2\\.5")
ub_col <- find_col("97\\.5%|upper|u95|q97\\.5")
rhat_col <- 12
cat("Mean:", cn[mean_col], " Lower:", cn[lb_col],
    " Upper:", cn[ub_col], " Rhat:", cn[rhat_col], "\n")

## 5. Extract posterior estimates and convergence
params <- names(row_map)
mean_mat <- matrix(NA_real_,I,length(params),dimnames=list(NULL,params))
lb_mat <- ub_mat <- mean_mat
Flag <- rep(0L,I)

for (i in seq_len(I)) {
  df <- data_frames[[i]]
  rhat <- suppressWarnings(as.numeric(df[[rhat_col]]))
  Flag[i] <- as.integer(any(is.finite(rhat)) &&
                          max(rhat,na.rm=TRUE)<1.3)
  for (p in params) {
    r <- row_map[p]
    mean_mat[i,p] <- as.numeric(df[r,mean_col])
    lb_mat[i,p] <- as.numeric(df[r,lb_col])
    ub_mat[i,p] <- as.numeric(df[r,ub_col])
  }
}

## 6. Calculate simulation metrics
calc_metrics <- function(est,lb,ub,true) {
  valid_est <- is.finite(est)
  valid_ci <- is.finite(lb) & is.finite(ub) & lb<=ub
  c(
    Mean=if(any(valid_est)) mean(est[valid_est]) else NA_real_,
    Truth=unname(true),
    Bias=if(any(valid_est)) mean(est[valid_est]-true) else NA_real_,
    MSE=if(any(valid_est)) mean((est[valid_est]-true)^2) else NA_real_,
    CP95=if(any(valid_ci)) mean(lb[valid_ci]<=true &
                                  ub[valid_ci]>=true) else NA_real_
  )
}

## 7. Summarize all and converged simulations
make_summary <- function(idx) {
  result <- do.call(rbind,lapply(params,function(p)
    calc_metrics(mean_mat[idx,p],lb_mat[idx,p],ub_mat[idx,p],truth[p])))
  data.frame(Parameter=params,round(result,3),row.names=NULL,check.names=FALSE)
}
summary_all <- make_summary(rep(TRUE,I))
summary_conv <- make_summary(Flag==1)

## 8. Print results
cat("\n========== ALL SIMULATIONS ==========\n")
print(summary_all,row.names=FALSE)
cat("\n========== CONVERGED SIMULATIONS ==========\n")
print(summary_conv,row.names=FALSE)
cat("\n========== CONVERGENCE ==========\n")
print(table(factor(Flag,levels=0:1)))
cat("Convergence rate:",round(mean(Flag==1)*100,2),"%\n")

## 9. Export results
write.csv(summary_all,"nonmixJM12_metrics_first200_all.csv",row.names=FALSE)
write.csv(summary_conv,"nonmixJM12_metrics_first200_converged.csv",row.names=FALSE)
write.csv(data.frame(Simulation=file_id,Flag=Flag),
          "nonmixJM12_convergence_first200.csv",row.names=FALSE)

