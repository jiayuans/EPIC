#!/usr/bin/env Rscript
library(tidyverse)

dirg <- "/Users/Shared/Windows/UCHealth/RA/Project/EPIC-CF/Analysis_Jiayuan/EPIC_Sim/"
setwd(dirg)

# ----------------------------
# Read follow-up times
# ----------------------------
long.time <- read.csv("long.data_new600.csv")
first.tt <- long.time[, 2]
last.tt  <- long.time[, 3]

N  <- length(last.tt)
id <- 1:N

t  <- round(first.tt)
tt <- round(last.tt)

k.pa <- (tt - t) * 4
kk   <- max(k.pa)

# ----------------------------
# Number of simulated datasets
# ----------------------------
Int <- 201
set.seed(123)

# ----------------------------
# TRUE VALUES
# ----------------------------
# PA
c10_true <- -3.5
c20_true <- -2.6
c_true   <- c(0.3, 0.3, -0.05)
cp1_mu_true <- 10
cp1_sd_true <- 1.5
pi_true <- c(0.45, 0.55)

# PE
a1_true  <- 2.5
a2_true  <- 0.5
b10_true <- -3.6
b20_true <- -2.2
b_true   <- c(0.2, 0.3)

ga10_true <- 0.8
ga20_true <- 0.5
ga_true   <- -0.1
ga11_true <- -0.2
ga12_true <- 0.3
ga21_true <- 0.7

pi_r_true <- c(0.6, 0.4)

# Random effects truths (precisions)
u_tau10_true <- 6.25        # sd = 0.4
u_tau20_true <- 6.25        # sd = 0.4
u_tau11_true <- 25          # sd = 0.2
u_tau12_true <- 25          # sd = 0.2
u_tau21_true <- 25          # sd = 0.2

w_tau1_true  <- 11.11111111 # sd = 0.3
w_tau2_true  <- 11.11111111 # sd = 0.3

# ----------------------------
# NHPP generator (0..T)
# Returns interval rows + a terminal censoring row at T with status=0
# ----------------------------
NHPP <- function(a, b, T) {
  mu <- b * T^a
  n  <- rpois(1, mu)
  
  if (n != 0) {
    u <- sort(runif(n, 0, 1))
    y <- T * u^(1/a)
    y[length(y) + 1] <- T
    
    y_0 <- rep(NA_real_, length(y))
    for (ii in 2:length(y_0)) y_0[ii] <- y[ii - 1]
    y_0[is.na(y_0)] <- 0
    
    cbind(y_0, y, c(rep(1, length(y) - 1), 0), n)
  } else {
    cbind(0, T, 0, n)
  }
}

# ----------------------------
# Simulation loop
# ----------------------------
for (r in 2:Int) {
  
  # Treatment vector
  X1 <- c(rep(1, floor(N/2)), rep(0, N - floor(N/2)))
  
  # ----------------------------
  # Latent variables exactly as in JAGS
  # ----------------------------
  u10 <- rnorm(N, 0, sqrt(1 / u_tau10_true))
  u20 <- rnorm(N, 0, sqrt(1 / u_tau20_true))
  u11 <- rnorm(N, 0, sqrt(1 / u_tau11_true))
  u12 <- rnorm(N, 0, sqrt(1 / u_tau12_true))
  u21 <- rnorm(N, 0, sqrt(1 / u_tau21_true))
  
  w1  <- rnorm(N, 0, sqrt(1 / w_tau1_true))
  w2  <- rnorm(N, 0, sqrt(1 / w_tau2_true))
  
  cp1 <- rnorm(N, cp1_mu_true, cp1_sd_true)
  cp1c <- cp1 - cp1_mu_true
  
  # Latent class indicators
  z   <- sample(1:2, size = N, prob = pi_true,  replace = TRUE)
  z_r <- sample(1:2, size = N, prob = pi_r_true, replace = TRUE)
  
  # Frailties
  v1 <- exp(ga10_true * u10 + w1 + ga_true * cp1c + ga11_true * u11 + ga12_true * u12)
  v2 <- exp(ga20_true * u20 + w2 + ga21_true * u21)
  
  # ----------------------------
  # Build PA design matrix X and responses Y
  # ----------------------------
  X <- matrix(NA_real_, nrow = N, ncol = kk)
  for (i in 1:N) {
    X[i, 1:k.pa[i]] <- seq(t[i], tt[i] - 0.25, by = 0.25)
  }
  
  Y <- matrix(NA_integer_, nrow = N, ncol = kk)
  
  for (i in 1:N) {
    for (j in 1:k.pa[i]) {
      
      Ind <- ifelse(X[i, j] < cp1[i], -1, 1)
      
      # Component 1
      logit1 <- c10_true +
        (c_true[1] + u11[i]) * (X[i, j] - cp1[i]) +
        (c_true[2] + u12[i]) * (X[i, j] - cp1[i]) * Ind +
        c_true[3] * X1[i] +
        u10[i]
      
      # Component 2
      logit2 <- c20_true +
        (c_true[1] - c_true[2] + u21[i]) * X[i, j] +
        c_true[3] * X1[i] +
        u20[i]
      
      p1 <- plogis(logit1)
      p2 <- plogis(logit2)
      p  <- if (z[i] == 1) p1 else p2
      
      Y[i, j] <- rbinom(1, 1, p)
    }
  }
  
  # ----------------------------
  # Simulate PE recurrent events
  # ----------------------------
  tau <- tt - 0.25
  sim_pe_list <- vector("list", N)
  
  for (i in 1:N) {
    
    if (z_r[i] == 1) {
      b_scale <- v1[i] * exp(b10_true + b_true[1] * X1[i])
      times <- NHPP(a = a1_true, b = b_scale, T = tau[i])
    } else {
      b_scale <- v2[i] * exp(b20_true + b_true[2] * X1[i])
      times <- NHPP(a = a2_true, b = b_scale, T = tau[i])
    }
    
    dat_i <- data.frame(
      id     = i,
      xi     = X1[i],
      Tei    = tau[i],
      n.rec  = as.integer(times[, 4]),
      start  = as.numeric(times[, 1]),
      stop   = as.numeric(times[, 2]),
      status = as.integer(times[, 3])
    )
    
    sim_pe_list[[i]] <- dat_i
  }
  
  simdat.pe <- bind_rows(sim_pe_list) %>%
    arrange(id, stop)
  
  # ----------------------------
  # Write outputs
  # ----------------------------
  write.csv(as.data.frame(X),
            file = paste0("mixJM.X_newdatars.", r - 2, ".csv"),
            row.names = FALSE)
  
  write.csv(as.data.frame(Y),
            file = paste0("mixJM.Y_newdatars.", r - 2, ".csv"),
            row.names = FALSE)
  
  write.csv(simdat.pe,
            file = paste0("mixJM.rec_newdatars.", r - 2, ".csv"),
            row.names = FALSE)
  
  # ----------------------------
  # Write truth file
  # ----------------------------
  truth <- data.frame(
    c10 = c10_true,
    c20 = c20_true,
    c1  = c_true[1],
    c2  = c_true[2],
    c3  = c_true[3],
    cp1_mu  = cp1_mu_true,
    cp1_tau = 1 / (cp1_sd_true^2),
    
    a1  = a1_true,
    a2  = a2_true,
    b10 = b10_true,
    b20 = b20_true,
    b1  = b_true[1],
    b2  = b_true[2],
    
    ga10 = ga10_true,
    ga20 = ga20_true,
    ga   = ga_true,
    ga11 = ga11_true,
    ga12 = ga12_true,
    ga21 = ga21_true,
    
    pi1  = pi_true[1],
    pi2  = pi_true[2],
    pi1r = pi_r_true[1],
    pi2r = pi_r_true[2],
    
    u_tau10 = u_tau10_true,
    u_tau20 = u_tau20_true,
    u_tau11 = u_tau11_true,
    u_tau12 = u_tau12_true,
    u_tau21 = u_tau21_true,
    
    w_tau1 = w_tau1_true,
    w_tau2 = w_tau2_true
  )
  
  write.csv(truth,
            file = paste0("mixJM.truthrs.", r - 2, ".csv"),
            row.names = FALSE)
}