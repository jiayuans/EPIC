#!/usr/bin/env Rscript
library(tidyverse)

dirg <- "/Users/Shared/Windows/UCHealth/RA/Project/EPIC-CF/Analysis_Jiayuan/EPIC_Sim/"
setwd(dirg)

# ----------------------------
# Read follow-up times
# ----------------------------
long.time <- read.csv("long.data_new600.csv")
first.tt <- long.time[,2]
last.tt  <- long.time[,3]

N  <- length(last.tt)
id <- 1:N

t  <- round(first.tt)
tt <- round(last.tt)

k.pa <- (tt-t)*4
kk   <- max(k.pa)

# ----------------------------
# Number of simulated datasets
# ----------------------------
Int <- 501
set.seed(123)

# ----------------------------
# TRUE VALUES
# ----------------------------

# PA
c0 <- -4
c  <- c(0.4, 0.2, 0.3, -0.05)   # c[1], c[2], c[3], c[4]

cp1_mu_true <- 9
cp1_sd_true <- 1

# PE
a_true  <- 2
b0_true <- -3
b_true  <- 0.1

# Association parameters
ga_true    <- 0.3
gacp1_true <- -0.2
gacp2_true <- 0.1

# Random-effect precisions
u_tau_true <- 4       # sd(u)=0.5
w_tau_true <- 25      # sd(w)=0.2

# ----------------------------
# NHPP generator (0..T)
# ----------------------------
NHPP <- function(a, b, T) {
  mu <- b*T^a
  n  <- rpois(1, mu)
  
  if(n != 0){
    u.temp <- sort(runif(n,0,1))
    y <- T*u.temp^(1/a)
    y[length(y)+1] <- T
    
    y_0 <- rep(NA_real_, length(y))
    for(ii in 2:length(y_0)) y_0[ii] <- y[ii-1]
    y_0[is.na(y_0)] <- 0
    
    cbind(y_0, y, c(rep(1,length(y)-1),0), n)
  } else {
    cbind(0,T,0,n)
  }
}

# ----------------------------
# Simulation loop
# ----------------------------
for(r in 2:Int){
  
  X1 <- c(rep(1,floor(N/2)), rep(0,N-floor(N/2)))
  
  # Random effects
  u <- rnorm(N,0,sqrt(1/u_tau_true))
  w <- rnorm(N,0,sqrt(1/w_tau_true))
  
  # First change point
  cp1 <- rnorm(N,cp1_mu_true,cp1_sd_true)
  
  while(any(cp1 > 21)){
    ind <- which(cp1 > 21)
    cp1[ind] <- rnorm(length(ind),cp1_mu_true,cp1_sd_true)
  }
  
  # Second change point
  # cp2 | cp1 ~ Uniform(cp1,21)
  cp2 <- runif(N,min=cp1,max=21)
  
  # Centered change points
  cp1c <- cp1-cp1_mu_true
  cp2_mu <- 0.5*(cp1+21)
  cp2c <- cp2-cp2_mu
  
  # Shared longitudinal contribution
  eta_shared <- ga_true*u +
    gacp1_true*cp1c +
    gacp2_true*cp2c
  
  # PE frailty
  v <- exp(w+eta_shared)
  
  # ----------------------------
  # PA data
  # ----------------------------
  X <- matrix(NA_real_,nrow=N,ncol=kk)
  
  for(i in 1:N){
    X[i,1:k.pa[i]] <- seq(t[i],tt[i]-0.25,by=0.25)
  }
  
  Y <- matrix(NA_integer_,nrow=N,ncol=kk)
  
  for(i in 1:N){
    for(j in 1:k.pa[i]){
      
      Ind1 <- ifelse(X[i,j] < cp1[i],-1,1)
      Ind2 <- ifelse(X[i,j] < cp2[i],-1,1)
      
      logit_p <- c0 +
        c[1]*(X[i,j]-cp1[i]) +
        c[2]*(X[i,j]-cp1[i])*Ind1 +
        c[3]*(X[i,j]-cp2[i])*Ind2 +
        c[4]*X1[i] +
        u[i]
      
      p <- plogis(logit_p)
      
      Y[i,j] <- rbinom(1,1,p)
    }
  }
  
  # ----------------------------
  # PE recurrent events
  # ----------------------------
  tau <- tt-0.25
  sim_pe_list <- vector("list",N)
  
  for(i in 1:N){
    
    b_scale <- v[i]*exp(b0_true+b_true*X1[i])
    
    times <- NHPP(
      a=a_true,
      b=b_scale,
      T=tau[i]
    )
    
    dat_i <- data.frame(
      id=i,
      xi=X1[i],
      Tei=tau[i],
      n.rec=as.integer(times[,4]),
      start=as.numeric(times[,1]),
      stop=as.numeric(times[,2]),
      status=as.integer(times[,3])
    )
    
    sim_pe_list[[i]] <- dat_i
  }
  
  simdat.pe <- bind_rows(sim_pe_list) %>%
    arrange(id,stop)
  
  # ----------------------------
  # Write simulated datasets
  # ----------------------------
  write.csv(
    as.data.frame(X),
    file=paste0("nonmixJM.X_newdata8.",r-2,".csv"),
    row.names=FALSE
  )
  
  write.csv(
    as.data.frame(Y),
    file=paste0("nonmixJM.Y_newdata8.",r-2,".csv"),
    row.names=FALSE
  )
  
  write.csv(
    simdat.pe,
    file=paste0("nonmixJM.rec_newdata8.",r-2,".csv"),
    row.names=FALSE
  )
  
  # ----------------------------
  # Truth file
  # ----------------------------
  truth <- data.frame(
    c0=c0,
    c1=c[1],
    c2=c[2],
    c3=c[3],
    c4=c[4],
    
    B1=c[1]-c[2]-c[3],
    B2=c[1]+c[2]-c[3],
    B3=c[1]+c[2]+c[3],
    
    cp1_mu=cp1_mu_true,
    cp1_tau=1/(cp1_sd_true^2),
    cp2_mean=(cp1_mu_true+21)/2,
    
    u_tau=u_tau_true,
    
    a=a_true,
    b0=b0_true,
    b=b_true,
    
    ga=ga_true,
    gacp1=gacp1_true,
    gacp2=gacp2_true,
    
    w_tau=w_tau_true
  )
  
  write.csv(
    truth,
    file=paste0("nonmixJM.truth8.",r-2,".csv"),
    row.names=FALSE
  )
}