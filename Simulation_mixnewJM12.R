#!/usr/bin/env Rscript
library(coda)
library(rjags)
library(runjags)
library(tidyverse)
library(loo)

long.time <- read.csv("long.data_new600.csv")
first.tt <- long.time[,2]
last.tt <- long.time[,3]

####time of first visit and last visit#######
N<-length(last.tt)
#participant ID
id<-rep(1:N)
length(id)

t<-round(first.tt)
tt<-round(last.tt)

X1 <- c(rep(1, floor(N/2)), rep(0, N - floor(N/2)))
k.pa<-(tt-t)*4


set.seed(123)

#############################################################
X <- as.matrix(read.csv(list.files(pattern="nonmixJM.X_newdata8.")))
Y <- as.matrix(read.csv(list.files(pattern="nonmixJM.Y_newdata8.")))
simdat.pe00 <- as.data.frame(read.csv(list.files(pattern="nonmixJM.rec_newdata8.")))
#############################################################

tt<-tt-0.25
timeS <- as.data.frame(cbind(id,t)) ## left truncation time

simdat.pe0 <- merge(simdat.pe00, timeS,all=TRUE)
simdat.pe <- subset(simdat.pe0, stop >= t)
simdat.pe <- simdat.pe %>% arrange(id, stop)

N <- length(tt)

# Event times only (status==1)
ev_list <- vector("list", N)

for (i in 1:N) {
  ev_list[[i]] <- simdat.pe$stop[
    simdat.pe$id == i & simdat.pe$status == 1
  ]
}

k.pe <- lengths(ev_list)
max.count <- max(k.pe)

Ti <- t(vapply(ev_list, function(v) {
  if (length(v) == 0) {
    rep(NA_real_, max.count)
  } else {
    c(v, rep(NA_real_, max.count - length(v)))
  }
}, numeric(max.count)))

E <- matrix(0L, nrow=N, ncol=max.count)

for (i in 1:N) {
  if (k.pe[i] > 0)
    E[i, 1:k.pe[i]] <- 1L
}

Ti2 <- Ti
Ti2[is.na(Ti2)] <- 1
#################Readingin data for X, t0, tau vectors#############################
time.t0 <- t
time.tau <- tt

############ Non-mixture joint model in JAGS #####################
modelrancp <- "
data { 
  for(i in 1:N){
    zeros[i] <- 0
  }
}
model { 
  for(i in 1:N){ 

    # ---- PA model: two random change points ----
    for(j in 1:k.pa[i]){
      Y[i,j] ~ dbin(p[i,j], 1)

      logit(p[i,j]) <- c0
        + c[1] * (X[i,j] - cp1[i])
        + c[2] * (X[i,j] - cp1[i]) * (2*step(X[i,j] - cp1[i]) - 1)
        + c[3] * (X[i,j] - cp2[i]) * (2*step(X[i,j] - cp2[i]) - 1)
        + c[4] * X1[i]
        + u[i]
    }

    # PA random effect and change points
    u[i] ~ dnorm(0, u.tau)
    cp1[i] ~ dnorm(cp1.mu, cp1.tau)T(, 21)
    cp2[i] ~ dunif(cp1[i], 21)

    # Center change points
    cp1c[i] <- cp1[i] - cp1.mu
    cp2.mu[i] <- 0.5 * (cp1[i] + 21)
    cp2c[i] <- cp2[i] - cp2.mu[i]

    # PA likelihood contribution
    L.a[i] <- prod(
      (p[i,1:k.pa[i]]^Y[i,1:k.pa[i]]) *
      ((1-p[i,1:k.pa[i]])^(1-Y[i,1:k.pa[i]]))
    )
    ll.a[i] <- log(L.a[i])

    # ---- PE model: single Weibull NHPP ----
    w[i] ~ dnorm(0, w.tau)

    eta.shared[i] <- ga10*u[i] + ga11*cp1c[i] + ga12*cp2c[i]
    v[i] <- exp(w[i] + eta.shared[i])

    for(j in 1:max.count){
      lambda0[i,j] <- a * (Ti2[i,j])^(a-1)
      lambda[i,j] <- lambda0[i,j] * v[i] * exp(b0 + b*X1[i])
      loghaz[i,j] <- E[i,j] * log(lambda[i,j])
    }

    logL[i] <- sum(loghaz[i,1:max.count]) -
      v[i] * exp(b0 + b*X1[i]) *
      (time.tau[i]^a - time.t0[i]^a)

    ll.e[i] <- logL[i]

    # zeros trick
    phi[i] <- max(-ll.e[i] + 10000, 0)
    zeros[i] ~ dpois(phi[i])
  }

  # Global likelihood summaries
  log_lik0.a <- sum(ll.a[])
  log_lik0.e <- sum(ll.e[])
  dev.a <- -2*log_lik0.a
  dev.e <- -2*log_lik0.e

  # PA fixed effects
  c0 ~ dnorm(0, 0.01)

  for(k in 1:4){
    c[k] ~ dnorm(0, 0.01)
  }

  B1 <- c[1] - c[2] - c[3]
  B2 <- c[1] + c[2] - c[3]
  B3 <- c[1] + c[2] + c[3]

  # PA random-effect variance
  u.tau ~ dgamma(16, 4)
  u.tau.inv <- 1/u.tau

  # Change-point distribution
  cp1.mu ~ dnorm(0, 0.01)
  cp1.tau ~ dgamma(1, 1)
  cp1.tau.inv <- 1/cp1.tau

  # PE model
  a ~ dgamma(0.1, 0.1)
  b0 ~ dnorm(0, 0.01)
  b ~ dnorm(0, 0.01)

  # Association parameters
  ga10 ~ dnorm(0, 1)
  ga11 ~ dnorm(0, 1)
  ga12 ~ dnorm(0, 1)

  # PE frailty variance
  w.tau ~ dgamma(4, 0.16)
  w.tau.inv <- 1/w.tau
}
"

####Observed DATA
data <- dump.format(list(N=N, X=X, Y=Y, X1=X1,k.pa=k.pa,max.count=max.count, time.t0=time.t0, time.tau=time.tau, Ti2=Ti2, E=E)) 
###initial Values
### Initial values
inits1 <- dump.format(list(
  c0=-4,
  c=c(0.4,0.2,0.3,-0.05),
  u.tau=4,
  cp1.mu=9,
  cp1.tau=1,
  b0=-3,
  b=0.1,
  a=2,
  w.tau=25,
  ga10=0.3,
  ga11=-0.2,
  ga12=0.1,
  .RNG.name="base::Super-Duper",
  .RNG.seed=1
))

inits2 <- dump.format(list(
  c0=-4.1,
  c=c(0.4,0.2,0.3,-0.05)+0.01,
  u.tau=3.6,
  cp1.mu=9.1,
  cp1.tau=0.9,
  b0=-3.1,
  b=0.2,
  a=2.1,
  w.tau=24,
  ga10=0.31,
  ga11=-0.21,
  ga12=0.11,
  .RNG.name="base::Super-Duper",
  .RNG.seed=2
))

#### Run the model and produce plots
res <- run.jags(model=modelrancp, adapt = 10000, burnin=10000, sample=6000,  
                monitor=c("B1","B2","B3",
                              "c0","c",
                              "cp1","cp2","cp2.mu",
                              "u.tau.inv",
                              "cp1.mu","cp1.tau.inv",
                              "b0","b","a",
                              "ga10","ga11","ga12",
                              "w.tau.inv",
                              "ll.a","ll.e"), 
                data=data, n.chains=2, method = "parallel", inits=c(inits1,inits2), thin=10)

summary <- summary(res)
summary
result_df <- as.data.frame(summary)
text <- list.files(pattern="nonmixJM.X_newdata8.")
num <- unlist(lapply(strsplit(text,'.',fixed=TRUE),function(x) x[[3]]))
write.csv(result_df, paste0("nonmixJM.newresult12.",num,".csv"))

res_jm <- res$mcmc

## =========================================================
## Helper functions
## =========================================================

colVars <- function(a){
  diff <- a - matrix(colMeans(a), nrow(a), ncol(a), byrow = TRUE)
  colSums(diff^2) / (nrow(a) - 1)
}

log_mean_exp <- function(x){
  m <- max(x)
  m + log(mean(exp(x - m)))
}

waic_from_loglik <- function(log_lik){
  
  lppd_i <- apply(log_lik, 2, log_mean_exp)
  lppd <- sum(lppd_i)
  
  p_waic_1 <- 2 * sum(lppd_i - colMeans(log_lik))
  p_waic_2 <- sum(colVars(log_lik))
  
  WAIC <- -2 * (lppd - p_waic_2)
  
  list(
    lppd = lppd,
    p_waic_1 = p_waic_1,
    p_waic_2 = p_waic_2,
    WAIC = WAIC
  )
}

dic_from_loglik <- function(log_lik){
  
  D <- -2 * rowSums(log_lik)
  
  mean_deviance <- mean(D)
  pD <- var(D) / 2
  DIC <- mean_deviance + pD
  
  list(
    mean_deviance = mean_deviance,
    pD = pD,
    DIC = DIC
  )
}


## =========================================================
## Combine posterior draws across chains
## =========================================================

post <- do.call(rbind, lapply(res_jm, as.matrix))
cn <- colnames(post)


## =========================================================
## Locate and order subject-level log-likelihood columns
## =========================================================

idx.a <- grep("^ll\\.a\\[[0-9]+\\]$", cn)
idx.e <- grep("^ll\\.e\\[[0-9]+\\]$", cn)

if(length(idx.a) == 0)
  stop("No ll.a[i] columns found in posterior samples.")

if(length(idx.e) == 0)
  stop("No ll.e[i] columns found in posterior samples.")

idx.a <- idx.a[
  order(as.integer(
    sub("^ll\\.a\\[([0-9]+)\\]$", "\\1", cn[idx.a])
  ))
]

idx.e <- idx.e[
  order(as.integer(
    sub("^ll\\.e\\[([0-9]+)\\]$", "\\1", cn[idx.e])
  ))
]

if(length(idx.a) != length(idx.e))
  stop("Different number of ll.a[i] and ll.e[i] columns.")


## =========================================================
## Subject-level log-likelihood matrices
## =========================================================

ll.a.mat <- post[, idx.a, drop = FALSE]
ll.e.mat <- post[, idx.e, drop = FALSE]

ll.total.mat <- ll.a.mat + ll.e.mat


## =========================================================
## DIC
## =========================================================

dic.pa    <- dic_from_loglik(ll.a.mat)
dic.pe    <- dic_from_loglik(ll.e.mat)
dic.total <- dic_from_loglik(ll.total.mat)


## =========================================================
## WAIC
## =========================================================

waic.pa    <- waic_from_loglik(ll.a.mat)
waic.pe    <- waic_from_loglik(ll.e.mat)
waic.total <- waic_from_loglik(ll.total.mat)


## =========================================================
## PSIS-LOO
## ========================================================= 

loo.pa    <- loo(ll.a.mat)
loo.pe    <- loo(ll.e.mat)
loo.total <- loo(ll.total.mat)


## =========================================================
## Final output: DIC + WAIC + LOO only
## =========================================================

dicwaic_df <- data.frame(
  DIC_PA    = dic.pa$DIC,
  WAIC_PA   = waic.pa$WAIC,
  LOOIC_PA  = -2 * loo.pa$estimates["elpd_loo", "Estimate"],
  
  DIC_PE    = dic.pe$DIC,
  WAIC_PE   = waic.pe$WAIC,
  LOOIC_PE  = -2 * loo.pe$estimates["elpd_loo", "Estimate"],
  
  DIC_Total   = dic.total$DIC,
  WAIC_Total  = waic.total$WAIC,
  LOOIC_Total = -2 * loo.total$estimates["elpd_loo", "Estimate"]
)

write.csv(
  dicwaic_df,
  paste0("dicwaic_nonmixJM12.", num, ".csv"),
  row.names = FALSE
)

