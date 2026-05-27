#!/usr/bin/env Rscript
library(coda)
library(rjags)
library(runjags)
library(tidyverse)
library(mcmcplots)

long.time <- read.csv("long.data_new.csv")
first.tt <- long.time[,2]
last.tt <- long.time[,3]

####time of first visit and last visit#######
N<-length(last.tt)
#participant ID
id<-rep(1:N)
length(id)

t<-round(first.tt)
tt<-round(last.tt)
k.pa<-(tt-t)*4

X1=c(rep(1,N/2),rep(0,N/2))
##X1=sample(c(1,0),N, replace = TRUE)

set.seed(123)

#############################################################
X <- as.matrix(read.csv(list.files(pattern="X_data_2rcpc.")))
Y <- as.matrix(read.csv(list.files(pattern="Y_data_2rcpc.")))
simdat.pe00 <- as.data.frame(read.csv(list.files(pattern="sim.pe_data_2rcpc.")))
#############################################################

tt<-tt-0.25
  timeS <- as.data.frame(cbind(id,t)) ## left truncation time
  timeE <- as.data.frame(cbind(id,tt))
  
  simdat.pe0 <- merge(simdat.pe00, timeS,all=TRUE)
  simdat.pe <- subset(simdat.pe0, stop >= t)
  
  time <- subset(simdat.pe,status==1)
  time1 <- time[,c("id","stop")]  
  simdat.pe1 <- merge(timeS,timeE,all=TRUE)
  simdat.pe2 <- merge(simdat.pe1,time1,all=TRUE)
  simdat.pe2$stop[which(is.na(simdat.pe2$stop))] <- 0
  
  #length(which(simdat.pe2$stop== 0))
  
  count <- simdat.pe2 %>% count(id)
  max.count <- max(count$n) 
  
  ##########################Assigning unique number to each subject##########################
  simdat.pe3 <- simdat.pe2 %>% group_by(id) %>% mutate(time = c(1:length(id)))
  Yd.temp <- data.frame(id = rep(unique(simdat.pe00$id),each=max.count), time = 1:max.count) 
  Y.epic <- merge(simdat.pe3,Yd.temp,by=c('id','time'),all.y=TRUE)
  
  #################Readingin data for time matrix#############################
  Ti <- matrix(Y.epic$stop, N, max.count, byrow=TRUE)
  
  #################Readingin data for X, t0, tau vectors#############################
  ##X1 <- as.numeric(X.dat.pe[,2]) ## sexf: female
  time.t0 <- t
  time.tau <- tt
  
  #################input variables for simulation#####################
  #### checking for how many individuals we have NAs in the middle of followup
  sum.na <- rep(NA,N)
  k.pe=rep(NA,N)
  
  ids <- unique(Y.epic$id) ## 103104 103125 103129 103145 103147
  for (i in 1:N){
    na.indices <- which(Y.epic$t[Y.epic$id==ids[i]] %in% NA)
    if (length(na.indices)==0){
      k.pe[i] <- max.count} else{
        k.pe[i] <- min(na.indices)-1}
  }
  

  ############Model in the JAGS format#####################
  ############Two fixed CP#####################  
  modelrancp <- "
data { 
  for(i in 1:N){
       zeros[i]<- 0
  }
}
model { 
  for(i in 1:N){ 
        for(j in 1:k.pa[i]){
  ### PA model
        Y[i,j] ~ dbin(p2[i,j],1)
        logit(p2[i,j]) <- c0 + c[1] * (X[i,j]-cp1[i]) + c[2] * (X[i,j]-cp1[i]) * (2*step(X[i,j]-cp1[i])-1) + c[3] * (X[i,j]-cp2[i]) * (2*step(X[i,j]-cp2[i])-1) + c[4] * X1[i] + u[i]
        }
        for(j in 1:k.pe[i]){
  ### PE model
       ## Weibull baseline
        lambda0[i,j] <- a*(Ti[i,j])^(a-1)
        lambda[i,j] <- lambda0[i,j]*v[i]*exp(b0+b*X1[i])
       }
        u[i] ~ dnorm(0,u.tau)
        cp1[i] ~ dnorm(cp1.mu,cp1.tau)	
	      z[i] ~ dbeta(3, 2)
        cp2.temp[i] <- z[i] * (21.45 - cp1[i])
        cp2[i] <- cp1[i] + cp2.temp[i]
        cp1c[i] <- cp1[i] - cp1.mu
        cp2.mu[i] <- cp1[i] + 0.6 * (21.45 - cp1[i]) # subject-specific expected value of cp2[i] given cp1[i]
        cp2c[i] <- cp2[i] - cp2.mu[i] 
        #cp2c[i] <- cp2[i] - 14.87
        L.a[i] <- prod(((p2[i,1:k.pa[i]])^(Y[i,1:k.pa[i]]))*((1-p2[i,1:k.pa[i]])^(1-Y[i,1:k.pa[i]])))
        ll.a[i] <- log(L.a[i])
        w[i] ~ dnorm(0,w.tau)
        v[i] <- exp(ga*u[i]+w[i]+ga1*cp1c[i]+ga2*cp2c[i])
        L.e[i] <- ifelse(Ti[i,1]!=0, prod(lambda[i,1:k.pe[i]]) * exp(v[i]*exp(b0+b*X1[i])*(time.t0[i]^a-time.tau[i]^a)), exp(v[i]*exp(b0+b*X1[i])*(time.t0[i]^a-time.tau[i]^a)))
        ll.e[i] <- log(L.e[i])
        phi[i] <- -ll.e[i] + 1000
        zeros[i] ~ dpois(phi[i])
  }
  log_lik0.a <- sum(ll.a[]) 
  log_lik0.e <- sum(ll.e[]) 
  dev.a <- -2*log_lik0.a
  dev.e <- -2*log_lik0.e
  c0 ~ dnorm(0,0.01)
	for (k in 1:4){
	      c[k] ~ dnorm(0,0.01)	
	}
  ## prior distributions
	u.tau ~ dgamma(0.01,0.01)
	cp1.mu ~ dnorm(0,0.01)
	cp1.tau ~ dgamma(1,1) # dgamma(0.01,0.01)
	cp1.tau.inv <- 1/cp1.tau  ## variance 
	B1 <-c[1]-c[2]-c[3]
  B2 <-c[1]+c[2]-c[3]
  B3 <-c[1]+c[2]+c[3]
  u.tau.inv <- 1/u.tau  ## variance 
  a ~ dgamma(0.01,0.01)
  b0 ~ dnorm(0,0.01)	
  b ~ dnorm(0,0.01)		
	ga ~ dnorm(0,0.1)
	ga1 ~ dnorm(0,0.1)
	ga2 ~ dnorm(0,0.1)
	w.tau ~ dgamma(3, 0.12)
	w.tau.inv <- 1/w.tau  ## variance 
}"
  
  ####Observed DATA
  data <- dump.format(list(X=X, Y=Y, N=N, k.pa=k.pa, 
                           X1=X1, k.pe=k.pe, time.t0=time.t0, time.tau=time.tau, Ti=Ti)) 
  ##initial Values
  inits1 <- dump.format(list(c0=-3, c=c(0.1,0.15,0.1,-0.1), u.tau=0.04, cp1.mu=5, cp1.tau=1, 
                             b0=-2, b=0.2, a=1.8, w.tau=0.04, ga=0.3, ga1=-0.05, ga2=-0.02,
                             .RNG.name="base::Super-Duper", .RNG.seed=1))
  inits2 <- dump.format(list(c0=-3.1, c=c(0.1,0.15,0.1,-0.1)+0.01, u.tau=0.04, cp1.mu=5, cp1.tau=1, 
                             b0=-2.1, b=0.21, a=1.81, w.tau=0.04, ga=0.31, ga1=-0.04, ga2=-0.019,
                             .RNG.name="base::Super-Duper", .RNG.seed=2))

  #### Run the model and produce plots
  res <- run.jags(model=modelrancp, burnin=10000, sample=6000, 
                  monitor=c("B1","B2","B3","cp1","cp2","c0","c","u.tau.inv",
                            "b0","b","a","ga","ga1","ga2","w.tau.inv",
                            "cp1.mu","cp1.tau.inv","cp2.temp",
                            "u","v","w","cp2.mu",
                            "u.tau","w.tau","cp1.tau","ll.a","ll.e","dev.a","dev.e"), 
                  data=data, n.chains=2, method = "parallel", inits=c(inits1,inits2), thin=6)
  
  summary <- summary(res)
  result_df <- as.data.frame(summary)
  text <- list.files(pattern="X_data_2rcpc.")
  num <- unlist(lapply(strsplit(text,'.',fixed=TRUE),function(x) x[[2]]))
  write.csv(result_df, paste0("result_2rcpcpn.",num,".csv"))
  #save(res, file=paste0("res_2rcpcpn.",num,".RData"))
  
  res_jm <- res$mcmc
  #vars<-mcmc.list(res_jm[[1]][,c(1:16)],res_jm[[2]][,c(1:16)])
  # pdf(file = paste0("traceplot_2rcpcpn.",num,".pdf"),   # The directory you want to save the file in
  #    width = 4, # The width of the plot in inches
  #    height = 4) # The height of the plot in inches
  #traplot(vars)
  #dev.off()

  ## =========================================================
  ## DIC / WAIC calculation for 2-RCP centered model
  ## =========================================================
  
  ## -----------------------------
  ## Helper functions
  ## -----------------------------
  colVars <- function(a) {
    cm <- colMeans(a)
    diff <- sweep(a, 2, cm, "-")
    apply(diff^2, 2, sum) / (nrow(a) - 1)
  }
  
  log_mean_exp <- function(x) {
    m <- max(x)
    m + log(mean(exp(x - m)))
  }
  
  waic_fun <- function(log_lik) {
    ## log_lik: rows = posterior draws, cols = subjects
    lppd_i <- apply(log_lik, 2, log_mean_exp)
    lppd <- sum(lppd_i)
    
    p_waic_1 <- 2 * sum(lppd_i - colMeans(log_lik))
    p_waic_2 <- sum(colVars(log_lik))
    
    waic_2 <- -2 * lppd + 2 * p_waic_2
    
    list(
      waic = waic_2,
      p_waic = p_waic_2,
      lppd = lppd,
      p_waic_1 = p_waic_1
    )
  }
  
  mean_dev_from_loglik <- function(log_lik) {
    ## mean posterior deviance
    mean(-2 * rowSums(log_lik))
  }
  
  get1 <- function(sum.df, nm) {
    as.numeric(sum.df[nm, "Mean"])
  }
  
  getv <- function(sum.df, pat) {
    r <- grep(pat, rownames(sum.df), value = TRUE)
    idx <- as.numeric(sub(".*\\[([0-9]+)\\]$", "\\1", r))
    r <- r[order(idx)]
    as.numeric(sum.df[r, "Mean"])
  }
  
  sign_step <- function(x) {
    ## JAGS step(x) is 1 if x >= 0, 0 otherwise
    ifelse(x >= 0, 1, -1)
  }
  
  expit <- function(x) {
    1 / (1 + exp(-x))
  }
  
  
  ## -----------------------------
  ## Extract posterior summary and draws
  ## -----------------------------
  sum.df <- result_df
  M <- N
  
  draw_mat <- as.matrix(as.mcmc.list(res_jm))
  
  ll_a_names <- paste0("ll.a[", 1:M, "]")
  ll_e_names <- paste0("ll.e[", 1:M, "]")
  
  if (!all(ll_a_names %in% colnames(draw_mat))) {
    stop("Some ll.a[i] columns are missing from monitored draws.")
  }
  
  if (!all(ll_e_names %in% colnames(draw_mat))) {
    stop("Some ll.e[i] columns are missing from monitored draws.")
  }
  
  log.like.a <- draw_mat[, ll_a_names, drop = FALSE]
  log.like.e <- draw_mat[, ll_e_names, drop = FALSE]
  
  ## Joint pointwise log-likelihood
  log.like.total <- log.like.a + log.like.e
  
  
  ## -----------------------------
  ## WAIC
  ## -----------------------------
  waic.jm.a     <- waic_fun(log.like.a)
  waic.jm.e     <- waic_fun(log.like.e)
  waic.jm.total <- waic_fun(log.like.total)
  
  waic.a     <- waic.jm.a$waic
  waic.e     <- waic.jm.e$waic
  total.waic <- waic.jm.total$waic
  
  
  ## -----------------------------
  ## Mean posterior deviance
  ## -----------------------------
  md.jm.a     <- mean_dev_from_loglik(log.like.a)
  md.jm.e     <- mean_dev_from_loglik(log.like.e)
  md.jm.total <- mean_dev_from_loglik(log.like.total)
  
  
  ## -----------------------------
  ## Extract posterior mean parameters
  ## -----------------------------
  c0 <- get1(sum.df, "c0")
  c  <- getv(sum.df, "^c\\[")
  
  c1 <- c[1]
  c2 <- c[2]
  c3 <- c[3]
  c4 <- c[4]
  
  b0 <- get1(sum.df, "b0")
  b  <- get1(sum.df, "b")
  
  a <- get1(sum.df, "a")
  
  ga  <- get1(sum.df, "ga")
  ga1 <- get1(sum.df, "ga1")
  ga2 <- get1(sum.df, "ga2")
  
  cp1.mu <- get1(sum.df, "cp1.mu")
  
  cp1 <- getv(sum.df, "^cp1\\[")
  cp2 <- getv(sum.df, "^cp2\\[")
  u   <- getv(sum.df, "^u\\[")
  w   <- getv(sum.df, "^w\\[")
  
  
  ## -----------------------------
  ## Reconstruct centered change points
  ## matching JAGS code
  ## -----------------------------
  cp1c <- cp1 - cp1.mu
  
  cp2.mu <- cp1 + 0.6 * (21.45 - cp1)
  cp2c   <- cp2 - cp2.mu
  
  v <- exp(ga * u + w + ga1 * cp1c + ga2 * cp2c)
  
  
  ## -----------------------------
  ## Reconstruct log-likelihood at posterior mean
  ## -----------------------------
  max_kpa <- max(k.pa)
  max_kpe <- max(k.pe)
  
  p2      <- matrix(NA_real_, M, max_kpa)
  lambda0 <- matrix(NA_real_, M, max_kpe)
  lambda  <- matrix(NA_real_, M, max_kpe)
  
  ll.a.hat <- rep(NA_real_, M)
  ll.e.hat <- rep(NA_real_, M)
  
  for (i in 1:M) {
    
    ## -----------------------
    ## PA model
    ## -----------------------
    for (j in 1:k.pa[i]) {
      d1 <- X[i, j] - cp1[i]
      d2 <- X[i, j] - cp2[i]
      
      s1 <- sign_step(d1)
      s2 <- sign_step(d2)
      
      eta_ij <- c0 +
        c1 * d1 +
        c2 * d1 * s1 +
        c3 * d2 * s2 +
        c4 * X1[i] +
        u[i]
      
      p2[i, j] <- expit(eta_ij)
    }
    
    ll.a.hat[i] <- sum(
      Y[i, 1:k.pa[i]] * log(p2[i, 1:k.pa[i]]) +
        (1 - Y[i, 1:k.pa[i]]) * log(1 - p2[i, 1:k.pa[i]])
    )
    
    
    ## -----------------------
    ## PE model
    ## -----------------------
    linpred_surv <- b0 + b * X1[i]
    
    for (j in 1:k.pe[i]) {
      lambda0[i, j] <- a * (Ti[i, j])^(a - 1)
      lambda[i, j]  <- lambda0[i, j] * v[i] * exp(linpred_surv)
    }
    
    if (Ti[i, 1] != 0) {
      ll.e.hat[i] <- sum(log(lambda[i, 1:k.pe[i]])) +
        v[i] * exp(linpred_surv) * (time.t0[i]^a - time.tau[i]^a)
    } else {
      ll.e.hat[i] <-
        v[i] * exp(linpred_surv) * (time.t0[i]^a - time.tau[i]^a)
    }
  }
  
  
  ## -----------------------------
  ## Deviance at posterior mean
  ## -----------------------------
  dev_hat.a     <- -2 * sum(ll.a.hat)
  dev_hat.e     <- -2 * sum(ll.e.hat)
  dev_hat.total <- -2 * sum(ll.a.hat + ll.e.hat)
  
  
  ## -----------------------------
  ## DIC
  ## DIC = 2 * mean(D(theta)) - D(E(theta))
  ## -----------------------------
  dic.a     <- 2 * md.jm.a     - dev_hat.a
  dic.e     <- 2 * md.jm.e     - dev_hat.e
  dic.total <- 2 * md.jm.total - dev_hat.total
  
  
  ## -----------------------------
  ## Final output
  ## -----------------------------
  dicwaic_df <- data.frame(
    DIC_a      = dic.a,
    WAIC_a     = waic.a,
    DIC_e      = dic.e,
    WAIC_e     = waic.e,
    DIC_total  = dic.total,
    WAIC_total = total.waic,
    
    p_WAIC_a      = waic.jm.a$p_waic,
    p_WAIC_e      = waic.jm.e$p_waic,
    p_WAIC_total  = waic.jm.total$p_waic,
    
    lppd_a      = waic.jm.a$lppd,
    lppd_e      = waic.jm.e$lppd,
    lppd_total  = waic.jm.total$lppd,
    
    mean_dev_a      = md.jm.a,
    mean_dev_e      = md.jm.e,
    mean_dev_total  = md.jm.total,
    
    dev_hat_a      = dev_hat.a,
    dev_hat_e      = dev_hat.e,
    dev_hat_total  = dev_hat.total
  )
  
  print(dicwaic_df)
  
  write.csv(
    dicwaic_df,
    paste0("dicwaic_2rcpcpn.", num, ".csv"),
    row.names = FALSE
  )