#!/usr/bin/env Rscript
library(coda)
library(rjags)
library(runjags)
library(tidyverse)
library(mcmcplots)

M<-400
#############################################################
X <- as.matrix(read.csv(list.files(pattern="mix.X_data.")))
Y <- as.matrix(read.csv(list.files(pattern="mix.Y_data.")))
#############################################################

  set.seed(123)
  t<-rep(0,400)
  tt<-rep(22,400)
  k<-(tt-t)*4
  alpha = c(1,1)
  
  ############Model in the JAGS format#####################
  ############Two fixed CP#####################  
  modelrancp <- "model { 
  for(i in 1:M){ 
        for(j in 1:k[i]){
  ### PA model
        Y[i,j] ~ dbin(p2[i,j],1)
        p2[i,j] <- p[i,j,z[i]]
        logit(p[i,j,1]) <- c10 + c1[1] * (X[i,j]-cp1[i]) + c1[2] * (X[i,j]-cp1[i]) * (2*step(X[i,j]-cp1[i])-1) + u1[i] ## increasing
        logit(p[i,j,2]) <- c20 + c2[1] * (X[i,j]-cp2[i]) + c2[2] * (X[i,j]-cp2[i]) * (2*step(X[i,j]-cp2[i])-1) + u2[i] ## decreasing
        }
        z[i]~dcat(pi[1:2])
        u1[i] ~ dnorm(0,u.tau1)
        u2[i] ~ dnorm(0,u.tau2)
        cp1[i] ~ dnorm(cp1.mu,cp1.tau)
        cp2[i] ~ dnorm(cp2.mu,cp2.tau)
        L[i] <- prod(((p2[i,1:k[i]])^(Y[i,1:k[i]]))*((1-p2[i,1:k[i]])^(1-Y[i,1:k[i]])))
        ll[i] <- log(L[i])
  }
  log_lik0 <- sum(ll[])
  dev <- -2*log_lik0
  pi[1:2] ~ ddirch(alpha[])
  c10 ~ dnorm(0,0.0001)
  c20 ~ dnorm(0,0.0001)
  B11 ~ dnorm(0,0.0001)
  B12 ~ dnorm(0,0.0001)
  B21 ~ dnorm(0,0.0001)
  delta ~ dgamma(0.001,0.001)
  B22 <- B12 - delta
  c1[1] <- (B12+B11)/2
  c1[2] <- (B12-B11)/2
  c2[1] <- (B22+B21)/2
  c2[2] <- (B22-B21)/2  
  u.tau1 ~ dgamma(0.001,0.001)
	u.tau.inv1 <- 1/u.tau1  ## variance
	u.tau2 ~ dgamma(0.001,0.001)
	u.tau.inv2 <- 1/u.tau2  ## variance
  cp1.mu ~ dnorm(0,0.01)
	cp1.tau ~ dgamma(0.01,0.01)
	cp1.tau.inv <- 1/cp1.tau  ## variance
	cp2.mu ~ dnorm(0,0.01)
	cp2.tau ~ dgamma(0.01,0.01)
	cp2.tau.inv <- 1/cp2.tau  ## variance
}"
  
  
  ####Observed DATA
  data <- dump.format(list(X=X, Y=Y, M=M, k=k, alpha=alpha )) 
  ###initial Values
  inits1 <- dump.format(list(c10=-3, c20=-3, B11=0.1, B12=0.5, B21=0.5, pi=c(0.7,0.3), u.tau1=1,u.tau2=1, cp1.mu=15, cp1.tau=1,cp2.mu=15, cp2.tau=1,
                             .RNG.name="base::Super-Duper", .RNG.seed=1)) 
  inits2 <- dump.format(list(c10=-3.1, c20=-3.1, B11=0.11, B12=0.51, B21=0.51, pi=c(0.69,0.31), u.tau1=1,u.tau2=1, cp1.mu=15.1, cp1.tau=1,cp2.mu=15.1, cp2.tau=1,
                             .RNG.name="base::Super-Duper", .RNG.seed=2))
  
  #### Run the model and produce plots
  res <- run.jags(model=modelrancp, burnin=10000, sample=3000,  
                  monitor=c("B11","B12","B21","B22","c10", "c20","c1","c2", "cp1","cp2",
                            "pi", "z", "u1","u2", "u.tau.inv1","u.tau.inv2", "u.tau1","u.tau2",
                            "cp1.mu","cp1.tau.inv","cp1.tau","cp2.mu","cp2.tau.inv","cp2.tau","delta","ll","dev"), 
                  data=data, n.chains=2, method = "parallel", inits=c(inits1,inits2), thin=20)
  summary <- summary(res)
  result_df <- as.data.frame(summary)
  text <- list.files(pattern="mix.X_data.")
  num <- unlist(lapply(strsplit(text,'.',fixed=TRUE),function(x) x[[3]]))
  write.csv(result_df, paste0("mix.result.",num,".csv"))
  
  res_jm <- res$mcmc
  vars<-mcmc.list(res_jm[[1]][,c(1:16)],res_jm[[2]][,c(1:16)])
  pdf(file = paste0("mix.traceplot.",num,".pdf"),   # The directory you want to save the file in
      width = 4, # The width of the plot in inches
      height = 4) # The height of the plot in inches
  traplot(vars)
  dev.off()
  