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
M<-length(last.tt)
#participant ID
id<-rep(1:M)
length(id)

t<-round(first.tt)
tt<-round(last.tt)
k<-(tt-t)*4

alpha = c(1,1)

set.seed(123)

#############################################################
X <- as.matrix(read.csv(list.files(pattern="mixLM.X_data.")))
Y <- as.matrix(read.csv(list.files(pattern="mixLM.Y_data.")))
#############################################################

  ############Model in the JAGS format#####################
  ############Two fixed CP#####################  
modelrancp <- "model { 
  for(i in 1:M){ 
        for(j in 1:k[i]){
  ### PA model
        Y[i,j] ~ dbin(p2[i,j],1)
        p2[i,j] <- p[i,j,z[i]]
        logit(p[i,j,1]) <- c10 + c1[1] * (X[i,j]-cp1[i]) + c1[2] * (X[i,j]-cp1[i]) * (2*step(X[i,j]-cp1[i])-1) + u1[i] ## increasing
        logit(p[i,j,2]) <- c20 + (c1[1]-c1[2]) * X[i,j] + u2[i] ## no cp
        }
        z[i]~dcat(pi[1:2])
        u1[i] ~ dnorm(0,u.tau1)
        u2[i] ~ dnorm(0,u.tau2)
        cp1[i] ~ dnorm(cp1.mu,cp1.tau)
        L[i] <- prod(((p2[i,1:k[i]])^(Y[i,1:k[i]]))*((1-p2[i,1:k[i]])^(1-Y[i,1:k[i]])))
        ll[i] <- log(L[i])
  }
  log_lik0 <- sum(ll[])
  dev <- -2*log_lik0
  pi[1:2] ~ ddirch(alpha[])
  c10 ~ dnorm(0,0.0001)
  c20 ~ dnorm(0,0.0001)
	c1[1] ~ dnorm(0,0.0001)	
	c1[2] ~ dnorm(0,0.0001)
	B11<-c1[1]-c1[2]
  B12<-c1[1]+c1[2]
  u.tau1 ~ dgamma(0.001,0.001)
	u.tau.inv1 <- 1/u.tau1  ## variance
	u.tau2 ~ dgamma(0.001,0.001)
	u.tau.inv2 <- 1/u.tau2  ## variance
  cp1.mu ~ dnorm(0,0.01)
	cp1.tau ~ dgamma(0.01,0.01)
	cp1.tau.inv <- 1/cp1.tau  ## variance
}"

  
  ####Observed DATA
  data <- dump.format(list(X=X, Y=Y, M=M, k=k, alpha=alpha)) 
  ###initial Values
  inits1 <- dump.format(list(c10=-3, c20=-3, c1=c(0.3,0.4),  pi=c(0.5,0.5), u.tau1=1,u.tau2=1, cp1.mu=13, cp1.tau=1,
                             .RNG.name="base::Super-Duper", .RNG.seed=1)) 
  inits2 <- dump.format(list(c10=-3.1, c20=-3, c1=c(0.3,0.4)+0.01, pi=c(0.49,0.51), u.tau1=1,u.tau2=1, cp1.mu=13.1, cp1.tau=1,
                             .RNG.name="base::Super-Duper", .RNG.seed=2))
  
  #### Run the model and produce plots
  res <- run.jags(model=modelrancp, burnin=10000, sample=3000,  
                  monitor=c("B11","B12","c10", "c20","c1", "cp1",
                            "pi", "z", "u1","u2", "u.tau.inv1","u.tau.inv2", "u.tau1","u.tau2",
                            "cp1.mu","cp1.tau.inv","cp1.tau","ll","dev"), 
                  data=data, n.chains=2, method = "parallel", inits=c(inits1,inits2), thin=20)
  
  summary <- summary(res)
  result_df <- as.data.frame(summary)
  text <- list.files(pattern="mixLM.X_data.")
  num <- unlist(lapply(strsplit(text,'.',fixed=TRUE),function(x) x[[3]]))
  write.csv(result_df, paste0("mixLM.result.",num,".csv"))
  
  res_jm <- res$mcmc
  vars<-mcmc.list(res_jm[[1]][,c(1:16)],res_jm[[2]][,c(1:16)])
  pdf(file = paste0("mixLM.traceplot.",num,".pdf"),   # The directory you want to save the file in
      width = 4, # The width of the plot in inches
      height = 4) # The height of the plot in inches
  traplot(vars)
  dev.off()
  