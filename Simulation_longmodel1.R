#!/usr/bin/env Rscript
library(coda)
library(rjags)
library(runjags)
library(tidyverse)
library(mcmcplots)

long.time <- read.csv("long.time.csv")
first.tt <- long.time[,2]
last.tt <- long.time[,3]

####time of first visit and last visit#######
N<-length(last.tt)

#s=23###starting seed####

#############################################################
X <- as.matrix(read.csv(list.files(pattern="X_data.")))
Y <- as.matrix(read.csv(list.files(pattern="Y_data.")))
r <- as.numeric(read.csv(list.files(pattern="r_data.")))
#############################################################

  set.seed(123)
  t<-round(first.tt)
  tt<-round(last.tt)
  k.pa<-(tt-t)*4
  
  X1=c(rep(1,N/2),rep(0,N/2))
  ##X1=sample(c(1,0),N, replace = TRUE)
  
  ############Model in the JAGS format#####################
  ############Two fixed CP#####################  
  modelrancp <- "
model { 
  for(i in 1:N){ 
        for(j in 1:k.pa[i]){
  ### PA model
        Y[i,j] ~ dbin(p2[i,j],1)
        logit(p2[i,j]) <- c0 + c[1] * (X[i,j]-cp1) + c[2] * (X[i,j]-cp1) * (2*step(X[i,j]-cp1)-1) + c[3] * (X[i,j]-cp2) * (2*step(X[i,j]-cp2)-1) + c[4] * X1[i] + u[i]
        }
        u[i] ~ dnorm(0,u.tau)
        L.a[i] <- prod(((p2[i,1:k.pa[i]])^(Y[i,1:k.pa[i]]))*((1-p2[i,1:k.pa[i]])^(1-Y[i,1:k.pa[i]])))
        ll.a[i] <- log(L.a[i])
  }
  log_lik0.a <- sum(ll.a[])
  dev.a <- -2*log_lik0.a
  c0 ~ dnorm(0,0.0001)
	for (k in 1:4){
	      c[k] ~ dnorm(0,0.0001)	
	}
  ## prior distributions
	u.tau ~ dgamma(0.001,0.001)
	cp1 ~ dnorm(cp1.mu,cp1.tau)	
	cp2.temp ~ dunif(0,max)
	cp2 <- cp1 + cp2.temp
	cp1.mu ~ dnorm(0,0.001)
	cp1.tau ~ dgamma(0.001,0.001)
	B1 <-c[1]-c[2]-c[3]
  B2 <-c[1]+c[2]-c[3]
  B3 <-c[1]+c[2]+c[3]
  u.tau.inv <- 1/u.tau  ## variance 
}"

  
  ####Observed DATA
  data <- dump.format(list(X=X, Y=Y, N=N, k.pa=k.pa, max=max(tt), X1=X1)) 
  ###initial Values
  inits1 <- dump.format(list(c0=-4.44, c=c(0.10,0.13,0.08,0.08), u.tau=1/(1.6^2), cp1=4.5, cp2.temp=10,
                             .RNG.name="base::Super-Duper", .RNG.seed=1))
  inits2 <- dump.format(list(c0=-4.45, c=c(0.10,0.13,0.08,0.08)+0.01, u.tau=1/(1.6^2), cp1=4.6, cp2.temp=10,
                             .RNG.name="base::Super-Duper", .RNG.seed=2))
  #### Run the model and produce plots
  res <- run.jags(model=modelrancp, burnin=6000, sample=6000, 
                  monitor=c("B1","B2","B3","cp1","cp2","c0","c","u.tau.inv","u",
                            "u.tau","cp1.mu","cp1.tau","cp2.temp","ll.a","dev.a","dic"), 
                  data=data, n.chains=2, inits=c(inits1,inits2), thin=10, module='dic')
  summary <- summary(res)
  result_df <- as.data.frame(summary)
  text <- list.files(pattern="X_data.")
  num <- unlist(lapply(strsplit(text,'.',fixed=TRUE),function(x) x[[2]]))
  write.csv(result_df, paste0("result.long.",num,".csv"))
  
  res_jm <- res$mcmc
  #dimnames(res_jm[[1]])
  vars<-mcmc.list(res_jm[[1]][,c(1:16)],res_jm[[2]][,c(1:16)])
  #str(vars)
  #plot(vars[,1])
  #summary(vars)
  pdf(file = paste0("traceplot.long.",num,".pdf"),   # The directory you want to save the file in
      width = 4, # The width of the plot in inches
      height = 4) # The height of the plot in inches
  traplot(vars)
  dev.off()
  
  ##B1.mean <-summary[1,4] 
  ##B2.mean <-summary[2,4] 
  ##B3.mean <-summary[3,4] 
  ##c0.mean <-summary[4,4] 
  ##c1.mean <-summary[5,4] 
  ##c2.mean <-summary[6,4] 
  ##c3.mean <-summary[7,4]
  ##c4.mean <-summary[8,4]
  ##cp1.mean <-summary[9,4] 
  ##cp2.mean <-summary[10,4] 
  ##u.mean <-mean(summary[11:410,4])
  ##u.tau.inv.mean <-summary[411,4] 
  ##cp1.mu.mean <-summary[413,4]
  ##cp1.tau.mean <-summary[414,4]
  ##cp2.temp.mean <-summary[415,4]
  
  ##b0.mean <-summary[416,4] 
  ##b1.mean <-summary[417,4] 
  ##a.mean <-summary[418,4] 
  ##v.mean <-mean(summary[419:818,4])
  ##ga.mean <-summary[819,4] 
  ##w.mean <-mean(summary[820:1219,4])
  ##w.tau.inv.mean <-summary[1221,4] 
  

  ##Sim.results=cbind(B1.mean,B2.mean,B3.mean,c0.mean,c1.mean,c2.mean,c3.mean,c4.mean,cp1.mean,cp2.mean,u.tau.inv.mean,u.mean,
  ##                b0.mean,b1.mean,a.mean,v.mean,ga.mean,w.mean,w.tau.inv.mean)
##print(Sim.results)