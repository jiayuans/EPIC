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
X <- as.matrix(read.csv(list.files(pattern="X_data_1rcpnc.")))
Y <- as.matrix(read.csv(list.files(pattern="Y_data_1rcpnc.")))
simdat.pe00 <- as.data.frame(read.csv(list.files(pattern="sim.pe_data_1rcpnc.")))
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
  ############Two fixed CP (Simulate from JM2, Fit JM2)#####################  
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
        logit(p2[i,j]) <- c0 + c[1] * (X[i,j]-cp1[i]) + c[2] * (X[i,j]-cp1[i]) * (2*step(X[i,j]-cp1[i])-1) + c[3] * X1[i] + u[i]
        }
        for(j in 1:k.pe[i]){
  ### PE model
       ## Weibull baseline
        lambda0[i,j] <- a*(Ti[i,j])^(a-1)
        lambda[i,j] <- lambda0[i,j]*v[i]*exp(b0+b*X1[i])
       }
        u[i] ~ dnorm(0,u.tau)
        cp1[i] ~ dnorm(cp1.mu,cp1.tau)	
        #cp1.c[i] <- cp1[i] - cp1.mu          # centered random CP
        L.a[i] <- prod(((p2[i,1:k.pa[i]])^(Y[i,1:k.pa[i]]))*((1-p2[i,1:k.pa[i]])^(1-Y[i,1:k.pa[i]])))
        ll.a[i] <- log(L.a[i])
        w[i] ~ dnorm(0,w.tau)
        v[i] <- exp(ga*u[i]+w[i]+ga1*cp1[i] + logv.offset)
        #L.e[i] <- ifelse(Ti[i,1]!=0, prod(lambda[i,1:k.pe[i]]) * exp(v[i]*exp(b0+b*X1[i])*(time.t0[i]^a-time.tau[i]^a)), exp(v[i]*exp(b0+b*X1[i])*(time.t0[i]^a-time.tau[i]^a)))
        #ll.e[i] <- log(L.e[i])
        #phi[i] <- -log(L.e[i]) + 1000
       
        H[i] <- v[i] * exp(b0 + b*X1[i]) * ( pow(time.tau[i], a) - pow(time.t0[i], a) )
        ll_e_events[i] <- ifelse(Ti[i,1] != 0, sum(log(lambda[i,1:k.pe[i]])), 0)
        ll.e[i] <- ll_e_events[i] - H[i]
        phi[i]   <- -ll.e[i] + 1000
        zeros[i] ~ dpois(phi[i])
  }
  log_lik0.a <- sum(ll.a[]) 
  log_lik0.e <- sum(ll.e[]) 
  dev.a <- -2*log_lik0.a
  dev.e <- -2*log_lik0.e
  c0 ~ dnorm(0,0.0001)
	for (k in 1:3){
	      c[k] ~ dnorm(0,0.0001)	
	}
  ## prior distributions
	u.tau ~ dgamma(0.01,0.01)
	cp1.mu ~ dnorm(0,0.01)
	cp1.tau ~ dgamma(0.01,0.01)
	cp1.tau.inv <- 1/cp1.tau  ## variance 
	B1 <-c[1]-c[2]
  B2 <-c[1]+c[2]
  u.tau.inv <- 1/u.tau  ## variance 
  a ~ dgamma(0.01,0.01)
  b0 ~ dnorm(0,0.0001)	
  b ~ dnorm(0,0.0001)
	ga ~ dnorm(0,0.01)
	ga1 ~ dnorm(0,0.01)
	w.tau ~ dgamma(0.01,0.01)
	w.tau.inv <- 1/w.tau  ## variance 
	logv.offset <- -0.5 * ( pow(ga,2)*u.tau.inv + w.tau.inv + pow(ga1,2)*cp1.tau.inv )- ga1*cp1.mu
}"

  
  ####Observed DATA
  data <- dump.format(list(X=X, Y=Y, N=N, k.pa=k.pa,
                           X1=X1, k.pe=k.pe, time.t0=time.t0, time.tau=time.tau, Ti=Ti)) 
  ##initial Values
  inits1 <- dump.format(list(c0=-3, c=c(0.3,0.3,-0.05), u.tau=1, cp1.mu=15, cp1.tau=1, 
                             b0=-4, b=0.2, a=1.8, w.tau=1, ga=0.3, ga1=-0.05,
                             .RNG.name="base::Super-Duper", .RNG.seed=1))
  inits2 <- dump.format(list(c0=-3.1, c=c(0.3,0.3,-0.05)+0.01, u.tau=1, cp1.mu=15.1, cp1.tau=1, 
                             b0=-4.1, b=0.21, a=1.8, w.tau=1, ga=0.31, ga1=-0.04,
                             .RNG.name="base::Super-Duper", .RNG.seed=2))

  #### Run the model and produce plots
  res <- run.jags(model=modelrancp, burnin=10000, sample=5000, 
                  monitor=c("B1","B2","cp1","c0","c","u.tau.inv",
                            "b0","b","a","ga","ga1","w.tau.inv", "cp1.mu","cp1.tau.inv","u","v","w",
                            "u.tau","w.tau","ll.a","ll.e","dev.a","dev.e"), 
                  data=data, n.chains=2, method = "parallel",inits=c(inits1,inits2), thin=10)
  
  #res <- run.jags(model=modelrancp, burnin=20000, sample=4000/10000/5000, 
  #               monitor=c("B1","B2","cp1","c0","c","u.tau.inv",
  #                          "b0","b","a","ga","ga1","w.tau.inv", "cp1.mu","cp1.tau.inv","u","v","w",
  #                          "u.tau","w.tau","ll.a","ll.e","dev.a","dev.e"), 
  #                data=data, n.chains=2, method = "parallel",inits=c(inits1,inits2), thin=20/30/20)
  
  summary <- summary(res)
  result_df <- as.data.frame(summary)
  text <- list.files(pattern="X_data_1rcpnc.")
  num <- unlist(lapply(strsplit(text,'.',fixed=TRUE),function(x) x[[2]]))
  write.csv(result_df, paste0("result_1rcpnc.",num,".csv"))
  save(res, file=paste0("res_1rcpnc.",num,".RData"))
  
  res_jm <- res$mcmc
  vars<-mcmc.list(res_jm[[1]][,c(1:2,403:413)],res_jm[[2]][,c(1:2,403:413)])
  pdf(file = paste0("traceplot_1rcpnc.",num,".pdf"),   # The directory you want to save the file in
      width = 4, # The width of the plot in inches
      height = 4) # The height of the plot in inches
  traplot(vars)
  dev.off()
  
  
   