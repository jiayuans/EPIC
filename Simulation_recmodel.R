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
simdat.pe <- as.data.frame(read.csv(list.files(pattern="rec.sim.pe_data.")))
#############################################################

  set.seed(123)
  t<-round(first.tt)
  tt<-round(last.tt)
  k.pa<-(tt-t)*4
  
  X1=c(rep(1,N/2),rep(0,N/2))
  ##X1=sample(c(1,0),N, replace = TRUE)

  timeS <- aggregate(simdat.pe$start, by=list(simdat.pe$id),
                     FUN=min, na.rm=TRUE)
  colnames(timeS) <- c("id","t0")
  
  timeE <-aggregate(simdat.pe$stop, by=list(simdat.pe$id),
                    FUN=max, na.rm=TRUE)
  colnames(timeE) <- c("id","tau")
  
  X.dat.pe <- simdat.pe[!duplicated(simdat.pe$id), ]
  
  time <- subset(simdat.pe,status==1)
  time$t <- time$stop
  time1 <- time[,c(1,8)]  
  simdat.pe1 <- merge(timeS,timeE,all=TRUE)
  simdat.pe2 <- merge(simdat.pe1,time1,all=TRUE)
  simdat.pe2$t[which(is.na(simdat.pe2$t))] <- 0
  
  #length(which(simdat.pe2$t== 0))
  
  count <- simdat.pe2 %>% count(id)
  max.count <- max(count$n) 
  
  ##########################Assigning unique number to each subject##########################
  simdat.pe3 <- simdat.pe2 %>% group_by(id) %>% mutate(time = c(1:length(id)))
  Yd.temp <- data.frame(id = rep(unique(simdat.pe$id),each=max.count), time = 1:max.count) 
  Y.epic <- merge(simdat.pe3,Yd.temp,by=c('id','time'),all.y=TRUE)
  
  #################Readingin data for time matrix#############################
  Ti <- matrix(Y.epic$t, N, max.count, byrow=TRUE)
  Ti.n <- Ti
  for (i in 1:N) {
    if (Ti.n[i,1] !=0){
      Ti.n[i,] <- Ti[i,]+t[i]   
    }
  }
  #################Readingin data for X, t0, tau vectors#############################
  ##X1 <- as.numeric(X.dat.pe[,2]) ## sexf: female
  time.t0 <- timeS$t0+t
  time.tau <- timeE$tau+t
  
  
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
        for(j in 1:k.pe[i]){
  ### PE model
       ## Weibull baseline
        lambda0[i,j] <- a*(Ti[i,j])^(a-1)
        lambda[i,j] <- lambda0[i,j]*v[i]*exp(b0+b*X1[i])
       }
        v[i] ~ dgamma(1/ph,1/ph) ## include ph with priors ~ gamma 
        L.e[i] <- ifelse(Ti[i,1]!=0, prod(lambda[i,1:k.pe[i]]) * exp(v[i]*exp(b0+b*X1[i])*(time.t0[i]^a-time.tau[i]^a)), exp(v[i]*exp(b0+b*X1[i])*(time.t0[i]^a-time.tau[i]^a)))
        ll.e[i] <- log(L.e[i])
        phi[i] <- -log(L.e[i]) + 1000
        zeros[i] ~ dpois(phi[i])
  }
  log_lik0.e <- sum(ll.e[]) 
  dev.e <- -2*log_lik0.e
  ## prior distributions
  a ~ dgamma(0.01,0.01)
  b0 ~ dnorm(0,0.0001)	
  b ~ dnorm(0,0.0001)		
	ph ~ dgamma(0.001,0.001)
}"

  
  ####Observed DATA
  data <- dump.format(list(N=N, X1=X1,k.pe=k.pe, time.t0=time.t0, time.tau=time.tau, Ti=Ti.n)) 
  ###initial Values
  inits1 <- dump.format(list(b0=-1.34, b=0.26, a=1.73, ph=.5,
                             .RNG.name="base::Super-Duper", .RNG.seed=1))
  inits2 <- dump.format(list(b0=-1.33,b=0.27, a=1.74, ph=.5,
                             .RNG.name="base::Super-Duper", .RNG.seed=2))
  #### Run the model and produce plots
  res <- run.jags(model=modelrancp, burnin=8000, sample=6000, 
                  monitor=c("b0","b","a","ph","v","ll.e","dev.e","dic"), 
                  data=data, n.chains=2, inits=c(inits1,inits2), thin=10, module='dic')

  summary <- summary(res)
  result_df <- as.data.frame(summary)
  text <- list.files(pattern="rec.sim.pe_data.")
  num <- unlist(lapply(strsplit(text,'.',fixed=TRUE),function(x) x[[2]]))
  write.csv(result_df, paste0("rec.result.",num,".csv"))
  
  res_jm <- res$mcmc
  #dimnames(res_jm[[1]])
  vars<-mcmc.list(res_jm[[1]][,c(1:16)],res_jm[[2]][,c(1:16)])
  #str(vars)
  #plot(vars[,1])
  #summary(vars)
  pdf(file = paste0("rec.traceplot.",num,".pdf"),   # The directory you want to save the file in
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