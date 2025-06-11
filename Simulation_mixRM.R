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

X1=c(rep(1,N/2),rep(0,N/2))

set.seed(123)

#############################################################
simdat.pe00 <- as.data.frame(read.csv(list.files(pattern="mixRM.rec_data.")))
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
alpha.r = c(1,1)

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
        lambda10[i,j] <- a1*(Ti[i,j])^(a1-1)
        lambda20[i,j] <- a2*(Ti[i,j])^(a2-1)
        lambda1[i,j] <- lambda10[i,j]*v1[i]*exp(b10+b[1]*X1[i])
        lambda2[i,j] <- lambda20[i,j]*v2[i]*exp(b20+b[2]*X1[i])
       }
        v1[i] ~ dgamma(1/ph1,1/ph1) ## include ph with priors ~ gamma 
        v2[i] ~ dgamma(1/ph2,1/ph2) ## include ph with priors ~ gamma 
        logL.e1[i] <- ifelse(Ti[i,1] != 0,
                     sum(log(lambda1[i,1:k.pe[i]])) + v1[i]*exp(b10 + b[1]*X1[i]) * (time.t0[i]^a1 - time.tau[i]^a1),
                     v1[i]*exp(b10 + b[1]*X1[i]) * (time.t0[i]^a1 - time.tau[i]^a1))
        L.e1[i] <- exp(logL.e1[i])
        logL.e2[i] <- ifelse(Ti[i,1] != 0,
                     sum(log(lambda2[i,1:k.pe[i]])) + v2[i]*exp(b20 + b[2]*X1[i]) * (time.t0[i]^a2 - time.tau[i]^a2),
                     v2[i]*exp(b20 + b[2]*X1[i]) * (time.t0[i]^a2 - time.tau[i]^a2))
        L.e2[i] <- exp(logL.e2[i])
        L.e[i] <- max(pi.r[1] * L.e1[i] + pi.r[2] * L.e2[i], 1e-300)
        ll.e[i] <- log(L.e[i])
        phi[i] <- max(-log(L.e[i]) + 1000, 0)
        zeros[i] ~ dpois(phi[i])
  }
  log_lik0.e <- sum(ll.e[]) 
  dev.e <- -2*log_lik0.e
  pi.r[1:2] ~ ddirch(alpha.r[])
  ## prior distributions
	a1 ~ dgamma(0.01,0.01)
	a2 ~ dgamma(0.01,0.01)
  b10 ~ dnorm(0,0.0001)	
  b20 ~ dnorm(0,0.0001)	
  for (p in 1:2){
	     b[p] ~ dnorm(0,0.0001)		
  }
  ph1~ dunif(0.01, 10)
	ph2~ dunif(0.01, 10)
}"


####Observed DATA
data <- dump.format(list(N=N, X1=X1,k.pe=k.pe, time.t0=time.t0, time.tau=time.tau, Ti=Ti, alpha.r=alpha.r)) 
###initial Values
inits1 <- dump.format(list(b10=-2.5,b20=0.5,b=c(0.8,2), a1=1, a2=1, ph1=1,ph2=0.5,pi.r=c(0.5,0.5),
                           .RNG.name="base::Super-Duper", .RNG.seed=1))
inits2 <- dump.format(list(b10=-2.6,b20=0.6,b=c(0.9,2.1), a1=1.1, a2=1.1, ph1=1.1,ph2=0.6,pi.r=c(0.51,0.49),
                           .RNG.name="base::Super-Duper", .RNG.seed=2))
#### Run the model and produce plots
res <- run.jags(model=modelrancp, burnin=10000, sample=3000,  
                monitor=c("b10","b20","b", "a1","a2","ph1","ph2","pi.r","v1","v2"), 
                data=data, n.chains=2, method = "parallel", inits=c(inits1,inits2), thin=20)

summary <- summary(res)
result_df <- as.data.frame(summary)
text <- list.files(pattern="mixRM.rec_data.")
num <- unlist(lapply(strsplit(text,'.',fixed=TRUE),function(x) x[[4]]))
write.csv(result_df, paste0("mixRM.result.",num,".csv"))

res_jm <- res$mcmc
vars<-mcmc.list(res_jm[[1]][,c(1:16)],res_jm[[2]][,c(1:16)])
pdf(file = paste0("mixRM.traceplot.",num,".pdf"),   # The directory you want to save the file in
    width = 4, # The width of the plot in inches
    height = 4) # The height of the plot in inches
traplot(vars)
dev.off()
