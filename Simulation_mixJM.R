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
k.pa<-(tt-t)*4

alpha = c(1,1)

set.seed(123)

#############################################################
X <- as.matrix(read.csv(list.files(pattern="mixJM.X_data.")))
Y <- as.matrix(read.csv(list.files(pattern="mixJM.Y_data.")))
simdat.pe00 <- as.data.frame(read.csv(list.files(pattern="mixJM.rec_data.")))
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
        p2[i,j] <- p[i,j,z[i]]
        logit(p[i,j,1]) <- c10 + c[1] * (X[i,j]-cp1[i]) + c[2] * (X[i,j]-cp1[i]) * (2*step(X[i,j]-cp1[i])-1) + c[3] * X1[i] + u1[i] ## increasing
        logit(p[i,j,2]) <- c20 + (c[1]-c[2]) * X[i,j] + c[3] * X1[i] + u2[i] ## no cp
        }
        for(j in 1:k.pe[i]){
  ### PE model
       ## Weibull baseline
        lambda0[i,j] <- a*(Ti[i,j])^(a-1)
        lambda1[i,j] <- lambda0[i,j]*v1[i]*exp(b10+b[1]*X1[i])
        lambda2[i,j] <- lambda0[i,j]*v2[i]*exp(b20+b[2]*X1[i])
       }
        z[i]~dcat(pi[1:2])
        u1[i] ~ dnorm(0,u.tau1)
        u2[i] ~ dnorm(0,u.tau2)
        cp1[i] ~ dnorm(cp1.mu,cp1.tau)
        L.a[i] <- prod(((p2[i,1:k.pa[i]])^(Y[i,1:k.pa[i]]))*((1-p2[i,1:k.pa[i]])^(1-Y[i,1:k.pa[i]])))
        ll.a[i] <- log(L.a[i])
        w1[i] ~ dnorm(0,w.tau1)
        w2[i] ~ dnorm(0,w.tau2)
        v1[i] <- exp(ga10*u1[i]+w1[i]+ga11*cp1[i])
        v2[i] <- exp(ga20*u2[i]+w2[i])
        #L.e1[i] <- ifelse(Ti[i,1]!=0, prod(lambda1[i,1:k.pe[i]]) * exp(v1[i]*exp(b10+b[1]*X1[i])*(time.t0[i]^a-time.tau[i]^a)), exp(v1[i]*exp(b10+b[1]*X1[i])*(time.t0[i]^a-time.tau[i]^a)))
        #L.e2[i] <- ifelse(Ti[i,1]!=0, prod(lambda2[i,1:k.pe[i]]) * exp(v2[i]*exp(b20+b[2]*X1[i])*(time.t0[i]^a-time.tau[i]^a)), exp(v2[i]*exp(b20+b[2]*X1[i])*(time.t0[i]^a-time.tau[i]^a)))
        
        logL1[i] <- ifelse(Ti[i,1]!=0, 
                     sum(log(lambda1[i,1:k.pe[i]])) + v1[i]*exp(b10+b[1]*X1[i])*(time.t0[i]^a-time.tau[i]^a), 
                     v1[i]*exp(b10+b[1]*X1[i])*(time.t0[i]^a-time.tau[i]^a))
        logL2[i] <- ifelse(Ti[i,1]!=0, 
                     sum(log(lambda2[i,1:k.pe[i]])) + v2[i]*exp(b20+b[2]*X1[i])*(time.t0[i]^a-time.tau[i]^a), 
                     v2[i]*exp(b20+b[2]*X1[i])*(time.t0[i]^a-time.tau[i]^a))
        
        maxlogL[i] <- max(logL1[i], logL2[i])
        
        ll.e[i] <- maxlogL[i] + log(
          pi.r[1] * exp(logL1[i] - maxlogL[i]) +
          pi.r[2] * exp(logL2[i] - maxlogL[i])
        )
        
        phi[i] <- -ll.e[i] + 1000
        zeros[i] ~ dpois(phi[i])
        
        prob_class[i,1] <- pi.r[1] * exp(logL1[i] - ll.e[i])
        prob_class[i,2] <- pi.r[2] * exp(logL2[i] - ll.e[i])
        z.r[i] ~ dcat(prob_class[i,1:2])
  }
  log_lik0.a <- sum(ll.a[]) 
  log_lik0.e <- sum(ll.e[]) 
  dev.a <- -2*log_lik0.a
  dev.e <- -2*log_lik0.e
  pi[1:2] ~ ddirch(alpha[])
  pi.r[1:2] ~ ddirch(alpha.r[])
  c10 ~ dnorm(0,0.0001)
  c20 ~ dnorm(0,0.0001)
	for (k in 1:3){
	      c[k] ~ dnorm(0,0.0001)	
	}
	B1<-c[1]-c[2]
  B2<-c[1]+c[2]
  u.tau1 ~ dgamma(0.001,0.001)
	u.tau.inv1 <- 1/u.tau1  ## variance
	u.tau2 ~ dgamma(0.001,0.001)
	u.tau.inv2 <- 1/u.tau2  ## variance
  cp1.mu ~ dnorm(0,0.01)
	cp1.tau ~ dgamma(0.01,0.01)
	cp1.tau.inv <- 1/cp1.tau  ## variance
	a ~ dgamma(0.01,0.01)
  b10 ~ dnorm(0,0.0001)	
  b20 ~ dnorm(0,0.0001)	
  for (p in 1:2){
	     b[p] ~ dnorm(0,0.0001)		
  }
	ga10 ~ dnorm(0,0.0001)
	ga20 ~ dnorm(0,0.0001)
	ga11 ~ dnorm(0,0.0001)
	w.tau1 ~ dgamma(0.001,0.001)
	w.tau.inv1 <- 1/w.tau1  ## variance 
	w.tau2 ~ dgamma(0.001,0.001)
	w.tau.inv2 <- 1/w.tau2 ## variance 
}"


####Observed DATA
data <- dump.format(list(N=N, X=X, Y=Y, X1=X1,k.pa=k.pa,k.pe=k.pe, time.t0=time.t0, time.tau=time.tau, Ti=Ti,alpha=alpha, alpha.r=alpha.r)) 
###initial Values
inits1 <- dump.format(list(c10=-3.3, c20=-2.6, c=c(0.3,0.3,-0.05), pi=c(0.55,0.45), pi.r=c(0.9,0.1), u.tau1=0.64,u.tau2=1, cp1.mu=14, cp1.tau=1,
                           b10=-4, b20=-3, b=c(0.2,0.3), a=1.8, w.tau1=0.04, w.tau2=0.04, 
                           .RNG.name="base::Super-Duper", .RNG.seed=1)) 
inits2 <- dump.format(list(c10=-3.2, c20=-2.5, c=c(00.3,0.3,-0.05)+0.01, pi=c(0.56,0.44), pi.r=c(0.89,0.11), u.tau1=0.65,u.tau2=1, cp1.mu=14.1, cp1.tau=1,
                           b10=-3.9, b20=-2.9, b=c(0.2,0.3)+0.1, a=1.8, w.tau1=0.04, w.tau2=0.04, 
                           .RNG.name="base::Super-Duper", .RNG.seed=2))

#### Run the model and produce plots
res <- run.jags(model=modelrancp, burnin=10000, sample=5000,  
                monitor=c("B1","B2","c10", "c20","c", "cp1",
                          "pi","pi.r","z","z.r","u1","u2", "u.tau.inv1","u.tau.inv2", "u.tau1","u.tau2",
                          "cp1.mu","cp1.tau.inv","cp1.tau",
                          "b10","b20","b", "a","v1","v2","ga10","ga20","ga11","w1","w2","w.tau1","w.tau2","w.tau.inv1","w.tau.inv2",
                          "prob_class","ll.a","ll.e","dev.a","dev.e"), 
                data=data, n.chains=2, method = "parallel", inits=c(inits1,inits2), thin=15)

summary <- summary(res)
summary
result_df <- as.data.frame(summary)
text <- list.files(pattern="mixJM.X_data.")
num <- unlist(lapply(strsplit(text,'.',fixed=TRUE),function(x) x[[3]]))
write.csv(result_df, paste0("mixJM.result.",num,".csv"))
save(res, file=paste0("mixJM.res.",num,".RData"))

res_jm <- res$mcmc
vars<-mcmc.list(res_jm[[1]][,c(1:16)],res_jm[[2]][,c(1:16)])
pdf(file = paste0("mixJM.traceplot.",num,".pdf"),   # The directory you want to save the file in
    width = 4, # The width of the plot in inches
    height = 4) # The height of the plot in inches
traplot(vars)
dev.off()
