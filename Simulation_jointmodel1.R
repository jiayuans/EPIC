## simulation_jointmodel1
library(mvtnorm)
library(rjags)
library(R2jags)
library(runjags)
library(tidyverse)

dirg <- "Z:/EJCStudents/ShiJ/EPIC-CF/Data/Source/"
##################################################################
##    Functions to calculate the WAIC and Read data (different priors)
## 
##################################################################
dat.pa <- read.csv(file=paste(dirg,"/Data-PA-cohort.csv",sep=""))
dat.pa <- dat.pa[,-1]
dat.pe <- read.csv(file=paste(dirg,"/Data-multiple-final-cohort-EPICstart-Bt-LengthPEx.csv",sep=""))

dat.pa1 <-aggregate(dat.pa$VisitAge, by=list(dat.pa$cffidno),
                 FUN=max, na.rm=TRUE)
names(dat.pa1) <- c('cffidno','age.max')

dat.pa2 <-aggregate(dat.pa$age.min, by=list(dat.pa$cffidno),
                 FUN=max, na.rm=TRUE)
names(dat.pa2) <- c('cffidno','age.min')

first.t<-dat.pa2$age.min
last.t<-dat.pa1$age.max
first.tt<-first.t[c(101:300,1001:1200)]
last.tt<-last.t[c(101:300,1001:1200)]

####time of first visit and last visit#######
N<-length(last.tt)
###set number of iterations#################################
I=2

###############set true values#########################################
c0=-4.4
c1=0.1
c2=0.1
c3=0.1
c4=0.1
Verror=1
cp1.true=4.5
cp2.true=14.4

s=23###starting seed####
#############################################################

B1.mean<-rep(NA,I-1)
B2.mean<-rep(NA,I-1)
B3.mean<-rep(NA,I-1)
c0.mean<-rep(NA,I-1)
c1.mean<-rep(NA,I-1)
c2.mean<-rep(NA,I-1)
c3.mean<-rep(NA,I-1)
cp1.mean<-rep(NA,I-1)
cp2.mean<-rep(NA,I-1)
u.mean<-rep(NA,I-1)
u.tau.inv.mean<-rep(NA,I-1)
cp1.mu<-rep(NA,I-1)
cp1.tau<-rep(NA,I-1)
cp2.temp<-rep(NA,I-1)

b0.mean<-rep(NA,I-1)
b1.mean<-rep(NA,I-1)
b2.mean<-rep(NA,I-1)
b3.mean<-rep(NA,I-1)
a.mean<-rep(NA,I-1)
v.mean<-rep(NA,I-1)
ga.mean<-rep(NA,I-1)
w.mean<-rep(NA,I-1)
w.tau.inv.mean<-rep(NA,I-1)

#participant ID
ID<-rep(1:N)
length(ID)

#######################################################
for (r in 2:I){
  ##r <- 2
  set.seed(s+100*(r-1))
  t<-round(first.tt)
  tt<-round(last.tt)
  k.pa<-(tt-t)*4
  kk=max(k.pa)
  
  b_0i<-rnorm(N,0,1.6)
  ##X1=c(rep(1,N/2),rep(0,N/2))
  X1=sample(c(1,0),N, replace = TRUE)
  
  I1<-matrix(NA, nrow=N, ncol=kk, byrow=TRUE)
  I2<-matrix(NA, nrow=N, ncol=kk, byrow=TRUE)
  p2<-matrix(NA, nrow=N, ncol=kk, byrow=TRUE)
  Y<-matrix(NA, nrow=N, ncol=kk, byrow=TRUE)
  X<-matrix(NA, nrow=N, ncol=kk, byrow=TRUE)
  for (i in 1:N){
    X[i,1:k.pa[i]]<-c(seq(t[i],tt[i]-0.25,0.25))
  }
  
  for (i in 1:N){
    for (j in 1:k.pa[i]){
      I1[i,j]<-ifelse(X[i,j]< cp1.true,-1,1)
      I2[i,j]<-ifelse(X[i,j]< cp2.true,-1,1)
      p2[i,j]=exp(c0+c1*(X[i,j]-cp1.true)+c2*(X[i,j]-cp1.true)*I1[i,j]+c3*(X[i,j]-cp2.true)*I2[i,j]+c4*X1[i]+b_0i[i])/(1+exp(c0+c1*(X[i,j]-cp1.true)+c2*(X[i,j]-cp1.true)*I1[i,j]+c3*(X[i,j]-cp2.true)*I2[i,j]+c4*X1[i]+b_0i[i]))
      Y[i,j]=rbinom(Verror, 1, p2[i,j])
    }
  }
  
  
  #########################################################################
  # Function that generates observations from a NHPP- returns event times
  # Input: parameters for the mean of a poisson process: a(shape parameter),b, T (exposure time)
  # Output: REturns the event times and a variable that indicates whether the observation is an event or a 
  #		  censoring time (no events observed in the whole interval); status=1 indicates event and 0 censoring
  
  NHPP<-function(a,b,T){
    mu <- b*T^a  # Mean of the Poisson process up to time T
    n <-rpois(1, mu)  #  number of events poisson
    if (n!=0) {
      u <- runif(n,0,1) # n uniforms
      u <- sort(u)
      y <- T*u^(1/a) 
      y[length(y)+1] <- T
      y_0 <- rep(NA,length(y))
      for (i in 2:length(y_0)){
        y_0[i] <- y[i-1]
      }
      y_0[which(is.na(y_0)==TRUE)] <- 0
      return(cbind(y_0,y,c(rep(1,length(y)-1),0),n))    #returns n event times
    } else 
      return(cbind(0,T,0,n)) 
  }
  
  #########################################################################
  # Function that creates an event times dataset for a poisson process (continuous data )
  # Input: parameters for the intensity function alpha; beta; beta0; x; ga (association parameter); Tei 
  # Output: A dataset with variables
  # 		 id, xi (treatment),Tei, time, status
  # -------------- Building the simulated poisson data -----
  poisson.d <- function(alpha,beta,beta0,x,ga,TTei){
    le <- length(x)
    c_0i <- rnorm(le,0,1.4)
    vi <- exp(ga*b_0i+c_0i)
    ##vi <- ifelse(rep(ph,le)==rep(0,le),rep(1,le),rgamma(le,shape=1/ph, scale=ph))
    
    times <- NHPP(b=vi[1]*exp(beta*x[1])*exp(beta0),a=alpha,T=TTei[1])
    start <-  times[,1]
    stop <- times[,2]
    status <- times[,3]
    n.rec <- times[,4]
    id <- rep(1,length(stop))
    xi <- rep(x[1],length(stop))
    Tei <- rep(TTei[1],length(stop))
    for (i in 2:length(x)){
      times2 <- NHPP(b=vi[i]*exp(beta0+beta*x[i]),a=alpha,T=TTei[i]) 
      start2 <-  times2[,1]
      stop2 <- times2[,2]
      status2 <- times2[,3]
      n.rec2 <- times2[,4]
      id <- c(id,rep(i,length(stop2)))
      xi <- c(xi,rep(x[i],length(stop2)))
      Tei <- c(Tei,rep(TTei[i],length(stop2)))
      
      start <- c(start,start2)
      stop <- c(stop,stop2)
      status <- c(status,status2)
      n.rec <- c(n.rec,n.rec2)
    }
    return(data.frame(id,xi,Tei,n.rec,start,stop,status))
  }
  
  Tei0 <- rep(20,N)

  simdat.pe <- poisson.d(alpha=1.3,beta=0.2,beta0=-2.5,x=X1,ga=.5,TTei=Tei0)

  timeE <-aggregate(simdat.pe$stop, by=list(simdat.pe$id),
                    FUN=max, na.rm=TRUE)
  colnames(timeE) <- c("id","tau")
  
  X.dat.pe <- simdat.pe[!duplicated(simdat.pe$id), ]
  
  time <- subset(simdat.pe,status==1)
  time$t <- time$stop
  time1 <- time[,c(1,8)]  
  simdat.pe2 <- merge(timeE,time1,all=TRUE)
  simdat.pe2$t[which(is.na(simdat.pe2$t))] <- 0
  
  length(which(simdat.pe2$t== 0))
  
  count <- simdat.pe2 %>% count(id)
  max.count <- max(count$n) 
  
  ##########################Assigning unique number to each subject##########################
  simdat.pe3 <- simdat.pe2 %>% group_by(id) %>% mutate(time = c(1:length(id)))
  Yd.temp <- data.frame(id = rep(unique(simdat.pe$id),each=max.count), time = 1:max.count) 
  Y.epic <- merge(simdat.pe3,Yd.temp,by=c('id','time'),all.y=TRUE)
  
  #################Readingin data for time matrix#############################
  Ti <- matrix(Y.epic$t, N, max.count, byrow=TRUE)
  
  #################Readingin data for X, t0, tau vectors#############################
  ##X1 <- as.numeric(X.dat.pe[,2]) ## sexf: female
  
  time.tau <- timeE$tau
  
  
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
        logit(p2[i,j]) <- c0 + c[1] * (X[i,j]-cp1) + c[2] * (X[i,j]-cp1) * (2*step(X[i,j]-cp1)-1) + c[3] * (X[i,j]-cp2) * (2*step(X[i,j]-cp2)-1) + c[4] * X1[i] + u[i]
        }
        for(j in 1:k.pe[i]){
  ### PE model
       ## Weibull baseline
        lambda0[i,j] <- a*(Ti[i,j])^(a-1)
        lambda[i,j] <- lambda0[i,j]*v[i]*exp(b0+b*X1[i])
       }
        u[i] ~ dnorm(0,u.tau)
        L.a[i] <- prod(((p2[i,1:k.pa[i]])^(Y[i,1:k.pa[i]]))*((1-p2[i,1:k.pa[i]])^(1-Y[i,1:k.pa[i]])))
        ll.a[i] <- log(L.a[i])
        w[i] ~ dnorm(0,w.tau)
        v[i] <- exp(ga*u[i]+w[i])
        L.e[i] <- ifelse(Ti[i,1]!=0, prod(lambda[i,1:k.pe[i]]) * exp(-v[i]*exp(b0+b*X1[i])*(time.tau[i]^a)), exp(-v[i]*exp(b0+b*X1[i])*(time.tau[i]^a)))
        ll.e[i] <- log(L.e[i])
        phi[i] <- -log(L.e[i]) + 1000
        zeros[i] ~ dpois(phi[i])
  }
  log_lik0.a <- sum(ll.a[])
  log_lik0.e <- sum(ll.e[]) 
  dev.a <- -2*log_lik0.a
  dev.e <- -2*log_lik0.e
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
  a ~ dgamma(0.01,0.01)
  b0 ~ dnorm(0,0.0001)	
  b ~ dnorm(0,0.0001)		
	ga ~ dnorm(0,0.0001)
	w.tau ~ dgamma(0.001,0.001)
	w.tau.inv <- 1/w.tau  ## variance 
}"

  
  ####Observed DATA X,
  data <- dump.format(list(X=X, Y=Y, N=N, k.pa=k.pa, max=max(tt),
                           X1=X1, k.pe=k.pe, time.tau=time.tau, Ti=Ti)) 
  ###initial Values
  inits1 <- dump.format(list(c0=-4.4, c=c(0.1,0.1,0.1,0.1), u.tau=0.5, cp1=4.5, cp2.temp=10,
                             b0=-2.5, b=0.2, a=1.3, w.tau=0.5, ga=0.5,
                             .RNG.name="base::Super-Duper", .RNG.seed=1))
  inits2 <- dump.format(list(c0=-4.5, c=c(0.1,0.1,0.1,0.1)+0.01, u.tau=0.6, cp1=4.6, cp2.temp=10.1,
                                  b0=-2.6,b=0.3, a=1.4, w.tau=0.6, ga=0.5,
                             .RNG.name="base::Super-Duper", .RNG.seed=2))

  #### Run the model and produce plots
  res <- run.jags(model=modelrancp, burnin=10000, sample=14000, 
                   monitor=c("B1", "B2","B3","c0", "c", "cp1", "cp2","u","u.tau.inv","u.tau","cp1.mu","cp1.tau", "cp2.temp",
                             "b0","b", "a","v","ga","w","w.tau","w.tau.inv","ll.a","ll.e","dev.a","dev.e","dic"), 
                   data=data, n.chains=2, inits=c(inits1,inits2), thin=10, module='dic')
  
  summary <- summary(res)
  ##sum <- as.data.frame(summary)
  
  B1.mean[r]<-summary[1,4] 
  B2.mean[r]<-summary[2,4] 
  B3.mean[r]<-summary[3,4] 
  c0.mean[r]<-summary[4,4] 
  c1.mean[r]<-summary[5,4] 
  c2.mean[r]<-summary[6,4] 
  c3.mean[r]<-summary[7,4]
  cp1.mean[r]<-summary[8,4] 
  cp2.mean[r]<-summary[9,4] 
  u.mean[r]<-mean(summary[10:209,4])
  u.tau.inv.mean[r]<-summary[210,4] 
  cp1.mu[r]<-summary[212,4]
  cp1.tau[r]<-summary[213,4]
  cp2.temp[r]<-summary[214,4]
  
  b0.mean[r]<-summary[215,4] 
  b1.mean[r]<-summary[216,4] 
  b2.mean[r]<-summary[217,4] 
  b3.mean[r]<-summary[218,4]
  a.mean[r]<-summary[219,4] 
  v.mean[r]<-mean(summary[220:419,4])
  ga.mean[r]<-summary[420,4] 
  w.mean[r]<-mean(summary[421:620,4])
  w.tau.inv.mean[r]<-summary[622,4] 
  
}

Sim.results=cbind(B1.mean,B2.mean,B3.mean,c0.mean,c1.mean,c2.mean,c3.mean,cp1.mean,cp2.mean,u.tau.inv.mean,u.mean,
                  b0.mean,b1.mean,b2.mean,b3.mean,a.mean,v.mean,ga.mean,w.mean,w.tau.inv.mean)
Sim.results=Sim.results[-1,]
write.csv(summary,"Z:/EJCStudents/ShiJ/EPIC-CF/Result/Simulation_JM1_summary_rhat.csv")
write.csv(Sim.results,"Z:/EJCStudents/ShiJ/EPIC-CF/Result/Simulation_JM1.csv")
Sim.results <- read.csv("Z:/EJCStudents/ShiJ/EPIC-CF/Result/Simulation_JM1.csv")

round(colMeans(Sim.results),2)
# B1.mean  B2.mean  B3.mean  c0.mean  c1.mean  c2.mean  c3.mean cp1.mean cp2.mean tau.mean   u.mean 
#-0.57    -0.04     0.29    -3.47    -0.14     0.27     0.16     4.11    14.06    38.56     0.00003