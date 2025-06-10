library(tidyverse)

dirg <- "/Users/Shared/Windows/UCHealth/RA/Project/EPIC-CF/Analysis_Jiayuan/EPIC_Simulation/"
#dirg <- "C:/UCHealth/RA/Project/EPIC-CF/Analysis_Jiayuan/EPIC/"
setwd(dirg)
##################################################################
##    Functions to Read data
## 
##################################################################

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

###set number of iterations#################################
I=201

#############################################################
set.seed(123)

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
poisson.d <- function(alpha,beta,beta0,x,ph,TTei){
  le <- length(x)
  vi <- ifelse(rep(ph,le)==rep(0,le),rep(1,le),rgamma(le,shape=1/ph, scale=ph))
  
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

p1r <- 0.5  # for example

#######################################################
for (r in 2:I){
  X1=c(rep(1,N/2),rep(0,N/2))
  simdat.pe1 <- poisson.d(alpha=1,beta=0.8,beta0=-2.5,x=X1,ph=1,TTei=tt-0.25)
  simdat.pe2 <- poisson.d(alpha=1,beta=2,beta0=0.5,x=X1,ph=.5,TTei=tt-0.25)  
  
  model_source <- rbinom(N, 1, p1r)  # 1 = from pe1, 0 = from pe2
  subject_ids_pe1 <- unique(simdat.pe1$id)
  subject_ids_pe2 <- unique(simdat.pe2$id)
  
  simdat.pe <- data.frame()
  
  for (i in 1:N) {
    if (model_source[i] == 1) {
      dat_i <- subset(simdat.pe1, id == subject_ids_pe1[i])
    } else {
      dat_i <- subset(simdat.pe2, id == subject_ids_pe2[i])
    }
    dat_i$id <- i  # Reset id to keep it consistent
    simdat.pe <- rbind(simdat.pe, dat_i)
  }
  
  simdat.pe_df <- as.data.frame(simdat.pe)
  filename <- paste0("mixRM.rec_data.", r-2, ".csv")
  write.csv(simdat.pe_df, file = filename, row.names = FALSE)
} 

