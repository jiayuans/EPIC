library(tidyverse)

dirg <- "C:/UCHealth/RA/Project/EPIC-CF/Analysis_Jiayuan/EPIC/"
setwd(dirg)
##################################################################
##    Functions to Read data
## 
##################################################################

long.time <- read.csv("long.time.csv")
first.tt <- long.time[,2]
last.tt <- long.time[,3]

####time of first visit and last visit#######
N<-length(last.tt)
###set number of iterations#################################
I=21

#s=23###starting seed####
#############################################################

#participant ID
ID<-rep(1:N)
length(ID)
set.seed(123)

#######################################################
for (r in 2:I){
  ##r <- 2
  ##set.seed(s+100*(r-1))
  t<-round(first.tt)
  tt<-round(last.tt)
  X1=c(rep(1,N/2),rep(0,N/2))
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
    #c_0i <- rnorm(le,0,1.3)
    ##vi <- exp(ga*b_0i+c_0i)
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
  
  Tei0 <- tt-0.25-t
  
  simdat.pe <- poisson.d(alpha=1.73,beta=0.26,beta0=-1.34,x=X1,ph=.5,TTei=Tei0)
  #simdat.pe <- poisson.d(alpha=1.26,beta=0.23,beta0=0.39,x=X1,ph=.5,TTei=Tei0)
  
  simdat.pe_df <- as.data.frame(simdat.pe)
  filename <- paste0("rec.sim.pe_data.", r-2, ".csv")
  write.csv(simdat.pe_df, file = filename, row.names = FALSE)
} 
 