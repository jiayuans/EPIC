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
I=31

###############set true values#########################################
c0=-4.44 
c1=0.10 
c2=0.13 
c3=0.08
c4=0.08
Verror=1
cp1.true=4.6
cp2.true=14.4

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
  k.pa<-(tt-t)*4
  kk=max(k.pa)
  
  b_0i<-rnorm(N,0,1) ##1.6
  X1=c(rep(1,N/2),rep(0,N/2))
  ##X1=sample(c(1,0),N, replace = TRUE)
  
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
  
  X_df <- as.data.frame(X)
  filename <- paste0("long.X_data.", r-2, ".csv")
  write.csv(X_df, file = filename, row.names = FALSE)
  
  Y_df <- as.data.frame(Y)
  filename <- paste0("long.Y_data.", r-2, ".csv")
  write.csv(Y_df, file = filename, row.names = FALSE)

} 
 