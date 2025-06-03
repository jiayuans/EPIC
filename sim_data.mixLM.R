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
k.pa<-(tt-t)*4
kk=max(k.pa)

###set number of iterations#################################
I=201

###############set true values#########################################
c10=-3
c20=-3
c1=0.3
c2=0.4 
Verror=1
cp.true=13
pi <- c(0.5, 0.5)

#############################################################
set.seed(123)

#######################################################
for (r in 2:I){
  b_10i<-rnorm(N,0,1) 
  b_20i<-rnorm(N,0,1)
  cp_i<-rnorm(N,cp.true,1) 
  z <- sample(1:2, size = N, prob = pi, replace = TRUE)
  
  I<-matrix(NA, nrow=N, ncol=kk, byrow=TRUE)
  p1<-matrix(NA, nrow=N, ncol=kk, byrow=TRUE)
  p2<-matrix(NA, nrow=N, ncol=kk, byrow=TRUE)
  p<-matrix(NA, nrow=N, ncol=kk, byrow=TRUE)
  Y<-matrix(NA, nrow=N, ncol=kk, byrow=TRUE)
  X<-matrix(NA, nrow=N, ncol=kk, byrow=TRUE)
  for (i in 1:N){
    X[i,1:k.pa[i]]<-c(seq(t[i],tt[i]-0.25,0.25))
  }
  
  for (i in 1:N){
    for (j in 1:k.pa[i]){
      I[i,j]<-ifelse(X[i,j]< cp_i[i],-1,1)
      p1[i,j]=exp(c10+c1*(X[i,j]-cp_i[i])+c2*(X[i,j]-cp_i[i])*I[i,j]+b_10i[i])/(1+exp(c10+c1*(X[i,j]-cp_i[i])+c2*(X[i,j]-cp_i[i])*I[i,j]+b_10i[i]))
      p2[i,j]=exp(c20+(c1-c2)*X[i,j]+b_20i[i])/(1+exp(c20+(c1-c2)*X[i,j]+b_20i[i]))
      p[i,j] <- if (z[i] == 1) p1[i,j] else  p2[i,j]
      Y[i,j]=rbinom(Verror, 1, p[i,j])
    }
  }

  X_df <- as.data.frame(X)
  filename <- paste0("mixLM.X_data.", r-2, ".csv")
  write.csv(X_df, file = filename, row.names = FALSE)
  
  Y_df <- as.data.frame(Y)
  filename <- paste0("mixLM.Y_data.", r-2, ".csv")
  write.csv(Y_df, file = filename, row.names = FALSE)
} 
