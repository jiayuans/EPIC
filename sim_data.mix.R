library(tidyverse)

dirg <- "C:/UCHealth/RA/Project/EPIC-CF/Analysis_Jiayuan/EPIC/"
setwd(dirg)
##################################################################
##    Functions to Read data
## 
##################################################################

N<-400
#participant ID
id<-rep(1:N)
length(id)

t<-rep(0,400)
tt<-rep(22,400)
k.pa<-(tt-t)*4
kk=max(k.pa)

###set number of iterations#################################
I=201

###############set true values#########################################
c10=-3
c20=-3
c11=0.3
c12=0.2 
c21=0.3
c22=-0.2
Verror=1
cp11.true=15
cp21.true=15
pi <- c(0.7, 0.3)

#############################################################
set.seed(123)

#######################################################
for (r in 2:I){
  b_10i<-rnorm(N,0,1) 
  b_20i<-rnorm(N,0,1)
  cp_11i<-rnorm(N,cp11.true,1) 
  cp_21i<-rnorm(N,cp11.true,1)
  z <- sample(1:2, size = N, prob = pi, replace = TRUE)
  
  I1<-matrix(NA, nrow=N, ncol=kk, byrow=TRUE)
  I2<-matrix(NA, nrow=N, ncol=kk, byrow=TRUE)
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
      I1[i,j]<-ifelse(X[i,j]< cp_11i[i],-1,1)
      I2[i,j]<-ifelse(X[i,j]< cp_21i[i],-1,1)
      p1[i,j]=exp(c10+c11*(X[i,j]-cp_11i[i])+c12*(X[i,j]-cp_11i[i])*I1[i,j]+b_10i[i])/(1+exp(c10+c11*(X[i,j]-cp_11i[i])+c12*(X[i,j]-cp_11i[i])*I1[i,j]+b_10i[i]))
      p2[i,j]=exp(c20+c21*(X[i,j]-cp_21i[i])+c22*(X[i,j]-cp_21i[i])*I2[i,j]+b_20i[i])/(1+exp(c20+c21*(X[i,j]-cp_21i[i])+c22*(X[i,j]-cp_21i[i])*I2[i,j]+b_20i[i]))
      p[i,j] <- if (z[i] == 1) p1[i,j] else  p2[i,j]
      Y[i,j]=rbinom(Verror, 1, p[i,j])
    }
  }

  X_df <- as.data.frame(X)
  filename <- paste0("mix.X_data.", r-2, ".csv")
  write.csv(X_df, file = filename, row.names = FALSE)
  
  Y_df <- as.data.frame(Y)
  filename <- paste0("mix.Y_data.", r-2, ".csv")
  write.csv(Y_df, file = filename, row.names = FALSE)
} 
