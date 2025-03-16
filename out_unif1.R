#setwd("C:/UCHealth/RA/Project/EPIC-CF/Analysis_Jiayuan/Result/Simulation_output")
setwd("C:/Users/jiayu/OneDrive/Desktop/Fixed_CP_unif11")
#setwd("C:/Users/jiayu/OneDrive/Desktop/Output_test")

###########################################################################
# Read csv files
text <- list.files(pattern="result1.")
text1 <- unlist(lapply(strsplit(text,'.',fixed=TRUE),function(x) x[[1]]))
ind <- which(text1=="result1")
num <- as.numeric(unlist(lapply(strsplit(text[ind],'.',fixed=TRUE),function(x) x[[2]])))

data_frames <- lapply(num, function(i) {
  file_name <- paste0("result1.", i, ".csv") 
  read.csv(file_name)
})

I=length(data_frames)

Flag<-rep(NA,I)
B1.mean<-rep(NA,I)
B2.mean<-rep(NA,I)
B3.mean<-rep(NA,I)
cp1.mean<-rep(NA,I)
cp2.mean<-rep(NA,I)
c0.mean<-rep(NA,I)
c1.mean<-rep(NA,I)
c2.mean<-rep(NA,I)
c3.mean<-rep(NA,I)
c4.mean<-rep(NA,I)

u.tau.inv.mean<-rep(NA,I)
u.tau1.inv.mean<-rep(NA,I)
u.tau2.inv.mean<-rep(NA,I)
u.tau3.inv.mean<-rep(NA,I)
b0.mean<-rep(NA,I)
b1.mean<-rep(NA,I)
a.mean<-rep(NA,I)
ga.mean<-rep(NA,I)
ga1.mean<-rep(NA,I)
ga2.mean<-rep(NA,I)
ga3.mean<-rep(NA,I)
w.tau.inv.mean<-rep(NA,I)
u.mean<-rep(NA,I)
u1.mean<-rep(NA,I)
u2.mean<-rep(NA,I)
u3.mean<-rep(NA,I)
v.mean<-rep(NA,I)
w.mean<-rep(NA,I)

for(i in 1:I){ 
Flag[i] <- ifelse(max(data_frames[[i]][,12])<1.2,1,0)
B1.mean[i] <- data_frames[[i]][1,5] 
B2.mean[i] <- data_frames[[i]][2,5] 
B3.mean[i] <-data_frames[[i]][3,5] 
cp1.mean[i] <-data_frames[[i]][4,5] 
cp2.mean[i] <-data_frames[[i]][5,5] 
c0.mean[i] <-data_frames[[i]][6,5] 
c1.mean[i] <-data_frames[[i]][7,5] 
c2.mean[i] <-data_frames[[i]][8,5] 
c3.mean[i] <-data_frames[[i]][9,5]
c4.mean[i] <-data_frames[[i]][10,5]

u.tau.inv.mean[i] <-data_frames[[i]][11,5] 
u.tau1.inv.mean[i] <-data_frames[[i]][12,5] 
u.tau2.inv.mean[i] <-data_frames[[i]][13,5] 
u.tau3.inv.mean[i] <-data_frames[[i]][14,5] 
b0.mean[i] <-data_frames[[i]][15,5] 
b1.mean[i] <-data_frames[[i]][16,5] 
a.mean[i] <-data_frames[[i]][17,5] 
ga.mean[i] <-data_frames[[i]][18,5]
ga1.mean[i] <-data_frames[[i]][19,5]
ga2.mean[i] <-data_frames[[i]][20,5]
ga3.mean[i] <-data_frames[[i]][21,5]
w.tau.inv.mean[i] <-data_frames[[i]][22,5] 
u.mean[i] <-mean(data_frames[[i]][23:422,5])
u1.mean[i] <-mean(data_frames[[i]][423:822,5])
u2.mean[i] <-mean(data_frames[[i]][823:1222,5])
u3.mean[i] <-mean(data_frames[[i]][1223:1622,5])
v.mean[i] <-mean(data_frames[[i]][1623:2022,5])
w.mean[i] <-mean(data_frames[[i]][2023:2422,5])
}

Sim.results=cbind(Flag,B1.mean,B2.mean,B3.mean,cp1.mean,cp2.mean,c0.mean,c1.mean,c2.mean,c3.mean,c4.mean,u.tau.inv.mean,
                  u.tau1.inv.mean,u.tau2.inv.mean,u.tau3.inv.mean,
               b0.mean,b1.mean,a.mean,ga.mean,ga1.mean,ga2.mean,ga3.mean,w.tau.inv.mean,u.mean,u1.mean,u2.mean,u3.mean,v.mean,w.mean)
est<-round(colMeans(Sim.results),2)
est
#           Flag         B1.mean         B2.mean         B3.mean        cp1.mean        cp2.mean         c0.mean         c1.mean 
#0.15            0.06            0.01            0.30            6.65           14.74           -4.41            0.18 
#c2.mean         c3.mean         c4.mean  u.tau.inv.mean u.tau1.inv.mean u.tau2.inv.mean u.tau3.inv.mean         b0.mean 
#-0.03            0.14            0.04            0.82            0.09            0.01            0.00           -4.52 
#b1.mean          a.mean         ga.mean        ga1.mean        ga2.mean        ga3.mean  w.tau.inv.mean          u.mean 
#0.26            1.80            0.04            0.39            5.78           27.29            0.21            0.00 
#u1.mean         u2.mean         u3.mean          v.mean          w.mean 
#0.00            0.00            0.00            2.92            0.00 

table(Flag)
Sim.results.1 <- subset(Sim.results,Flag==1)
round(colMeans(Sim.results.1),2)

#           Flag         B1.mean         B2.mean         B3.mean        cp1.mean        cp2.mean         c0.mean         c1.mean 
#1.00            0.06            0.01            0.28            6.80           14.00           -4.19            0.17 
#c2.mean         c3.mean         c4.mean  u.tau.inv.mean u.tau1.inv.mean u.tau2.inv.mean u.tau3.inv.mean         b0.mean 
#-0.02            0.13            0.11            0.94            0.09            0.01            0.00           -4.59 
#b1.mean          a.mean         ga.mean        ga1.mean        ga2.mean        ga3.mean  w.tau.inv.mean          u.mean 
#0.29            1.82            0.14            0.38            5.10           31.33            0.20            0.00 
#u1.mean         u2.mean         u3.mean          v.mean          w.mean 
#0.00            0.00            0.00            2.91            0.00 

B1=rep(0.09,I)
B2=rep(0.05,I)
B3=rep(0.25,I)
c0=rep(-3.8,I)
c1=rep(0.17,I)
c2=rep(-0.02,I) 
c3=rep(0.1,I)
c4=rep(0.1,I)
cp1=rep(6.9,I)
cp2=rep(14.5,I)
u.sigma2=rep(1.96,I)
u1.sigma2=rep(0.09,I)
u2.sigma2=rep(0.01,I)
u3.sigma2=rep(0.0016,I)
alpha=rep(1.8,I)
beta=rep(0.25,I)
beta0=rep(-4.3,I)
ga=rep(0.11,I)
ga1=rep(0.16,I)
ga2=rep(8,I)
ga3=rep(25,I)
w.sigma2=rep(0.25,I)

dat <- as.data.frame(cbind(Sim.results,B1,B2,B3,cp1,cp2,c0,c1,c2,c3,c4,u.sigma2,u1.sigma2,u2.sigma2,u3.sigma2,beta0,beta,alpha,ga,ga1,ga2,ga3,w.sigma2))
bias <- c(sum(dat$B1.mean-dat$B1)/I,sum(dat$B2.mean-dat$B2)/I,sum(dat$B3.mean-dat$B3)/I,sum(dat$cp1.mean-dat$cp1)/I,sum(dat$cp2.mean-dat$cp2)/I,sum(dat$c0.mean-dat$c0)/I,
          sum(dat$c1.mean-dat$c1)/I,sum(dat$c2.mean-dat$c2)/I,sum(dat$c3.mean-dat$c3)/I,sum(dat$c4.mean-dat$c4)/I,
          sum(dat$u.tau.inv.mean-dat$u.sigma2)/I,sum(dat$u.tau1.inv.mean-dat$u1.sigma2)/I,sum(dat$u.tau2.inv.mean-dat$u2.sigma2)/I,sum(dat$u.tau3.inv.mean-dat$u3.sigma2)/I,
          sum(dat$b0.mean-dat$beta0)/I,sum(dat$b1.mean-dat$beta)/I,sum(dat$a.mean-dat$alpha)/I,
          sum(dat$ga.mean-dat$ga)/I,sum(dat$ga1.mean-dat$ga1)/I,sum(dat$ga2.mean-dat$ga2)/I,sum(dat$ga3.mean-dat$ga3)/I,
          sum(dat$w.tau.inv.mean-dat$w.sigma2)/I)
mse <- c(sum((dat$B1.mean-dat$B1)^2)/I,sum((dat$B2.mean-dat$B2)^2)/I,sum((dat$B3.mean-dat$B3)^2)/I,sum((dat$cp1.mean-dat$cp1)^2)/I,sum((dat$cp2.mean-dat$cp2)^2)/I,sum((dat$c0.mean-dat$c0)^2)/I,
         sum((dat$c1.mean-dat$c1)^2)/I,sum((dat$c2.mean-dat$c2)^2)/I,sum((dat$c3.mean-dat$c3)^2)/I,sum((dat$c4.mean-dat$c4)^2)/I,
         sum((dat$u.tau.inv.mean-dat$u.sigma2)^2)/I,sum((dat$u.tau1.inv.mean-dat$u1.sigma2)^2)/I,sum((dat$u.tau2.inv.mean-dat$u2.sigma2)^2)/I,sum((dat$u.tau3.inv.mean-dat$u3.sigma2)^2)/I,
         sum((dat$b0.mean-dat$beta0)^2)/I,sum((dat$b1.mean-dat$beta)^2)/I,sum((dat$a.mean-dat$alpha)^2)/I,
         sum((dat$ga.mean-dat$ga)^2)/I,sum((dat$ga1.mean-dat$ga1)^2)/I,sum((dat$ga2.mean-dat$ga2)^2)/I,sum((dat$ga3.mean-dat$ga3)^2)/I,
         sum((dat$w.tau.inv.mean-dat$w.sigma2)^2)/I)


B1.low<-rep(NA,I)
B2.low<-rep(NA,I)
B3.low<-rep(NA,I)
cp1.low<-rep(NA,I)
cp2.low<-rep(NA,I)
c0.low<-rep(NA,I)
c1.low<-rep(NA,I)
c2.low<-rep(NA,I)
c3.low<-rep(NA,I)
c4.low<-rep(NA,I)

u.tau.inv.low<-rep(NA,I)
u.tau1.inv.low<-rep(NA,I)
u.tau2.inv.low<-rep(NA,I)
u.tau3.inv.low<-rep(NA,I)
b0.low<-rep(NA,I)
b1.low<-rep(NA,I)
a.low<-rep(NA,I)
ga.low<-rep(NA,I)
ga1.low<-rep(NA,I)
ga2.low<-rep(NA,I)
ga3.low<-rep(NA,I)
w.tau.inv.low<-rep(NA,I)

for(i in 1:I){ 
  B1.low[i] <- data_frames[[i]][1,2] 
  B2.low[i] <- data_frames[[i]][2,2] 
  B3.low[i] <-data_frames[[i]][3,2] 
  cp1.low[i] <-data_frames[[i]][4,2] 
  cp2.low[i] <-data_frames[[i]][5,2] 
  c0.low[i] <-data_frames[[i]][6,2] 
  c1.low[i] <-data_frames[[i]][7,2] 
  c2.low[i] <-data_frames[[i]][8,2] 
  c3.low[i] <-data_frames[[i]][9,2]
  c4.low[i] <-data_frames[[i]][10,2]
  
  u.tau.inv.low[i] <-data_frames[[i]][11,2] 
  u.tau1.inv.low[i] <-data_frames[[i]][12,2] 
  u.tau2.inv.low[i] <-data_frames[[i]][13,2] 
  u.tau3.inv.low[i] <-data_frames[[i]][14,2] 
  b0.low[i] <-data_frames[[i]][15,2] 
  b1.low[i] <-data_frames[[i]][16,2] 
  a.low[i] <-data_frames[[i]][17,2] 
  ga.low[i] <-data_frames[[i]][18,2]
  ga1.low[i] <-data_frames[[i]][19,2]
  ga2.low[i] <-data_frames[[i]][20,2]
  ga3.low[i] <-data_frames[[i]][21,2]
  w.tau.inv.low[i] <-data_frames[[i]][22,2] 
}


B1.high<-rep(NA,I)
B2.high<-rep(NA,I)
B3.high<-rep(NA,I)
cp1.high<-rep(NA,I)
cp2.high<-rep(NA,I)
c0.high<-rep(NA,I)
c1.high<-rep(NA,I)
c2.high<-rep(NA,I)
c3.high<-rep(NA,I)
c4.high<-rep(NA,I)

u.tau.inv.high<-rep(NA,I)
u.tau1.inv.high<-rep(NA,I)
u.tau2.inv.high<-rep(NA,I)
u.tau3.inv.high<-rep(NA,I)
b0.high<-rep(NA,I)
b1.high<-rep(NA,I)
a.high<-rep(NA,I)
ga.high<-rep(NA,I)
ga1.high<-rep(NA,I)
ga2.high<-rep(NA,I)
ga3.high<-rep(NA,I)
w.tau.inv.high<-rep(NA,I)

for(i in 1:I){ 
  B1.high[i] <- data_frames[[i]][1,4] 
  B2.high[i] <- data_frames[[i]][2,4] 
  B3.high[i] <-data_frames[[i]][3,4] 
  cp1.high[i] <-data_frames[[i]][4,4] 
  cp2.high[i] <-data_frames[[i]][5,4] 
  c0.high[i] <-data_frames[[i]][6,4] 
  c1.high[i] <-data_frames[[i]][7,4] 
  c2.high[i] <-data_frames[[i]][8,4] 
  c3.high[i] <-data_frames[[i]][9,4]
  c4.high[i] <-data_frames[[i]][10,4]
  
  u.tau.inv.high[i] <-data_frames[[i]][11,4] 
  u.tau1.inv.high[i] <-data_frames[[i]][12,4] 
  u.tau2.inv.high[i] <-data_frames[[i]][13,4] 
  u.tau3.inv.high[i] <-data_frames[[i]][14,4] 
  b0.high[i] <-data_frames[[i]][15,4] 
  b1.high[i] <-data_frames[[i]][16,4] 
  a.high[i] <-data_frames[[i]][17,4] 
  ga.high[i] <-data_frames[[i]][18,4] 
  ga1.high[i] <-data_frames[[i]][19,4] 
  ga2.high[i] <-data_frames[[i]][20,4] 
  ga3.high[i] <-data_frames[[i]][21,4] 
  w.tau.inv.high[i] <-data_frames[[i]][22,4] 
}

dat1 <- as.data.frame( cbind(dat,B1.low,B2.low,B3.low,cp1.low,cp2.low,c0.low,c1.low,c2.low,c3.low,c4.low,u.tau.inv.low,u.tau1.inv.low,u.tau2.inv.low,u.tau3.inv.low,
                                 b0.low,b1.low,a.low,ga.low,ga1.low,ga2.low,ga3.low,w.tau.inv.low,B1.high,B2.high,B3.high,cp1.high,cp2.high,c0.high,
                                 c1.high,c2.high,c3.high,c4.high,u.tau.inv.high,u.tau1.inv.high,u.tau2.inv.high,u.tau3.inv.high,b0.high,b1.high,a.high,
                             ga.high,ga1.high,ga2.high,ga3.high,w.tau.inv.high))
dat1$B1.cp <- ifelse(dat1$B1>dat1$B1.low & dat1$B1<dat1$B1.high,1,0)
dat1$B2.cp <- ifelse(dat1$B2>dat1$B2.low & dat1$B2<dat1$B2.high,1,0)
dat1$B3.cp <- ifelse(dat1$B3>dat1$B3.low & dat1$B3<dat1$B3.high,1,0)
dat1$cp1.cp <- ifelse(dat1$cp1>dat1$cp1.low & dat1$cp1<dat1$cp1.high,1,0)
dat1$cp2.cp <- ifelse(dat1$cp2>dat1$cp2.low & dat1$cp2<dat1$cp2.high,1,0)
dat1$c0.cp <- ifelse(dat1$c0>dat1$c0.low & dat1$c0<dat1$c0.high,1,0)
dat1$c1.cp <- ifelse(dat1$c1>dat1$c1.low & dat1$c1<dat1$c1.high,1,0)
dat1$c2.cp <- ifelse(dat1$c2>dat1$c2.low & dat1$c2<dat1$c2.high,1,0)
dat1$c3.cp <- ifelse(dat1$c3>dat1$c3.low & dat1$c3<dat1$c3.high,1,0)
dat1$c4.cp <- ifelse(dat1$c4>dat1$c4.low & dat1$c4<dat1$c4.high,1,0)
dat1$u.tau.inv.cp <- ifelse(dat1$u.sigma2>dat1$u.tau.inv.low & dat1$u.sigma2<dat1$u.tau.inv.high,1,0)
dat1$u.tau1.inv.cp <- ifelse(dat1$u1.sigma2>dat1$u.tau1.inv.low & dat1$u1.sigma2<dat1$u.tau1.inv.high,1,0)
dat1$u.tau2.inv.cp <- ifelse(dat1$u2.sigma2>dat1$u.tau2.inv.low & dat1$u2.sigma2<dat1$u.tau2.inv.high,1,0)
dat1$u.tau3.inv.cp <- ifelse(dat1$u3.sigma2>dat1$u.tau3.inv.low & dat1$u3.sigma2<dat1$u.tau3.inv.high,1,0)
dat1$b0.cp <- ifelse(dat1$beta0>dat1$b0.low & dat1$beta0<dat1$b0.high,1,0)
dat1$b1.cp <- ifelse(dat1$beta>dat1$b1.low & dat1$beta<dat1$b1.high,1,0)
dat1$a.cp <- ifelse(dat1$alpha>dat1$a.low & dat1$alpha<dat1$a.high,1,0)
dat1$ga.cp <- ifelse(dat1$ga>dat1$ga.low & dat1$ga<dat1$ga.high,1,0)
dat1$ga1.cp <- ifelse(dat1$ga1>dat1$ga1.low & dat1$ga<dat1$ga1.high,1,0)
dat1$ga2.cp <- ifelse(dat1$ga2>dat1$ga2.low & dat1$ga<dat1$ga2.high,1,0)
dat1$ga3.cp <- ifelse(dat1$ga3>dat1$ga3.low & dat1$ga<dat1$ga3.high,1,0)
dat1$w.tau.inv.cp <- ifelse(dat1$w.sigma2>dat1$w.tau.inv.low & dat1$w.sigma2<dat1$w.tau.inv.high,1,0)


cp <- c(sum(dat1$B1.cp)/I,sum(dat1$B2.cp)/I,sum(dat1$B3.cp)/I,sum(dat1$cp1.cp)/I,sum(dat1$cp2.cp)/I,sum(dat1$c0.cp)/I,
        sum(dat1$c1.cp)/I,sum(dat1$c2.cp)/I,sum(dat1$c3.cp)/I,sum(dat1$c4.cp)/I,
        sum(dat1$u.tau.inv.cp)/I,sum(dat1$u.tau1.inv.cp)/I,sum(dat1$u.tau2.inv.cp)/I,sum(dat1$u.tau3.inv.cp)/I,
        sum(dat1$b0.cp)/I,sum(dat1$b1.cp)/I,sum(dat1$a.cp)/I,
        sum(dat1$ga.cp)/I,sum(dat1$ga1.cp)/I,sum(dat1$ga2.cp)/I,sum(dat1$ga3.cp)/I,sum(dat1$w.tau.inv.cp)/I)

cbind(round(bias,3),round(mse,3),round(cp,3))
