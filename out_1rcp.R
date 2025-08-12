#setwd("C:/UCHealth/RA/Project/EPIC-CF/Analysis_Jiayuan/Result/Simulation_output")
setwd("C:/Users/jiayu/OneDrive/Desktop/RCP1")

###########################################################################
# Read csv files
text <- list.files(pattern="result_1rcp.")
text1 <- unlist(lapply(strsplit(text,'.',fixed=TRUE),function(x) x[[1]]))
ind <- which(text1=="result_1rcp.")
num <- as.numeric(unlist(lapply(strsplit(text[ind],'.',fixed=TRUE),function(x) x[[2]])))

data_frames <- lapply(num, function(i) {
  file_name <- paste0("result_1rcp.", i, ".csv") 
  read.csv(file_name)
})

I=length(data_frames)

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
b0.mean<-rep(NA,I)
b1.mean<-rep(NA,I)
a.mean<-rep(NA,I)
ga.mean<-rep(NA,I)
w.tau.inv.mean<-rep(NA,I)
u.mean<-rep(NA,I)
v.mean<-rep(NA,I)
w.mean<-rep(NA,I)

for(i in 1:I){ 
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
b0.mean[i] <-data_frames[[i]][12,5] 
b1.mean[i] <-data_frames[[i]][13,5] 
a.mean[i] <-data_frames[[i]][14,5] 
ga.mean[i] <-data_frames[[i]][15,5] 
w.tau.inv.mean[i] <-data_frames[[i]][16,5] 
u.mean[i] <-mean(data_frames[[i]][17:416,5])
v.mean[i] <-mean(data_frames[[i]][417:816,5])
w.mean[i] <-mean(data_frames[[i]][817:1216,5])
}

Sim.results=cbind(B1.mean,B2.mean,B3.mean,cp1.mean,cp2.mean,c0.mean,c1.mean,c2.mean,c3.mean,c4.mean,u.tau.inv.mean,
               b0.mean,b1.mean,a.mean,ga.mean,w.tau.inv.mean,u.mean,v.mean,w.mean)
est<-round(colMeans(Sim.results),2)
est

par(mfrow=c(2,2))
hist(B1.mean)
hist(cp1.mean)
hist(cp2.mean)
summary(B1.mean)
summary(cp1.mean)
summary(cp2.mean)


#x10 <- round(colMeans(Sim.results.1),2)
#x <- round(colMeans(Sim.results),2)
#dat <- cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x)
#write.csv(dat, "resultall.csv")

B1=rep(-0.2,I)
B2=rep(0.2,I)
B3=rep(0.4,I)
c0=rep(-4.5,I)
c1=rep(0.1,I)
c2=rep(0.2,I) 
c3=rep(0.1,I)
c4=rep(0.1,I)
cp1=rep(4.5,I)
cp2=rep(14.5,I)
u.sigma2=rep(1,I)
alpha=rep(1.8,I)
beta=rep(0.25,I)
beta0=rep(-4.5,I)
ga=rep(0.25,I)
w.sigma2=rep(1,I)

dat <- as.data.frame(cbind(Sim.results,B1,B2,B3,cp1,cp2,c0,c1,c2,c3,c4,u.sigma2,beta0,beta,alpha,ga,w.sigma2))
bias <- c(sum(dat$B1.mean-dat$B1)/I,sum(dat$B2.mean-dat$B2)/I,sum(dat$B3.mean-dat$B3)/I,sum(dat$cp1.mean-dat$cp1)/I,sum(dat$cp2.mean-dat$cp2)/I,sum(dat$c0.mean-dat$c0)/I,
          sum(dat$c1.mean-dat$c1)/I,sum(dat$c2.mean-dat$c2)/I,sum(dat$c3.mean-dat$c3)/I,sum(dat$c4.mean-dat$c4)/I,sum(dat$u.tau.inv.mean-dat$u.sigma2)/I,sum(dat$b0.mean-dat$beta0)/I,sum(dat$b1.mean-dat$beta)/I,
          sum(dat$a.mean-dat$alpha)/I,sum(dat$ga.mean-dat$ga)/I,sum(dat$w.tau.inv.mean-dat$w.sigma2)/I)
mse <- c(sum((dat$B1.mean-dat$B1)^2)/I,sum((dat$B2.mean-dat$B2)^2)/I,sum((dat$B3.mean-dat$B3)^2)/I,sum((dat$cp1.mean-dat$cp1)^2)/I,sum((dat$cp2.mean-dat$cp2)^2)/I,sum((dat$c0.mean-dat$c0)^2)/I,
         sum((dat$c1.mean-dat$c1)^2)/I,sum((dat$c2.mean-dat$c2)^2)/I,sum((dat$c3.mean-dat$c3)^2)/I,sum((dat$c4.mean-dat$c4)^2)/I,sum((dat$u.tau.inv.mean-dat$u.sigma2)^2)/I,sum((dat$b0.mean-dat$beta0)^2)/I,sum((dat$b1.mean-dat$beta)^2)/I,
         sum((dat$a.mean-dat$alpha)^2)/I,sum((dat$ga.mean-dat$ga)^2)/I,sum((dat$w.tau.inv.mean-dat$w.sigma2)^2)/I)


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
b0.low<-rep(NA,I)
b1.low<-rep(NA,I)
a.low<-rep(NA,I)
ga.low<-rep(NA,I)
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
  b0.low[i] <-data_frames[[i]][12,2] 
  b1.low[i] <-data_frames[[i]][13,2] 
  a.low[i] <-data_frames[[i]][14,2] 
  ga.low[i] <-data_frames[[i]][15,2] 
  w.tau.inv.low[i] <-data_frames[[i]][16,2] 
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
b0.high<-rep(NA,I)
b1.high<-rep(NA,I)
a.high<-rep(NA,I)
ga.high<-rep(NA,I)
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
  b0.high[i] <-data_frames[[i]][12,4] 
  b1.high[i] <-data_frames[[i]][13,4] 
  a.high[i] <-data_frames[[i]][14,4] 
  ga.high[i] <-data_frames[[i]][15,4] 
  w.tau.inv.high[i] <-data_frames[[i]][16,4] 
}

dat1 <- as.data.frame( cbind(dat,B1.low,B2.low,B3.low,cp1.low,cp2.low,c0.low,c1.low,c2.low,c3.low,c4.low,u.tau.inv.low,
                                 b0.low,b1.low,a.low,ga.low,w.tau.inv.low,B1.high,B2.high,B3.high,cp1.high,cp2.high,c0.high,
                                 c1.high,c2.high,c3.high,c4.high,u.tau.inv.high,b0.high,b1.high,a.high,ga.high,w.tau.inv.high))
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
dat1$b0.cp <- ifelse(dat1$beta0>dat1$b0.low & dat1$beta0<dat1$b0.high,1,0)
dat1$b1.cp <- ifelse(dat1$beta>dat1$b1.low & dat1$beta<dat1$b1.high,1,0)
dat1$a.cp <- ifelse(dat1$alpha>dat1$a.low & dat1$alpha<dat1$a.high,1,0)
dat1$ga.cp <- ifelse(dat1$ga>dat1$ga.low & dat1$ga<dat1$ga.high,1,0)
dat1$w.tau.inv.cp <- ifelse(dat1$w.sigma2>dat1$w.tau.inv.low & dat1$w.sigma2<dat1$w.tau.inv.high,1,0)


cp <- c(sum(dat1$B1.cp)/I,sum(dat1$B2.cp)/I,sum(dat1$B3.cp)/I,sum(dat1$cp1.cp)/I,sum(dat1$cp2.cp)/I,sum(dat1$c0.cp)/I,
        sum(dat1$c1.cp)/I,sum(dat1$c2.cp)/I,sum(dat1$c3.cp)/I,sum(dat1$c4.cp)/I,sum(dat1$u.tau.inv.cp)/I,
        sum(dat1$b0.cp)/I,sum(dat1$b1.cp)/I,sum(dat1$a.cp)/I,sum(dat1$ga.cp)/I,sum(dat1$w.tau.inv.cp)/I)

cbind(round(bias,3),round(mse,3),round(cp,2))
