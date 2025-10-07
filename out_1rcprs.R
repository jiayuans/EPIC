#scp "jiayuan.shi@ap40.uw.osg-htc.org:/home/jiayuan.shi/EPIC/result_1rcp.*.csv" /Users/Shared/Windows/UCHealth/RA/Project/EPIC-CF/Analysis_Jiayuan/EPIC_Sim_Results/1RCP_run
setwd("/Users/Shared/Windows/UCHealth/RA/Project/EPIC-CF/Analysis_Jiayuan/EPIC_Sim_Results/1RCPRS_1006")
setwd("/Volumes/dept/SPH/SPH-BIOS/EJCStudents/ShiJ/EPIC-CF/Simulation/1RCPRS_0928") ##### best estimates in folder 1RCP_new
load("/Users/Shared/Windows/UCHealth/RA/Project/EPIC-CF/Analysis_Jiayuan/EPIC_Sim_Results/1RCPRS_1003/res_1rcprs.1.RData")
load("/Volumes/dept/SPH/SPH-BIOS/EJCStudents/ShiJ/EPIC-CF/Simulation/1RCPRS_0928/res_1rcprs.1.RData")


###########################################################################
# Read csv files
text <- list.files(pattern="result_1rcprs.")
num <- unlist(lapply(strsplit(text,'.',fixed=TRUE),function(x) x[[2]]))

data_frames <- lapply(num, function(i) {
  file_name <- paste0("result_1rcprs.", i, ".csv") 
  read.csv(file_name)
})

I=length(data_frames)
I

Flag<-rep(NA,I)
B1.mean<-rep(NA,I)
B2.mean<-rep(NA,I)
cp1.mean<-rep(NA,I)
c0.mean<-rep(NA,I)
c1.mean<-rep(NA,I)
c2.mean<-rep(NA,I)
c3.mean<-rep(NA,I)

u.tau.inv.mean<-rep(NA,I)
u.tau1.inv.mean<-rep(NA,I)
u.tau2.inv.mean<-rep(NA,I)
b0.mean<-rep(NA,I)
b1.mean<-rep(NA,I)
a.mean<-rep(NA,I)
ga0.mean<-rep(NA,I)
ga.mean<-rep(NA,I)
ga1.mean<-rep(NA,I)
ga2.mean<-rep(NA,I)
w.tau.inv.mean<-rep(NA,I)
cp1var.mean<-rep(NA,I)
cp1mu.mean<-rep(NA,I)
u.mean<-rep(NA,I)
u1.mean<-rep(NA,I)
u2.mean<-rep(NA,I)
v.mean<-rep(NA,I)
w.mean<-rep(NA,I)

for(i in 1:I){ 
Flag[i] <- ifelse(max(data_frames[[i]][,12])<1.3,1,0)
B1.mean[i] <- data_frames[[i]][1,5] 
B2.mean[i] <- data_frames[[i]][2,5] 
cp1.mean[i] <-mean(data_frames[[i]][3:402,5])
c0.mean[i] <-data_frames[[i]][403,5] 
c1.mean[i] <-data_frames[[i]][404,5] 
c2.mean[i] <-data_frames[[i]][405,5] 
c3.mean[i] <-data_frames[[i]][406,5]

u.tau.inv.mean[i] <-data_frames[[i]][407,5] 
u.tau1.inv.mean[i] <-data_frames[[i]][408,5] 
u.tau2.inv.mean[i] <-data_frames[[i]][409,5] 
b0.mean[i] <-data_frames[[i]][410,5] 
b1.mean[i] <-data_frames[[i]][411,5] 
a.mean[i] <-data_frames[[i]][412,5] 
ga0.mean[i] <-data_frames[[i]][413,5] 
ga.mean[i] <-data_frames[[i]][414,5] 
ga1.mean[i] <-data_frames[[i]][415,5] 
ga2.mean[i] <-data_frames[[i]][416,5] 
w.tau.inv.mean[i] <-data_frames[[i]][417,5] 
cp1mu.mean[i] <-data_frames[[i]][418,5] 
cp1var.mean[i] <-data_frames[[i]][419,5] 
u.mean[i] <-mean(data_frames[[i]][420:819,5])
u1.mean[i] <-mean(data_frames[[i]][820:1219,5])
u2.mean[i] <-mean(data_frames[[i]][1220:1619,5])
v.mean[i] <-mean(data_frames[[i]][1620:2019,5])
w.mean[i] <-mean(data_frames[[i]][2020:2419,5])
}

Sim.results=cbind(Flag,B1.mean,B2.mean,cp1.mean,c0.mean,c1.mean,c2.mean,c3.mean,u.tau.inv.mean,u.tau1.inv.mean,u.tau2.inv.mean,
               b0.mean,b1.mean,a.mean,ga0.mean,ga.mean,ga1.mean,ga2.mean,w.tau.inv.mean,cp1mu.mean,cp1var.mean,u.mean,u1.mean,u2.mean,w.mean)
est<-round(colMeans(Sim.results),2)
est

table(Flag)
Sim.results.1 <- subset(Sim.results,Flag==1)
round(colMeans(Sim.results.1),2)


#x10 <- round(colMeans(Sim.results.1),2)
#x <- round(colMeans(Sim.results),2)
#dat <- cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x)
#write.csv(dat, "resultall.csv")

B1=rep(0.08,I)
B2=rep(0.1,I)
c0=rep(-3,I) #-3
c1=rep(0.09,I)
c2=rep(0.01,I) 
c3=rep(-0.05,I) #-0.05
cp1=rep(8,I)
u.sigma2=rep(0.25,I)
u1.sigma2=rep(0.04,I)
u2.sigma2=rep(0.04,I)
alpha=rep(1.8,I)
beta=rep(0.2,I)
beta0=rep(-2,I)
ga0=rep(0.1,I)
ga=rep(-0.05,I)
ga1=rep(-0.1,I) 
ga2=rep(1,I) 
w.sigma2=rep(0.04,I)
true <- c(B1[1],B2[1],cp1[1],c0[1],c1[1],c2[1],c3[1],
          u.sigma2[1],u1.sigma2[1],u2.sigma2[1],
          beta0[1],beta[1],alpha[1],
          ga0[1],ga[1],ga1[1],ga2[1],w.sigma2[1])


dat <- as.data.frame(cbind(Sim.results,B1,B2,cp1,c0,c1,c2,c3,u.sigma2,u1.sigma2,u2.sigma2,beta0,beta,alpha,ga0,ga,ga1,ga2,w.sigma2))
bias <- c(sum(dat$B1.mean-dat$B1)/I,sum(dat$B2.mean-dat$B2)/I,sum(dat$cp1.mean-dat$cp1)/I,sum(dat$c0.mean-dat$c0)/I,
          sum(dat$c1.mean-dat$c1)/I,sum(dat$c2.mean-dat$c2)/I,sum(dat$c3.mean-dat$c3)/I,sum(dat$u.tau.inv.mean-dat$u.sigma2)/I,sum(dat$u.tau1.inv.mean-dat$u1.sigma2)/I,sum(dat$u.tau2.inv.mean-dat$u2.sigma2)/I,
          sum(dat$b0.mean-dat$beta0)/I,sum(dat$b1.mean-dat$beta)/I,
          sum(dat$a.mean-dat$alpha)/I,sum(dat$ga0.mean-dat$ga0)/I,sum(dat$ga.mean-dat$ga)/I,sum(dat$ga1.mean-dat$ga1)/I,sum(dat$ga2.mean-dat$ga2)/I,sum(dat$w.tau.inv.mean-dat$w.sigma2)/I)
mse <- c(sum((dat$B1.mean-dat$B1)^2)/I,sum((dat$B2.mean-dat$B2)^2)/I,sum((dat$cp1.mean-dat$cp1)^2)/I,sum((dat$c0.mean-dat$c0)^2)/I,
         sum((dat$c1.mean-dat$c1)^2)/I,sum((dat$c2.mean-dat$c2)^2)/I,sum((dat$c3.mean-dat$c3)^2)/I,sum((dat$u.tau.inv.mean-dat$u.sigma2)^2)/I,sum((dat$u.tau1.inv.mean-dat$u1.sigma2)^2)/I,sum((dat$u.tau2.inv.mean-dat$u2.sigma2)^2)/I,
         sum((dat$b0.mean-dat$beta0)^2)/I,sum((dat$b1.mean-dat$beta)^2)/I,
         sum((dat$a.mean-dat$alpha)^2)/I,sum((dat$ga0.mean-dat$ga0)^2)/I,sum((dat$ga.mean-dat$ga)^2)/I,sum((dat$ga1.mean-dat$ga1)^2)/I,sum((dat$ga2.mean-dat$ga2)^2)/I,sum((dat$w.tau.inv.mean-dat$w.sigma2)^2)/I)


B1.low<-rep(NA,I)
B2.low<-rep(NA,I)
cp1.low<-rep(NA,I)
c0.low<-rep(NA,I)
c1.low<-rep(NA,I)
c2.low<-rep(NA,I)
c3.low<-rep(NA,I)

u.tau.inv.low<-rep(NA,I)
u.tau1.inv.low<-rep(NA,I)
u.tau2.inv.low<-rep(NA,I)
b0.low<-rep(NA,I)
b1.low<-rep(NA,I)
a.low<-rep(NA,I)
ga0.low<-rep(NA,I)
ga.low<-rep(NA,I)
ga1.low<-rep(NA,I)
ga2.low<-rep(NA,I)
w.tau.inv.low<-rep(NA,I)

for(i in 1:I){ 
  B1.low[i] <- data_frames[[i]][1,2] 
  B2.low[i] <- data_frames[[i]][2,2] 
  cp1.low[i] <-mean(data_frames[[i]][3:402,2])
  c0.low[i] <-data_frames[[i]][403,2] 
  c1.low[i] <-data_frames[[i]][404,2] 
  c2.low[i] <-data_frames[[i]][405,2] 
  c3.low[i] <-data_frames[[i]][406,2]
  
  u.tau.inv.low[i] <-data_frames[[i]][407,2] 
  u.tau1.inv.low[i] <-data_frames[[i]][408,2] 
  u.tau2.inv.low[i] <-data_frames[[i]][409,2] 
  b0.low[i] <-data_frames[[i]][410,2] 
  b1.low[i] <-data_frames[[i]][411,2] 
  a.low[i] <-data_frames[[i]][412,2] 
  ga0.low[i] <-data_frames[[i]][413,2] 
  ga.low[i] <-data_frames[[i]][414,2] 
  ga1.low[i] <-data_frames[[i]][415,2] 
  ga2.low[i] <-data_frames[[i]][416,2] 
  w.tau.inv.low[i] <-data_frames[[i]][417,2] 
}


B1.high<-rep(NA,I)
B2.high<-rep(NA,I)
cp1.high<-rep(NA,I)
c0.high<-rep(NA,I)
c1.high<-rep(NA,I)
c2.high<-rep(NA,I)
c3.high<-rep(NA,I)

u.tau.inv.high<-rep(NA,I)
u.tau1.inv.high<-rep(NA,I)
u.tau2.inv.high<-rep(NA,I)
b0.high<-rep(NA,I)
b1.high<-rep(NA,I)
a.high<-rep(NA,I)
ga0.high<-rep(NA,I)
ga.high<-rep(NA,I)
ga1.high<-rep(NA,I)
ga2.high<-rep(NA,I)
w.tau.inv.high<-rep(NA,I)

for(i in 1:I){ 
  B1.high[i] <- data_frames[[i]][1,4] 
  B2.high[i] <- data_frames[[i]][2,4]
  cp1.high[i] <-mean(data_frames[[i]][3:402,4])
  c0.high[i] <-data_frames[[i]][403,4] 
  c1.high[i] <-data_frames[[i]][404,4] 
  c2.high[i] <-data_frames[[i]][405,4] 
  c3.high[i] <-data_frames[[i]][406,4]
  
  u.tau.inv.high[i] <-data_frames[[i]][407,4] 
  u.tau1.inv.high[i] <-data_frames[[i]][408,4] 
  u.tau2.inv.high[i] <-data_frames[[i]][409,4] 
  b0.high[i] <-data_frames[[i]][410,4] 
  b1.high[i] <-data_frames[[i]][411,4] 
  a.high[i] <-data_frames[[i]][412,4] 
  ga0.high[i] <-data_frames[[i]][413,4] 
  ga.high[i] <-data_frames[[i]][414,4] 
  ga1.high[i] <-data_frames[[i]][415,4] 
  ga2.high[i] <-data_frames[[i]][416,4] 
  w.tau.inv.high[i] <-data_frames[[i]][417,4] 
}

dat1 <- as.data.frame( cbind(dat,B1.low,B2.low,cp1.low,c0.low,c1.low,c2.low,c3.low,u.tau.inv.low,u.tau1.inv.low,u.tau2.inv.low,
                                 b0.low,b1.low,a.low,ga0.low,ga.low,ga1.low,ga2.low,w.tau.inv.low,B1.high,B2.high,cp1.high,c0.high,
                                 c1.high,c2.high,c3.high,u.tau.inv.high,u.tau1.inv.high,u.tau2.inv.high,b0.high,b1.high,a.high,ga0.high,ga.high,ga1.high,ga2.high,w.tau.inv.high))
dat1$B1.cp <- ifelse(dat1$B1>=dat1$B1.low & dat1$B1<=dat1$B1.high,1,0)
dat1$B2.cp <- ifelse(dat1$B2>=dat1$B2.low & dat1$B2<=dat1$B2.high,1,0)
dat1$cp1.cp <- ifelse(dat1$cp1>=dat1$cp1.low & dat1$cp1<=dat1$cp1.high,1,0)
dat1$c0.cp <- ifelse(dat1$c0>=dat1$c0.low & dat1$c0<=dat1$c0.high,1,0)
dat1$c1.cp <- ifelse(dat1$c1>=dat1$c1.low & dat1$c1<=dat1$c1.high,1,0)
dat1$c2.cp <- ifelse(dat1$c2>=dat1$c2.low & dat1$c2<=dat1$c2.high,1,0)
dat1$c3.cp <- ifelse(dat1$c3>=dat1$c3.low & dat1$c3<=dat1$c3.high,1,0)
dat1$u.tau.inv.cp <- ifelse(dat1$u.sigma2>=dat1$u.tau.inv.low & dat1$u.sigma2<=dat1$u.tau.inv.high,1,0)
dat1$u.tau1.inv.cp <- ifelse(dat1$u1.sigma2>=dat1$u.tau1.inv.low & dat1$u1.sigma2<=dat1$u.tau1.inv.high,1,0)
dat1$u.tau2.inv.cp <- ifelse(dat1$u2.sigma2>=dat1$u.tau2.inv.low & dat1$u2.sigma2<=dat1$u.tau2.inv.high,1,0)
dat1$b0.cp <- ifelse(dat1$beta0>=dat1$b0.low & dat1$beta0<=dat1$b0.high,1,0)
dat1$b1.cp <- ifelse(dat1$beta>=dat1$b1.low & dat1$beta<=dat1$b1.high,1,0)
dat1$a.cp <- ifelse(dat1$alpha>=dat1$a.low & dat1$alpha<=dat1$a.high,1,0)
dat1$ga0.cp <- ifelse(dat1$ga0>=dat1$ga0.low & dat1$ga0<=dat1$ga0.high,1,0)
dat1$ga.cp <- ifelse(dat1$ga>=dat1$ga.low & dat1$ga<=dat1$ga.high,1,0)
dat1$ga1.cp <- ifelse(dat1$ga1>=dat1$ga1.low & dat1$ga1<=dat1$ga1.high,1,0)
dat1$ga2.cp <- ifelse(dat1$ga2>=dat1$ga2.low & dat1$ga2<=dat1$ga2.high,1,0)
dat1$w.tau.inv.cp <- ifelse(dat1$w.sigma2>=dat1$w.tau.inv.low & dat1$w.sigma2<=dat1$w.tau.inv.high,1,0)


cp <- c(sum(dat1$B1.cp)/I,sum(dat1$B2.cp)/I,sum(dat1$cp1.cp)/I,sum(dat1$c0.cp)/I,
        sum(dat1$c1.cp)/I,sum(dat1$c2.cp)/I,sum(dat1$c3.cp)/I,sum(dat1$u.tau.inv.cp)/I,sum(dat1$u.tau1.inv.cp)/I,sum(dat1$u.tau2.inv.cp)/I,
        sum(dat1$b0.cp)/I,sum(dat1$b1.cp)/I,sum(dat1$a.cp)/I,sum(dat1$ga0.cp)/I,sum(dat1$ga.cp)/I,sum(dat1$ga1.cp)/I,sum(dat1$ga2.cp)/I,sum(dat1$w.tau.inv.cp)/I)

cbind(true, as.numeric(c(est[2:19])), round(bias,3),round(mse,3),round(cp,2))



## keep only flagged simulations
Sim.results.1 <- subset(Sim.results, Flag == 1)
idx <- which(Flag == 1)
n <- nrow(Sim.results.1)

if (n == 0L) stop("No simulations with Flag == 1.")

## quick sanity check
print(table(Flag))
est1 <- round(colMeans(Sim.results.1), 2)

## ----- truths (length = n) -----
B1      <- rep(0.08, n)
B2      <- rep(0.10, n)
cp1     <- rep(8,    n)
c0      <- rep(-3,   n)
c1      <- rep(0.09, n)
c2      <- rep(0.01, n)
c3      <- rep(-0.05,n)
u.sigma2  <- rep(0.09, n)
u1.sigma2 <- rep(0.04, n)
u2.sigma2 <- rep(0.04, n)
alpha   <- rep(1.8,  n)
beta    <- rep(0.2,  n)
beta0   <- rep(-2,   n)
ga0     <- rep(0.15,  n)
ga      <- rep(-0.05,n)
ga1     <- rep(-0.1, n)
ga2     <- rep(1,    n)
w.sigma2<- rep(0.04, n)

## ----- assemble working data.frame using ONLY flagged rows -----
dat <- cbind(
  Sim.results.1,
  B1,B2,cp1,c0,c1,c2,c3,
  u.sigma2,u1.sigma2,u2.sigma2,
  beta0,beta,alpha,ga0,ga,ga1,ga2,w.sigma2
)
dat <- as.data.frame(dat)

## ----- bias & MSE over flagged sims only (divide by n) -----
bias <- c(
  sum(dat$B1.mean - dat$B1)/n,
  sum(dat$B2.mean - dat$B2)/n,
  sum(dat$cp1.mean - dat$cp1)/n,
  sum(dat$c0.mean  - dat$c0)/n,
  sum(dat$c1.mean  - dat$c1)/n,
  sum(dat$c2.mean  - dat$c2)/n,
  sum(dat$c3.mean  - dat$c3)/n,
  sum(dat$u.tau.inv.mean  - dat$u.sigma2)/n,
  sum(dat$u.tau1.inv.mean - dat$u1.sigma2)/n,
  sum(dat$u.tau2.inv.mean - dat$u2.sigma2)/n,
  sum(dat$b0.mean - dat$beta0)/n,
  sum(dat$b1.mean - dat$beta)/n,
  sum(dat$a.mean  - dat$alpha)/n,
  sum(dat$ga0.mean - dat$ga0)/n,
  sum(dat$ga.mean  - dat$ga)/n,
  sum(dat$ga1.mean - dat$ga1)/n,
  sum(dat$ga2.mean - dat$ga2)/n,
  sum(dat$w.tau.inv.mean - dat$w.sigma2)/n
)

mse <- c(
  sum((dat$B1.mean - dat$B1)^2)/n,
  sum((dat$B2.mean - dat$B2)^2)/n,
  sum((dat$cp1.mean - dat$cp1)^2)/n,
  sum((dat$c0.mean  - dat$c0)^2)/n,
  sum((dat$c1.mean  - dat$c1)^2)/n,
  sum((dat$c2.mean  - dat$c2)^2)/n,
  sum((dat$c3.mean  - dat$c3)^2)/n,
  sum((dat$u.tau.inv.mean  - dat$u.sigma2)^2)/n,
  sum((dat$u.tau1.inv.mean - dat$u1.sigma2)^2)/n,
  sum((dat$u.tau2.inv.mean - dat$u2.sigma2)^2)/n,
  sum((dat$b0.mean - dat$beta0)^2)/n,
  sum((dat$b1.mean - dat$beta)^2)/n,
  sum((dat$a.mean  - dat$alpha)^2)/n,
  sum((dat$ga0.mean - dat$ga0)^2)/n,
  sum((dat$ga.mean  - dat$ga)^2)/n,
  sum((dat$ga1.mean - dat$ga1)^2)/n,
  sum((dat$ga2.mean - dat$ga2)^2)/n,
  sum((dat$w.tau.inv.mean - dat$w.sigma2)^2)/n
)

## ----- CI bounds pulled ONLY from the matching flagged runs -----
## we assume `data_frames` is a list aligned with all sims; subset it by idx
df_sub <- data_frames[idx]

B1.low <- B2.low <- cp1.low <- c0.low <- c1.low <- c2.low <- c3.low <- numeric(n)
u.tau.inv.low <- u.tau1.inv.low <- u.tau2.inv.low <- b0.low <- b1.low <- a.low <- ga0.low <- ga.low <- ga1.low <- ga2.low <- w.tau.inv.low <- numeric(n)

B1.high <- B2.high <- cp1.high <- c0.high <- c1.high <- c2.high <- c3.high <- numeric(n)
u.tau.inv.high <- u.tau1.inv.high <- u.tau2.inv.high <- b0.high <- b1.high <- a.high <- ga0.high <- ga.high <- ga1.high <- ga2.high <- w.tau.inv.high <- numeric(n)

for (k in seq_len(n)) {
  m <- df_sub[[k]]
  ## low (col 2)
  B1.low[k] <- m[1,2];  B2.low[k] <- m[2,2];  cp1.low[k] <- mean(m[3:402,2])
  c0.low[k] <- m[403,2]; c1.low[k] <- m[404,2]; c2.low[k] <- m[405,2]; c3.low[k] <- m[406,2]
  u.tau.inv.low[k]  <- m[407,2]; u.tau1.inv.low[k] <- m[408,2]; u.tau2.inv.low[k] <- m[409,2]
  b0.low[k] <- m[410,2]; b1.low[k] <- m[411,2]
  a.low[k]  <- m[412,2]   ## NOTE: was 410 in your code; 412 matches the 'high' block structure
  ga0.low[k] <- m[413,2]; ga.low[k] <- m[414,2]; ga1.low[k] <- m[415,2]; ga2.low[k] <- m[416,2]
  w.tau.inv.low[k] <- m[417,2]
  
  ## high (col 4)
  B1.high[k] <- m[1,4];  B2.high[k] <- m[2,4];  cp1.high[k] <- mean(m[3:402,4])
  c0.high[k] <- m[403,4]; c1.high[k] <- m[404,4]; c2.high[k] <- m[405,4]; c3.high[k] <- m[406,4]
  u.tau.inv.high[k]  <- m[407,4]; u.tau1.inv.high[k] <- m[408,4]; u.tau2.inv.high[k] <- m[409,4]
  b0.high[k] <- m[410,4]; b1.high[k] <- m[411,4]
  a.high[k]  <- m[412,4]
  ga0.high[k] <- m[413,4]; ga.high[k] <- m[414,4]; ga1.high[k] <- m[415,4]; ga2.high[k] <- m[416,4]
  w.tau.inv.high[k] <- m[417,4]
}

dat1 <- cbind(
  dat,
  B1.low,B2.low,cp1.low,c0.low,c1.low,c2.low,c3.low,
  u.tau.inv.low,u.tau1.inv.low,u.tau2.inv.low,
  b0.low,b1.low,a.low,ga0.low,ga.low,ga1.low,ga2.low,w.tau.inv.low,
  B1.high,B2.high,cp1.high,c0.high,c1.high,c2.high,c3.high,
  u.tau.inv.high,u.tau1.inv.high,u.tau2.inv.high,
  b0.high,b1.high,a.high,ga0.high,ga.high,ga1.high,ga2.high,w.tau.inv.high
)
dat1 <- as.data.frame(dat1)

## coverage probs (divide by n)
dat1$B1.cp        <- as.integer(dat1$B1       >= dat1$B1.low        & dat1$B1       <= dat1$B1.high)
dat1$B2.cp        <- as.integer(dat1$B2       >= dat1$B2.low        & dat1$B2       <= dat1$B2.high)
dat1$cp1.cp       <- as.integer(dat1$cp1      >= dat1$cp1.low       & dat1$cp1      <= dat1$cp1.high)
dat1$c0.cp        <- as.integer(dat1$c0       >= dat1$c0.low        & dat1$c0       <= dat1$c0.high)
dat1$c1.cp        <- as.integer(dat1$c1       >= dat1$c1.low        & dat1$c1       <= dat1$c1.high)
dat1$c2.cp        <- as.integer(dat1$c2       >= dat1$c2.low        & dat1$c2       <= dat1$c2.high)
dat1$c3.cp        <- as.integer(dat1$c3       >= dat1$c3.low        & dat1$c3       <= dat1$c3.high)
dat1$u.tau.inv.cp <- as.integer(dat1$u.sigma2 >= dat1$u.tau.inv.low & dat1$u.sigma2 <= dat1$u.tau.inv.high)
dat1$u.tau1.inv.cp<- as.integer(dat1$u1.sigma2>= dat1$u.tau1.inv.low& dat1$u1.sigma2<= dat1$u.tau1.inv.high)
dat1$u.tau2.inv.cp<- as.integer(dat1$u2.sigma2>= dat1$u.tau2.inv.low& dat1$u2.sigma2<= dat1$u.tau2.inv.high)
dat1$b0.cp        <- as.integer(dat1$beta0    >= dat1$b0.low        & dat1$beta0    <= dat1$b0.high)
dat1$b1.cp        <- as.integer(dat1$beta     >= dat1$b1.low        & dat1$beta     <= dat1$b1.high)
dat1$a.cp         <- as.integer(dat1$alpha    >= dat1$a.low         & dat1$alpha    <= dat1$a.high)
dat1$ga0.cp       <- as.integer(dat1$ga0      >= dat1$ga0.low       & dat1$ga0      <= dat1$ga0.high)
dat1$ga.cp        <- as.integer(dat1$ga       >= dat1$ga.low        & dat1$ga       <= dat1$ga.high)
dat1$ga1.cp       <- as.integer(dat1$ga1      >= dat1$ga1.low       & dat1$ga1      <= dat1$ga1.high)
dat1$ga2.cp       <- as.integer(dat1$ga2      >= dat1$ga2.low       & dat1$ga2      <= dat1$ga2.high)
dat1$w.tau.inv.cp <- as.integer(dat1$w.sigma2 >= dat1$w.tau.inv.low & dat1$w.sigma2 <= dat1$w.tau.inv.high)

cp <- c(
  sum(dat1$B1.cp)/n, sum(dat1$B2.cp)/n, sum(dat1$cp1.cp)/n, sum(dat1$c0.cp)/n,
  sum(dat1$c1.cp)/n, sum(dat1$c2.cp)/n, sum(dat1$c3.cp)/n, sum(dat1$u.tau.inv.cp)/n,
  sum(dat1$u.tau1.inv.cp)/n, sum(dat1$u.tau2.inv.cp)/n, sum(dat1$b0.cp)/n, sum(dat1$b1.cp)/n,
  sum(dat1$a.cp)/n, sum(dat1$ga0.cp)/n, sum(dat1$ga.cp)/n, sum(dat1$ga1.cp)/n, sum(dat1$ga2.cp)/n,
  sum(dat1$w.tau.inv.cp)/n
)

## final table
cbind(true, as.numeric(c(est1[2:19])), round(bias, 3), round(mse, 3), round(cp, 2))

