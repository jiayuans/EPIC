setwd("C:/Users/jiayu/OneDrive/Desktop/Output")

###########################################################################
# Read csv files
text <- list.files(pattern="rec.result.")
num <- as.numeric(unlist(lapply(strsplit(text,'.',fixed=TRUE),function(x) x[[3]])))

data_frames <- lapply(num, function(i) {
  file_name <- paste0("rec.result.", i, ".csv") 
  read.csv(file_name)
})

I=length(data_frames)

Flag<-rep(NA,I)
b0.mean<-rep(NA,I)
b1.mean<-rep(NA,I)
a.mean<-rep(NA,I)
ga.mean<-rep(NA,I)
u.tau.inv.mean<-rep(NA,I)
w.tau.inv.mean<-rep(NA,I)
ga.mean<-rep(NA,I)
u.mean<-rep(NA,I)
v.mean<-rep(NA,I)
w.mean<-rep(NA,I)
u.tau.mean<-rep(NA,I)
w.tau.mean<-rep(NA,I)

for(i in 1:I){ 
Flag[i] <- ifelse(max(data_frames[[i]][,12])<1.1,1,0)
b0.mean[i] <-data_frames[[i]][1,5] 
b1.mean[i] <-data_frames[[i]][2,5] 
a.mean[i] <-data_frames[[i]][3,5] 
ga.mean[i] <-data_frames[[i]][4,5] 
u.tau.inv.mean[i] <-data_frames[[i]][5,5] 
w.tau.inv.mean[i] <-data_frames[[i]][6,5] 
u.mean[i] <-mean(data_frames[[i]][7:406,5])
v.mean[i] <-mean(data_frames[[i]][407:806,5])
w.mean[i] <-mean(data_frames[[i]][807:1206,5])
u.tau.mean[i] <-data_frames[[i]][1207,5] 
w.tau.mean[i] <-data_frames[[i]][1208,5]
}

Sim.results=cbind(Flag,b0.mean,b1.mean,a.mean,ga.mean,u.tau.inv.mean,w.tau.inv.mean,u.mean,v.mean,w.mean,u.tau.mean,w.tau.mean)
table(Flag)
Sim.results.1 <- subset(Sim.results,Flag==1)
round(colMeans(Sim.results.1),2)
round(colMeans(Sim.results),2)
#This result not looks good.
#   Flag b0.mean b1.mean  a.mean ph.mean  v.mean 
#1.00   -1.36    0.26    1.73    0.50    1.00 
#poisson.d(alpha=1.78,beta=0.23,beta0=-4.32,x=X1,ga=.25,TTei=tt)