setwd("C:/Users/jiayu/OneDrive/Desktop/Output")

###########################################################################
# Read csv files
text <- list.files(pattern="rec0.result.")
num <- as.numeric(unlist(lapply(strsplit(text,'.',fixed=TRUE),function(x) x[[3]])))

data_frames <- lapply(num, function(i) {
  file_name <- paste0("rec0.result.", i, ".csv") 
  read.csv(file_name)
})

I=length(data_frames)

Flag<-rep(NA,I)
b0.mean<-rep(NA,I)
b1.mean<-rep(NA,I)
a.mean<-rep(NA,I)
ph.mean<-rep(NA,I)
v.mean<-rep(NA,I)

for(i in 1:I){ 
Flag[i] <- ifelse(max(data_frames[[i]][,12])<1.1,1,0)
b0.mean[i] <-data_frames[[i]][1,5] 
b1.mean[i] <-data_frames[[i]][2,5] 
a.mean[i] <-data_frames[[i]][3,5] 
ph.mean[i] <-data_frames[[i]][4,5] 
v.mean[i] <-mean(data_frames[[i]][5:404,5])
}

Sim.results=cbind(Flag,b0.mean,b1.mean,a.mean,ph.mean,v.mean)
table(Flag)
Sim.results.1 <- subset(Sim.results,Flag==1)
round(colMeans(Sim.results.1),2)
round(colMeans(Sim.results),2)
#This result looks good.
#   Flag b0.mean b1.mean  a.mean ph.mean  v.mean 
#1.00   -1.36    0.26    1.73    0.50    1.00 
#poisson.d(alpha=1.73,beta=0.26,beta0=-1.34,x=X1,ph=.5,TTei=tt)