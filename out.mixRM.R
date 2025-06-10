setwd("/Users/Shared/Windows/UCHealth/RA/Project/EPIC-CF/Analysis_Jiayuan/EPIC_Sim_Results/mixRM")

###########################################################################
# Read csv files
text <- list.files(pattern="mixRM.result.")
num <- as.numeric(unlist(lapply(strsplit(text,'.',fixed=TRUE),function(x) x[[3]])))

data_frames <- lapply(num, function(i) {
  file_name <- paste0("mixRM.result.", i, ".csv") 
  read.csv(file_name)
})

I=length(data_frames)

Flag<-rep(NA,I)
B11.mean<-rep(NA,I)
B12.mean<-rep(NA,I)
c10.mean<-rep(NA,I)
c20.mean<-rep(NA,I)
c1.mean<-rep(NA,I)
c2.mean<-rep(NA,I)
cp1.mean<-rep(NA,I)
pi1.mean<-rep(NA,I)
pi2.mean<-rep(NA,I)
u10.mean<-rep(NA,I)
u10.tau.inv.mean<-rep(NA,I)
u20.mean<-rep(NA,I)
u20.tau.inv.mean<-rep(NA,I)
u11.mean<-rep(NA,I)
u11.tau.inv.mean<-rep(NA,I)
u12.mean<-rep(NA,I)
u12.tau.inv.mean<-rep(NA,I)
u21.mean<-rep(NA,I)
u21.tau.inv.mean<-rep(NA,I)
cp1.mu.mean<-rep(NA,I)
cp1.tau.mean<-rep(NA,I)

for(i in 1:I){ 
  Flag[i] <- ifelse(max(data_frames[[i]][,12], na.rm = TRUE)<1.1,1,0)
  B11.mean[i] <- data_frames[[i]][1,5] 
  B12.mean[i] <- data_frames[[i]][2,5] 
  c10.mean[i] <-data_frames[[i]][3,5] 
  c20.mean[i] <-data_frames[[i]][4,5] 
  c1.mean[i] <-data_frames[[i]][5,5] 
  c2.mean[i] <-data_frames[[i]][6,5] 
  cp1.mean[i] <-mean(data_frames[[i]][7:406,5])
  pi1.mean[i] <-data_frames[[i]][407,5] 
  pi2.mean[i] <-data_frames[[i]][408,5] 
  u10.mean[i] <-mean(data_frames[[i]][809:1208,5])
  u10.tau.inv.mean[i] <-data_frames[[i]][2809,5]
  u20.mean[i] <-mean(data_frames[[i]][1209:1608,5])
  u20.tau.inv.mean[i] <-data_frames[[i]][2810,5]
  u11.mean[i] <-mean(data_frames[[i]][1609:2008,5])
  u11.tau.inv.mean[i] <-data_frames[[i]][2811,5]
  u12.mean[i] <-mean(data_frames[[i]][2009:2408,5])
  u12.tau.inv.mean[i] <-data_frames[[i]][2812,5]
  u21.mean[i] <-mean(data_frames[[i]][2409:2808,5])
  u21.tau.inv.mean[i] <-data_frames[[i]][2813,5]
  cp1.mu.mean[i] <-data_frames[[i]][2819,5]
  cp1.tau.mean[i] <-data_frames[[i]][2820,5]
}

Sim.results=cbind(Flag,B11.mean,B12.mean,cp1.mean,c10.mean,c20.mean,c1.mean,c2.mean,pi1.mean,pi2.mean,
                  u10.tau.inv.mean,u10.mean,u20.tau.inv.mean,u20.mean,u11.tau.inv.mean,u11.mean,u12.tau.inv.mean,u12.mean,u21.tau.inv.mean,u21.mean,
                  cp1.mu.mean,cp1.tau.mean)
table(Flag)
Sim.results.1 <- subset(Sim.results,Flag==1)
round(colMeans(Sim.results.1),2)
round(colMeans(Sim.results),2)