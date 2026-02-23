setwd("/Volumes/dept/SPH/SPH-BIOS/EJCStudents/ShiJ/EPIC-CF/Simulation/mixRM")
setwd("/Users/Shared/Windows/UCHealth/RA/Project/EPIC-CF/Analysis_Jiayuan/EPIC_Sim_Results/mixRM2_020725_ok")
setwd("/Users/Shared/Windows/UCHealth/RA/Project/EPIC-CF/Analysis_Jiayuan/EPIC_Sim_Results/mixRM1_020725")

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
b10.mean<-rep(NA,I)
b20.mean<-rep(NA,I)
b1.mean<-rep(NA,I)
b2.mean<-rep(NA,I)
a1.mean<-rep(NA,I)
a2.mean<-rep(NA,I)
ph1.mean<-rep(NA,I)
ph2.mean<-rep(NA,I)
pi1.mean<-rep(NA,I)
pi2.mean<-rep(NA,I)
ga10.mean<-rep(NA,I)
ga20.mean<-rep(NA,I)
ga11.mean<-rep(NA,I)
z.mean<-rep(NA,I)
v1.mean<-rep(NA,I)
v2.mean<-rep(NA,I)

for(i in 1:I){ 
  Flag[i] <- ifelse(max(data_frames[[i]][,12], na.rm = TRUE)<1.1,1,0)
  b10.mean[i] <-data_frames[[i]][1,5] 
  b20.mean[i] <-data_frames[[i]][2,5] 
  b1.mean[i] <-data_frames[[i]][3,5] 
  b2.mean[i] <-data_frames[[i]][4,5] 
  a1.mean[i] <-data_frames[[i]][5,5] 
  a2.mean[i] <-data_frames[[i]][6,5] 
  ph1.mean[i] <-data_frames[[i]][7,5] 
  ph2.mean[i] <-data_frames[[i]][8,5] 
  pi1.mean[i] <-data_frames[[i]][9,5] 
  pi2.mean[i] <-data_frames[[i]][10,5] 
  z.mean[i] <-mean(data_frames[[i]][11:410,5])
  v1.mean[i] <-mean(data_frames[[i]][411:810,5])
  v2.mean[i] <-mean(data_frames[[i]][811:1210,5])
}

Sim.results=cbind(Flag,b10.mean,b20.mean,b1.mean,b2.mean,a1.mean,a2.mean,pi1.mean,pi2.mean,ph1.mean,ph2.mean,z.mean,v1.mean,v2.mean)
table(Flag)
Sim.results.1 <- subset(Sim.results,Flag==1)
round(colMeans(Sim.results.1),2)
round(colMeans(Sim.results),2)