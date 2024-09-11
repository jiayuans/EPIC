setwd("C:/UCHealth/RA/Project/EPIC-CF/Analysis_Jiayuan/Result/Simulation_output/long")

###########################################################################
# Read csv files
text <- list.files(pattern="long.result.")
num <- as.numeric(unlist(lapply(strsplit(text,'.',fixed=TRUE),function(x) x[[3]])))

data_frames <- lapply(num, function(i) {
  file_name <- paste0("long.result.", i, ".csv") 
  read.csv(file_name)
})

I=length(data_frames)

Flag<-rep(NA,I)
B1.mean<-rep(NA,I)
B2.mean<-rep(NA,I)
B3.mean<-rep(NA,I)
c0.mean<-rep(NA,I)
c1.mean<-rep(NA,I)
c2.mean<-rep(NA,I)
c3.mean<-rep(NA,I)
c4.mean<-rep(NA,I)
cp1.mean<-rep(NA,I)
cp2.mean<-rep(NA,I)
u.mean<-rep(NA,I)
u.tau.inv.mean<-rep(NA,I)
cp1.mu.mean<-rep(NA,I)
cp1.tau.mean<-rep(NA,I)
cp2.temp.mean<-rep(NA,I)

for(i in 1:I){ 
Flag[i] <- ifelse(max(data_frames[[i]][,12])<1.1,1,0)
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
u.mean[i] <-mean(data_frames[[i]][12:411,5])
}

Sim.results=cbind(Flag,B1.mean,B2.mean,B3.mean,cp1.mean,cp2.mean,c0.mean,c1.mean,c2.mean,c3.mean,c4.mean,u.tau.inv.mean,u.mean)
table(Flag)
Sim.results.1 <- subset(Sim.results,Flag==1)
round(colMeans(Sim.results.1),2)
round(colMeans(Sim.results),2)

#> round(colMeans(Sim.results.1),2)
#Flag        B1.mean        B2.mean        B3.mean       cp1.mean       cp2.mean        c0.mean        c1.mean        c2.mean        c3.mean        c4.mean u.tau.inv.mean 
#1.00          -0.23           0.14           0.38           3.92          14.96          -5.09           0.07           0.19           0.12           0.16           2.78 
#u.mean 
#0.00 
#> round(colMeans(Sim.results),2)
#Flag        B1.mean        B2.mean        B3.mean       cp1.mean       cp2.mean        c0.mean        c1.mean        c2.mean        c3.mean        c4.mean u.tau.inv.mean 
#0.07          -0.12           0.17           0.38           4.33          16.59          -4.79           0.13           0.14           0.10           0.06           2.54 
#u.mean 
#0.00 

#c0=-4.44 
#c1=0.10 
#c2=0.13 
#c3=0.08
#c4=0.08
#cp1.true=4.6
#cp2.true=14.4
#u.tau.inv=2.56
