setwd("/Users/Shared/Windows/UCHealth/RA/Project/EPIC-CF/Analysis_Jiayuan/EPIC_Sim_Results/mixJM_020225")
setwd("/Users/Shared/Windows/UCHealth/RA/Project/EPIC-CF/Analysis_Jiayuan/EPIC_Sim_Results/mixJM_021125")
setwd("/Users/Shared/Windows/UCHealth/RA/Project/EPIC-CF/Analysis_Jiayuan/EPIC_Sim_Results/mixnewJM3_032225")

###########################################################################
# Read csv files
text <- list.files(pattern="mixJM.newresult3.")
num <- as.numeric(unlist(lapply(strsplit(text,'.',fixed=TRUE),function(x) x[[3]])))

data_frames <- lapply(num, function(i) {
  file_name <- paste0("mixJM.newresult3.", i, ".csv") 
  read.csv(file_name)
})

I=length(data_frames)
I

Flag<-rep(NA,I)
B1.mean<-rep(NA,I)
B2.mean<-rep(NA,I)
c10.mean<-rep(NA,I)
c20.mean<-rep(NA,I)
c1.mean<-rep(NA,I)
c2.mean<-rep(NA,I)
c3.mean<-rep(NA,I)
cp1.mean<-rep(NA,I)
pi1.mean<-rep(NA,I)
pi2.mean<-rep(NA,I)
pi1r.mean<-rep(NA,I)
pi2r.mean<-rep(NA,I)
#u1.mean<-rep(NA,I)
u1.tau.inv.mean<-rep(NA,I)
#u2.mean<-rep(NA,I)
u2.tau.inv.mean<-rep(NA,I)
cp1.mu.mean<-rep(NA,I)
cp1.tau.mean<-rep(NA,I)

b10.mean<-rep(NA,I)
b20.mean<-rep(NA,I)
b1.mean<-rep(NA,I)
b2.mean<-rep(NA,I)
a1.mean<-rep(NA,I)
a2.mean<-rep(NA,I)
ga10.mean<-rep(NA,I)
ga20.mean<-rep(NA,I)
ga11.mean<-rep(NA,I)
#w1.mean<-rep(NA,I)
w1.tau.inv.mean<-rep(NA,I)
#w2.mean<-rep(NA,I)
w2.tau.inv.mean<-rep(NA,I)

for(i in 1:I){ 
  Flag[i] <- ifelse(max(data_frames[[i]][,12], na.rm = TRUE)<1.3,1,0)
  B1.mean[i] <- data_frames[[i]][1,5] 
  B2.mean[i] <- data_frames[[i]][2,5] 
  c10.mean[i] <-data_frames[[i]][3,5] 
  c20.mean[i] <-data_frames[[i]][4,5] 
  c1.mean[i] <-data_frames[[i]][5,5] 
  c2.mean[i] <-data_frames[[i]][6,5] 
  c3.mean[i] <-data_frames[[i]][7,5] 
  cp1.mean[i] <-mean(data_frames[[i]][8:607,5])
  pi1.mean[i] <-data_frames[[i]][608,5] 
  pi2.mean[i] <-data_frames[[i]][609,5] 
  pi1r.mean[i] <-data_frames[[i]][610,5] 
  pi2r.mean[i] <-data_frames[[i]][611,5] 
  #u1.mean[i] <-mean(data_frames[[i]][812:1611,5])
  u1.tau.inv.mean[i] <-data_frames[[i]][612,5]
  #u2.mean[i] <-mean(data_frames[[i]][1612:2011,5])
  u2.tau.inv.mean[i] <-data_frames[[i]][613,5]
  cp1.mu.mean[i] <-data_frames[[i]][616,5]
  cp1.tau.mean[i] <-data_frames[[i]][617,5]
  
  b10.mean[i] <-data_frames[[i]][619,5] 
  b20.mean[i] <-data_frames[[i]][620,5] 
  b1.mean[i] <-data_frames[[i]][621,5] 
  b2.mean[i] <-data_frames[[i]][622,5] 
  a1.mean[i] <-data_frames[[i]][623,5] 
  a2.mean[i] <-data_frames[[i]][624,5] 
  ga10.mean[i] <-data_frames[[i]][625,5] 
  ga20.mean[i] <-data_frames[[i]][626,5] 
  ga11.mean[i] <-data_frames[[i]][627,5] 
  #w1.mean[i] <-mean(data_frames[[i]][2027:2426,5])
  w1.tau.inv.mean[i] <-data_frames[[i]][630,5]
  #w2.mean[i] <-mean(data_frames[[i]][2427:2826,5])
  w2.tau.inv.mean[i] <-data_frames[[i]][631,5]
}

Sim.results=cbind(Flag,B1.mean,B2.mean,cp1.mean,c10.mean,c20.mean,c1.mean,c2.mean,c3.mean,pi1.mean,pi2.mean,pi1r.mean,pi2r.mean,
                  u1.tau.inv.mean,u2.tau.inv.mean,cp1.mu.mean,cp1.tau.mean,
                  b10.mean,b20.mean,b1.mean,b2.mean,a1.mean,a2.mean,ga10.mean,ga20.mean,ga11.mean,w1.tau.inv.mean,w2.tau.inv.mean)
table(Flag)
Sim.results.1 <- subset(Sim.results,Flag==1)
round(colMeans(Sim.results.1),2)
round(colMeans(Sim.results),2)







setwd("/Users/Shared/Windows/UCHealth/RA/Project/EPIC-CF/Analysis_Jiayuan/EPIC_Sim_Results/mixnewJM3bu_031625")

###########################################################################
# Read csv files
text <- list.files(pattern="mixJM.newresult3bu.")
num <- as.numeric(unlist(lapply(strsplit(text,'.',fixed=TRUE),function(x) x[[3]])))

data_frames <- lapply(num, function(i) {
  file_name <- paste0("mixJM.newresult3bu.", i, ".csv") 
  read.csv(file_name)
})

I=length(data_frames)
I

Flag<-rep(NA,I)
B1.mean<-rep(NA,I)
B2.mean<-rep(NA,I)
c10.mean<-rep(NA,I)
c20.mean<-rep(NA,I)
c1.mean<-rep(NA,I)
c2.mean<-rep(NA,I)
c3.mean<-rep(NA,I)
cp1.mean<-rep(NA,I)
pi1.mean<-rep(NA,I)
pi2.mean<-rep(NA,I)
pi1r.mean<-rep(NA,I)
pi2r.mean<-rep(NA,I)
#u1.mean<-rep(NA,I)
u1.tau.inv.mean<-rep(NA,I)
#u2.mean<-rep(NA,I)
u2.tau.inv.mean<-rep(NA,I)
cp1.mu.mean<-rep(NA,I)
cp1.tau.mean<-rep(NA,I)

b10.mean<-rep(NA,I)
b20.mean<-rep(NA,I)
b1.mean<-rep(NA,I)
b2.mean<-rep(NA,I)
a.mean<-rep(NA,I)
ga10.mean<-rep(NA,I)
ga20.mean<-rep(NA,I)
ga11.mean<-rep(NA,I)
#w1.mean<-rep(NA,I)
w1.tau.inv.mean<-rep(NA,I)
#w2.mean<-rep(NA,I)
w2.tau.inv.mean<-rep(NA,I)

for(i in 1:I){ 
  Flag[i] <- ifelse(max(data_frames[[i]][,12], na.rm = TRUE)<1.3,1,0)
  B1.mean[i] <- data_frames[[i]][1,5] 
  B2.mean[i] <- data_frames[[i]][2,5] 
  c10.mean[i] <-data_frames[[i]][3,5] 
  c20.mean[i] <-data_frames[[i]][4,5] 
  c1.mean[i] <-data_frames[[i]][5,5] 
  c2.mean[i] <-data_frames[[i]][6,5] 
  c3.mean[i] <-data_frames[[i]][7,5] 
  cp1.mean[i] <-mean(data_frames[[i]][8:607,5])
  pi1.mean[i] <-data_frames[[i]][608,5] 
  pi2.mean[i] <-data_frames[[i]][609,5] 
  pi1r.mean[i] <-data_frames[[i]][610,5] 
  pi2r.mean[i] <-data_frames[[i]][611,5] 
  #u1.mean[i] <-mean(data_frames[[i]][812:1611,5])
  u1.tau.inv.mean[i] <-data_frames[[i]][612,5]
  #u2.mean[i] <-mean(data_frames[[i]][1612:2011,5])
  u2.tau.inv.mean[i] <-data_frames[[i]][613,5]
  cp1.mu.mean[i] <-data_frames[[i]][616,5]
  cp1.tau.mean[i] <-data_frames[[i]][617,5]
  
  b10.mean[i] <-data_frames[[i]][619,5] 
  b20.mean[i] <-data_frames[[i]][620,5] 
  b1.mean[i] <-data_frames[[i]][621,5] 
  b2.mean[i] <-data_frames[[i]][622,5] 
  a.mean[i] <-data_frames[[i]][623,5] 
  ga10.mean[i] <-data_frames[[i]][624,5] 
  ga20.mean[i] <-data_frames[[i]][625,5] 
  ga11.mean[i] <-data_frames[[i]][626,5] 
  #w1.mean[i] <-mean(data_frames[[i]][2027:2426,5])
  w1.tau.inv.mean[i] <-data_frames[[i]][629,5]
  #w2.mean[i] <-mean(data_frames[[i]][2427:2826,5])
  w2.tau.inv.mean[i] <-data_frames[[i]][630,5]
}

Sim.results=cbind(Flag,B1.mean,B2.mean,cp1.mean,c10.mean,c20.mean,c1.mean,c2.mean,c3.mean,pi1.mean,pi2.mean,pi1r.mean,pi2r.mean,
                  u1.tau.inv.mean,u2.tau.inv.mean,cp1.mu.mean,cp1.tau.mean,
                  b10.mean,b20.mean,b1.mean,b2.mean,a.mean,ga10.mean,ga20.mean,ga11.mean,w1.tau.inv.mean,w2.tau.inv.mean)
table(Flag)
Sim.results.1 <- subset(Sim.results,Flag==1)
round(colMeans(Sim.results.1),2)
round(colMeans(Sim.results),2)




setwd("/Users/Shared/Windows/UCHealth/RA/Project/EPIC-CF/Analysis_Jiayuan/EPIC_Sim_Results/mixnewJM2_031525")

###########################################################################
# Read csv files
text <- list.files(pattern="mixJM.newresult2.")
num <- as.numeric(unlist(lapply(strsplit(text,'.',fixed=TRUE),function(x) x[[3]])))

data_frames <- lapply(num, function(i) {
  file_name <- paste0("mixJM.newresult2.", i, ".csv") 
  read.csv(file_name)
})

I=length(data_frames)
I

Flag<-rep(NA,I)
B1.mean<-rep(NA,I)
B2.mean<-rep(NA,I)
c10.mean<-rep(NA,I)
c20.mean<-rep(NA,I)
c1.mean<-rep(NA,I)
c2.mean<-rep(NA,I)
c3.mean<-rep(NA,I)
cp1.mean<-rep(NA,I)
pi1.mean<-rep(NA,I)
pi2.mean<-rep(NA,I)
pi1r.mean<-rep(NA,I)
pi2r.mean<-rep(NA,I)
u1.mean<-rep(NA,I)
u1.tau.inv.mean<-rep(NA,I)
u2.mean<-rep(NA,I)
u2.tau.inv.mean<-rep(NA,I)
cp1.mu.mean<-rep(NA,I)
cp1.tau.mean<-rep(NA,I)

b10.mean<-rep(NA,I)
b20.mean<-rep(NA,I)
b1.mean<-rep(NA,I)
b2.mean<-rep(NA,I)
a.mean<-rep(NA,I)
ga10.mean<-rep(NA,I)
ga20.mean<-rep(NA,I)
ga11.mean<-rep(NA,I)
w1.mean<-rep(NA,I)
w1.tau.inv.mean<-rep(NA,I)
w2.mean<-rep(NA,I)
w2.tau.inv.mean<-rep(NA,I)

for(i in 1:I){ 
  Flag[i] <- ifelse(max(data_frames[[i]][,12], na.rm = TRUE)<1.3,1,0)
  B1.mean[i] <- data_frames[[i]][1,5] 
  B2.mean[i] <- data_frames[[i]][2,5] 
  c10.mean[i] <-data_frames[[i]][3,5] 
  c20.mean[i] <-data_frames[[i]][4,5] 
  c1.mean[i] <-data_frames[[i]][5,5] 
  c2.mean[i] <-data_frames[[i]][6,5] 
  c3.mean[i] <-data_frames[[i]][7,5] 
  cp1.mean[i] <-mean(data_frames[[i]][8:407,5])
  pi1.mean[i] <-data_frames[[i]][408,5] 
  pi2.mean[i] <-data_frames[[i]][409,5] 
  pi1r.mean[i] <-data_frames[[i]][410,5] 
  pi2r.mean[i] <-data_frames[[i]][411,5] 
  u1.mean[i] <-mean(data_frames[[i]][1212:1611,5])
  u1.tau.inv.mean[i] <-data_frames[[i]][2012,5]
  u2.mean[i] <-mean(data_frames[[i]][1612:2011,5])
  u2.tau.inv.mean[i] <-data_frames[[i]][2013,5]
  cp1.mu.mean[i] <-data_frames[[i]][2016,5]
  cp1.tau.mean[i] <-data_frames[[i]][2017,5]
  
  b10.mean[i] <-data_frames[[i]][2019,5] 
  b20.mean[i] <-data_frames[[i]][2020,5] 
  b1.mean[i] <-data_frames[[i]][2021,5] 
  b2.mean[i] <-data_frames[[i]][2022,5] 
  a.mean[i] <-data_frames[[i]][2023,5] 
  ga10.mean[i] <-data_frames[[i]][2024,5] 
  ga20.mean[i] <-data_frames[[i]][2025,5] 
  ga11.mean[i] <-data_frames[[i]][2026,5] 
  w1.mean[i] <-mean(data_frames[[i]][2027:2426,5])
  w1.tau.inv.mean[i] <-data_frames[[i]][2829,5]
  w2.mean[i] <-mean(data_frames[[i]][2427:2826,5])
  w2.tau.inv.mean[i] <-data_frames[[i]][2830,5]
}

Sim.results=cbind(Flag,B1.mean,B2.mean,cp1.mean,c10.mean,c20.mean,c1.mean,c2.mean,c3.mean,pi1.mean,pi2.mean,pi1r.mean,pi2r.mean,
                                     u1.tau.inv.mean,u1.mean,u2.tau.inv.mean,u2.mean,cp1.mu.mean,cp1.tau.mean,
                                     b10.mean,b20.mean,b1.mean,b2.mean,a.mean,ga10.mean,ga20.mean,ga11.mean,w1.tau.inv.mean,w1.mean,w2.tau.inv.mean,w2.mean)
table(Flag)
Sim.results.1 <- subset(Sim.results,Flag==1)
round(colMeans(Sim.results.1),2)
round(colMeans(Sim.results),2)
