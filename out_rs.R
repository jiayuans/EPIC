#setwd("C:/UCHealth/RA/Project/EPIC-CF/Analysis_Jiayuan/Result/Simulation_output")
setwd("C:/Users/jiayu/OneDrive/Desktop/Test_rs")

###########################################################################
# Read csv files
text <- list.files(pattern="result.")
text1 <- unlist(lapply(strsplit(text,'.',fixed=TRUE),function(x) x[[1]]))
ind <- which(text1=="result")
num <- as.numeric(unlist(lapply(strsplit(text[ind],'.',fixed=TRUE),function(x) x[[2]])))

data_frames <- lapply(num, function(i) {
  file_name <- paste0("result.", i, ".csv") 
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

for(i in 1:I){ 
Flag[i] <- ifelse(max(data_frames[[i]][,12])<2,1,0)
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
}

Sim.results=cbind(Flag,B1.mean,B2.mean,B3.mean,cp1.mean,cp2.mean,c0.mean,c1.mean,c2.mean,c3.mean,c4.mean,u.tau.inv.mean,
                  u.tau1.inv.mean,u.tau2.inv.mean,u.tau3.inv.mean,
               b0.mean,b1.mean,a.mean,ga.mean,ga1.mean,ga2.mean,ga3.mean,w.tau.inv.mean)
table(Flag)
Sim.results.1 <- subset(Sim.results,Flag==1)
round(colMeans(Sim.results.1),2)
round(colMeans(Sim.results),2)

# not match the true results for some parameters, need to correct
#> round(colMeans(Sim.results.1),2)
#Flag         B1.mean         B2.mean         B3.mean        cp1.mean        cp2.mean         c0.mean         c1.mean         c2.mean 
#1.00           -0.01           -0.04            0.22            6.83           14.53           -4.05            0.10           -0.01 
#c3.mean         c4.mean  u.tau.inv.mean u.tau1.inv.mean u.tau2.inv.mean u.tau3.inv.mean         b0.mean         b1.mean          a.mean 
#0.13            0.18            0.71            1.02            1.01            1.01           -4.49            0.21            1.81 
#ga.mean        ga1.mean        ga2.mean        ga3.mean  w.tau.inv.mean 
#0.99            0.01            1.15            0.98            0.32 
#> round(colMeans(Sim.results),2)
#Flag         B1.mean         B2.mean         B3.mean        cp1.mean        cp2.mean         c0.mean         c1.mean         c2.mean 
#0.10           -0.01            0.00            0.22            6.87           14.52           -4.14            0.11            0.00 
#c3.mean         c4.mean  u.tau.inv.mean u.tau1.inv.mean u.tau2.inv.mean u.tau3.inv.mean         b0.mean         b1.mean          a.mean 
#0.11            0.17            0.72            1.05            0.98            1.03           -4.53            0.27            1.80 
#ga.mean        ga1.mean        ga2.mean        ga3.mean  w.tau.inv.mean 
#0.08            0.08            1.12            1.00            0.44 
