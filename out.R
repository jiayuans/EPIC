setwd("C:/UCHealth/RA/Project/EPIC-CF/Analysis_Jiayuan/Result/Simulation_output")

filenames = list.files(path=getwd(),pattern="R.out.796099.")
df <- matrix(NA,100,18)

for(i in 1:100){ 
  dat <- read.delim(filenames[i])
  nrow <- nrow(dat)
  char <- dat[c((nrow-5):nrow),]
  
  # Extract the values (excluding the row index)
  values1 <- strsplit(trimws(char[2]), "\\s+")[[1]][-1]
  values2 <- strsplit(trimws(char[4]), "\\s+")[[1]][-1]
  values3 <- strsplit(trimws(char[6]), "\\s+")[[1]][-1]
  
  # Convert values to numeric
  numeric_values <- as.numeric(c(values1,values2,values3))
  
  # Create the data frame
  df[i,] <- t(numeric_values)
  
}

# Extract the column names
column_names1 <- strsplit(trimws(char[1]), "\\s+")[[1]]
column_names2 <- strsplit(trimws(char[3]), "\\s+")[[1]]
column_names3 <- strsplit(trimws(char[5]), "\\s+")[[1]]
df <- as.data.frame(df)
colnames(df) <- c(column_names1,column_names2,column_names3)

# Print the data frame
print(df)
round(colMeans(df),2)
write.csv(df, "result_combined_n100.csv")


###########################################################################
# Read csv files
I=30
data_frames <- lapply(c(0:29), function(i) {
  file_name <- paste0("result.", i, ".csv") 
  read.csv(file_name)
})

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

b0.mean<-rep(NA,I)
b1.mean<-rep(NA,I)
b2.mean<-rep(NA,I)
b3.mean<-rep(NA,I)
a.mean<-rep(NA,I)
v.mean<-rep(NA,I)
ga.mean<-rep(NA,I)
w.mean<-rep(NA,I)
w.tau.inv.mean<-rep(NA,I)


for(i in 1:30){ 
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
print(Sim.results)
round(colMeans(Sim.results),2)
write.csv(Sim.results, "result_combined_n100_unif.csv")
