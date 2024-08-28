setwd("C:/UCHealth/RA/Project/EPIC-CF/Analysis_Jiayuan/Result/Simulation_output")

filenames = list.files(path=getwd(),pattern="R.out.796040.")
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
