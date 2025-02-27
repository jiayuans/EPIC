dirg <- "C:/UCHealth/RA/Project/EPIC-CF/Analysis_Jiayuan/EPIC/"
setwd(dirg)
library(dplyr)


#############################################################################
##################use matrix data #################
##################read in data for PA+ #################
##################Reading in main dataset##########################
#################################################################

dirg <- "Z:/EJCStudents/ShiJ/EPIC-CF/Data/Source/"
dat.pa <- read.csv(file=paste(dirg,"/Data-PA-cohort.csv",sep=""))
dat.pa <- dat.pa[,-1]
head(dat.pa, n=10)

dat.pa_f1 <-aggregate(dat.pa$VisitAge, by=list(dat.pa$cffidno),
                      FUN=min, na.rm=TRUE)
names(dat.pa_f1) <- c('cffidno','min.aage')
head(dat.pa_f1)

dat.pa_f2 <-aggregate(dat.pa$VisitAge, by=list(dat.pa$cffidno),
                      FUN=max, na.rm=TRUE)
names(dat.pa_f2) <- c('cffidno','max.aage')
head(dat.pa_f2)

dat.pa_f <- merge(dat.pa_f1, dat.pa_f2, by=c('cffidno'))
dat.pa_f$fage.a <-  dat.pa_f$max.aage - dat.pa_f$min.aage


#############################################################################
##################use matrix data #################
##################read in data for PEX #################
##################Reading in main dataset##########################
#################################################################

dat.pe0 <- read.csv(file=paste(dirg,"/Data-multiple-final-cohort-EPICstart-Bt-LengthPEx.csv",sep=""))
dat.pe0 <- dat.pe0[with(dat.pe0, order(cffidno, tstart)), ]
length(unique(dat.pe0$cffidno)) 

dat.pe <- dat.pe0[, c(1,5:7,21,24,25)]
summary(dat.pe)
head(dat.pe, n=10)

dat.pe_f1 <-aggregate(dat.pe$tstart, by=list(dat.pe$cffidno),
                      FUN=min, na.rm=TRUE)
names(dat.pe_f1) <- c('cffidno','min.eage')
head(dat.pe_f1)
nrow(dat.pe_f1)
dat.pe_f2 <-aggregate(dat.pe$tend, by=list(dat.pe$cffidno),
                      FUN=max, na.rm=TRUE)
names(dat.pe_f2) <- c('cffidno','max.eage')
head(dat.pe_f2)

dat.pe_f <- merge(dat.pe_f1, dat.pe_f2, by=c('cffidno'))
dat.pe_f$fage.e <-  dat.pe_f$max.eage - dat.pe_f$min.eage


dat_f <- merge(dat.pa_f, dat.pe_f, by='cffidno')
dat_f$min.age <- pmax(dat_f$min.aage, dat_f$min.eage, na.rm = TRUE)
dat_f$max.age <- pmin(dat_f$max.aage, dat_f$max.eage, na.rm = TRUE)
dat_f$fage <-  dat_f$max.age - dat_f$min.age

dat_f1 <- dat_f[,c('cffidno',"max.age","min.age","fage")]
dat.pa1 <- merge(dat.pa,dat_f1,by="cffidno")
dat.pa2 <- subset(dat.pa1, VisitAge>=min.age & VisitAge<=max.age)

dat.pe1 <- merge(dat.pe,dat_f1,by="cffidno")
dat.pe2 <- subset(dat.pe1, tstart>=min.age & tend<=max.age)

# minimum fu at least 5
dat.pa_sub <- subset(dat.pa2, fage>=5) 
length(unique(dat.pa_sub$cffidno)) #1415

dat.pe_sub <- subset(dat.pe2, fage>=5) 
length(unique(dat.pe_sub$cffidno)) #1415



dat.pa_sub1 <- dat.pa_sub[!duplicated(dat.pa_sub$cffidno,dat.pa_sub$min.age,dat.pa_sub$max.age), ]
first.t<-dat.pa_sub1$min.age
last.t<-dat.pa_sub1$max.age

dat <- as.data.frame(cbind(first.t,last.t))
dat$fu <- dat$last.t -dat$first.t

# Assuming your data frame is called `df` and you want to sort by `column_name`
top_dat <- dat %>%
  arrange(desc(fu)) %>%
  head(400)

first.tt<-top_dat$first.t
last.tt<-top_dat$last.t

lt <- cbind(first.tt,last.tt)
write.csv(lt,"long.data_unif.csv")

