dirg <- "C:/UCHealth/RA/Project/EPIC-CF/Analysis_Jiayuan/EPIC/"
setwd(dirg)
library(dplyr)
##################################################################
##    Functions to Read data
## 
##################################################################
##dat.pa <- read.csv(file=paste(dirg,"/Data-PA-cohort.csv",sep=""))
##dat.pa <- dat.pa[,-1]

##dat.pa1 <-aggregate(dat.pa$VisitAge, by=list(dat.pa$cffidno),
##                 FUN=max, na.rm=TRUE)
##names(dat.pa1) <- c('cffidno','age.max')
##
##dat.pa2 <-aggregate(dat.pa$age.min, by=list(dat.pa$cffidno),
##                 FUN=max, na.rm=TRUE)
##names(dat.pa2) <- c('cffidno','age.min')

##first.t<-dat.pa2$age.min
##last.t<-dat.pa1$age.max
##first.tt<-first.t[c(101:300,1001:1200)]
##last.tt<-last.t[c(101:300,1001:1200)]

##tt <- cbind(first.tt,last.tt)
##write.csv(tt,"long.time.csv")

dat.pa <- read.csv(file=paste("C:/UCHealth/RA/Project/EPIC-CF/Data/Source","/Data-PA-cohort.csv",sep=""))
dat.pa <- dat.pa[,-1]

sexf <- dat.pa[!duplicated(dat.pa$cffidno,dat.pa$sexf),5]

dat.pa1 <-aggregate(dat.pa$VisitAge, by=list(dat.pa$cffidno),
                    FUN=max, na.rm=TRUE)
names(dat.pa1) <- c('cffidno','age.max')

dat.pa2 <-aggregate(dat.pa$age.min, by=list(dat.pa$cffidno),
                    FUN=max, na.rm=TRUE)
names(dat.pa2) <- c('cffidno','age.min')

first.t<-dat.pa2$age.min
last.t<-dat.pa1$age.max

dat <- as.data.frame(cbind(first.t,last.t))
dat$fu <- dat$last.t -dat$first.t


# Assuming your data frame is called `df` and you want to sort by `column_name`
top_500_dat <- dat %>%
  arrange(desc(fu)) %>%
  head(500)

first.tt<-top_500_dat$first.t
last.tt<-top_500_dat$last.t

lt <- cbind(first.tt,last.tt)
write.csv(lt,"long.data_new500.csv")

