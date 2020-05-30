################################################################# 
# Vladimir Chlouba, The Ohio State University
# chlouba.1@osu.edu
# May 2020
#################################################################
# Disney Data Preparation
#################################################################

# loading the usual R packages
library(foreign)
library(ggplot2)
library(stargazer)
library(MASS)
library(ordinal)
library(QuantPsyc)
library(reshape2)
library(plyr)
library(boot)
library(texreg)
library(arm)
library(plyr)
library(nnet)
library(Amelia)
library(effects)
library(Hmisc)
library(gridExtra)
library(MatchIt)
library(rgenoud)
library(Zelig)
library(multiwayvcov)
library(interplot)
library(rgdal)
library(dplyr)
library(stargazer)
library(lfe)
library(interplot)
library(cobalt)
library(fixest)

#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~

# loading the datasets
kilimanjaro <- read.csv("C:/Users/lord_/Desktop/The Ohio State University/other/Code Boot Camp/final project/data/Disney/kilimanjaro_safaris.csv")

metadata <- read.csv("C:/Users/lord_/Desktop/The Ohio State University/other/Code Boot Camp/final project/data/Disney/metadata.csv")
#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~

# selecting metadata of interest
my.vars <- c('DATE', 'HOLIDAYPX', 'HOLIDAY', 'HOLIDAYM', 'AKHOURS', 'AKHOURSEMH', 'AKEMHMORN',
             'AKEMHEVE', 'inSession', 'inSession_Florida', 'inSession_sqrt_WDW', 'WDWTICKETSEASON',
             'WDWevent', 'AKevent', 'WDWMAXTEMP', 'WDWMINTEMP')

metadata <- metadata[, my.vars]

# removing percentage sign from selected variables
metadata$inSession = as.numeric(gsub("[\\%,]", "", metadata$inSession))
metadata$inSession_Florida = as.numeric(gsub("[\\%,]", "", metadata$inSession_Florida))
metadata$inSession_sqrt_WDW = as.numeric(gsub("[\\%,]", "", metadata$inSession_sqrt_WDW))


# removing nonsensical values from waiting times
kilimanjaro$SPOSTMIN[kilimanjaro$SPOSTMIN==-999] <- NA


# combining stanby posted wait time and actual wait time to produce one measure
kilimanjaro$wait_time <- rowSums(kilimanjaro[,c("SPOSTMIN", "SACTMIN")], na.rm=TRUE)


# calculating average wait time for each day
data_avg <- ddply(kilimanjaro,.(DATE),summarise,wait_time=mean(wait_time,na.rm=T))


# sorting data by ascending date
data <- data_avg[order(as.Date(data_avg$DATE, format="%m/%d/%Y")),] # THIS IS VERY IMPORTANT


# merging wait times with metadata and again sorting by ascending date
final <- merge(data, unique(metadata), by = 'DATE')
final <- final[order(as.Date(final$DATE, format="%m/%d/%Y")),]


#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~
# saving the resulting dataset, check what is saved
write.csv(file="C:/Users/lord_/Desktop/The Ohio State University/other/Code Boot Camp/final project/data/Disney/kilimanjaro_covs.csv", 
          x=final)

#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~

