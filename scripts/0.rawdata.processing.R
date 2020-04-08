### DATA PROCESSING ###
library(dplyr)


# set wd
setwd("~/Documents/MusData/Data")

#read data
KMdata <- read.csv2("/Users/pedronicolau/OccupancyAbundanceCalibration/data/joint_CRDATA.csv", stringsAsFactors = FALSE)
str(KMdata)
#get rid of NAs
KMdata1 <- filter(KMdata, species == "GRAASIDEMUS")

KMdata1$weight <- as.numeric(KMdata1$weight)
hist(KMdata1$weight)
unique(KMdata1$datetime)

number2date <- function(ndate)
{
  ndate2 <- as.character(ndate)
  ndatelist <- strsplit(ndate2,"")[[1]]
  
  leng <- length(ndatelist)
  asdate <- "20"
    while (leng > 1)
  {
  asdate <- paste0(asdate, ndatelist[(leng-1)],ndatelist[leng])
  leng <- leng-2
  asdate <- ifelse(leng>0,paste0(asdate,"-"),asdate)
                   
  }
  if(leng>0) asdate <- paste0(asdate, "0",ndatelist[leng])
  
  return(asdate)
}

KMdata1$date <- as.Date(sapply(KMdata1$datetime, number2date))

str(KMdata1)
# sort
#Remove useless vars
# CODE CAPTURE HISTORY
KM1 <- KMdata1
no_NAs <- length(KM1$Nindnum[is.na(KM1$Nindnum)==TRUE])

### create labels for the NAs ##
NA_label <- c()
for(i in 1:no_NAs) NA_label[i] <- paste0("UNMNA",i)

KM1$Nindnum[is.na(KM1$Nindnum)==TRUE] <- NA_label

# function to know if it's untagged
isitU <- function(code)
{
  code <- as.character(code)
  words <- strsplit(code,"")[[1]]
  word1 <- words[1]
  answer <- ifelse(word1=="U",TRUE,FALSE)
  return(answer)
}

## NON TAGGED ##
KM1$tagged <- 1
# if it is untagged, mark as 0
KM1$tagged[sapply(KM1$Nindnum,isitU)==TRUE] <- 0
KM1$month <- months(KM1$date)
unique(KM1$date)

# %% MANUAL CHECK %% #
#### UNTAGGED INDIVIDUALS WHICH WERE GIVEN SAME CODE ON SAME STATION ####
# UNME
nE <- nrow(KM1[KM1$Nindnum=="UNME",])
Elab <- c()
for(i in 1:nE) Elab[i] <- paste0("UNME",i)
KM1$Nindnum[KM1$Nindnum=="UNME"] <- Elab
# UNMJ
nJ <- nrow(KM1[KM1$Nindnum=="UNMJ",])
Jlab <- c()
for(i in 1:nJ) Jlab[i] <- paste0("UNMJ",i)
KM1$Nindnum[KM1$Nindnum=="UNMJ"] <- Jlab

### UNIQUE ID OF INDIVIDUALS ###
uniqueid <- distinct(KM1,year,station,Nindnum,month,transect)
uniqueid$id <- 1:nrow(uniqueid)
nrow(uniqueid)
# attach labels for the different individuals according to
KM2 <- left_join(KM1,uniqueid)
### RETRIEVE CAPTURE HISTORY ###
# get capture history
# the sum will tell us the capture history
# 1+0 = 1 "10"
# 0+2 = 2 "01"
# 1+2 = 3 "11"
idch <- aggregate(check~year+month+station+tagged+id,data=KM2, FUN=sum)
colnames(idch)[ncol(idch)] <- "category"
# attach capture history
KM3 <- idch
KM3$c1 <- ifelse(KM3$category==2,0,1)
KM3$c2 <- ifelse(KM3$category==1,0,1)

### weight and sex ###
KM0 <- left_join(KM1,idch) # original dataset with ids
sex01 <- function(x) ifelse(is.na(x),NA,ifelse(x=="F",0,1))
KM0$sex <- sapply(KM0$sex,sex01)

# WEIGHT #
Kweight <- aggregate(weight~id,data=KM0, FUN=mean.rm)

# add mean weight
KM4 <- left_join(KM3,Kweight,"id")

median.rm <- function(x) median(x,na.rm=TRUE)

Ksex <- aggregate(sex~id,data=KM0, FUN=median.rm)

KM5 <- left_join(KM4,Ksex)
head(KM5,50)
## trapping season ##
tseason <- distinct(KM0,year,month,date)
tseason2 <- arrange(tseason,date)
tseason3 <- tseason2[seq(1,nrow(tseason2)-1,2),]
tseason3$trapseason <- 1:nrow(tseason3)

KM6 <- arrange(left_join(KM5,tseason3),trapseason,station)
KM7 <- left_join(KM6,uniqueid) # add original id name
KM7$transect <- ifelse(KM7$transect=="MASOY","MASOY","PORSANGER")

write.csv2(KM7, "/Users/pedronicolau/OccupancyAbundanceCalibration/data/CR_processed.csv",sep = ",")
KM7$count <- 1
crcounts <- aggregate(count~trapseason+date+station,data=KM7,sum)
