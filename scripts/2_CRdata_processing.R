### DATA PROCESSING ###
library(dplyr)

#read data
#KMdata <- read.csv2("/Users/pedronicolau/OccupancyAbundanceCalibration/data/joint_CRDATA.csv", stringsAsFactors = FALSE)
KMdata <- read.csv2("C:/Eivind/GitProjects/OccupancyAbundanceCalibration/data/joint_CRDATA.csv", stringsAsFactors = FALSE)
?read.csv2
#get rid of NAs
KMdata1 <- KMdata

#KMdata1$weight <- as.numeric(KMdata1$weight) #this introduces a lot of NA's

sum(is.na(KMdata1$weight))

#replacing "lesser than" notation with numeric weights
KMdata1$weight[KMdata1$weight=="<12"] <- 11
KMdata1$weight[KMdata1$weight=="<10"] <- 10
KMdata1$weight[KMdata1$weight==">10"] <- 10

unique(KMdata1$weight)

#make NA's a character to be able to run nchar
KMdata1$weight[is.na(KMdata1$weight)] <- "mi"

for(i in 1:length(KMdata1$weight)){
if(nchar(KMdata1$weight[i])>2) { 
  KMdata1$weight[i] <- strsplit(KMdata1$weight[i],",")[[1]][1]
}}

# set NA's back to NA
KMdata1$weight[KMdata1$weight=="mi"] <- NA
KMdata1$weight <- as.numeric(KMdata1$weight)

unique(KMdata1$datetime)

ndate <- KMdata1$datetime[5]
number2date <- function(ndate)
  # converts weird date format to proper date format
{
  ndate2 <- as.character(ndate)
  ndatelist <- strsplit(ndate2,"")[[1]]
  
  leng <- length(ndatelist)
  asdate <- "20"
    while (leng > 1) # runs until condition is satisfied
  {
  asdate <- paste0(asdate, ndatelist[(leng-1)],ndatelist[leng])
  leng <- leng-2
  asdate <- ifelse(leng>0,paste0(asdate,"-"),asdate)
  }
  if(leng>0) asdate <- paste0(asdate, "0",ndatelist[leng])
  
  return(asdate)
}

# apply vectorial transformation to date
KMdata1$date <- as.Date(sapply(KMdata1$datetime, FUN=number2date))

# CODE CAPTURE HISTORY
tseason <- distinct(KMdata1,year,date)
tseason2 <- arrange(tseason,date)
tseason2$trapseason <- sort(rep.int(1:(nrow(tseason2)/2),2))

# add trap season
KM1 <- left_join(KMdata1,tseason2)

# fix names
KM1$species[KM1$species=="R\303\230YSKATT"] <- "ROYSKATT"
KM1$species[KM1$species=="SN\330MUS"] <- "SNOMUS"

# how many individuals are not labeled
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
unique(KM1$Nindnum[KM1$tagged==0])

KM1$tagged <- 1
# if it is untagged, mark as 0
KM1$tagged[sapply(KM1$Nindnum,isitU)==TRUE] <- 0

# MAKE SURE EACH UNMARKED HAS ITS OWN LABEL
# AN UNMARKED IS CONSIDERED TO HAVE THE FIRST LETTER AS U
no_Us <- nrow(KM1[KM1$tagged==0,])
NA_Ulabel <- c()
for(i in 1:no_Us) NA_Ulabel[i] <- paste0("UNM",i)
KM1$Nindnum[KM1$tagged==0] <- NA_Ulabel

# get month
KM1$month <- months(KM1$date)

table(KM1$weight[KM1$tagged==0])
# %% MANUAL CHECK %% #
#### UNTAGGED INDIVIDUALS WHICH WERE GIVEN SAME CODE ON SAME STATION ####
# # UNME
# nE <- nrow(KM1[KM1$Nindnum=="UNME",])
# Elab <- c()
# for(i in 1:nE) Elab[i] <- paste0("UNME",i)
# KM1$Nindnum[KM1$Nindnum=="UNME"] <- Elab
# 
# # UNMJ
# nJ <- nrow(KM1[KM1$Nindnum=="UNMJ",])
# Jlab <- c()
# for(i in 1:nJ) Jlab[i] <- paste0("UNMJ",i)
# KM1$Nindnum[KM1$Nindnum=="UNMJ"] <- Jlab

KM1.2 <- KM1

#### repeated observation ####
### UNIQUE ID OF INDIVIDUALS ###
uniqueid <- distinct(KM1.2,year,station,Nindnum,trapseason,transect,species)

uniqueid$id <- 1:nrow(uniqueid)
# attach labels for the different individuals according to
KM2 <- left_join(KM1.2,uniqueid)

### RETRIEVE CAPTURE HISTORY ###
# get capture history
# the sum will tell us the capture history
# 1+0 = 1 "10"
# 0+2 = 2 "01"
# 1+2 = 3 "11"

# THERE ARE 5 FAKE INDIVIDUALS ? 
idch <- aggregate(check~year+trapseason+station+tagged+id+species,data=KM2, FUN=sum)
# there are 5 missing individuals
nrow(idch)
nrow(uniqueid)

# KMdata1[KMdata$Nindnum=="0007D60D46",]
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
median.rm <- function(x) median(x,na.rm=TRUE)
Kweight <- aggregate(weight~id,data=KM0, FUN=median.rm)

# add mean weight
KM4 <- left_join(KM3,Kweight,"id")

# add median sex
Ksex <- aggregate(sex~id,data=KM0, FUN=median.rm)

KM5 <- left_join(KM4,Ksex)

KM6 <- left_join(KM5,uniqueid) # add original id name
KM6$transect <- ifelse(KM6$transect=="MASOY","MASOY","PORSANGER")
KM7 <- left_join(KM6,tseason2[seq(1,nrow(tseason2)-1,2),])
KM7$julian <- julian(as.Date(KM7$date), origin=as.Date("2018-06-15"))
KM7$weekofyear <- as.numeric(strftime(as.Date(KM7$date), format = "%V"))
table(KM7$species)

write.csv2(KM7, "C:/Eivind/GitProjects/OccupancyAbundanceCalibration/data/CR_processed.csv")


KM8 <- filter(KM7, !(species%in% c("ROYSKATT","SNOMUS")))
KM8$count <- 1
crcounts <- aggregate(count~trapseason+date+station,data=KM8,sum)
summary(KM7)

unique(KM7$date)
