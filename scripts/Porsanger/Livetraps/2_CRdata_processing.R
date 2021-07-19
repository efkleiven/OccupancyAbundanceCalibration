### DATA PROCESSING ###
library(dplyr)
library(readr)
#read data
KMdata <- readRDS("data/capture_recapture/porsanger/cr_porsanger_0618_0920.rds")

#get rid of NAs
unique(KMdata$station)
KMdata1 <- KMdata

sum(is.na(KMdata1$weight))


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
KMdata1$datetime <- as.Date(sapply(KMdata1$datetime, FUN=number2date))

# CODE CAPTURE HISTORY
tseason <- distinct(KMdata1,year,datetime)
tseason2 <- arrange(tseason,datetime)
tseason2$trapseason <- sort(rep.int(1:(nrow(tseason2)/2),2))

# add trap season
KM1 <- left_join(KMdata1,tseason2)
KM1[is.na(KM1$species),"marked"]

# filter predators and NAs #
KMROD <- filter(KM1, species %in% c("FJELLROTTE","GRAASIDEMUS","MARKMUS","ROEDMUS"))

#### unnecessary: corrected at data base ####
# how many individuals are not labeled
#no_NAs <- length(KMROD$Nindnum[is.na(KMROD$Nindnum)==TRUE])

### create labels for the NAs ##
#NA_label <- c()
#for(i in 1:no_NAs) NA_label[i] <- paste0("UNMNAX",i)
#KM1$Nindnum[is.na(KM1$Nindnum)==TRUE] <- NA_label

# NOT NEEDED ANYMORE; CORRECTED AT DATABASE LEVEL
# function to know if it's untagged
# isitU <- function(code)
# {
#   code <- as.character(code)
#   words <- strsplit(code,"")[[1]]
#   word1 <- words[1]
#   answer <- ifelse(word1=="U",TRUE,FALSE)
#   return(answer)
# }

# ## NON TAGGED ##
# unique(KM1$Nindnum[KM1$tagged==0])
# head(KM1)
# KM1$tagged <- 1
# # if it is untagged, mark as 0
# KM1$tagged[sapply(KM1$Nindnum,isitU)==TRUE] <- 0
# 
# # MAKE SURE EACH UNMARKED HAS ITS OWN LABEL
# # AN UNMARKED IS CONSIDERED TO HAVE THE FIRST LETTER AS U
# no_Us <- nrow(KM1[KM1$tagged==0,])
# NA_Ulabel <- c()
# for(i in 1:no_Us) NA_Ulabel[i] <- paste0("UNM",i)
# KM1$Nindnum[KM1$tagged==0] <- NA_Ulabel

# get month
# KM1$month <- months(KM1$date)

# table(KM1$weight[KM1$tagged==0])
# %% MANUAL CHECK %% #
### UNTAGGED INDIVIDUALS WHICH WERE GIVEN SAME CODE ON SAME STATION ###
# UNME
nE <- nrow(KMROD[KMROD$Nindnum=="UNME",])
Elab <- c()
for(i in 1:nE) Elab[i] <- paste0("UNME",i)
KMROD$Nindnum[KMROD$Nindnum=="UNME"] <- Elab

# 
# # UNMJ
# nJ <- nrow(KM1[KM1$Nindnum=="UNMJ",])
# Jlab <- c()
# for(i in 1:nJ) Jlab[i] <- paste0("UNMJ",i)
# KM1$Nindnum[KM1$Nindnum=="UNMJ"] <- Jlab




#### UNIQUE ID OF INDIVIDUALS ####

# remove some variables not important at this stage
KM1.2 <- select(KMROD, -c(transect,trapnum,marked,note))

# obtain unique combination of individuals
uniqueid <- distinct(KM1.2,year,station,Nindnum,trapseason,species)
# adding new id number
uniqueid$id <- 1:nrow(uniqueid)

# attach labels for the different individuals 
KM2 <- left_join(KM1.2,uniqueid)
### RETRIEVE CAPTURE HISTORY ###
# get capture history
# the sum will tell us the capture history
# 1+0 = 1 "10"
# 0+2 = 2 "01"
# 1+2 = 3 "11"
idch <- tibble(aggregate(check~year+trapseason+station+id+species,data=KM2, FUN=sum))
colnames(idch)[ncol(idch)] <- "category"
summary(idch)
table(idch$category)
# attach capture history
KM3 <- tibble(idch)
KM3$c1 <- ifelse(KM3$category==2,0,1)
KM3$c2 <- ifelse(KM3$category==1,0,1)

# add id
KM0 <- left_join(KM2,idch) # original dataset with ids

### weight and sex ###

# if NA -> NA
# if F -> 0
# if M, NM or N -> M
sex01 <- function(x) ifelse(is.na(x),NA,ifelse(x=="F",0,1))
KM0$sex01 <- sapply(KM0$sex,sex01)

# WEIGHT #
median.rm <- function(x) median(x,na.rm=TRUE)
Kweight <- tibble(aggregate(weight~id,data=KM0, FUN=median.rm))

# add mean weight
KM4 <- left_join(KM3,Kweight,"id")

# add "median" sex
Ksex <- tibble(aggregate(sex01~id,data=KM0, FUN=median.rm))

KM5 <- left_join(KM4,Ksex)

KM6 <- left_join(KM5,uniqueid) # add original id name
# KM6$transect <- ifelse(KM6$transect=="MASOY","MASOY","PORSANGER")
KM7 <- left_join(KM6,tseason2[seq(1,nrow(tseason2)-1,2),])
KM8 <- arrange(KM7, datetime,station,species,c1,id)

#write.csv2(KM7, "/Users/pni026/Documents/OccupancyAbundanceCalibration/data/CR_CaptHist_KMdata.csv")
saveRDS(KM8, "data/capture_recapture/porsanger/CR_CaptHist_0618_0920.rds")
# filter other species and trap seasons of mid summer
# KM9 <- filter(KM8, species=="GRAASIDEMUS" & trapseason %in% c(1,3,4,6,7,9))
# KM10 <- select(KM9, -c(category, species))
# write.csv2(KM10, "/Users/pni026/OneDrive - UiT Office 365/Vole Synchrony/data/CR_CaptHist_KMdata_18_20.csv")
