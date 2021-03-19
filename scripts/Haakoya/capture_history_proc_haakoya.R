### DATA PROCESSING ###
library(dplyr)


#read data
# KMdata <- read.csv2("/Users/pedronicolau/OccupancyAbundanceCalibration/data/joint_CRDATA.csv", stringsAsFactors = FALSE)
# KMdata <- read.csv2("C:/Eivind/GitProjects/OccupancyAbundanceCalibration/data/joint_CRDATA.csv", stringsAsFactors = FALSE)

wd <- "data/capture_recapture/haakoya/processed"
setwd(wd)

Hdata <- readr :: read_csv2("haakoya_crdata_4stations.csv")[,-1]

#get rid of NAs
Hdata1 <- Hdata

unique(Hdata1$trap)
filter(Hdata1, trap=="8d")
##### CODE CAPTURE HISTORY #####


#### repeated observation ####
### UNIQUE ID OF INDIVIDUALS ###
uniqueid <- distinct(Hdata1,ind,trapsession,station)

uniqueid$id <- 1:nrow(uniqueid)
# attach labels for the different individuals according to
Hdata2 <- left_join(Hdata1,uniqueid)

### RETRIEVE CAPTURE HISTORY ###
# get capture history
# the sum will tell us the capture history
# day 1 : 1
# day 2 : 3
# day 3 : 5
Hdata2$check2 <- ifelse(Hdata2$check==2,3,ifelse(Hdata2$check==3,5,Hdata2$check))
# 1+0+0 = 1 "100"
# 0+3+0 = 3 "010"
# 0+0+5 = 5 "001"
# 0+3+5 = 8 "011"
# 1+0+5 = 6 "101"
# 1+3+0 = 4 "110"
# 1+3+5 = 9 "111"

idch <- tibble(aggregate(check2~trapsession+station+id,data=Hdata2, FUN=sum))

# KMdata1[KMdata$Nindnum=="0007D60D46",]
colnames(idch)[ncol(idch)] <- "category"

# attach capture history
chlabel <- tibble(category=c(1,3,5,8,6,4,9), c1=c(1,0,0,0,1,1,1), c2=c(0,1,0,1,0,1,1),c3=c(0,0,1,1,1,0,1))

idch2 <- left_join(idch,chlabel,by="category")
idch3 <- select(idch2, -category)

#### START FROM HERE ####
# old code 
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
