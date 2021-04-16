## calibration ##
library(dplyr)
library(INLA)
library(scales)

# install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)

eividwd <- "C:/Eivind/GitProjects/OccupancyAbundanceCalibration/"
pnwd <- "~/Documents/OccupancyAbundanceCalibration/"

setwd(pnwd)

#livetrapdata <- read.csv2("data/CR_processed.csv", stringsAsFactors = FALSE)
#cameradata <- read.csv2("/Users/pedronicolau/OccupancyAbundanceCalibration/data/camera_data_075confidence_processed.csv", stringsAsFactors = FALSE)
#abundance <- readRDS("/Users/pedronicolau/OccupancyAbundanceCalibration/data/estimated_abundance.rds")



# read in camera data
cameradata <- tibble(readRDS("data/cameratrap/haakoya/processed/haakoya_cameradata_final.rds"))
cameradata2 <- tibble(filter(cameradata, confidence1 > 0.95 & species == "vole"))
cameradata2$station <- sapply(cameradata2$site, function(x) strsplit(x,"")[[1]][2])
cameradata3 <- select(cameradata2, c("DateTimeOriginal", "station", "confidence1"))
cameradata3$date <- as.Date(format(cameradata3$DateTimeOriginal,format='%Y/%m/%d'))

cameradata3$station <- as.integer(cameradata3$station)

# read in trap data
livetrapdata <- readRDS("data/capture_recapture/haakoya/processed/haakoya_abundance.rds")
trapdates <- readRDS("data/capture_recapture/haakoya/processed/haakoya_trap_dates.rds")
trapdates2 <- filter(trapdates, check == 1)
trapdates2$date <- as.Date(format(trapdates2$date,format='%Y/%m/%d'))
trapdates2$julian <- julian(trapdates2$date, origin = as.Date("2018-05-29"))

### time format for live traps ###

# Rolf stations
Rcam <- filter(cameradata3, station < 4)
Rtdates <- filter(trapdates2, who == "R")

# Jon station
Jcam <- filter(cameradata3, station == 4)
Jtdates <- filter(trapdates2, who == "J")


camlist <- list(Jcam, Rcam)
tdatelist <- list(Jtdates,Rtdates)

updatedcam <- list()

for ( j in 1:length(tdatelist) ){

  tdates <- tdatelist[[j]]
# which days do we use for comparison
  
  uniquedates <- sort(unique(tdates$date))
  
  for(hd in seq(-10,10)){
    
    if(hd == -10) hdays <- uniquedates+hd
    else  hdays <- c(hdays,uniquedates+hd)  # day h of cr experiment
  }
  
  sort_hdays <- sort(hdays)

  dayframe <- tibble(date=sort_hdays, trapsession=rep(1:15,each=length(-10:10)),label=rep(-10:10,15))
  cam_data <- filter(camlist[[j]], date %in% sort_hdays)
  cam_data2 <- left_join(cam_data,dayframe)
  cam_data3 <- arrange(cam_data2, station, DateTimeOriginal)
  
  
  updatedcam[[j]] <- cam_data3

}

joint_camera <- arrange(bind_rows(updatedcam),station)
joint_camera$count <- 1

captdays <- filter(joint_camera, label %in% 0:2)
agg_camera1 <- aggregate(count ~ station+trapsession, data=captdays, FUN = sum)
camsetabs <- as.data.frame(xtabs(count~station+trapsession,data=agg_camera1))
names(camsetabs)[3] <- "days012"

captdaysbefore <- filter(joint_camera, label %in% -3:-1)
agg_camera2 <- aggregate(count ~ station+trapsession, data=captdaysbefore, FUN = sum)
camsetabs2 <- as.data.frame(xtabs(count~station+trapsession,data=agg_camera2))
names(camsetabs2)[3] <- "days-31"

captdaysafter <- filter(joint_camera, label %in% 3:5)
agg_camera3 <- aggregate(count ~ station+trapsession, data=captdaysafter, FUN = sum)
camsetabs3 <- as.data.frame(xtabs(count~station+trapsession,data=agg_camera3))
names(camsetabs3)[3] <- "days35"

joincamdata <- left_join(camsetabs,camsetabs2)
joincamdata2 <- tibble(left_join(joincamdata,camsetabs3))
joincamdata2$station <- as.numeric(joincamdata2$station)
joincamdata2$trapsession <- as.numeric(joincamdata2$trapsession)

tibble(livetrapdata)
fulldata <- left_join(joincamdata2,livetrapdata)
par(mfrow=c(1,1))
plot(fulldata$trapsession,fulldata$inlacr, ylim=c(0,200))
points(fulldata$trapsession,fulldata$days012, ylim=c(0,200), col="Red")

plot(fulldata$inlacr,fulldata$days012)
summary(lm(fulldata$inlacr~fulldata$days35))

# process live data
livedata$count <- 1
# once again, add all combinations in such a way to get the zero captures
livextabs <- as.data.frame(xtabs(count~station+trapseason, data=livedata), stringsAsFactors = FALSE)

# formatting
livextabs$trapseason <- as.numeric((livextabs$trapseason))
colnames(livextabs)[3] <- "cr"

# join cr dataset with camera data
jointset <- left_join(aggcameradata,livextabs)
jointset2 <- arrange(jointset, station,trapseason)

# add mean count for two days after 
jointset2$after_2 <- apply(jointset2[,3:4],1,mean)
jointset2$during_2 <- apply(jointset2[,1:2],1,mean)

# add log variables
jointset2$logafter_2 <- log(jointset2$after_2+1)
jointset2$logcr <- log(jointset2$cr+1)

# add abundance estimates
jointset2_1 <- left_join(jointset2,abundance)

#saveRDS(jointset2_1, "/Users/pedronicolau/OccupancyAbundanceCalibration/data/data_for_regression.rds")
saveRDS(jointset2_1, "C:/Eivind/GitProjects/OccupancyAbundanceCalibration/data/data_for_regression.rds")
