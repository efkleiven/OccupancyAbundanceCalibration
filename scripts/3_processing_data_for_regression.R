## calibration ##
library(dplyr)
library(INLA)
library(scales)

# install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)


#livetrapdata <- read.csv2("/Users/pedronicolau/OccupancyAbundanceCalibration/data/CR_processed.csv", stringsAsFactors = FALSE)
#cameradata <- read.csv2("/Users/pedronicolau/OccupancyAbundanceCalibration/data/camera_data_075confidence_processed.csv", stringsAsFactors = FALSE)
#abundance <- readRDS("/Users/pedronicolau/OccupancyAbundanceCalibration/data/estimated_abundance.rds")

livetrapdata <- read.csv2("C:/Eivind/GitProjects/OccupancyAbundanceCalibration/data/CR_processed.csv", stringsAsFactors = FALSE)
cameradata <- read.csv2("C:/Eivind/GitProjects/OccupancyAbundanceCalibration/data/camera_data_075confidence_processed.csv", stringsAsFactors = FALSE)
abundance <- readRDS("C:/Eivind/GitProjects/OccupancyAbundanceCalibration/data/estimated_abundance.rds")

abundance[,3:4] <- round(abundance[,3:4],0)

#### TIME FORMATTING ####
cameradata$station <- sapply(cameradata$station, formatGstations) # this function is in 1_reading_CR_data.R
ctstations <- unique(cameradata$station)

# voles only
livedata <- filter(livetrapdata, station %in% ctstations)

# source formatGstations function

# which days do we use for comparison
jdays <- c(unique(livetrapdata$julian),   # day 1 of cr experiment
           unique(livetrapdata$julian)+1, # day 2 of cr experiment
           unique(livetrapdata$julian)-1, # day 1 before cr experiment
           unique(livetrapdata$julian)+2, # day 1 after cr experiment
           unique(livetrapdata$julian)-2, # day 2 before cr experiment
           unique(livetrapdata$julian)+3  # day 2 after cr experiment
           )

#
jlength <- length(unique(livetrapdata$julian))

# getting labels for the given days
jlabels <- c(rep("01",jlength),rep("02",jlength),rep("b1",jlength),rep("a1",jlength),
             rep("b2",jlength),rep("a2",jlength)
             )

# format days in which trapping was conducted
tseas <- sort(rep.int(unique(livetrapdata$trapseason),length(unique(jlabels))))
juliandf <- arrange(data.frame(julian=jdays,julianlabels=jlabels),julian)
juliandf$trapseason <- tseas

#### FORMAT DATA FOR ANALYSIS ####

# filter camera data by corresponding dates of live trapping
camera_filtered <- arrange(filter(cameradata, julian %in% jdays), julian)

camera2 <- left_join(camera_filtered, juliandf)
cvoles <- filter(camera2, species == "vole")
cvoles$count <- 1

cvolesagg <- aggregate(count~station+year+date+julian+julianlabels+trapseason,data=cvoles, FUN=sum)
cvolesagg2 <- arrange(cvolesagg,julian, station)

### convert counts on different days to variables
i=1
seasnumber <- max(unique(cvolesagg2$trapseason))
#seasnumber <- 9

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# something goes wrong in this loop!!! #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

for (i in 1:seasnumber){
  print(i)
  # filter by trap season
  camset <- filter(cvolesagg2, trapseason==i)
  # make sure we get all the combinations between station and the days
  # aka get the zeros for stations in which no animal was trapped on a given day
  camset$station <- factor(camset$station, levels = c(unique(livedata$station)))
  
  camsetabs <- as.data.frame.matrix(xtabs(count~station+julianlabels,data=camset))
  camsetabs$station <- rownames(camsetabs)
  camsetabs$trapseason <- i
  rownames(camsetabs) <- NULL
  
  if(i==1) {
  camsetabs$b1 <- NA
  camsetabs$b2 <- NA}
  
  if(i==5) {
    camsetabs$a1 <- 0
    camsetabs$a2 <- 0
    camsetabs$"02" <- 0}
  
  if(i==6) {
    camsetabs$b1 <- 0
    camsetabs$b2 <- 0
    camsetabs$"01" <- 0}
  
  if(i==8) camsetabs$"01" <- 0
  if(i==1) aggcameradata <- camsetabs
  if(i>1) aggcameradata <- rbind(aggcameradata,camsetabs)
}

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
