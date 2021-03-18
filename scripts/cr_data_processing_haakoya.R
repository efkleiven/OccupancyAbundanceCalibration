### JOIN CAPTURE RECAPTURE DATASETS ###
# import porsanger data
# set working dir
#wd <- "/Users/pedronicolau/OccupancyAbundanceCalibration/data/capture_recapture/porsanger"
#wd <- "C:/Eivind/OccupancyAbundanceCalibration/data/capture_recapture/haakoya"
wd <- "data/capture_recapture/haakoya"
setwd(wd)

# set up command not in
'%!in%' <- function(x,y)!('%in%'(x,y))
library(dplyr)
library("readxl")
library(xlsx)

# go through the different years in folder porsanger
# to retrieve the data and lump them together
ds <- 0 # set counter

hakoydset <- list() #empty list

# read data
hakoydset[[1]] <- read.table(dir()[1], header=T, sep="\t", stringsAsFactors = FALSE)
hakoydset[[2]] <- read.table(dir()[3], header=T, sep="\t", stringsAsFactors = FALSE)
hakoydset[[3]] <- read.table(dir()[5], header=T, sep="\t", stringsAsFactors = FALSE)
  

head(hakoydset[[1]])
head(hakoydset[[2]])
head(hakoydset[[3]])

# go through lists
for(df in 1:length(porsdset))
{
  dataf <- porsdset[[df]]
  
  # just because not all dataframes are the same
  if(ncol(dataf) > 16 ) dataf <- dataf[,1:16] #
  if(ncol(dataf) < 16 ) dataf <- data.frame(dataf[,1:8],indnum_old=NA,dataf[,9:15]) #reordering columns
  
  # to start off the data frame
  if ( df == 1 )
  {
    jointdata <- dataf
    rnames <- names(jointdata)
  }
  
  if ( df != 1 )
  {
    colnames(dataf) <- rnames
    jointdata <- rbind(jointdata,dataf)
  }
  print(nrow(jointdata))
  
}
# uniformize names
jointdata$transect[jointdata$transect%!in%c("1","2","3","4","5")] <- "MASOY"

## split datasets by transect ##
masdata <- filter(jointdata, transect=="MASOY")
pordata <- filter(jointdata, transect!="MASOY")
porstation <- paste0("T",pordata$transect,"-",pordata$station)
pordata$station <- porstation


crdata <- rbind(masdata,pordata)
#write.csv2(crdata,"/Users/pedronicolau/OccupancyAbundanceCalibration/data/joint_CRDATA.csv")
write.csv2(crdata,"C:/Eivind/OccupancyAbundanceCalibration/data/joint_CRDATA.csv")
