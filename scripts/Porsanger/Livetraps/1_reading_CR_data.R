library(dplyr)
library(readr)

### JOIN CAPTURE RECAPTURE DATASETS ###
# import porsanger data
# set working dir
#wd <- "C:/Eivind/GitProjects/OccupancyAbundanceCalibration/data/capture_recapture/porsanger"
wd <- "/Users/pni026/Documents/OccupancyAbundanceCalibration/data/capture_recapture/porsanger"
setwd(wd)

# set up command not in
'%!in%' <- function(x,y)!('%in%'(x,y))

# go through the different years in folder porsanger
# to retrieve the data and lump them together
ds <- 0 # set counter
porsdset <- list() #empty list

usefulvars <- c("year","area","seas","datetime","check","transect","station","trapnum",
                "Nindnum","sex","status","weight","species","marked","note")
yr=1
getwd()
for(yr in 1:length(dir())) #check year in directory
{
  #set working directory in year yr
  year <- as.numeric(dir()[yr]) # retrive year as numeric from name
  setwd(paste0(wd,"/",dir()[yr])) 
  
  # creating list with all the data frames
  for(dset in 1:length(dir()))
  {
    ds <- ds+1 # increase counter
    # read data
    data1 <- read_delim(dir()[dset],delim="\t")
    # add year
    data1$year <- year
    data1$transect <- as.character(data1$transect)
    data1$station <- as.character(data1$station)
    
    #remove useless var
    data2 <- select(data1, usefulvars)
    # attach to list
    porsdset[[ds]] <- data2
    print(names(data2))
    print(ncol(data2))
  }
  setwd(wd)
  
}

# bind rows
jointdata <- bind_rows(porsdset)
unique(jointdata$transect)
## split datasets by transect ##
masdata <- filter(jointdata, transect=="MASOY")
pordata <- filter(jointdata, transect!="MASOY")
porstation <- paste0("T",pordata$transect,"-",pordata$station)
pordata$station <- porstation

formatGstations <- function(station_name)
  # uniformizes stations' names
  # for instance, G01 and G1
{
  # split station name characters
  sname <- strsplit(station_name,"")[[1]]
  if (sname[1]=="G"){
  # return numeric value of station number
  snumber <- as.numeric(paste(sname[2:length(sname)],collapse = ""))
  # paste G and station number
  station_name <- paste0("G",snumber)}
  
  return(station_name)
}
# remove NA's (should look into why we have NA's in station names!)
# masdata <- masdata[!is.na(masdata$station),]

masdata$station <- as.character(sapply(masdata$station,formatGstations))

crdata <- bind_rows(masdata,pordata)

saveRDS(crdata,"/Users/pni026/Documents/OccupancyAbundanceCalibration/data/capture_recapture/porsanger/cr_porsanger_0618_0920.rds")
#write.csv2(crdata,"C:/Eivind/GitProjects/OccupancyAbundanceCalibration/data/CRDATA_raw.csv")

unique(crdata$datetime)
