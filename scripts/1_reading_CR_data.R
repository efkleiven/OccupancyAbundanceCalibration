### JOIN CAPTURE RECAPTURE DATASETS ###
# import porsanger data
# set working dir
wd <- "/Users/pedronicolau/OccupancyAbundanceCalibration/data/capture_recapture/porsanger"
setwd(wd)

# set up command not in
'%!in%' <- function(x,y)!('%in%'(x,y))
library(dplyr)

# go through the different years in folder porsanger
# to retrieve the data and lump them together
ds <- 0 # set counter
porsdset <- list() #empty list

usefulvars <- c("year","area","seas","datetime","check","transect","station","trapnum",
                "Nindnum","sex","status","weight","species","marked","note")

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
    data1 <- read.table(dir()[dset], header=T, sep="\t", stringsAsFactors = FALSE)
    # add year
    data1$year <- year
    #remove useless var
    data2 <- select(data1, usefulvars)
    
    # attach to list
    porsdset[[ds]] <- data2
    print(names(data2))
    print(ncol(data2))
  }
  setwd(wd)
  
}



# go through lists
for(df in 1:length(porsdset))
{
  dataf <- porsdset[[df]]
  
  # # just because not all dataframes are the same
  # if(ncol(dataf) > 16 ) dataf <- dataf[,1:16] #
  # if(ncol(dataf) < 16 ) dataf <- data.frame(dataf[,1:8],indnum_old=NA,dataf[,9:15]) #reordering columns
  
  # to start off the data frame
  if ( df == 1 )
  {
    jointdata <- dataf
    cnames <- names(jointdata)
  }
  
  if ( df != 1 )
  {
      # change names of columns
      colnames(dataf) <- cnames
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
masdata$station <- sapply(masdata$station,formatGstations)

crdata <- rbind(masdata,pordata)
write.csv2(crdata,"/Users/pedronicolau/OccupancyAbundanceCalibration/data/joint_CRDATA.csv")
