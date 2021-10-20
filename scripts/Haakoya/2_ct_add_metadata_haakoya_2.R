# set working directory
wd1 <- "C:/Eivind/GitProjects/OccupancyAbundanceCalibration/"
wd1 <- "/Users/pni026/Documents/OccupancyAbundanceCalibration/"
setwd(wd1)

data1 <- readRDS("C:/Eivind/GitProjects/OccupancyAbundanceCalibration/data/camera_data_095confidence_processed_haakoya.rds")

metwd <- "data/cameratrap/metadata/Haakoy/"
dirmet <- dir(metwd)

ctlist <- list()
listindex=0 # to track how long the list of data.frames will be
str(ctlist)

# combine meta data files
for (i in 1:length(dirmet))
{
  newdir <- dirmet[i]
  newmet <- paste0(metwd,"/",newdir)
  
  yeardir <- dir(newmet)
  
  for(j in 1:length(yeardir))
  {
    listindex = listindex + 1
    ct_data <- read.table(paste0(newmet,"/",yeardir[j]), header=TRUE)
    ctlist[[listindex]] <- ct_data
  }
}
# convert list into data frame
metadf <- do.call(rbind, ctlist)

# change name in data1 to match metadf
names(data1)[5] <- "NewFileName"

# add metadata variables to haakoya camera data
cameratrap1 <- dplyr::left_join(data1, metadf, by="NewFileName")

# format time and data
cameratrap1$DateTimeOriginal <- strptime(cameratrap1$DateTimeOriginal,
                                          format="%Y:%m:%d %H:%M:%S", tz="CET")  #Central European Time

# remove unimportant variables
cameratrap2 <- dplyr::select(cameratrap1, site, DateTimeOriginal,guess1, species, confidence1, TriggerMode, Sequence)

# get species onto a column
#whichsp <- function(x) 
#{label <- c("unknown", "empty", "bird", "vole", "least_weasel", "lemming", "shrew", "stoat")
#return(label[x+1])}
# apply whichsp 
#cameratrap2$species <- sapply(cameratrap2$guess1,whichsp)

tibble(cameratrap2)

cameratrap3 <- tibble(filter(cameratrap2, TriggerMode != "T"))
cameratrap4 <- filter(cameratrap3,!(species %in% c("empty","unknown")))

#lubridate:: round_date(cameratrap1$DateTimeOriginal[2],unit="5 seconds")
#cameratrap1$DateTimeOriginal[1]

#saveRDS(cameratrap2, "data/cameratrap/haakoya/processed/haakoya_cameradata_final.rds")

#cameratrap2 <- readRDS("data/cameratrap/haakoya/processed/haakoya_cameradata_final.rds")

#ct0 <- arrange(cameratrap4,site,DateTimeOriginal)
#ct1 <- filter(ct0, DateTimeOriginal > as.Date("2018-05-20"))

# compute differences between adjacent times
#difference <- diff(ct1$DateTimeOriginal)
# which are due to camera settings
#dif5 <- which(abs(difference)<2)

# slice set of "duplicated" rows

#duprows <- sort(c(dif5,dif5+1))
#duplicates <- ct1[duprows,]
#duplicates$id <- rep(1:(nrow(duplicates)/2), each=2)

# check manually what data is repeated
# checkcombs <- function(x) paste0(sort(x), sep="", collapse="")
# check0 <- aggregate(guess1~id+site, data=duplicates, FUN=checkcombs)
# table(check0$guess1)

# sort by site and confidence
#duplicates2 <- arrange(duplicates, site,id,confidence1)

# highest confidence is on even rows
#evenpos <- seq(2,nrow(duplicates2),2)
#duplicates3 <- slice(duplicates2, evenpos)

# remove now useless variable
#duplicates4 <- select(duplicates3, -"id")
# retrieve non duplicates
#non_dups <- slice(ct1, -duprows)
# join all datasets

#cameratrap5 <- arrange(bind_rows(duplicates4, non_dups), site, DateTimeOriginal)

cameratrap5 <- filter(arrange(cameratrap4,site, DateTimeOriginal), Sequence == "1 2")

saveRDS(cameratrap5, "data/cameratrap/haakoya/processed/haakoya_cameradata_nodup.rds")

