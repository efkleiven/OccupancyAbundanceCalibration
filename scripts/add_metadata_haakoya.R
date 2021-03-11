# set working directory
#wd <- "Y:/bilder_smagnagerfotobokser/Data/MLWIC_classification"
wd1 <- "/Users/pni026/Documents/OccupancyAbundanceCalibration/"
setwd(wd1)

data1 <- read.csv("data/haakoya_camera_answer.csv", head=TRUE)
str(data1)

metwd <- "data/cameratrap/metadata/Haakoy/"
dirmet <- dir(metwd)

ctlist <- list()
listindex=0 # to track how long the list of data.frames will be
str(ctlist)

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
str(ctlist)

metadf <- do.call(rbind, ctlist)
metadf$SourceFile[1]
data1$fileName[1]

cameratrap1 <- dplyr::left_join(data1, metadf, by="NewFileName")
cameratrap1$DateTimeOriginal <- strptime(cameratrap1$DateTimeOriginal,
                                          format="%Y:%m:%d %H:%M:%S", tz="CET") #Central European Time

cameratrap2 <- dplyr::select(cameratrap1, site, DateTimeOriginal,guess1, species, confidence1)
head(cameratrap2)
table(cameratrap2$site)
