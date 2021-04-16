# set working directory
#wd <- "Y:/bilder_smagnagerfotobokser/Data/MLWIC_classification"
wd1 <- "/Users/pni026/Documents/OccupancyAbundanceCalibration/"
setwd(wd1)

data1 <- read.csv("data/cameratrap/haakoya/processed/haakoya_camera_answer.csv", head=TRUE)

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

# add metadata variables to haakoya camera data
cameratrap1 <- dplyr::left_join(data1, metadf, by="NewFileName")
# format time and data
cameratrap1$DateTimeOriginal <- strptime(cameratrap1$DateTimeOriginal,
                                          format="%Y:%m:%d %H:%M:%S", tz="CET") #Central European Time

# remove unimportant variables
cameratrap2 <- dplyr::select(cameratrap1, site, DateTimeOriginal,guess1, species, confidence1)

# get species onto a column
whichsp <- function(x) 
{label <- c("unknown", "empty", "bird", "vole", "least_weasel", "lemming", "shrew", "stoat")
return(label[x+1])}
# apply whichsp 
cameratrap2$species <- sapply(cameratrap2$guess1,whichsp)
table(cameratrap2$species)

saveRDS(cameratrap2, "data/cameratrap/haakoya/processed/haakoya_cameradata_final.rds")
