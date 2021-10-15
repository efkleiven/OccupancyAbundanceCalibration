library(dplyr)
library(lubridate)
getwd()
#wd <- "/Users/pedronicolau/OccupancyAbundanceCalibration/data/cameratrap/porsanger"
wd <- "C:/Eivind/GitProjects/OccupancyAbundanceCalibration/data/cameratrap/new_classifictions/Haakoy"
#wd <- "~/Documents/OccupancyAbundanceCalibration/data/cameratrap/new_classifictions/Haakoy"
setwd(wd)
dir()

j=1
csvlist <- list()
yr=1

for (yr in 1:3) {
  
  setwd(dir()[yr])
  for (i in 1:length(dir()))
  {
    ctdata1 <- read.table(dir()[i], stringsAsFactors = FALSE, header=TRUE)
    csvlist[[j]] <- ctdata1
    j=j+1
    print(j)
  }
  setwd("..")
}


ctdata <- tibble(bind_rows(csvlist))
filename <- ctdata$filename[1]
datesplit <- function(filename)  # retrieve date and station from file name
{
  filename2 <- as.character(filename)
  string <- strsplit(filename2,"\\\\")[[1]]
  #string2 <- strsplit(string[[1]][3], "_")
  string2 <- strsplit(string, "_")[[1]]
  
  station1 <- string2[1]
  
  #station4 <- strsplit(station3,"")[[1]]
  #station5 <- paste0(station4[2],station4[3])
  #station <- as.numeric(station5)
  
  
  date1<- string2[2]
  
  outputdata <- c(station1,date1)
  
  return(outputdata)
}

# join dataset with camera info 
datas <- as.data.frame(t(sapply(ctdata$filename,datesplit)))
colnames(datas) <- c("station","date")
datas$date <- as.Date(datas$date)
datas2 <- tibble(datas)

#combine data sets
ctdata2 <- tibble(datas,ctdata)

# keep only values with confidence above 0.75
ctdata2$answer <- ifelse(ctdata2$confidence1>0.95,ctdata2$guess1,0)
ctdata3 <- filter(ctdata2,answer>0)

# is it an animal
ctdata3$animal <- ifelse(ctdata3$answer>2,1,0)
hist(ctdata3$confidence1)
unique(ctdata3$guess1)
labels <- c("empty","bird","vole","least_weasel","lemming","shrew","stoat")
whichsp <- function(number) labels[number]
ctdata3$species <- sapply(ctdata3$answer, FUN=whichsp)
table(ctdata3$species)
ctdata3$julian <- julian(as.Date(ctdata3$date), origin=as.Date("2018-06-15"))
ctdata3$weekofyear <- as.numeric(strftime(as.Date(ctdata3$date), format = "%V"))
ctdata3$month <- format(ctdata3$date, "%Y-%m")


format(date, "%m/%Y")
?strftime

ctdata3$species[ctdata3$species=="lemming"] <- "vole"
ctdata3$species[ctdata3$species=="least_weasel"] <- "stoat"
                
#write.csv2(ctdatax, "/Users/pedronicolau/OccupancyAbundanceCalibration/data/camera_data_075confidence_processed.csv")
saveRDS(ctdata3, "C:/Eivind/GitProjects/OccupancyAbundanceCalibration/data/camera_data_095confidence_processed_haakoya.rds")


# find filename for lemming

filter(ctdata3, species=="lemming")
filter(ctdata3, species=="least_weasel")


