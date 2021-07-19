setwd("..")
getwd()
library(dplyr)
porscamera <- readRDS("data/cameratrap/porsanger/processed/porsanger_imported.rds")
tibble(porscamera)

# obtain aggregated time
porscamera$DateTimeOriginal <- paste0(porscamera$Date," ",porscamera$Time)
# format as date
porscamera$DateTimeOriginal <- strptime(porscamera$DateTimeOriginal,
                                        format="%d.%m.%Y %H:%M:%S", tz="CET") #Central European Time

# sort data by location and time
# remove the photos triggered automatically
pors1 <- filter(arrange(porscamera,Location, DateTimeOriginal), Trigger != "T")
unique(porscamera$Trigger)
# compute differences between adjacent times
# differences at the borders of location should always be larger because the data is sorted by station
difference <- diff(pors1$DateTimeOriginal)
# which are due to camera settings
dif1 <- which(abs(difference)<5)

# slice set of "duplicated" rows
duprows <- sort(c(dif1,dif1+1))
duplicates <- pors1[duprows,]
duplicates$id <- rep(1:(nrow(duplicates)/2), each=2)

# # check short differences
# dif2 <- which(abs(difference)%in%seq(2,59))
# duprows2 <- sort(c(dif2,dif2-1,dif2+1))
# ttt2 <- slice(pors1,duprows2)

#diff(ttt2$DateTimeOriginal)

## CHECK
## check if there are individuals with different ids
# duptest <- aggregate(species~id,data=duplicates, FUN=paste0, collapse="")
# table(duptest$species)
## there are not ! 

# highest confidence is on even rows
oddpos <- seq(1,nrow(duplicates),2)
duplicates2 <- slice(duplicates, oddpos)

## CHECK
# length(unique(duplicates2$id))==nrow(duplicates)/2
# nrow(duplicates2)/nrow(duplicates) == 0.5 # needs to be true: it is

# remove now useless variable
duplicates3 <- select(duplicates2, -"id")
# retrieve non duplicates
non_dups <- slice(pors1, -duprows)
# join all datasets
cameratrap1 <- arrange(bind_rows(duplicates3, non_dups), Location, DateTimeOriginal)

# "_" separates station, apply function vectorially
get_station <- function(x) strsplit(x,"_")[[1]][1]
cameratrap1$station <- sapply(cameratrap1$Location, get_station)
cameratrap2 <- select(cameratrap1, -c(Date,Time,Location,Trigger))

# should be true: it is
nrow(pors1)-nrow(duplicates)/2 == nrow(cameratrap1)
tibble(cameratrap2)


saveRDS(cameratrap2, "data/cameratrap/porsanger/processed/porsanger_camera_processing1.rds")

