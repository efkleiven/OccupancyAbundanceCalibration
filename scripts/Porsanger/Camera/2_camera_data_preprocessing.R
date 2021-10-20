setwd("~/OccupancyAbundanceCalibration/data")
setwd("./data")

getwd()
library(dplyr)
porscamera <- readRDS("cameratrap/porsanger/processed/porsanger_imported.rds")

# we just assume that the first observation is always best
pors0 <- filter(arrange(porscamera,Location, DateTimeOriginal), Trigger == "M 1/2")


# obtain aggregated time

# sort data by location and time
# remove the photos triggered automatically
# pors1 <- filter(arrange(porscamera,Location, DateTimeOriginal), Trigger != "T")






# compute differences between adjacent times
# differences at the borders of location should always be larger because the data is sorted by station and time
# difference <- diff(pors1$DateTimeOriginal)
# 
# # which are due to camera settings
# dif1 <- which(abs(difference)<3)
# 
# # slice set of "duplicated" rows
# duprows <- unique(sort(c(dif1,dif1+1)))
# duplicates <- pors1[duprows,]
# 
# duplicates2 <- filter(duplicates, Trigger == "M 1/2")
# ## CHECK
# # length(unique(duplicates2$id))==nrow(duplicates)/2
# # nrow(duplicates2)/nrow(duplicates) == 0.5 # needs to be true: it is
# 
# # remove now useless variable
# # retrieve non duplicates
# non_dups <- pors1[-duprows,]
# nrow(non_dups)+nrow(duplicates2)
# nrow(pors1)-nrow(duplicates2)
# # join all datasets
# cameratrap1 <- arrange(bind_rows(duplicates2, non_dups), Location, DateTimeOriginal)
cameratrap1 <- pors0
# # "_" separates station, apply function vectorially
get_station <- function(x) strsplit(x,"_")[[1]][1]
cameratrap1$station <- sapply(cameratrap1$Location, get_station)
cameratrap2 <- select(cameratrap1, -c(Location,Trigger))
# 
# # should be true: it is
# nrow(pors1)-nrow(duplicates)/2 == nrow(cameratrap2)
# tibble(cameratrap2)
# View(cameratrap2)


#saveRDS(cameratrap2, "cameratrap/porsanger/processed/porsanger_camera_processing1.rds")
saveRDS(cameratrap2, "data/cameratrap/porsanger/processed/porsanger_camera_processing1.rds")

