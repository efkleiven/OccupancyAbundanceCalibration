# FORMAT DAtA HÅKØYA 3 capture days
library(dplyr)
# library(INLA)

# set working directory
crdata <- readRDS("~/Documents/OccupancyAbundanceCalibration/data/capture_recapture/haakoya/processed/CH_Haakoya_withcovs.rds")

crdata2 <- crdata

# quick fix in case it is needed
# add isitNA variable
crdata2$covNA <- 0
crdata2$covNA[is.na(crdata2$wgt)==TRUE] <- 1
NAsex <- length(crdata2$sex[is.na(crdata2$sex)==TRUE])
crdata2$covNA[is.na(crdata2$sex)==TRUE] <- 1

crdata2$chist <- paste0(crdata2$c1,",",crdata2$c2,",",crdata2$c3)

chistories <- unique(crdata2$chist)

## obtain capture history variable
### TRANSFORM DATASET FOR INLA ###
## add count variable
crdata2$count <- 1


library(dplyr)
crdata3 <- slice(crdata2,rep(1:n(), each = 7))
crdata3$count <- rep(c(1,rep(0,6)), nrow(crdata3)/7)

newchist <- c()
for(i in 1:nrow(crdata2))
{ # which position in the vector matches the capture history
  posch <- match(crdata2[i,"chist"],chistories)
  # remove observed capture history
  chistories2 <- chistories[-posch]
  # make sure the observed capture history is the first in line
  newchist <- c(newchist, chistories[posch],chistories2)
  
}

crdata3$chist <- newchist

altiddf <- tibble(chist=chistories,alt.id=1:7)
crdata4 <- left_join(crdata3, altiddf, by="chist")
# re-name
inladata2 <- crdata4
inladata3 <- arrange(inladata2,trapsession,station,id,alt.id)

# add id term with NAs
idre <- rep(NA,nrow(inladata3))
idre[seq(1,length(idre),7)]<-1:(length(idre)/7)
inladata3$id_re <- idre

# add station formatted for inla
inladata3$station.i <- NA
inladata3$station.i[seq(1,nrow(inladata3),7)] <-as.character(inladata3$station[seq(1,nrow(inladata3),7)])

saveRDS(inladata3,"~/Documents/OccupancyAbundanceCalibration/data/capture_recapture/haakoya/processed/haakoya_inlaformat.rds")
