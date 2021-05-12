#################### Transform data 4 inla #################### 

library(dplyr)
library(INLA)

# set working directory
crdata <- read.csv2("/Users/pni026/Documents/OccupancyAbundanceCalibration/data/CR_processed.csv", stringsAsFactors = FALSE)

crdata2 <- crdata

# quick fix in case it is needed
# add isitNA variable
crdata2$covNA <- 0
crdata2$covNA[is.na(crdata2$weight)==TRUE] <- 1
NAsex <- length(crdata2$sex[is.na(crdata2$sex)==TRUE])
crdata2$covNA[is.na(crdata2$sex)==TRUE] <- 1

crdata2$chist <- paste0(crdata2$c1,",",crdata2$c2)
## obtain capture history variable
### TRANSFORM DATASET FOR INLA ###
## add count variable
crdata2$count <- 1
## copy dataset
crdata3 <- crdata2
## add count 0
crdata3$count <- 0
## copy again
crdata4 <- crdata3
## on the new copied set 1, invert all the labels
crdata3$chist <- as.factor(ifelse(crdata2$chist == "1,1", "1,0", ifelse(crdata2$chist == "1,0", 
                                                                        "0,1", "1,1")))
## on the new copied set 2, invert all the labels again
crdata4$chist <- as.factor(ifelse(crdata3$chist == "1,1", "1,0", ifelse(crdata3$chist == "1,0", 
                                                                        "0,1", "1,1")))

## final set involves all combined ones
inladata <- rbind(crdata2,crdata3,crdata4)

# factor variables
inladata$sex <- as.factor(inladata$sex)
inladata$station <- as.factor(inladata$station)
inladata$chist <- as.factor(inladata$chist)
# alternative specific index
inladata$alt.id <- ifelse(inladata$chist=="0,1",1,ifelse(inladata$chist=="1,0",2,3))
# add repeated variable for INLA 
inladata$alt.id2 <- inladata$alt.id
# re-name
inladata2 <- inladata
inladata3 <- arrange(inladata2,id,alt.id)

# add id term with NAs
idre <- rep(NA,nrow(inladata3))
idre[seq(1,length(idre),3)]<-1:(length(idre)/3)
inladata3$id_re <- idre

# add station formatted for inla
inladata3$station.i <- NA
inladata3$station.i[seq(1,nrow(inladata3),3)] <-as.character(inladata3$station[seq(1,nrow(inladata3),3)])

data4inla <- tibble(inladata3[,-1])

saveRDS(data4inla,"/Users/pedronicolau/OccupancyAbundanceCalibration/data/porsanger_inlaformat.rds")

