
## Compute growth_rates for Håkøya and Porsanger
library(dplyr)
####### Håkøya #######
Porsanger=FALSE
dat4hak <- readRDS("data/haakoya_mean_intervals.rds")
dat4hak[,3:ncol(dat4hak)]<- log(dat4hak[,3:ncol(dat4hak)]+1)
nheader=2 # 2 for Haakoya, 3 Pors
jointset5 <- dat4hak
ntp <- length(unique(jointset5$trapsession)) # Hakoya
correctfactor <- 0

####### Porsanger #######
# Porsanger=TRUE
# porD <- readRDS("data/porsanger_mean_intervals.rds")
# dat4pors <- dplyr::filter(PorD, species=="GRAASIDEMUS" & trapseason > 1 & !(station == "G19" & trapseason <= 6))
# dat4pors$Abundance_HT  <- dat4pors$Abundance_HT/16 # divide by number of traps
# dat4pors[,4:ncol(dat4pors)]<- log(dat4pors[,4:ncol(dat4pors)]+1)
# nheader=3 # 2 for Haakoya, 3 Pors
# jointset5 <- dat4pors
# ntp <- length(unique(jointset5$trapseason)) # Porsanger
# correctfactor <- -5


### GROWTH RATES Computation ####
# initiate data frame with 360 rows (9 timepoints per 12 stations per 2 species)
nstations <- length(unique(as.character(jointset5$station)))

nrow(dat4pors)
# missing station data for porsanger
grdf <- matrix(nrow=nstations*(ntp-1)+correctfactor, ncol=ncol(jointset5))
# get labeling from 14 rows per station
nstations <- length(unique(jointset5$station))
row2remove <- (1:nrow(jointset5))[-seq(1,nrow(jointset5),ntp)]
# add station labeling + trapsession
grdf[,1:nheader] <- as.matrix(jointset5[row2remove,1:nheader])
colnames(grdf) <- colnames(jointset5)
# station labels
stalab <- unique(jointset5$station)

# compute growth rates for each station
nr = 1 # initiate number of rows
st=1
for (st in 1:nstations) {
  d1 <- filter(jointset5, station == stalab[st])
  if(Porsanger==TRUE) ntp2 <- length(unique(d1$trapseason))
  if(Porsanger==FALSE) ntp2 <- length(unique(d1$trapsession))
  
  grdf[nr:(nr + ntp2-2), (nheader+1):ncol(grdf)] <- diff(as.matrix(d1[, (nheader+1):ncol(d1)] + 1))
  nr = nr + ntp2 - 1
 }

# convert to numeric df
grdf2 <- as.data.frame(grdf)
grdf2[,(nheader+1):ncol(grdf2)] <- sapply(grdf2[,(nheader+1):ncol(grdf2)], as.numeric)

if(Porsanger==FALSE) hkgrdf <- grdf2
if(Porsanger==TRUE) porgrdf <- grdf2


# Håkøya
if(Porsanger==FALSE)saveRDS(hkgrdf,"data/calibration/regression_haakoya_mean_logplus1_GR.rds")
if(Porsanger==TRUE)saveRDS(porgrdf,"data/calibration/regression_porsanger_mean_logplus1_GR.rds")

