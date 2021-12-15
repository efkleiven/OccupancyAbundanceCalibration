
## Compute growth_rates for Håkøya and Porsanger

Porsanger=FALSE
dat4hak <- readRDS("data/calibration/regression_haakoya_mean_logplus1_ab.rds")
dat4hak[,c("D25","DT")] <- log(dat4hak[,c("D25","DT")]+1)

# Porsanger=TRUE
# dat4pors <- readRDS("data/calibration/regression_porsanger_mean_logplus1_ab.rds")
# dat4pors[,c("counts","Abundance")] <- log(dat4pors[,c("counts", "Abundance")]+1)


### GROWTH RATES ####
library(dplyr)
jointset5 <- dat4hak
# jointset5 <- dat4pors

# initiate data frame with 360 rows (9 timepoints per 12 stations per 2 species)
nstations <- length(unique(as.character(jointset5$station)))

ntp <- length(unique(jointset5$trapsession)) # Hakoya
# ntp <- length(unique(jointset5$trapseason)) # Porsanger

correctfactor <- ifelse (Porsanger==TRUE,-5,0)

grdf <- matrix(nrow=nstations*(ntp-1)+correctfactor, ncol=ncol(jointset5))
nrow(grdf)
# get labeling from 14 rows per station
nstations <- length(unique(jointset5$station))

row2remove <- (1:nrow(jointset5))[-seq(1,nrow(jointset5),ntp)]
# add station labeling + trapsession
nheader=2 # 2 for Haakoya, 3 Pors
grdf[,1:nheader] <- as.matrix(jointset5[row2remove,1:nheader])
colnames(grdf) <- colnames(jointset5)

#speclab <- unique(jointset4$species)
stalab <- unique(jointset5$station)
# compute growth rates for each station
nr = 1 # initiate number of rows
st=1
for (st in 1:nstations) {
  d1 <- filter(jointset5, station == stalab[st])
  ntp2 <- length(unique(d1$trapsession))
  grdf[nr:(nr + ntp2-2), (nheader+1):ncol(grdf)] <- diff(as.matrix(d1[, (nheader+1):ncol(d1)] + 1))
  nr = nr + ntp2 - 1
 }


grdf2 <- as.data.frame(grdf)

# convert to numeric
grdf2[,(nheader+1):ncol(grdf2)] <- sapply(grdf2[,(nheader+1):ncol(grdf2)], as.numeric)

hkgrdf <- grdf2
# porgrdf <- grdf2

# Håkøya
saveRDS(hkgrdf,"data/calibration/regression_haakoya_mean_logplus1_GR.rds")
# saveRDS(porgrdf,"data/calibration/regression_porsanger_mean_logplus1_GR.rds")

