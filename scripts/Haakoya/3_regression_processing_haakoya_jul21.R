## calibration ##
library(dplyr)
library(INLA)
library(scales)

 #install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)


cameradata <- tibble(readRDS("data/cameratrap/haakoya/processed/haakoya_cameradata_nodup.rds"))
cameradata2 <- tibble(filter(cameradata, confidence1 > 0.95 & species == "vole"))
cameradata2$station <- sapply(cameradata2$site, function(x) strsplit(x,"")[[1]][2])
cameradata3 <- select(cameradata2, c("DateTimeOriginal", "station", "confidence1"))
cameradata3$date <- as.Date(format(cameradata3$DateTimeOriginal,format='%Y/%m/%d'))


cameradata3$station <- as.integer(cameradata3$station)

# read in trap data
livetrapdata <- readRDS("data/capture_recapture/haakoya/processed/haakoya_abundance_radius.rds")
trapdates <- readRDS("data/capture_recapture/haakoya/processed/haakoya_trap_dates.rds")
trapdates2 <- filter(trapdates, check == 1)
trapdates2$date <- as.Date(format(trapdates2$date,format='%Y/%m/%d'))
trapdates2$julian <- julian(trapdates2$date, origin = as.Date("2018-05-29"))

ntraps <- data.frame(station=rep(1:4,each=15),trapsession=rep(1:15,4),
                     nt25=c(rep(c(7,5,8),each=15),rep(13,3),rep(10,12)),
                     nt50=rep(c(9,9,20,18),each=15),
                     nt100=rep(c(12,28,31,29),each=15),
                     ntNA=rep(c(14,11,20,30),each=15)
)


### time format for live traps ###

# Rolf stations
Rcam <- filter(cameradata3, station < 4)
Rtdates <- filter(trapdates2, who == "R")

# Jon station
Jcam <- filter(cameradata3, station == 4)
Jtdates <- filter(trapdates2, who == "J")


camlist <- list(Jcam, Rcam)
tdatelist <- list(Jtdates,Rtdates)

updatedcam <- list()

daymargin=25
labels4days <- paste0("d",rep(-daymargin:daymargin,15))
for ( j in 1:length(tdatelist) ){
  
  tdates <- tdatelist[[j]]
  # which days do we use for comparison
  
  uniquedates <- sort(unique(tdates$date))
  
  for(hd in seq(-daymargin,daymargin)){
    
    if(hd == -daymargin) hdays <- uniquedates+hd
    else  hdays <- c(hdays,uniquedates+hd)  # day h of cr experiment
  }
  
  sort_hdays <- sort(hdays)
  
  dayframe <- tibble(date=sort_hdays, trapsession=rep(1:15,each=length(-daymargin:daymargin)),label=labels4days)
  cam_data <- filter(camlist[[j]], date %in% sort_hdays)
  cam_data2 <- left_join(cam_data,dayframe)
  cam_data3 <- arrange(cam_data2, station, DateTimeOriginal)
  
  
  updatedcam[[j]] <- cam_data3
  
}

joint_camera <- arrange(bind_rows(updatedcam),station)
joint_camera$count <- 1





#### FORMAT DATA FOR ANALYSIS ####
cvolesagg <- aggregate(count~station+date+label+trapsession,data=joint_camera, FUN=sum)
cvolesagg2 <- arrange(cvolesagg,station,trapsession)

### convert counts on different days to variables
seasnumber <- max(unique(cvolesagg2$trapsession))


# make sure we get all the combinations between station and the days
# aka get the zeros for stations in which no animal was trapped on a given day
cvolesagg2$station <- factor(cvolesagg2$station, levels = c(1,2,3,4))
cvolesagg2$label <- factor(cvolesagg2$label, levels = unique(labels4days))
seasnumber <- max(cvolesagg2$trapsession)

tslist <- list()

# we want to get all the combinations of counts for all the trap dates and stations
for (t in 1:seasnumber) {
  print(t)
  
  # filter by trap season
  camset <- filter(cvolesagg2, trapsession == t)
  # do the xtabs to expand the data 
  camsetabs <-
    as.data.frame.matrix(xtabs(count ~ station + label, data = camset))
  # adding station as first column
  camsetabs2 <-
    tibble::add_column(camsetabs,
                       station = rownames(camsetabs),
                       .after = 0)
  # adding trapsession after statuib
  camsetabs3 <-
    tibble::add_column(camsetabs2, trapsession = t, .after = 1)
  
  # append to list
  tslist[[t]] <- camsetabs3
  
}


full_df <- tibble(bind_rows(tslist))
rownames(full_df) <- NULL
full_df$station <- as.numeric(full_df$station)
saveRDS(full_df,"data/calibration/Haakoya/tv_photos_haakoya.rds")

jointsetx <- left_join(livetrapdata,full_df, by=c("station", "trapsession"))

# obtain densities by dividing by number of traps
fulldata2 <- left_join(jointsetx,ntraps)
fulldata2$D25 <- fulldata2$N25/fulldata2$nt25
fulldata2$D50 <- fulldata2$N50/fulldata2$nt50
fulldata2$D100 <- fulldata2$N100/fulldata2$nt100
fulldata2$DT <- fulldata2$N.est/fulldata2$ntNA


### Interval variables ----

jointset0 <- fulldata2[,c("trapsession","station","D25","D50","DT")]
centerpos <- which(colnames(fulldata2)=="d1")

val = 0

for(i in 1:12)
{
  var_up <- centerpos+val
  var_lo <- centerpos-val
  fullname <- paste0("int_",var_up-var_lo)
  x <- apply(fulldata2[var_lo:var_up],1,mean)
  jointset0$x <- x
  colnames(jointset0)[5+i] <- fullname
  val=val+1
  
}
jointset0$previousday <-  fulldata2$`d-1`
saveRDS(jointset0, "data/haakoya_mean_intervals.rds")


## Previous days only ----

jointset1 <- fulldata2[,c("trapsession","station","D25","D50","DT")]
centerpos <- which(colnames(fulldata2)=="d-1")

val = 0
for(i in 1:22)
{
  var_up <- centerpos
  var_lo <- centerpos-val
  fullname <- paste0("int_",var_up-var_lo)
  x <- apply(fulldata2[var_lo:var_up],1,mean)
  jointset1$x <- x
  colnames(jointset1)[5+i] <- fullname
  val=val+1
  
}
View(fulldata2)

saveRDS(jointset1, "data/haakoya_mean_intervals_prewindow.rds")

#####
names(fulldata2)
# day zero is day of first capture
# day -1 is day of setting traps
# day -2 is day before traps
fulldata2$previousday <- (data.frame(fulldata2)[,17]) # day-1
names(fulldata2)

fulldata2$experimdays.sum <- apply(fulldata2[,18:21],1,sum) # day-1 to day 1
fulldata2$previous3day.sum <- apply(fulldata2[,15:17],1,sum) # day -2 to -4
fulldata2$previous5day.sum <- apply(fulldata2[,13:17],1,sum)
fulldata2$previous10day.sum <- apply(fulldata2[,8:17],1,sum)

fulldata2$'int1day.sum' <- apply(fulldata2[,17:22],1,sum) # +-1
fulldata2$'int3day.sum' <- apply(fulldata2[,15:24],1,sum) # +-3
fulldata2$'int8day.sum' <- apply(fulldata2[,10:29],1,sum) # +-8

### sum variables should be used to GRs ###
fulldata2$'6daysperiod.sum' <- apply(fulldata2[,17:22],1,sum) # +-1
fulldata2$'10daysperiod.sum' <- apply(fulldata2[,15:24],1,sum) # +-3
fulldata2$'20daysperiod.sum' <- apply(fulldata2[,10:29],1,sum) # +-8
fulldata2$experimdays.sum <- apply(fulldata2[,18:21],1,sum) # day-1 to day 1
fulldata2$previous3day.sum <- apply(fulldata2[,15:17],1,sum) # day -2 to -4
fulldata2$previous5day.sum <- apply(fulldata2[,13:17],1,sum)
fulldata2$previous9day.sum <- apply(fulldata2[,8:17],1,sum)

# fulldata2$'6daysperiod.median' <- apply(fulldata2[,17:22],1,median) # +-1
# fulldata2$'10daysperiod.median' <- apply(fulldata2[,15:24],1,median) # +-3
# fulldata2$'16daysperiod.median' <- apply(fulldata2[,12:27],1,median) # +-6
# fulldata2$'20daysperiod.median' <- apply(fulldata2[,10:29],1,median) # +-8

saveRDS(fulldata2, "data/calibration/Haakoya/regression_data_haakoya_ab_sum.rds")

### GROWTH RATES

jointset5 <- arrange(fulldata2,station,trapsession )
# initiate data frame with 360 rows (14 timepoints per 4 stations)
grdf <- matrix(nrow=56, ncol=ncol(jointset5))
# get labeling from 14 rows per station
row2remove <- (1:nrow(fulldata2))[-seq(1,nrow(jointset5),15)]
# add station labeling + trapsession
grdf[,1:2] <- as.matrix(jointset5[row2remove,1:2])
colnames(grdf) <- colnames(jointset5)

stalab <- unique(fulldata2$station)
# compute growth rates for each station
nr = 1 # initiate number of rows
for (st in 1:4) {
  d1 <- filter(jointset5, station == stalab[st])
  grdf[nr:(nr + 13), 3:ncol(grdf)] <-
    diff(as.matrix(log(d1[, 3:ncol(d1)] + 1)))
  nr = nr + 14 # number of time points
}

grdf2 <- as.data.frame(grdf)

# convert to numeric
grdf2[,3:ncol(grdf2)] <- sapply(grdf2[,3:ncol(grdf2)], as.numeric)
tail(grdf2,50)
grdf3 <- select(grdf2, -c(nt25,nt50,nt100,ntNA))
View(grdf3)
saveRDS(grdf3, "data/calibration/Haakoya/GR_regression_data_haakoya_sum.rds")

