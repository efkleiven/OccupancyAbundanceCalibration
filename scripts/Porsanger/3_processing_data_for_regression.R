## calibration ##
library(dplyr)
library(INLA)
library(scales)

# install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)


#livetrapdata <- read.csv2("/Users/pedronicolau/OccupancyAbundanceCalibration/data/CR_processed.csv", stringsAsFactors = FALSE)
#cameradata <- read.csv2("/Users/pedronicolau/OccupancyAbundanceCalibration/data/camera_data_075confidence_processed.csv", stringsAsFactors = FALSE)
#abundance <- readRDS("/Users/pedronicolau/OccupancyAbundanceCalibration/data/estimated_abundance.rds")
getwd()
livetrapdata <- read.csv2("./data/CR_processed.csv", stringsAsFactors = FALSE)
# cameradata <- read.csv2("./data/camera_data_075confidence_processed.csv", stringsAsFactors = FALSE)
cameradata <- readRDS("data/cameratrap/porsanger/processed/porsanger_camera_processing1.rds")
abundance <- readRDS("data/capture_recapture/porsanger/porsanger_abundance_perspecies.rds")

abundance[,3:4] <- round(abundance[,3:4],0)

# make names uniform between both live trap data and camera
cameradata$station[cameradata$station=="T1.2"] <- "T1-2"
cameradata$station[cameradata$station=="T2.1"] <- "T2-1"
cameradata$station[cameradata$station=="T2.2"] <- "T2-2"
cameradata$station[cameradata$station=="T3.2"] <- "T3-2"
cameradata$station[cameradata$station=="T5.2"] <- "T5-2"


## TIME FORMATTING ####
# source formatGstations function

#### function ----
formatGstations <- function(station_name)
# uniformizes stations' names
# for instance, G01 and G1
{
  # split station name characters
  sname <- strsplit(station_name,"")[[1]]
  if (sname[1]=="G"){
    # return numeric value of station number
    snumber <- as.numeric(paste(sname[2:length(sname)],collapse = ""))
    # paste G and station number
    station_name <- paste0("G",snumber)}
  
  return(station_name)
}
#####
cameradata$station <- sapply(cameradata$station, formatGstations) # this function is in 1_reading_CR_data.R
unique(cameradata$station)
ctstations <- unique(cameradata$station)

# filter out stations without camera data
livedata <- filter(abundance, station %in% ctstations)

# which days do we use for comparison
livedata$julian <- julian(livedata$datetime,origin = as.Date("2018-06-01"))

d <- 12 # how many days before
jdays0 <- unique(livedata$julian)
for (i in 1:d)
{
  jdays0 <-
    c(jdays0,
      unique(livedata$julian) + i,
      unique(livedata$julian) - i)
}
jdays <- sort(jdays0)

jlength <- length(unique(livedata$julian))

jlabels <- paste0("d",rep(-d:d, jlength))

# format days in which trapping was conducted
tseas <- sort(rep.int(unique(livedata$trapseason),length(unique(jlabels))))
juliandf <- arrange(data.frame(julian=jdays,julianlabels=jlabels),julian)
juliandf$trapseason <- tseas

#### FORMAT DATA FOR ANALYSIS ####

# filter camera data by corresponding dates of live trapping
cameradata$julian <- as.numeric(floor(julian(cameradata$DateTimeOriginal,origin = as.Date("2018-06-01"))))

camera_filtered <- arrange(filter(cameradata, julian %in% jdays), station,julian)

camera2 <- left_join(camera_filtered, juliandf)

camera2$count <- 1
camera2$date <- format(camera2$DateTimeOriginal,format='%Y/%m/%d')

cvolesagg <- aggregate(count~station+species+date+julian+julianlabels+trapseason,data=camera2, FUN=sum)
cvolesagg2 <- arrange(cvolesagg,species,station,julian)

### convert counts on different days to variables
i=1
seasnumber <- max(unique(cvolesagg2$trapseason))

specieslab <- unique(cvolesagg2$species)

# make sure we get all the combinations between station and the days
# aka get the zeros for stations in which no animal was trapped on a given day
cvolesagg2$station <- factor(cvolesagg2$station, levels = c(unique(livedata$station)))
cvolesagg2$julianlabels <- factor(cvolesagg2$julianlabels, levels = unique(jlabels))

speclist <- list()
for(s in 1:length(specieslab)) {
  
  print(paste0("species ", specieslab[[s]]))
  tslist <- list() 
  for (t in 1:seasnumber) {
    print(t)
    
    # filter by trap season
    camset <- filter(cvolesagg2, trapseason == t)
    

    camset2 <- filter(camset, species == specieslab[s])
    camsetabs <-
      as.data.frame.matrix(xtabs(count ~ station + julianlabels, data = camset2))
    library(tibble)
    camsetabs2 <- tibble::add_column(camsetabs, station = rownames(camsetabs), .after = 0)
    camsetabs3 <- tibble::add_column(camsetabs2, trapseason = t, .after = 1)
    camsetabs4 <- tibble::add_column(camsetabs3, species = specieslab[s], .after = 2)
    rownames(camsetabs) <- NULL
    
    tslist[[t]] <- camsetabs4
    
  }
  speclist[[s]] <- tibble(bind_rows(tslist))
}
# process live data
full_df <- bind_rows(speclist)
full_df$species[full_df$species=="G"] <- "GRAASIDEMUS"
full_df$species[full_df$species=="M"] <- "MARKMUS"
full_df$species[full_df$species=="R"] <- "ROEDMUS"

unique(full_df$station)

GS_photos_pors <- filter(full_df, species == "GRAASIDEMUS")
saveRDS(GS_photos_pors, "data/GS_photos_porsanger.rds")

# once again, add all combinations in such a way to get the zero captures
livextabs <- as.data.frame(xtabs(N.est~station+trapseason+species, data=livedata), stringsAsFactors = FALSE)
livextabs2 <- as.data.frame(xtabs(counts~station+trapseason+species, data=livedata), stringsAsFactors = FALSE)
livextabs$trapseason <- as.numeric((livextabs$trapseason))
colnames(livextabs)[4] <- "Abundance_HT"

livextabs3 <- tibble::add_column(livextabs, counts = livextabs2$Freq, .after = 4)

full_df2 <- filter(full_df, species %in% c("GRAASIDEMUS"))
fd1 <- arrange(full_df2, species,station,trapseason)
jointset <- left_join(full_df2,livextabs3)
jointset2 <- arrange(jointset, species,station,trapseason)
jointset3 <- jointset2[,c(1:3,(ncol(jointset2)-1),ncol(jointset2),4:(ncol(jointset2)-2))]
jointset4 <- jointset3[,1:5]

## Interval variables ----

jointset5 <- jointset4
centerpos <- which(colnames(jointset3)=="d1")

val = 0

for(i in 1:12)
{
  var_up <- centerpos+val
  var_lo <- centerpos-val
  fullname <- paste0("int_",var_up-var_lo)
  x <- apply(jointset3[var_lo:var_up],1,mean)
  jointset5$x <- x
  colnames(jointset5)[5+i] <- fullname
  val=val+1

}
jointset5$previousday <-  jointset3$`d-1`
saveRDS(jointset5, "data/porsanger_mean_intervals.rds")

## Previous days only ----

jointset6 <- jointset4
centerpos <- which(colnames(jointset3)=="d-1")

val = 0
for(i in 1:11)
{
  var_up <- centerpos
  var_lo <- centerpos-val
  fullname <- paste0("int_",var_up-var_lo)
  x <- apply(jointset3[var_lo:var_up],1,mean)
  jointset6$x <- x
  colnames(jointset6)[5+i] <- fullname
  val=val+1
  
}
View(jointset3)

saveRDS(jointset6, "data/porsanger_mean_intervals_prewindow.rds")


# ### CHOOSE VARIABLES ####
# # day zero is day of first capture
# # day -1 is day of setting traps
# # day -2 is day before traps
# 
# jointset4$previousday <- (data.frame(jointset3)[,17]) # day-1
# jointset4$experimdays.sum <- apply(jointset3[,17:19],1,sum) # day-1 to day 1
# jointset4$previous3day.sum <- apply(jointset3[,14:16],1,sum) # day -2 to -4
# jointset4$previous5day.sum <- apply(jointset3[,12:16],1,sum)
# jointset4$previous10day.sum <- apply(jointset3[,7:16],1,sum)
# jointset4$'5daysperiod.sum' <- apply(jointset3[,16:20],1,sum)
# jointset4$'9daysperiod.sum' <- apply(jointset3[,15:23],1,sum)
# jointset4$'19daysperiod.sum' <- apply(jointset3[,9:27],1,sum)
# # jointset4$'5daysperiod.med' <- apply(jointset3[,16:20],1,median)
# # jointset4$'9daysperiod.med' <- apply(jointset3[,15:23],1,median)
# # jointset4$'19daysperiod.med' <- apply(jointset3[,9:27],1,median)
# # jointset4$'5daysperiod.mean' <- apply(jointset3[,16:20],1,mean)
# # jointset4$'9daysperiod.mean' <- apply(jointset3[,15:23],1,mean)
# # jointset4$'19daysperiod.mean' <- apply(jointset3[,9:27],1,mean)
# #### mean ####
# # jointset4$previousday <- (data.frame(jointset3)[,16]) # day-1
# # jointset4$experimdays.mean <- apply(jointset3[,17:19],1,sum) # day-1 to day 1
# # jointset4$previous3day.mean<- apply(jointset3[,14:16],1,mean) # day -2 to -4
# # jointset4$previous5day.mean <- apply(jointset3[,12:16],1,mean)
# # jointset4$previous10day.mean <- apply(jointset3[,7:16],1,mean)
# # jointset4$'interval_1day.mean' <- apply(jointset3[,16:20],1,mean)
# # jointset4$'interval_3day.mean' <- apply(jointset3[,15:23],1,mean)
# # jointset4$'interval_8day.mean' <- apply(jointset3[,9:27],1,mean)
# #####
# #saveRDS(jointset4, "data/cameratrap/porsanger/processed/regression_data_porsanger.rds")
# saveRDS(jointset4, "data/cameratrap/porsanger/processed/regression_data_porsanger_sum.rds")
# View(jointset4)
# 
# ### GROWTH RATES ####
# library(dplyr)
# jointset5 <- arrange(jointset4, species,station,trapseason )
# nrow(jointset5)
# unique(jointset5$station)
# # initiate data frame with 360 rows (9 timepoints per 12 stations per 2 species)
# nstations <- length(unique(jointset5$station))
# ntp <- length(unique(jointset5$trapseason))
# nsp <- length(unique(jointset5$species))
# 
# grdf <- matrix(nrow=nstations*(ntp-1)*nsp, ncol=ncol(jointset5))
# nrow(grdf)
# # get labeling from 14 rows per station
# nstations <- length(unique(jointset5$station))
# row2remove <- (1:nrow(jointset5))[-seq(1,nrow(jointset5),9)]
# # add station labeling + trapsession
# grdf[,1:3] <- as.matrix(jointset5[row2remove,1:3])
# colnames(grdf) <- colnames(jointset5)
# 
# speclab <- unique(jointset4$species)
# stalab <- unique(jointset4$station)
# # compute growth rates for each station
# nr = 1 # initiate number of rows
# for(sp in 1:nsp)    {
#   d0 <- filter(jointset5, species == speclab[sp])
# 
#   for (st in 1:nstations) {
#     d1 <- filter(d0, station == stalab[st])
#     grdf[nr:(nr + 7), 4:ncol(grdf)] <-
#       diff(as.matrix(log(d1[, 4:ncol(d1)] + 1)))
#     nr = nr + 8
#     }
# }
# 
# grdf2 <- as.data.frame(grdf)
# View(grdf2)
# # convert to numeric
# grdf2[,4:ncol(grdf2)] <- sapply(grdf2[,4:ncol(grdf2)], as.numeric)
# head(grdf2,50)
# 
# View(grdf2)
# saveRDS(grdf2, "data/cameratrap/porsanger/processed/GR_regression_data_porsanger_sum.rds")
# 
