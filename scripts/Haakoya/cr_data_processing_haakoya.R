### JOIN CAPTURE RECAPTURE DATASETS HÅKØYA ###
# import haakoya data
# set working dir
wd <- "/Users/pni026/Documents/OccupancyAbundanceCalibration/"
#wd <- "C:/Eivind/OccupancyAbundanceCalibration/data/capture_recapture/haakoya"
wd <- "data/capture_recapture/haakoya/processed"
setwd(wd)
getwd()
# set up command not in
#'%!in%' <- function(x,y)!('%in%'(x,y))

library(dplyr)
library("readxl")

file_list <- c("fangst_håkøy_Rolf_18_modified.xls", "fangst_håkøy_Rolf_19_modified.xls", "fangst_håkøy_Rolf_20_modified.xls",
 "jon_hakoya_modified.xls")  

############ Rolf data ############ 
# 1. bind 3 separate files
# 2. remove useless variables
# 3. add trapping session
# 4. split traps into stations

# 1. bind 3 separate files
hakoyRolf <- list() #empty list

for(i in 1:3){
  # read data
  hakoyRolf[[i]] <- read_xls(file_list[i])
}
# lump datasets 
hakoyR <- bind_rows(hakoyRolf)
# correct name
names(hakoyR)[3] <- "trap"
# 2. remove useless variables
hakoyR2 <- select(hakoyR, date,period, trap,ind, sex, wgt,cap,com) # select vars

# 3. add trapping session
trapsessR <- tibble(date=sort(unique(hakoyR$date)), trapsession = rep(1:15,each=3), check=rep(1:3,15))
hakoyR3 <- left_join(hakoyR2,trapsessR )

# correct typo : 49 to 39
fortyniner <- which(hakoyR3$trap==49)
hakoyR3[fortyniner,"trap"] <- 39
# 4. split traps into stations
Rstations <- tibble(trap=1:45,station=c(rep(1,14),rep(2,7),rep(3,20),rep(2,4)))
hakoyR4 <- left_join(hakoyR3,Rstations)
hakoyR4$trap <- as.character(hakoyR4$trap)
############  Jon Data  ############ 
# 1. access file
# 2. combine trapline and trap column into trap
# 3. select variables
# 4. filter older data
# 5. obtain trapping session (different days)
# 6. correct line
# 7. add station variable

# 1. access file
hakoyJon <- read_xls(file_list[4]) # last file in directory
# 2. combine trapline and trap column into trap
hakoyJon$trap <- paste0(hakoyJon$trapline,hakoyJon$trapcolumn) #create station var
# 3. select variables

hakoyJon2 <- select(hakoyJon, date,period, trap,ind, sex, wgt,cap,com, trapline) # select vars
# 4. filter older data

hakoyJon3 <- filter(hakoyJon2, date > "2018-01-01")
# 5. obtain trapping session (different days)
trapsessJ <- tibble(date=sort(unique(hakoyJon3$date)), trapsession = rep(1:15,each=3), check=rep(1:3,15))
hakoyJon4 <- left_join(hakoyJon3,trapsessJ )

# 6. correct line
ca20 <- which(hakoyJon4$wgt=="ca20")
hakoyJon4$wgt[ca20] <- 20
hakoyJon4$wgt <- as.numeric(hakoyJon4$wgt)

# 7. add station variable
hakoyJon4$station <- 4

#### bind Jon and Rolf ####

hakoydat <- bind_rows(hakoyR4,hakoyJon4)

#### correct minor fixes ####
novoles <- which(hakoydat$ind=="no voles")
hakoydat2 <- hakoydat[-novoles,]

novoles2 <- which(hakoydat$ind=="NA")
hakoydat3 <- hakoydat2[-novoles2,]

write.csv2(hakoydat3,"haakoya_crdata_4stations.csv")


#### write dataset with dates and trap session ####
trapsessJ$who <- "J"
trapsessR$who <- "R"
jointtraps <- bind_rows(trapsessJ,trapsessR)
saveRDS(jointtraps, "haakoya_trap_dates.rds")

radius <- read.csv("data/capture_recapture/haakoya/haakoya_stations_radius.csv")

hakoydat3 <- read.csv2("haakoya_crdata_4stations.csv")[,-1]
hakoydat3$year <- lubridate::year(hakoydat3$date)
hakoydat4 <- left_join(hakoydat3, radius)
saveRDS(hakoydat4,"haakoya_crdata_wradius.csv")

