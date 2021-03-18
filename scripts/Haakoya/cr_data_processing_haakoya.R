### JOIN CAPTURE RECAPTURE DATASETS HÅKØYA ###
# import haakoya data
# set working dir
#wd <- "/Users/pni026/Documents/OccupancyAbundanceCalibration/"
#wd <- "C:/Eivind/OccupancyAbundanceCalibration/data/capture_recapture/haakoya"
wd <- "data/capture_recapture/haakoya/processed"
setwd(wd)

# set up command not in
#'%!in%' <- function(x,y)!('%in%'(x,y))

library(dplyr)
library("readxl")



############ Rolf data ############ 
# 1. bind 3 separate files
# 2. remove useless variables
# 3. add trapping session
# 4. split traps into stations

# 1. bind 3 separate files
hakoyRolf <- list() #empty list

for(i in 1:3){
  # read data
  hakoyRolf[[i]] <- read_xls(dir()[i])
}
# lump datasets 
hakoyR <- bind_rows(hakoyRolf)
# correct name
names(hakoyR)[3] <- "trap"
# 2. remove useless variables
hakoyR2 <- select(hakoyR, date,period, trap,ind, sex, wgt,cap,com) # select vars

# 3. add trapping session
trapsessR <- tibble(date=unique(hakoyR$date), trapsession = rep(1:15,each=3))
hakoyR3 <- left_join(hakoyR2,trapsessR )

# 4. split traps into stations
Rstations <- tibble(trap=1:45,station=c(rep(1,14),rep(2,7),rep(3,20),rep(2,4)))
hakoyR4 <- left_join(hakoyR3,Rstations)
hakoyR4$trap <- as.character(hakoyR$trap )

############  Jon Data  ############ 
# 1. access file
# 2. combine trapline and trap column into trap
# 3. select variables
# 4. filter older data
# 5. obtain trapping session (different days)
# 6. correct line
# 7. add station variable

# 1. access file
hakoyJon <- read_xls(dir()[4]) # last file in directory
# 2. combine trapline and trap column into trap
hakoyJon$trap <- paste0(hakoyJon$trapline,hakoyJon$trapcolumn) #create station var
# 3. select variables

hakoyJon2 <- select(hakoyJon, date,period, trap,ind, sex, wgt,cap,com, trapline) # select vars
# 4. filter older data

hakoyJon3 <- filter(hakoyJon2, date > "2018-01-01")
# 5. obtain trapping session (different days)
trapsessJ <- tibble(date=unique(hakoyJon3$date), trapsession = rep(1:15,each=3))
hakoyJon4 <- left_join(hakoyJon3,trapsessJ )

# 6. correct line
ca20 <- which(hakoyJon4$wgt=="ca20")
hakoyJon4$wgt[ca20] <- 20
hakoyJon4$wgt <- as.numeric(hakoyJon4$wgt)

# 7. add station variable
hakoyJon4$station <- 4

#### bind Jon and Rolf ####

hakoydat <- bind_rows(hakoyR4,hakoyJon4)
tail(hakoydat,20)
write.csv2(hakoydat,"haakoya_crdata_4stations.csv")
