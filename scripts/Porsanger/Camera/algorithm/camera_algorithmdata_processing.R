wd <- "/Users/pedronicolau/OccupancyAbundanceCalibration/data/cameratrap/porsanger"
# wd <- "C:/Eivind/GitProjects/OccupancyAbundanceCalibration/data/cameratrap/porsanger/manual"
getwd()
setwd(wd)
dir() # checking what files are in the directory

# load packages
library(dplyr)
require(lubridate)

#import data
ctdata <- read.csv(dir()[1])

filename <- ctdata$fileName[100]

datesplit <- function(filename)  # retrieve date and station from file name
{
  filename2 <- as.character(filename)
  string <- strsplit(filename2,"_")
  
  station1 <- string[[1]][2]
  station2 <- strsplit(station1,"/")[[1]]
  station3 <- station2[length(station2)]
  
  #station4 <- strsplit(station3,"")[[1]]
  #station5 <- paste0(station4[2],station4[3])
  #station <- as.numeric(station5)
  
  
  date1<- string[[1]][3]
  date2 <- strsplit(date1,"-")
  outputdata <- c(station3,date2[[1]],date1)
  return(outputdata)
}

# join dataset with camera info 
datas <- as.data.frame(t(sapply(ctdata$fileName,datesplit)))
colnames(datas) <- c("station","year","month","day","date")

factor2number <- function(x) return(as.numeric(as.character(x)))

datas[,2:4] <- sapply(datas[,2:4], factor2number)
datas[,c(1,5)] <- sapply(datas[,c(1,5)], as.character)

ctdata2 <- data.frame(datas,ctdata[,4:13])
ctdata2$answer <- ifelse(ctdata2$confidence1>0.75,ctdata2$guess1,0)    # select only pictures with confidense above 75%

ctdatax <- ctdata2[,c(1:5,11,16)]
ctdatax$animal <- ifelse(ctdatax$answer>2,1,0)      # animal or not animal
agganimal <- aggregate(animal~date, data=ctdatax,mean)
agganimal$date <- as.Date(agganimal$date)
agganimal$julian <- julian(as.Date(agganimal$date), origin=as.Date("2018-06-16"))
agganimal$month <- months(as.Date(agganimal$date))

plot(animal ~ julian, data=agganimal, xaxt = "n", pch=19, ylab="Proportion of photos with id'ed animal",xlab="months")
axis(1, at=seq(1+15,480-15,30),
     labels = agganimal$month[seq(1+15,480-15,30)], cex.axis = .7)


ctdata3 <- ctdata2[ctdata2$answer>1,c(1:5,11,16)]
labels <- c("empty","bird","vole","least_weasel","lemming","shrew","stoat")
whichsp <- function(number) labels[number]
ctdata3$species <- sapply(ctdata3$answer, FUN=whichsp)

head(ctdatax)
nrow(filter(ctdatax,answer==3))
nrow(volescr)



aggct1 <- aggregate(count~station+year+month+species, data=ctdata3, FUN=sum) # doesn't work because count dosn't excist

aggct2 <- arrange(aggct1,species,station,year,month) # dosen't work because the line above dosn't run


#### vole data ####
timeframe <- data.frame(daysofyear=1:366,weeks=c(rep.int(1:52,7),53,53))

volescr <- filter(ctdata3,species=="vole")
unique(sort(volescr$date))
volescr$julianday <- julian(as.Date(volescr$date), origin=as.Date("2018-06-15"))
volescr$dayofyear <- julian(as.Date(volescr$date), origin=as.Date("2018-06-"))

volescr$month <- months(as.Date(volescr$date))
volescr$week <- strftime(as.Date(volescr$date), format = "%V")



stations <- unique(volescr$station)
volescr$count <- 1
aggvoles <- aggregate(count~julianday+month, data=volescr,FUN=sum) # count still doesn't exist?

monthlab <- c("July",      "August",    "September", "October",   "November",  "December",  "January",   "February", 
              "March",     "April",     "May",       "June",      "July", "August", "September")
axis(1, at=seq(1+15,480-15,30),
     labels = monthlab, cex.axis = .7)

aggvolesweek <- aggregate(count~week+year, data=volescr,FUN=sum)
aggvolesweek$julianweek <- aggvolesweek$jweek


for(st in 1:length(stations)){
volesg00 <- filter(volescr, station==stations[st])
print(stations[st])
print(sum(volesg00$count))
}

volesg00$timepoint <- 1:nrow(volesg00)

plot(count~timepoint,data=volesg00,pch=19, xaxt="n", xlab="month", type="b")
axis(1, at=volesg01$timepoint, labels = volesg01$month, cex.axis = .7)

unique(volesg00$station)
volesg01$date <- as.Date(volesg01$date, "%m/%Y")

plot(count ~ date, data=volesg01, xaxt = "n", pch=19)
sort(unique(KM10$station))
sort(stations)
KM10 <- KM7[KM7$station%in%stations,]
crcounts <- aggregate(count~trapseason+date,data=KM10,sum)
crcounts$julianday <- julian(as.Date(crcounts$date), origin=as.Date("2018-06-15"))

plot(crcounts$julianday,crcounts$count,pch=19,cex=2,col="red",xlim=c(0,452),ylim=c(0,310),
     ylab="Counts",xaxt="n", xlab="")
points(aggvoles$julianday, aggvoles$count, pch=19,cex=1)
monthlab <- c("J",      "A",    "S", "O",   "N",  "D",  "J",   "F", 
              "M",     "A",     "M",       "J",      "J", "A", "S")
legend(226,300,legend=c("Photos","CR Counts"),pch=19, col=c("black","red"),bty="n")
axis(1, at=seq(1+15,480-15,30),
     labels = monthlab, cex.axis = .7)
