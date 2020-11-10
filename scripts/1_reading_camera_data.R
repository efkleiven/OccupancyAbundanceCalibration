library(dplyr)
library(lubridate)
getwd()
#wd <- "/Users/pedronicolau/OccupancyAbundanceCalibration/data/cameratrap/porsanger"
wd <- "C:/Eivind/GitProjects/OccupancyAbundanceCalibration/data/cameratrap/porsanger"

setwd(wd)

dir()
ctdata1 <- read.csv(dir()[2], stringsAsFactors = FALSE)
ctdata2 <- read.csv(dir()[3], stringsAsFactors = FALSE)

ctdata <- rbind(ctdata1,ctdata2)

datesplit <- function(filename)  # retrieve date and station from file name
{
  filename2 <- as.character(filename)
  string <- strsplit(filename2,"\\\\")
  string2 <- strsplit(string[[1]][3], "_")
  
  station1 <- string2[[1]][1]
  
  #station4 <- strsplit(station3,"")[[1]]
  #station5 <- paste0(station4[2],station4[3])
  #station <- as.numeric(station5)
  
  
  date1<- string2[[1]][2]
  date2 <- strsplit(date1,"-")
  outputdata <- c(station1,date2[[1]],date1)
  return(outputdata)
  }

# join dataset with camera info 
datas <- as.data.frame(t(sapply(ctdata$fileName,datesplit)))
colnames(datas) <- c("station","year","month","day","date")
str(datas)

# convert factors to numeric
factor2number <- function(x) return(as.numeric(as.character(x)))

# just some formatting
datas[,2:4] <- sapply(datas[,2:4], factor2number)
datas[,c(1,5)] <- sapply(datas[,c(1,5)], as.character)

#combine data sets
ctdata2 <- data.frame(datas,ctdata[,4:13])
# keep only values with confidence above 0.75
ctdata2$answer <- ifelse(ctdata2$confidence1>0.9,ctdata2$guess1,0)
ctdata3 <- filter(ctdata2,answer>0)
ctdatax <- ctdata3[,c(1:5,11,16)]
# is it an animal
ctdatax$animal <- ifelse(ctdatax$answer>2,1,0)
hist(ctdatax$confidence1)

labels <- c("empty","bird","vole","least_weasel","lemming","shrew","stoat")
whichsp <- function(number) labels[number]
ctdatax$species <- sapply(ctdatax$answer, FUN=whichsp)
unique(ctdatax$answer)
ctdatax$julian <- julian(as.Date(ctdatax$date), origin=as.Date("2018-06-15"))
ctdatax$weekofyear <- as.numeric(strftime(as.Date(ctdatax$date), format = "%V"))

#write.csv2(ctdatax, "/Users/pedronicolau/OccupancyAbundanceCalibration/data/camera_data_075confidence_processed.csv")
write.csv2(ctdatax, "C:/Eivind/GitProjects/OccupancyAbundanceCalibration/data/camera_data_075confidence_processed.csv")

### additional exploratory stuff ###

 # Eivind can't run most of the code below here?

# aggregate
ctdatax$count <- 1
aggct1 <- aggregate(count~station+year+month+species, data=ctdatax, FUN=sum)
aggct2 <- arrange(aggct1,species,station,year,month)

head(aggct2)

### 
#### vole data ####
volescr <- filter(ctdatax,species=="vole")
unique(sort(volescr$date))
volescr$julianday <- julian(as.Date(volescr$date), origin=as.Date("2018-06-15"))

volescr$month <- months(as.Date(volescr$date))

stations <- unique(volescr$station)
aggvoles <- aggregate(count~julianday+month, data=volescr,FUN=sum)

aggvolesweek <- aggregate(count~weekofyear+year, data=volescr,FUN=sum)
aggvolesweek$julianweek <- aggvolesweek$jweek


for(st in 1:length(stations)){
volesg00 <- filter(volescr, station==stations[st])
print(stations[st])
print(sum(volesg00$count))
}

volesg00$timepoint <- 1:nrow(volesg00)

plot(count~timepoint,data=volesg00,pch=19, xaxt="n", xlab="month", type="b")
axis(1, at=volesg00$timepoint, labels = volesg00$month, cex.axis = .7)

unique(volesg00$station)
volesg01$date <- as.Date(volesg01$date, "%m/%Y")

plot(count ~ date, data=volesg00, xaxt = "n", pch=19)
sort(unique(KM10$station))
sort(stations)

KM10 <- KM8[KM8$station%in%stations,]
crcounts <- aggregate(count~trapseason+date,data=KM10,sum)
crcounts$julianday <- julian(as.Date(crcounts$date), origin=as.Date("2018-06-15"))

plot(crcounts$julianday,crcounts$count,pch=19,cex=2,col="red",xlim=c(0,820),ylim=c(0,310),
     ylab="Counts",xaxt="n", xlab="")
points(aggvoles$julianday, aggvoles$count, pch=19,cex=1)
#points(aggvolesweek$julianday, aggvolesweek$count, pch=19,cex=1, col="blue")
monthlab <- c("J", "A", "S", "O", "N", "D", "J", "F", "M", "A", "M", "J",
              "J", "A", "S", "O", "N", "D", "J", "F", "M", "A", "M", "J",
              "J", "A", "S")
axis(1, at=seq(1+15,820-15,30),
     labels = monthlab, cex.axis = .7)

legend(626,300,legend=c("Photos","CR Counts"),pch=19, col=c("black","red"),bty="n")

