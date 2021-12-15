library(dplyr)
library(lubridate)
getwd()
#wd <- "/Users/pedronicolau/OccupancyAbundanceCalibration/data/cameratrap/porsanger"
wd <- "C:/Eivind/GitProjects/OccupancyAbundanceCalibration/data/cameratrap/new_classifictions/porsanger"
wd <- "~/Documents/OccupancyAbundanceCalibration/data/cameratrap/new_classifictions/Porsanger"
setwd(wd)
dir()

j=1
csvlist <- list()
yr=1
for (yr in 1:2) {
  
  setwd(dir()[yr])
  for (i in 1:length(dir()))
  {
    ctdata1 <- read.table(dir()[i], stringsAsFactors = FALSE, header=TRUE)
    csvlist[[j]] <- ctdata1
    j=j+1
    print(j)
  }
  setwd("..")
}


ctdata <- tibble(bind_rows(csvlist))
filename <- ctdata$filename[1]
datesplit <- function(filename)  # retrieve date and station from file name
{
  filename2 <- as.character(filename)
  string <- strsplit(filename2,"\\\\")[[1]]
  #string2 <- strsplit(string[[1]][3], "_")
  string2 <- strsplit(string, "_")[[1]]
  
  station1 <- string2[1]
  
  #station4 <- strsplit(station3,"")[[1]]
  #station5 <- paste0(station4[2],station4[3])
  #station <- as.numeric(station5)
  
  
  date1<- string2[2]

  outputdata <- c(station1,date1)
  
  return(outputdata)
}

# join dataset with camera info 
datas <- as.data.frame(t(sapply(ctdata$filename,datesplit)))
colnames(datas) <- c("station","date")
datas$date <- as.Date(datas$date)
datas2 <- tibble(datas)

#combine data sets
ctdata2 <- tibble(datas,ctdata)

# keep only values with confidence above 0.75
ctdata2$answer <- ifelse(ctdata2$confidence1>0.95,ctdata2$guess1,0)
ctdata3 <- filter(ctdata2,answer>0)

# is it an animal
ctdata3$animal <- ifelse(ctdata3$answer>2,1,0)
hist(ctdata3$confidence1)
unique(ctdata3$guess1)
labels <- c("empty","bird","vole","least_weasel","lemming","shrew","stoat")
whichsp <- function(number) labels[number]
ctdata3$species <- sapply(ctdata3$answer, FUN=whichsp)
table(ctdata3$species)
ctdata3$julian <- julian(as.Date(ctdata3$date), origin=as.Date("2018-06-15"))
ctdata3$weekofyear <- as.numeric(strftime(as.Date(ctdata3$date), format = "%V"))
ctdata3$month <- format(ctdata3$date, "%Y-%m")

ctdata4 <- filter(ctdata3, species != "empty")
table(ctdata4$species)

saveRDS(ctdata4, "~/Documents/OccupancyAbundanceCalibration/data/camera_porsanger_095_allsp.rds")

### additional exploratory stuff ####

 # Eivind can't run most of the code below here?

# aggregate
library(data.table)


ctdatax <- filter(ctdata3, species!= "empty")
ctdata3$count <- 1
aggct1 <- aggregate(count~date+species+station, data=ctdatax, FUN=sum)
aggct2 <- aggregate(count~date+species+station, data=ctdatax, FUN=sum)

vdata <- filter(aggct1, species=="vole")
lwdata <- filter(aggct1, species=="least_weasel")
sdata <- filter(aggct1, species=="stoat")
sldata <- filter(aggct1, species %in% c("stoat","least_weasel"))

table(sdata$station)
par(mfrow=c(1,1))
plot(vdata$date,vdata$count, pch=19, col=alpha("gray20",.8))
points(sdata$date,sdata$count, pch=19, col=alpha(2,.8))
plot(vdata$date,vdata$count, pch=19, col=alpha("gray20",.8))

head(aggct2)


### EVERY TWO WEEKS ###
DT <- data.table(vdata)
v2 <- DT[, .(vole = sum(count)), 
           keyby = .(Date = 7 * (as.numeric(date - min(vdata$date)) %/% 7) + min(vdata$date))]

DT <- data.table(sdata)
s2 <- DT[, .(stoat = sum(count)), 
         keyby = .(Date = 7 * (as.numeric(date - min(vdata$date)) %/% 7) + min(vdata$date))]

DT <- data.table(lwdata)
lw2 <- DT[, .(lweasel = sum(count)), 
         keyby = .(Date = 7 * (as.numeric(date - min(vdata$date)) %/% 7) + min(vdata$date))]

DT <- data.table(lwdata)
slw2 <- DT[, .(sum_value = sum(count)), 
          keyby = .(Date = 14 * (as.numeric(date - min(date)) %/% 14) + min(date))]

plot(v2$Date,log(v2$sum_value), pch=19, ylim=c(0,8), lty=1, type="b")
lines(s2$Date,log(s2$sum_value), col=2, pch=19, type="b")
points(s2$Date,log(s2$sum_value), col=2, pch=19)
lines(lw2$Date,log(lw2$sum_value), col=3, pch=19, type="b")
points(lw2$Date,log(lw2$sum_value), col=3, pch=19)

points(slw2$Date,log(slw2$sum_value), col=3, pch=19)
lines(smooth.spline(slw2$Date,log(slw2$sum_value), spar=.4), col=4, pch=19)

v3 <- as.data.frame(v2)
s3 <- as.data.frame(s2)
l3 <- as.data.frame(lw2)
d1 <- left_join(v3,s3, by="Date")
d2 <- left_join(d1,l3, by="Date")

d2$stoat <- ifelse(is.na(d2$stoat),0,d2$stoat)
d2$lweasel <- ifelse(is.na(d2$lweasel),0,d2$lweasel)
unique(d1$sum_value.y)
par(mfrow=c(1,2))
plot(d2$Date,log(d2$vole+1), pch=19, ylim=c(0,8), lty=1, type="b", xaxt="n", ylab="log(photo counts+1)", xlab="")
points(d2$Date,log(d2$stoat), pch=19, ylim=c(0,8), col=2, type="b")
#lines(smooth.spline(d2$Date,log(d2$stoat),spar=.3), pch=19, ylim=c(0,8), lty=1, type="l", col=2)
legend("topright",c("vole sp.", "stoat"),pch=19, col=c(1,2), bty="n")
axis(1, format(seq(min(d2$Date),max(d2$Date),60), "%b %Y"), at = seq(min(d2$Date),max(d2$Date),60),
     cex=.5)

plot(d2$Date,log(d2$vole+1), pch=19, ylim=c(0,8), lty=1, type="b", xaxt="n", ylab="log(photo counts+1)", xlab="")
points(d2$Date,log(d2$lweasel), pch=19, ylim=c(0,8), col=4, type="b")
#lines(smooth.spline(d2$Date,log(d2$lweasel+1),spar=.2), pch=19, ylim=c(0,8), lty=1, type="l", col=4)
axis(1, format(seq(min(d2$Date),max(d2$Date),60), "%b %Y"), 
     at = seq(min(d2$Date),max(d2$Date),60))

legend("topright",c("vole sp.","least weasel"),pch=19, col=c(1,4), bty="n")

cor.test(d2$vole,d2$stoat)
cor.test(d2$stoat,d2$vole)

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

