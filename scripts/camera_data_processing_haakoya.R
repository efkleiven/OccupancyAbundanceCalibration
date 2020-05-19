# set working directory
wd <- "Y:/bilder_smagnagerfotobokser/Data/MLWIC_classification"
setwd(wd)

# look at files in the directory
dir() 

# import files from Komag based on file position in the folder
ctdata <- list() 
ctdata[[1]] <-read.csv(dir()[7]) 
ctdata[[2]] <-read.csv(dir()[8])

str(ctdata)
ctdata <- do.call(rbind, ctdata)

datesplit <- function(filename)  # retrieve date and station from file name
{
  filename2 <- as.character(filename)
  string <- strsplit(filename2,"_")        # split filename at _
  
  station <- strsplit(string[[1]][3],"/")[[1]][3] # pick the stationID
  date <- string[[1]][4] # pick the date
  
  outdf <- c(station, date) # collect the things you want the function to output
  return(outdf)
} # end function

datesite <- as.data.frame(t(sapply(ctdata$fileName,datesplit))) # run function and store as df
head(datesite)

names(datesite) <- c("site","date") # change colnames

datesite$date <- as.Date(datesite$date)
datesite$day <- format(datesite$date, format = "%d")
datesite$month <- format(datesite$date, format="%m")
datesite$year <- format(datesite$date, format="%Y")
datesite$julian <- julian(datesite$date, origin=min(datesite$date)) #different origin than for porsanger

df <- cbind(ctdata,datesite) # merge date and site df with initial df
head(df) # check that its ok

#
#0=bad quality, 1=empty, 2=bird, 3=vole, 4=least_weasel, 5=lemming, 6=shrew, 7=stoat, 

# select images classified with more than 75% certainty
df$answer <- ifelse(df$confidence1>0.75,df$guess1,0)

df <- df[,-c(1:2,4:8,10:13)] # remove unuseful columns
str(df)# check that the df is fine

table(df$answer)

# add column for vole, lemming, stoat and least weasel
df$vole <- ifelse(df$answer==3,1,0)
table(df$vole)

df$stoat <- ifelse(df$answer==7,1,0)
table(df$stoat)

# sum animals per day (count)
vole_c <- aggregate(vole~date, data=df,sum)
vole_c$date <- as.Date(vole_c$date)

stoat_c <- aggregate(stoat~date, data=df,sum)
stoat_c$date <- as.Date(stoat_c$date)

setwd("C:/Eivind/OccupancyAbundanceCalibration")
png(filename = "hakoya_cam_count.png", width=2000, height=1200)
par(mar = c(5,7,4,2) + 0.1)
plot(vole~date, data=vole_c, type="l", col="black", ylim=c(0,190), ylab="count",main="2018-2019",
     lwd=3, cex=2, cex.main=2,  cex.lab=2, axes=F)
axis(side=1, as.Date(c("2017-11-15","2018-01-15","2018-03-15","2018-05-15","2018-07-15","2018-09-15","2018-11-15",
                       "2019-01-15","2019-03-15","2019-05-15","2019-07-15","2019-09-15","2019-11-15")),
     c("nov","jan","mar","may","jul","sep","nov","jan","mar","may","jul","sep","nov"), cex.axis=1.5)
axis(side=2, c(0,50,100,150,200), cex.axis=2)
lines(stoat~date, data=stoat_c, col="red", lwd=3)
legend("topleft",legend=c("vole","stoat"),lty=1, lwd=2,
       col=c("black","red"), cex=2)
dev.off()

# mean animals per day (occuoancy)
vole_occ <- aggregate(vole~date, data=df,max)

vole_occ$date <- as.Date(vole_occ$date)

stoat_occ <- aggregate(stoat~date, data=df,max)
stoat_occ$date <- as.Date(stoat_occ$date)

png(filename = "hakoya_cam_occ.png", width=2000, height=1200)
par(mar = c(5,7,4,2) + 0.1)
plot(vole~date, data=vole_occ, type="p", col="black", ylab="count", main="2018-2019", cex=2, pch=19,
     axes=F, cex.lab=2.5, cex.main=2)
axis(side=1, as.Date(c("2017-11-15","2018-01-15","2018-03-15","2018-05-15","2018-07-15","2018-09-15","2018-11-15",
                       "2019-01-15","2019-03-15","2019-05-15","2019-07-15","2019-09-15","2019-11-15")),
     c("nov","jan","mar","may","jul","sep","nov","jan","mar","may","jul","sep","nov"), cex.axis=1.5)
axis(side=2, c(0,0.5,1), cex.axis=2)
points(stoat~date, data=stoat_occ, col="red", cex=4)
legend("left",legend=c("vole","stoat"),pch=19,
       col=c("black","red"), cex=2)
dev.off()
