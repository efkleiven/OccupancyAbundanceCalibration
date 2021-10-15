# set working directory
#wd <- "Y:/bilder_smagnagerfotobokser/Data/MLWIC_classification"
wd1 <- "/Users/pni026/Documents/OccupancyAbundanceCalibration/"
setwd(wd1)

# this is the old link, should now be data/cameratrap/new_classifications/haakoy,
# but the data structure is also different, now with folders for each year!
wd2 <- "data/cameratrap/haakoya"

# look at files in the directory
dir(wd2) 

wd3 <- paste0(wd1,wd2)
# import files from Komag based on file position in the folder
ctdata <- list() 
ctdata[[1]] <- read.csv(paste0(wd3,"/",dir(wd2)[1]))
ctdata[[2]] <- read.csv(paste0(wd3,"/",dir(wd2)[2]))
ctdata[[3]] <- read.csv(paste0(wd3,"/",dir(wd2)[3]))

# convert list to data frame 
ctdata <- do.call(rbind, ctdata)


datesplit <- function(filename)  # retrieve date and station from file name
{
  filename2 <- as.character(filename)
  string <- strsplit(filename2,"_")        # split filename at _
  string2 <- strsplit(filename2,"\\\\")[[1]] # split filename at \\\\
  
  station <- strsplit(string[[1]][3],"/")[[1]][3] # pick the stationID
  date <- string[[1]][4] # pick the date
  
  # remove last "'" 
  # obtain file name isolated
  newfilename1 <- strsplit(string2[3],"'")[[1]][1]
  
  outdf <- c(station, date, newfilename1) # collect the things you want the function to output
  return(outdf)
} # end 

# apply datesplit and store as df
datesite <- as.data.frame(t(sapply(ctdata$fileName,datesplit)))
rownames(datesite) <- NULL
names(datesite) <- c("site","date","NewFileName")
# NewFileName is needed to add the metadata

datesite$date <- as.Date(datesite$date)
# datesite$day <- format(datesite$date, format = "%d")
# datesite$month <- format(datesite$date, format="%m")
# datesite$year <- format(datesite$date, format="%Y")
# datesite$julian <- julian(datesite$date, origin=min(datesite$date)) #different origin than for porsanger

df <- cbind(ctdata,datesite) # merge date and site df with initial df
head(df) # check that its ok

# get species onto a column
whichsp <- function(x) 
  {label <- c("unknown", "empty", "bird", "vole", "least_weasel", "lemming", "shrew", "stoat")
  return(label[x+1])}
# apply whichsp 
df$species <- sapply(df$answer,whichsp)

# select images classified with more than 75% certainty

df$answer <- ifelse(df$confidence1>0.95,df$guess1,0)

# df2 <- df[,-c(1:2,4:8,10:13)] # remove useless columns
df3 <- df[,c(4,9,14,16,21)] # keep informative columns

# export data frame for metadata
# write.csv(df3, "data/haakoya_camera_answer.csv")

#### Plots ####


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
