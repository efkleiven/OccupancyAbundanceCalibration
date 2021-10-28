## Plot Håkøya Counts over time
library(data.table)
library(dplyr)

# Håkøya data
# cameratrap2 <- readRDS("data/cameratrap/haakoya/processed/haakoya_cameradata_allsp.rds")
# Porsanger data
#cameratrap2 <- readRDS("~/Documents/OccupancyAbundanceCalibration/data/camera_porsanger_095_allsp.rds")


#table(cameratrap2$species)
#min(cameratrap2$date)

# for Håkøya
cameratrap2 <- readRDS("C:/Eivind/GitProjects/OccupancyAbundanceCalibration/data/camera_data_095confidence_processed_haakoya.rds")

# ht0 <- filter(cameratrap2, species %in% c("vole","stoat"))
cameratrap2$count <- 1
ht0 <- cameratrap2
#ht0$date <- as.Date(ht0$DateTimeOriginal)
ht1 <- aggregate(count~date+species+site, data=ht0, FUN=sum)
table(ht1$species)
hvoledata <- filter(ht1, species=="vole")
hstoatdata <- filter(ht1, species=="stoat")
hlemdata <- filter(ht1, species=="lemming")
hshrewdata <- filter(ht1, species=="shrew")
hlweaseldata <- filter(ht1, species=="least_weasel")
#min(d4$date)

### EVERY Month ###

timewindow <- 30 #days
DT <- data.table(hvoledata)
v2 <- DT[, .(vole = sum(count)), 
         keyby = .(Date = timewindow * (as.numeric(date - min(hvoledata$date)) %/% timewindow) + min(hvoledata$date))]

DT <- data.table(hstoatdata)
st2 <- DT[, .(stoat = sum(count)), 
         keyby = .(Date = timewindow * (as.numeric(date - min(hvoledata$date)) %/% timewindow) + min(hvoledata$date))]


DT <- data.table(hshrewdata)
sh2 <- DT[, .(shrew = sum(count)), 
         keyby = .(Date = timewindow * (as.numeric(date - min(hvoledata$date)) %/% timewindow) + min(hvoledata$date))]

#DT <- data.table(hlemdata)
#l2 <- DT[, .(lemming = sum(count)), 
#         keyby = .(Date = timewindow * (as.numeric(date - min(hvoledata$date)) %/% timewindow) + min(hvoledata$date))]

#DT <- data.table(hlweaseldata)
#lw2 <- DT[, .(least_weasel = sum(count)), 
#         keyby = .(Date = timewindow * (as.numeric(date - min(hvoledata$date)) %/% timewindow) + min(hvoledata$date))]

DT <- data.table(hshrewdata)
sh2 <- DT[, .(shrew = sum(count)), 
         keyby = .(Date = timewindow * (as.numeric(date - min(hvoledata$date)) %/% timewindow) + min(hvoledata$date))]

v3 <- as.data.frame(v2)
st3 <- as.data.frame(st2)
#lw3 <- as.data.frame(lw2)
sh3 <- as.data.frame(sh2)
#l3 <- as.data.frame(l2)

d1 <- left_join(v3,st3, by="Date")
#d2 <- left_join(d1,lw3, by="Date")
d3 <- left_join(d1,sh3, by="Date")
#d4 <- left_join(d3,l3, by="Date")


getwd()
#pdf("~/Documents/OccupancyAbundanceCalibration/plots/haakoya_species_counts_month.pdf", width=8*1.2, height=6*1.2)
pdf("~/Documents/OccupancyAbundanceCalibration/plots/hakoya_species_counts_month.pdf", width=8*1.2, height=6*1.2)
pdf("./plots/hakoya_species_counts_month3.pdf", width=8*1.2, height=6*1.2)

par(mfrow=c(1,1))
plot(d1$Date,log(d3$vole+1), pch=19, ylim=c(0,12), lty=1, type="b", xaxt="n", ylab="log(photo counts+1)", xlab="", cex=1.5, main="Håkøya")
points(d1$Date,log(d3$stoat), pch=19, ylim=c(0,8), col=2, type="b", cex=1.5)
#points(d1$Date,log(d4$least_weasel), pch=19, ylim=c(0,8), col=3, type="b")
#points(d1$Date,log(d4$lemming), pch=19, ylim=c(0,8), col=4, type="b")
points(d1$Date,log(d3$shrew), pch=19, ylim=c(0,8), col=7, type="b", cex=1.5)

#lines(smooth.spline(d2$Date,log(d2$stoat),spar=.3), pch=19, ylim=c(0,8), lty=1, type="l", col=2)
legend("topright",c(
                  # "tundra vole", 
                    "vole sp.",
                    "stoat", "shrew"),pch=19, col=c(1,2,7), bty="n", cex=1.5)
axis(1, format(seq(min(d1$Date),max(d1$Date),60), "%b %Y"), at = seq(min(d1$Date),max(d1$Date),60),
     cex=.7)
dev.off()

cor.test(d3$shrew,d3$vole)

#plot(d1$Date,d4$vole, pch=19, ylim=c(0,1900), lty=1, type="b", xaxt="n", ylab="log(photo counts+1)", xlab="")
#points(d1$Date,d4$stoat, pch=19, ylim=c(0,8), col=2, type="b")
#points(d1$Date,d4$least_weasel, pch=19, ylim=c(0,8), col=3, type="b")
#points(d1$Date,d4$lemming, pch=19, ylim=c(0,8), col=4, type="b")
#points(d1$Date,d4$shrew, pch=19, ylim=c(0,8), col=7, type="b")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# make species plot per station on Håkøya for vole

h1 <- filter(hvoledata, site=="H1")
h2 <- filter(hvoledata, site=="H2")
h3 <- filter(hvoledata, site=="H3")
h4 <- filter(hvoledata, site=="H4")

# montly
timewindow <- 30 #days

H1 <- data.table(h1)
v21 <- H1[, .(vole = sum(count)), 
         keyby = .(Date = timewindow * (as.numeric(date - min(hvoledata$date)) %/% timewindow) + min(hvoledata$date))]

H2 <- data.table(h2)
v22 <- H2[, .(vole = sum(count)), 
          keyby = .(Date = timewindow * (as.numeric(date - min(hvoledata$date)) %/% timewindow) + min(hvoledata$date))]

H3 <- data.table(h3)
v23 <- H3[, .(vole = sum(count)), 
          keyby = .(Date = timewindow * (as.numeric(date - min(hvoledata$date)) %/% timewindow) + min(hvoledata$date))]

H4 <- data.table(h4)
v24 <- H4[, .(vole = sum(count)), 
          keyby = .(Date = timewindow * (as.numeric(date - min(hvoledata$date)) %/% timewindow) + min(hvoledata$date))]

v31 <- as.data.frame(v21)
names(v31)[2]<-"h1"
v32 <- as.data.frame(v22)
names(v32)[2]<-"h2"
v33 <- as.data.frame(v23)
names(v33)[2]<-"h3"
v34 <- as.data.frame(v24)
names(v34)[2]<-"h4"

d1 <- left_join(v32,v31, by="Date")
d2 <- left_join(d1,v33, by="Date")
d3 <- left_join(d2,v34, by="Date")

# change wd to save plot
setwd("./plots")

png(filename = "spatial_cor_hakoya_month.png",width = 480*3, height = 480*1.5)

par(mfrow=c(1,1))
plot(d3$Date,log(d3$h1+1), pch=19, ylim=c(0,8), lty=1, type="b", xaxt="n", ylab="log(photo counts+1)", xlab="", cex=1.5, cex.lab=1.5, lwd=2, cex.axis=2, cex.main=2, main="Håkøya")
lines(d3$Date,log(d3$h2+1), pch=19, lty=1, type="b", col=2, cex=1.5, lwd=2)
lines(d3$Date,log(d3$h3+1), pch=19, lty=1, type="b", col=3, cex=1.5, lwd=2)
lines(d3$Date,log(d3$h4+1), pch=19, lty=1, type="b", col=4, cex=1.5, lwd=2)
legend("topright",c("h1","h2", "h3", "h4"), pch=19, col=c(1:4), bty="n", cex=1.5)
axis(1, format(seq(min(d1$Date),max(d1$Date),60), "%b %Y"), at = seq(min(d1$Date),max(d1$Date),60),
     cex=1.5, cex.axis=2)


dev.off()

# weekly
timewindow <- 7 #days

H1 <- data.table(h1)
v21 <- H1[, .(vole = sum(count)), 
          keyby = .(Date = timewindow * (as.numeric(date - min(hvoledata$date)) %/% timewindow) + min(hvoledata$date))]

H2 <- data.table(h2)
v22 <- H2[, .(vole = sum(count)), 
          keyby = .(Date = timewindow * (as.numeric(date - min(hvoledata$date)) %/% timewindow) + min(hvoledata$date))]

H3 <- data.table(h3)
v23 <- H3[, .(vole = sum(count)), 
          keyby = .(Date = timewindow * (as.numeric(date - min(hvoledata$date)) %/% timewindow) + min(hvoledata$date))]

H4 <- data.table(h4)
v24 <- H4[, .(vole = sum(count)), 
          keyby = .(Date = timewindow * (as.numeric(date - min(hvoledata$date)) %/% timewindow) + min(hvoledata$date))]

v31 <- as.data.frame(v21)
names(v31)[2]<-"h1"
v32 <- as.data.frame(v22)
names(v32)[2]<-"h2"
v33 <- as.data.frame(v23)
names(v33)[2]<-"h3"
v34 <- as.data.frame(v24)
names(v34)[2]<-"h4"

d1 <- left_join(v32,v31, by="Date")
d2 <- left_join(d1,v33, by="Date")
d3 <- left_join(d2,v34, by="Date")

png(filename = "spatial_cor_hakoya_week.png", width = 480*3, height = 480*1.5)
par(mfrow=c(1,1))
plot(d3$Date,log(d3$h1+1), pch=19, ylim=c(0,7), lty=1, type="b", xaxt="n", ylab="photo counts", xlab="",cex=1.5, cex.lab=1.5, lwd=2, cex.axis=2, cex.main=2, main="Håkøya")
lines(d3$Date,log(d3$h2+1), pch=19, lty=1, type="b", col=2, cex=1.5, lwd=2)
lines(d3$Date,log(d3$h3+1), pch=19, lty=1, type="b", col=3,cex=1.5, lwd=2)
lines(d3$Date,log(d3$h4+1), pch=19, lty=1, type="b", col=4, cex=1.5,lwd=2)
legend("topright",c("h1","h2", "h3", "h4"), pch=19, col=c(1:4), bty="n", cex=1.5)
axis(1, format(seq(min(d1$Date),max(d1$Date),60), "%b %Y"), at = seq(min(d1$Date),max(d1$Date),60),
     cex=1.5, cex.axis=2)

dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# for Porsanger

cameratrap2 <- readRDS("./data/camera_porsanger_095_allsp.rds")

# ht0 <- filter(cameratrap2, species %in% c("vole","stoat"))
cameratrap2$count <- 1
ht0 <- cameratrap2
#ht0$date <- as.Date(ht0$DateTimeOriginal)
ht1 <- aggregate(count~date+species+site, data=ht0, FUN=sum)
table(ht1$species)
hvoledata <- filter(ht1, species=="vole")
hstoatdata <- filter(ht1, species=="stoat")
hlemdata <- filter(ht1, species=="lemming")
hshrewdata <- filter(ht1, species=="shrew")
hlweaseldata <- filter(ht1, species=="least_weasel")
#min(d4$date)

### EVERY Month ###

timewindow <- 30 #days
DT <- data.table(hvoledata)
v2 <- DT[, .(vole = sum(count)), 
         keyby = .(Date = timewindow * (as.numeric(date - min(hvoledata$date)) %/% timewindow) + min(hvoledata$date))]

DT <- data.table(hstoatdata)
st2 <- DT[, .(stoat = sum(count)), 
          keyby = .(Date = timewindow * (as.numeric(date - min(hvoledata$date)) %/% timewindow) + min(hvoledata$date))]


DT <- data.table(hshrewdata)
sh2 <- DT[, .(shrew = sum(count)), 
          keyby = .(Date = timewindow * (as.numeric(date - min(hvoledata$date)) %/% timewindow) + min(hvoledata$date))]

DT <- data.table(hlemdata)
l2 <- DT[, .(lemming = sum(count)), 
         keyby = .(Date = timewindow * (as.numeric(date - min(hvoledata$date)) %/% timewindow) + min(hvoledata$date))]

DT <- data.table(hlweaseldata)
lw2 <- DT[, .(least_weasel = sum(count)), 
         keyby = .(Date = timewindow * (as.numeric(date - min(hvoledata$date)) %/% timewindow) + min(hvoledata$date))]

DT <- data.table(hshrewdata)
sh2 <- DT[, .(shrew = sum(count)), 
          keyby = .(Date = timewindow * (as.numeric(date - min(hvoledata$date)) %/% timewindow) + min(hvoledata$date))]

v3 <- as.data.frame(v2)
st3 <- as.data.frame(st2)
lw3 <- as.data.frame(lw2)
sh3 <- as.data.frame(sh2)
l3 <- as.data.frame(l2)

d5 <- left_join(v3,st3, by="Date")
d6 <- left_join(d5,lw3, by="Date")
d7 <- left_join(d6,sh3, by="Date")
d8 <- left_join(d7,l3, by="Date")


getwd()
#pdf("~/Documents/OccupancyAbundanceCalibration/plots/haakoya_species_counts_month.pdf", width=8*1.2, height=6*1.2)
pdf("~/Documents/OccupancyAbundanceCalibration/plots/porsanger_species_counts_month.pdf", width=8*1.2, height=6*1.2)
pdf("./plots/porsanger_species_counts_month.pdf", width=8*1.2, height=6*1.2)

par(mfrow=c(1,1))
plot(d8$Date,log(d8$vole+1), pch=19, ylim=c(0,12), lty=1, type="b", xaxt="n", ylab="log(photo counts+1)", xlab="", cex=1.5, main="Porsanger")
points(d8$Date,log(d8$stoat), pch=19, ylim=c(0,8), col=2, type="b", cex=1.5)
points(d8$Date,log(d8$least_weasel), pch=19, ylim=c(0,8), col=3, type="b", cex=1.5)
points(d8$Date,log(d8$lemming), pch=19, ylim=c(0,8), col=4, type="b", cex=1.5)
points(d8$Date,log(d8$shrew), pch=19, ylim=c(0,8), col=7, type="b", cex=1.5)

#lines(smooth.spline(d2$Date,log(d2$stoat),spar=.3), pch=19, ylim=c(0,8), lty=1, type="l", col=2)
legend("topright",c(
  # "tundra vole", 
  "vole sp.",
  "stoat", "least weasel","lemming" ,"shrew"),pch=19, col=c(1,2,3,4,7), bty="n", cex=1.5)
axis(1, format(seq(min(d8$Date),max(d8$Date),60), "%b %Y"), at = seq(min(d8$Date),max(d8$Date),60),
     cex=.7)
dev.off()

cor.test(d3$shrew,d3$vole)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pdf("./plots/species_counts_month.pdf", width=16*1.2, height=6*1.2)

par(mfrow=c(1,2))
plot(d8$Date,log(d8$vole+1), pch=19, ylim=c(0,12), lty=1, type="b", xaxt="n", ylab="log(photo counts+1)", xlab="", cex=1.5, cex.axis=1.5, cex.lab=1.5)
points(d8$Date,log(d8$stoat), pch=19, ylim=c(0,8), col=2, type="b", cex=1.5)
points(d8$Date,log(d8$least_weasel), pch=19, ylim=c(0,8), col=3, type="b", cex=1.5)
points(d8$Date,log(d8$lemming), pch=19, ylim=c(0,8), col=4, type="b", cex=1.5)
points(d8$Date,log(d8$shrew), pch=19, ylim=c(0,8), col=7, type="b", cex=1.5)
text(x=(min(d8$Date)+80), y=11.5, "Porsanger", cex=2)

#lines(smooth.spline(d2$Date,log(d2$stoat),spar=.3), pch=19, ylim=c(0,8), lty=1, type="l", col=2)
legend("topright",c(
  # "tundra vole", 
  "vole sp.",
  "stoat", "least weasel","lemming" ,"shrew"),pch=19, col=c(1,2,3,4,7), bty="n", cex=1.5)
axis(1, format(seq(min(d8$Date),max(d8$Date),60), "%b %Y"), at = seq(min(d8$Date),max(d8$Date),60),
     cex.axis=1.5)

plot(d1$Date,log(d3$vole+1), pch=19, ylim=c(0,12), lty=1, type="b", xaxt="n", ylab="log(photo counts+1)", xlab="", cex=1.5, cex.axis=1.5, cex.lab=1.5)
points(d1$Date,log(d3$stoat), pch=19, ylim=c(0,8), col=2, type="b", cex=1.5)
#points(d1$Date,log(d4$least_weasel), pch=19, ylim=c(0,8), col=3, type="b")
#points(d1$Date,log(d4$lemming), pch=19, ylim=c(0,8), col=4, type="b")
points(d1$Date,log(d3$shrew), pch=19, ylim=c(0,8), col=7, type="b", cex=1.5)
text(x=(min(d1$Date)+60), y=11.5, "Håkøya", cex=2)

#lines(smooth.spline(d2$Date,log(d2$stoat),spar=.3), pch=19, ylim=c(0,8), lty=1, type="l", col=2)
legend("topright",c(
 "tundra vole", 
  #"vole sp.",
  "stoat", "shrew"),pch=19, col=c(1,2,7), bty="n", cex=1.5)
axis(1, format(seq(min(d1$Date),max(d1$Date),60), "%b %Y"), at = seq(min(d1$Date),max(d1$Date),60),
     cex.axis=1.5)

dev.off()

cor.test(d3$shrew,d3$vole)



