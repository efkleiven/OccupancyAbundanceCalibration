## Plot Håkøya Counts over time
library(data.table)
library(dplyr)
cameratrap2 <- readRDS("data/cameratrap/haakoya/processed/haakoya_cameradata_allsp.rds")

# ht0 <- filter(cameratrap2, species %in% c("vole","stoat"))
cameratrap2$count <- 1
ht0 <- cameratrap2
ht0$date <- as.Date(ht0$DateTimeOriginal)
ht1 <- aggregate(count~date+species+site, data=ht0, FUN=sum)
table(ht1$species)
hvoledata <- filter(ht1, species=="vole")
hstoatdata <- filter(ht1, species=="stoat")
hlemdata <- filter(ht1, species=="lemming")
hshrewdata <- filter(ht1, species=="shrew")
hlweaseldata <- filter(ht1, species=="least_weasel")


### EVERY Month ###
timewindow <- 14

DT <- data.table(hvoledata)
v2 <- DT[, .(vole = sum(count)), 
         keyby = .(Date = 7 * (as.numeric(date - min(hvdata$date)) %/% timewindow) + min(hvdata$date))]

DT <- data.table(hstoatdata)

st2 <- DT[, .(stoat = sum(count)), 
         keyby = .(Date = 7 * (as.numeric(date - min(hvdata$date)) %/% timewindow) + min(hvdata$date))]

DT <- data.table(hlemdata)
l2 <- DT[, .(lemming = sum(count)), 
         keyby = .(Date = 7 * (as.numeric(date - min(hvdata$date)) %/% timewindow) + min(hvdata$date))]

DT <- data.table(hlweaseldata)
lw2 <- DT[, .(least_weasel = sum(count)), 
         keyby = .(Date = 7 * (as.numeric(date - min(hvdata$date)) %/% timewindow) + min(hvdata$date))]

DT <- data.table(hshrewdata)
sh2 <- DT[, .(shrew = sum(count)), 
         keyby = .(Date = 7 * (as.numeric(date - min(hvdata$date)) %/% timewindow) + min(hvdata$date))]


v3 <- as.data.frame(v2)
st3 <- as.data.frame(st2)
lw3 <- as.data.frame(lw2)
sh3 <- as.data.frame(sh2)
l3 <- as.data.frame(l2)

d1 <- left_join(v3,st3, by="Date")
d2 <- left_join(d1,lw3, by="Date")
d3 <- left_join(d2,sh3, by="Date")
d4 <- left_join(d3,l3, by="Date")



d1$stoat <- ifelse(is.na(d1$stoat),0,d1$stoat)

par(mfrow=c(1,1))
plot(d1$Date,log(d4$vole+1), pch=19, ylim=c(0,12), lty=1, type="b", xaxt="n", ylab="log(photo counts+1)", xlab="")
points(d1$Date,log(d4$stoat), pch=19, ylim=c(0,8), col=2, type="b")
points(d1$Date,log(d4$least_weasel), pch=19, ylim=c(0,8), col=3, type="b")
points(d1$Date,log(d4$lemming), pch=19, ylim=c(0,8), col=4, type="b")
points(d1$Date,log(d4$shrew), pch=19, ylim=c(0,8), col=7, type="b")

#lines(smooth.spline(d2$Date,log(d2$stoat),spar=.3), pch=19, ylim=c(0,8), lty=1, type="l", col=2)
legend("topright",c("vole sp.", "stoat","least weasel", "lemming", "shrew"),pch=19, col=c(1,2,3,4,7), bty="n")
axis(1, format(seq(min(d1$Date),max(d1$Date),60), "%b %Y"), at = seq(min(d1$Date),max(d1$Date),60),
     cex=.5)

cor.test(d4$shrew,d4$vole)
?cor
