
# libraries
library(dplyr)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Import data files from Porsanger

# latest file for abundance encompassing days 
setwd("C:/Users/ekl013/OneDrive - UiT Office 365/GitProjects/OccupancyAbundanceCalibration/data")
PorD <- readRDS("porsanger_mean_intervals.rds")

# filter only gray-sided vole and remove observations from G19 when the flash had failed
pormean <- filter(PorD, species=="GRAASIDEMUS"  & !(station == "G19" & trapseason <= 6))

# standardize CMR estimate to be per livetrap
pormean$Abundance_HT  <- pormean$Abundance_HT/16 # divide by number of traps

# transform to log(x + 1)
pormean[,4:ncol(pormean)]<- log(pormean[,4:ncol(pormean)]+1)

# mean per season for best variable (int_2) and CMR estimated abundace
pormean2_ct <- aggregate(int_2~trapseason,data=pormean, FUN=mean)
pormean2_cmr <- aggregate(Abundance_HT~trapseason,data=pormean, FUN=mean)

# sd per season for best variable (int_2) and CMR estimated abundace
porsd_ct <- aggregate(int_2~trapseason,data=pormean, FUN=sd)
porsd_cmr <- aggregate(Abundance_HT~trapseason,data=pormean, FUN=sd)

# calculate SE
po_se_ct <-porsd_ct/sqrt(15)
po_se_cmr <-porsd_cmr/sqrt(15) 

# import data file containing time of trapping seasons on porsanger
setwd("./capture_recapture/porsanger")

cr <- readRDS("porsanger_abundance_perspecies.rds")

time <- cr$datetime[1:9]  # removing dates for the removed G19 season (where flash failed)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# prepare data from Håkøya

# latest file for abundance encompassing days 
setwd("C:/Users/ekl013/OneDrive - UiT Office 365/GitProjects/OccupancyAbundanceCalibration/data")
HaD <- readRDS("haakoya_mean_intervals.rds")
summary(HaD)

# transform to log(x + 1)
HaD[,4:ncol(HaD)]<- log(HaD[,4:ncol(HaD)]+1)

# mean per season for best variable (int_2) and CMR estimated abundace
hamean_ct <- aggregate(previousday~trapsession,data=HaD, FUN=mean)
hamean_cmr <- aggregate(D50~trapsession,data=HaD, FUN=mean)

# sd per season for best variable (int_2) and CMR estimated abundace
hasd_ct <- aggregate(previousday~trapsession,data=HaD, FUN=sd)
hasd_cmr <- aggregate(D50~trapsession,data=HaD, FUN=sd)

# calculate SE
se_ct <-hasd_ct/sqrt(4)
se_cmr <-hasd_cmr/sqrt(4) 

# import data file contining time of trapping seasons on porsanger
# read CMR-data
setwd("./capture_recapture/haakoya")

### read trapping dates
dates_h <- tibble(readRDS("./processed/haakoya_trap_dates.rds"))

dates_h2 <- filter(dates_h, who=="R")

dates_h3 <- aggregate(date ~ trapsession + check,data=dates_h2, FUN=median)

time_h <- as.Date(dates_h3$date[1:15])


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# make plot
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#setwd("../../../plots")

xlab <- as.Date(time)[c(2,5,8)]

#############
## without bars 

#png("TimeSeries_CTcountsCRabundance_2.png", width = 1200, height=600)

par(mfrow=c(1,2), mar=c(5,7,4,5))

plot(pormean2_ct$int_2~as.Date(time), type="b",pch=16, lwd=2, col="skyblue", ylim=c(0,3),lty=2 ,xlab="", ylab="",cex.main=1.5, cex.lab=1.5,cex.axis=1.5 , cex=2, main="Gray-sided vole", xaxt='n')
lines(pormean2_cmr$Abundance_HT~as.Date(time),  type="b", lty=2, pch=17,cex=2, lwd=2, col="orange")
axis(1, at=as.Date(xlab), lab=c("2018", "2019", "2020"),  cex.axis=1.5)
#mtext("CMR-Abundance",side=4, padj=3, cex=1.5 )
#mtext("CT-counts",side=2, padj=-3.5 , cex=1.5)


#par(mar=c(5,4,4,3.5))
plot(hamean_ct$previousday ~ time_h, type="b", pch=16,lwd=2, col="skyblue", ylim=c(0,3),xlab="",lty=2, ylab="",cex.main=1.5, cex.lab=1.5, cex.axis=1.5, cex=2, main="Tundra vole", xaxt='n')
lines(hamean_cmr$D50~time_h, type="b", lty=2, lwd=2, pch=17,cex=2, col="orange")
axis(1, at=as.Date(xlab), lab=c("2018", "2019", "2020"),  cex.axis=1.5)
#axis(4, at=c(20,40,60,80,100), lab=c(20,40,60,80,100)*0.5,cex.axis=1.5)
#mtext("CMR-Abundance",side=4, padj=3.5, cex=1.5)
#mtext("CT-counts",side=2, padj=-3.5, cex=1.5)
legend("topright", legend=c("CMR-abundance","CT-counts"), col=c("orange", "skyblue"), pch=c(17,16), cex=1.3)
#dev.off()


###############
## With SE bars

setwd("C:/Users/ekl013/OneDrive - UiT Office 365/GitProjects/OccupancyAbundanceCalibration/plots")
png("TimeSeries_LogCTcountsCRabundance_2.png", width = 1200, height=600)
par(mfrow=c(1,2), mar=c(5,7,4,5))

library(gplots)

#gplots :: plotCI(as.Date(timelab$date),log(vole2$count/15+1),vecsd_po, col="skyblue", ylim=c(-0.9,5), ylab="CT-index / CMR abundance", xlab="",
#                 pch=19, cex.main=1.5, cex.lab=1.5,cex.axis=1.5 , cex=2, main="Gray-sided vole", xaxt='n', yaxt='n', type="b", lty=2) 

plotCI(as.Date(time), pormean2_ct$int_2, po_se_ct$int_2, col="skyblue", ylim=c(0,3), ylab="CT-index / CMR abundance", xlab="", pch=19, cex.main=1.5, cex.lab=1.5,cex.axis=1.5 , cex=2, main="Gray-sided vole", 
                 xaxt='n', yaxt='n', type="b",  lwd=2.5, gap=0) 
plotCI(as.Date(time)+10, pormean2_cmr$Abundance_HT, po_se_cmr$Abundance_HT, col="orange", ylim=c(0,4), ylab="CT-index", cex.lab=1.3, pch=19, cex=2, add=T, type="b", lwd=2.5, gap=0) 
axis(1, at=as.Date(xlab), lab=c("2018", "2019", "2020"),  cex.axis=1.5)
axis(2, at=c(0,1,2,3,4), lab=c(0,1,2,3,4),  cex.axis=1.5)


#plot(log(vole2$count/15+1)~as.Date(timelab$date), type="b",pch=16, lwd=2, col="skyblue", ylim=c(0,4),lty=2 ,xlab="", ylab="CT-index / CMR-abundance",cex.main=1.5, cex.lab=1.5,cex.axis=1.5 , cex=2, main="Gray-sided vole", xaxt='n')
#lines(log(N.est/15+1)~time, data=CMR, type="b", lty=2, pch=17,cex=2, lwd=2, col="orange")
#axis(1, at=as.Date(xlab), lab=c("2018", "2019", "2020"),  cex.axis=1.5)
#legend("topleft", legend=c("CMR-abundance","CT-index"), col=c("orange", "skyblue"), pch=c(17,16), cex=1.3)

#mtext("CMR-Abundance",side=4, padj=3, cex=1.5 )
#mtext("CT-counts",side=2, padj=-3.5 , cex=1.5)

# Tundra vole
plotCI(time_h,hamean_ct$previousday,se_ct$previousday, col="skyblue", ylim=c(0,3), ylab="CT-index / CMR abundance", xlab="", pch=19, cex.main=1.5, cex.lab=1.5,cex.axis=1.5 , cex=2, main="Tundra vole", xaxt='n', yaxt='n', type="b", lwd=2.5, gap=0) 
plotCI(time_h+10,hamean_cmr$D50, se_cmr$D50, col="orange", ylim=c(0,3), ylab="CT-index", cex.lab=1.3, pch=19, cex=2, add=T, type="b", lwd=2.5, gap=0) 
axis(1, at=as.Date(xlab), lab=c("2018", "2019", "2020"),  cex.axis=1.5)
axis(2, at=c(0,1,2,3,4), lab=c(0,1,2,3,4),  cex.axis=1.5)
legend("topright", legend=c("CMR-abundance","CT-index"), col=c("orange", "skyblue"), pch=c(17,16), cex=1.3)

#par(mar=c(5,4,4,3.5))
#plot(log(CT_all_h$count/4+1) ~ datesR$date, type="b", pch=16,lwd=2, col="skyblue", ylim=c(0,4),xlab="",lty=2, ylab="",cex.main=1.5, cex.lab=1.5, cex.axis=1.5, cex=2, main="Tundra vole", xaxt='n')
#lines(log(CMR_h$inlacr/4+1) ~ datesR$date,type="b", lty=2, lwd=2, pch=17,cex=2, col="orange")
#axis(1, at=as.Date(xlab), lab=c("2018", "2019", "2020"),  cex.axis=1.5)
#axis(4, at=c(20,40,60,80,100), lab=c(20,40,60,80,100)*0.5,cex.axis=1.5)
#mtext("CMR-Abundance",side=4, padj=3.5, cex=1.5)
#mtext("CT-counts",side=2, padj=-3.5, cex=1.5)

dev.off()




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# calculate mean, sd and range for table

mean(pormean$int_2)
sd(pormean$int_2)
range(pormean$int_2)


mean(pormean$Abundance_HT)
sd(pormean$Abundance_HT)
range(pormean$Abundance_HT)

mean(HaD$previousday)
sd(HaD$previousday)
range(HaD$previousday)

mean(HaD$D50)
sd(HaD$D50)
range(HaD$D50)
