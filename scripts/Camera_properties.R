# Correlogram data -----
GSVdata <- readRDS("data/cameratrap/porsanger/processed/GS_photos_porsanger.rds")
TVdata <- readRDS("data/cameratrap/haakoya/processed/tv_photos_haakoya.rds")
View(TVdata)
# cameras were not in place yet
GSVdata[GSVdata$trapseason==1,][,4:13] <- NA


## correlogram data processing -----
# create distance matrix (time distance matrix)
DMAT <-outer(-10:10, -10:10, '-')
DMAT2 <- DMAT[lower.tri(DMAT, diag = FALSE)]
rownames(DMAT) <- paste0("d",-10:10)
colnames(DMAT) <- paste0("d",-10:10)
diag(DMAT)
DMAT[upper.tri(DMAT, diag = TRUE)]
m <- data.frame(t(combn(rownames(DMAT),2)), dist=DMAT2)


# for Håkøya

exp.days <- c("d1","d2", "d3", "d0")
before <- paste0("d",-10:-1)
after <- paste0("d",10:4)

voledata <- TVdata[,3:23]

par(mfrow=c(1,2))

CR2 <- cor(voledata, use = "pairwise.complete.obs")
CR2[upper.tri(CR2)] <- NA
diag(CR2) <- NA
nCR <- reshape2::melt(CR2, varnames = c('X2', 'X1'), na.rm = TRUE)
mat2 <- dplyr::left_join(m,nCR)

# For Porsanger

voledata <- GSVdata[,4:24]


CR2 <- cor(voledata, use = "pairwise.complete.obs")
CR2[upper.tri(CR2)] <- NA
diag(CR2) <- NA
nCR <- reshape2::melt(CR2, varnames = c('X2', 'X1'), na.rm = TRUE)
mat3 <- dplyr::left_join(m,nCR)




# GSV 
PorD <- readRDS("data/porsanger_mean_intervals.rds")
pormean <- dplyr::filter(PorD, species=="GRAASIDEMUS" & trapseason > 1 & !(station == "G19" & trapseason <= 6))
pormean$Abundance_HT  <- pormean$Abundance_HT/16 # divide by number of traps
pormean[,4:ncol(pormean)]<- log(pormean[,4:ncol(pormean)]+1)

summary(pormean)
colMeans(pormean[,4:ncol(pormean)])
colMeans(porpre[,4:ncol(porpre)])

PorD2 <- readRDS("data/porsanger_mean_intervals_prewindow.rds")
porpre <- dplyr::filter(PorD2, species=="GRAASIDEMUS" & trapseason > 1 & !(station == "G19" & trapseason <= 6))
porpre$Abundance_HT  <- porpre$Abundance_HT/16 # divide by number of traps
porpre[,4:ncol(porpre)]<- log(porpre[,4:ncol(porpre)]+1)

summary(pormean)
summary(porpre)
# CMR-enc
encmeansP <- apply(pormean[,6:ncol(pormean)],2,"mean")
encupperP <- apply(pormean[,6:ncol(pormean)],2,sd)
enclowerP <- apply(pormean[,6:ncol(pormean)],2,sd)
# CMR-pre
premeansP <- apply(porpre[,6:ncol(porpre)],2,mean)
preupperP <- apply(porpre[,6:ncol(porpre)],2,sd)
prelowerP <- apply(porpre[,6:ncol(porpre)],2,sd)

meansP <- c(premeansP[1:10],encmeansP[1:12])
upperP <- c(preupperP[1:10],encupperP[1:12])
lowerP <- c(prelowerP[1:10],enclowerP[1:12])

# TV
hakenc <- readRDS("data/haakoya_mean_intervals.rds")
hakenc[,3:ncol(hakenc)]<- log(hakenc[,3:ncol(hakenc)]+1)
Hakpre <- readRDS("data/haakoya_mean_intervals_prewindow.rds")
Hakpre[,3:ncol(Hakpre)]<- log(Hakpre[,3:ncol(Hakpre)]+1)

encmeansH <- apply(hakenc[,6:ncol(hakenc)],2,mean)
encupperH <- apply(hakenc[,6:ncol(hakenc)],2,sd)
enclowerH <- apply(hakenc[,6:ncol(hakenc)],2,sd)

premeansH <- apply(Hakpre[,6:ncol(Hakpre)],2,mean)
preupperH <-apply(Hakpre[,6:ncol(Hakpre)],2,sd)
prelowerH <-apply(Hakpre[,6:ncol(Hakpre)],2,sd)

meansH <- c(premeansH[1:10],encmeansH[1:12])
upperH <- c(preupperH[1:10],encupperH[1:12])
lowerH <- c(prelowerH[1:10],enclowerH[1:12])

Plength <- Hlength <- c(c(1:10),seq(1,23,2))

# plots -----
library(plotrix)
pdf("Plots/Camera_properties.pdf",height=6, width=8)
par(mfrow=c(2,2),oma=c(0.5, 4, .5, .5))
Plength <- Hlength <- c(sort(-10:-1, decreasing=TRUE),seq(1,23,2))

# Gray-sided Vole
# CT variation
plotCI(Plength,meansP,upperP,lowerP, ylim=c(-0.5,2.5), col=c(rep("gray70",10),rep("gray40",12)), pch=19,
       ylab = "CT-index", xlab="Window")
mtext("Gray-sided Vole", cex=1.5, side=2, line = 5.5)

# correlogms
plot(mat2$dist,mat2$value, ylim=c(0,1), xlab = "Days apart", ylab="Correlation in Number of Photos", 
     main="", col=scales::alpha(1,.6), pch=19)
lines(smooth.spline(mat3$dist,mat3$value), col=1, lwd=2)
abline(h=0.5,lty=2)

# Tundra Vole
# CT variation
plotCI(x=Hlength,y=meansH,uiw=upperH,liw=lowerH, ylim=c(-0.5,2.5), col=c(rep("gray70",10),rep("gray40",12)), pch=19,
       ylab = "CT-index", xlab="Window")
mtext("Tundra Vole", cex=1.5, side=2, line = 5.5)

# correlogram
plot(mat2$dist,mat2$value, ylim=c(0,1), xlab = "Days apart", ylab="Correlation in Number of Photos", 
     main="Tundra Vole", col=1, pch=19)
lines(smooth.spline(mat2$dist,mat2$value), col=1, lwd=2)
abline(h=0.5,lty=2)

dev.off()

pdf("Plots/CT_correlogram.pdf",height=5, width=8.35)
par(mfrow=c(1,2),oma=c(0.5, 4, .5, .5))
Plength <- Hlength <- c(sort(-10:-1, decreasing=TRUE),seq(1,23,2))

# correlogms
plot(mat2$dist,mat2$value, ylim=c(0,1), xlab = "Days apart", ylab="Correlation in Number of Photos", 
     main="Gray-sided Vole", col=scales::alpha(1,.6), pch=19)
lines(smooth.spline(mat3$dist,mat3$value), col=1, lwd=2)
abline(h=0.5,lty=2)


# correlogram
plot(mat2$dist,mat2$value, ylim=c(0,1), xlab = "Days apart", ylab="Correlation in Number of Photos", 
     main="Tundra Vole", col=1, pch=19)
lines(smooth.spline(mat2$dist,mat2$value), col=1, lwd=2)
abline(h=0.5,lty=2)

dev.off()

### Porsanger Investigation
GS_photos_pors <- readRDS("data/GS_photos_porsanger.rds")
TVdata <- readRDS("data/cameratrap/haakoya/processed/tv_photos_haakoya.rds")
summary(GS_photos_pors)
GS1 <- dplyr::filter(GS_photos_pors, species=="GRAASIDEMUS" & trapseason > 1 & !(station == "G19" & trapseason <= 6))
GS2 <- select(GS1, -c('d-12','d-11','d12','d11'))
GS3 <- log(GS2[,4:ncol(GS2)]+1)

meanGS <- apply(GS3,2,mean)
sdGS <- apply(GS3,2,sd)

TV1 <- log(TVdata[,3:ncol(TVdata)]+1)
meanTV <- apply(TV1,2,mean)
sdTV <- apply(TV1,2,sd)

plotCI(x=-10:10,y=meanGS,uiw=sdGS,liw=sdGS, pch=19,
       ylab = "median log-counts", xlab="Days")

plotCI(x=-10:10,y=meanTV,uiw=sdTV,liw=sdTV, pch=19,
       ylab = "median log-counts", xlab="Days")
cnames  <- names(GS1[,4:ncol(GS1)])

## mean photos per day plots -----
pdf("Plots/Camera_properties.pdf",height=6, width=8)
par(mfrow=c(2,2),oma=c(0.5, 4, .5, .5))
Plength <- Hlength <- c(sort(-10:-1, decreasing=TRUE),seq(1,23,2))

# Gray-sided Vole
# CT variation
plotCI(x=-10:10,y=meanGS,uiw=sdGS,liw=sdGS, pch=19,
       ylab = "Mean CT-counts", xlab="Days")
mtext("Gray-sided Vole", cex=1.5, side=2, line = 5.5)

# correlogms
plot(mat3$dist,mat3$value, ylim=c(0,1), xlim=c(0,20),xlab = "Days apart", ylab="Correlation in CT-counts", 
     main="", col=scales::alpha(1,.6), pch=19)
lines(smooth.spline(mat3$dist,mat3$value), col=1, lwd=2)
abline(h=0.5,lty=2)

# Tundra Vole
# CT variation
plotCI(x=-10:10,y=meanTV,uiw=sdTV,liw=sdTV, pch=19,
       ylab = "Mean CT-counts", xlab="Days")
mtext("Tundra Vole", cex=1.5, side=2, line = 5.5)

# correlogram
plot(mat2$dist,mat2$value, ylim=c(0,1), xlab = "Days apart", ylab="Correlation in CT-counts", 
     main="", col=scales::alpha(1,.6), pch=19)
lines(smooth.spline(mat2$dist,mat2$value), col=1, lwd=2)
abline(h=0.5,lty=2)

dev.off()
