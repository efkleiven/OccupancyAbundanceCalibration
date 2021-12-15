ABDF_h <- readRDS("data/calibration/Haakoya/regression_data_haakoya_ab_jul21.rds")
# ABDF_hm <- readRDS("data/calibration/Haakoya/regression_data_haakoya_ab_mean.rds")
GRDF_h <- tibble(readRDS("data/calibration/Haakoya/GR_regression_data_haakoya_jul21.rds"))
getwd()

### REGRESSION ####
library(dplyr)

# for trap season 1, we have missing values for all the photos taken before the trapping
# for the GR dataset, trapseason 2 correponds to season 2 - season 1, thus it needs to be removed


filtdataab <- ABDF_h
filtdatagr <- GRDF_h

names(filtdatagr)

modcoefs <- matrix(NA,ncol=3,nrow=8)

i = 1
par(mfrow=c(2,4))
plotlab <- c("Previous Day","Experiment Days","Previous 3 Days","Previous 5 Days", "Previous 10 Days",
             "+-1 Days Interval","+-3 Days Interval","+-8 Days Interval")
for(i in 1:nrow(modcoefs))
{
  newAB <- filtdataab[,c(1:2,38,38+i)] # 36 is D50
  newGR <- filtdatagr[,c(1:2,34,34+i)] # 32 is D50
  
  modcoefs[i,1] <- colnames(newAB)[6]
  
  colnames(newAB)[3] = colnames(newGR)[3] = "Dens"
  colnames(newAB)[4] = colnames(newGR)[4] = "var"
  
  modGR <- npreg::ss(newGR$Dens,newGR$var
                     ,method="REML")
  predy <- modGR$y
  modGR$y(newGR$Dens,newGR$var,method="REML")
  modpred <- npreg::ss(predy,newGR$Dens,method="REML")
  plot(modpred)
  summary(modpred)

  modAB <- npreg::ss(newAB$Dens,newAB$var
                     ,method="REML"
                     )
  
  modcoefs[i,1] <- names(filtdataab)[38+i]
  modcoefs[i,2] <- summary(modAB)$r.squared
  modcoefs[i,3] <- summary(modGR)$r.squared
  # 
  plot(modGR, main=plotlab[i], ylab="Density Index (GR)",
       xlab="Camera Triggers (GR)", ylim=c(-2,2))
  points(newGR$Dens,newGR$var, pch=19, col=scales::alpha(3,.4))
  # 
  # abline(0,1,lty=2)
  # r2x <- round(summary(modGR)$adj.r.squared,3)
  # mtext(bquote(adj~R^2==.(r2x)), cex=.8)
  # 
  #PLOT Density
  plot(modAB, main=plotlab[i], xlab="CR Density",
  ylab="CT Density Index", ylim=c(0,15))
  points(newAB$Dens,newAB$var, pch=19, col=scales::alpha(3,.4))
  abline(0,1,lty=2)
  r2x <- round(summary(modAB)$adj.r.squared,3)
  mtext(bquote(adj~R^2==.(r2x)), cex=.8)
  #
  
}

plotlab <- c("Setting Day","Experiment Days","Previous 3 Days","Previous 5 Days", "Previous 10 Days",
             "+-1 Days Interval","+-3 Days Interval","+-8 Days Interval")
par(mfrow=c(2,4))
for(i in 1:nrow(modcoefs))
{
  newAB <- filtdataab[,c(1:5,5+i)]
  newGR <- filtdatagr[,c(1:5,5+i)]
  
  modcoefs[i,1] <- colnames(newAB)[6]
  
  colnames(newAB)[6] = colnames(newGR)[6] = "var"
  
  #modAB <- gam(Abundance_HT~s(var), data=newAB)
  #modGR <- gam(Abundance_HT~s(var), data=newGR)
  
  
  modcoefs[i,2] <- summary(modAB)$r.squared
  modcoefs[i,3] <- modAB$df
  
  modcoefs[i,4] <- summary(modGR)$r.squared
  modcoefs[i,5] <- modGR$df
  
  # Plot GR
  plot(modGR, main=plotlab[i], ylim=c(-4,4),xlim=c(-4,4), ylab="Density Index (GR)",
       xlab="Camera Triggers (GR)")
  points(newGR$Abundance_HT,newGR$var, pch=19, col=scales::alpha(2,.4))
  abline(0,1,lty=2)
  r2x <- round(summary(modGR)$r.squared,3)
  mtext(bquote(R^2==.(r2x)), cex=.8)
  
  # PLOT Density
  # plot(modAB, main=plotlab[i], ylab="Density Index",
  # xlab="Camera Triggers")
  # points(newAB$Abundance_HT,newAB$var, pch=19, col=scales::alpha(2,.4))
  # abline(0,1,lty=2)
  # r2x <- round(summary(modAB)$r.squared,3)
  # mtext(bquote(R^2==.(r2x)), cex=.8)
  
}





mdf <- data.frame(var=modcoefs[,1],ABcoef=as.numeric(modcoefs[,2]),
                  GRcoef=as.numeric(modcoefs[,3]))

# lm1 <- lm(counts ~ `9daysperiod.mean`, data=filtdataab)
lm2 <-  lm(D25 ~ `6daysperiod.sum`, data=filtdatagr, subset = station==3)
abline(lm2, col="green")
abline(a=0,b=1)
summary(lm2)
confint(lm2)
summary(lm2)
plot(lm2)

par(mfrow=c(2,2))
plot(GRDF_h$DT[GRDF_h$station==1],GRDF_h$settingday[GRDF_h$station==1], col=alpha(1,.7), pch=19)
abline(a=0,b=1, col="red")

plot(GRDF_h$DT[GRDF_h$station==2],GRDF_h$settingday[GRDF_h$station==2], col=alpha(2,.7), pch=19)
abline(a=0,b=1, col="red")

plot(GRDF_h$DT[GRDF_h$station==3],GRDF_h$settingday[GRDF_h$station==3], col=alpha(3,.7), pch=19)
abline(a=0,b=1, col="red")

plot(GRDF_h$DT[GRDF_h$station==4],GRDF_h$settingday[GRDF_h$station==4], col=alpha(4,.7), pch=19)
abline(a=0,b=1, col="red")


lines(smooth.spline(GRDF_h$D25,GRDF_h$settingday,spar=1.4), lty=2)



plot(ABDF$Abundance_HT,ABDF$`9daysperiod.mean`)
lines(smooth.spline(ABDF$Abundance_HT,ABDF$`9daysperiod.mean`,spar=1))

plot(GRDF$Abundance_HT,GRDF$`9daysperiod.sum`, col="blue")
lines(smooth.spline(GRDF$Abundance_HT,GRDF$`9daysperiod.sum`,spar=1))

lines(smooth.spline(GRDF$Abundance_HT,GRDF$`9daysperiod.mean`,spar=1))
abline(a=0,b=1, col="red")
abline(a=5.4,b=0.26, col="red")


mod1 <- npreg::ss(newGR$Abundance_HT,newGR$var,method="REML")
plot(mod1)
points(newGR$Abundance_HT,newGR$var, pch=19)
predy <- mod1$y
predx <- mod1$x

plot(predy,predx)
modpred <- npreg::ss(predy,predx,method="REML")
plot(modpred)

sf1 <- splinefun(predy,predx)
sf2 <- splinefun(newGR$Abundance_HT,newGR$var)
