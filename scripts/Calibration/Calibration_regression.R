

ABDF <- readRDS("data/cameratrap/porsanger/processed/regression_data_porsanger.rds")

GRDF <- readRDS("data/cameratrap/porsanger/processed/GR_regression_data_porsanger.rds")
getwd()

### REGRESSION ####
library(dplyr)

# for trap season 1, we have missing values for all the photos taken before the trapping
# for the GR dataset, trapseason 2 correponds to season 2 - season 1, thus it needs to be removed


filtdataab <- filter(ABDF, species=="GRAASIDEMUS" & trapseason > 1 & !(station == "G19" & trapseason <= 6) & station != "G4")
filtdatagr <- filter(GRDF, species=="GRAASIDEMUS" & trapseason > 2 & !(station == "G19" & trapseason <= 7) & station != "G4")


modcoefs <- matrix(NA,ncol=5,nrow=8)
modlistAB <- list()
modlistGR <- list()
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
  
  modGR <- npreg::ss(newGR$Abundance_HT,newGR$var,spar=.6)
  modAB <- npreg::ss(newAB$Abundance_HT,newAB$var, spar=.6)

  modlistAB[[i]] <- modAB
  modlistGR[[i]] <- modGR
  
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

mdf <- data.frame(var=modcoefs[,1],AB_R2=as.numeric(modcoefs[,2]),AB_DF=as.numeric(modcoefs[,3]),
                  GR_R2=as.numeric(modcoefs[,4]), GR_DF=as.numeric(modcoefs[,5]))
plot(modlistGR[[6]])
points(filtdatagr$Abundance_HT,filtdatagr$`5daysperiod.sum`, pch=19)
abline(0,1)
plot(seq(-4,4,.01),predict(modGR,newdata), type="l")
summary(mdf)

datGR0 <- select(filtdatagr,Abundance_HT,`5daysperiod.sum`,`9daysperiod.sum`)
datAB0 <- select(filtdataab,Abundance_HT,`5daysperiod.sum`,`9daysperiod.sum`)

colnames(datGR0)[2:3] = colnames(datAB0)[2:3] <- c("fivesum","ninesum")

plot(modGR0)
plot(datGR0$Abundance_HT,datGR0$fivesum)
lines(newdata$fivesum,predict(modGR0, newdata))
length(seq(-4,4,.01))
?predict

newdata=data.frame(ninesum=seq(-4,4,.01))

modGR0 <- gam(Abundance_HT~s(ninesum, bs="ts", k=8, fx=TRUE), data=datGR0)
plot(modGR0)
plot(datGR0$Abundance_HT,datGR0$ninesum)
lines(newdata$ninesum,predict(modGR0, newdata), type="l")
points(datGR0$Abundance_HT,datGR0$ninesum, col=1, pch=19)
abline(0,1,lty=2)

modss1 <- npreg::ss(datGR0$Abundance_HT,datGR0$ninesum, spar=0.1)
modss2 <- npreg::ss(datAB0$Abundance_HT,datAB0$ninesum, spar=0.5)
summary(modss2)
plot(modss1)
points(datGR0$Abundance_HT,datGR0$fivesum)
points(datAB0$Abundance_HT,datAB0$fivesum)
summary(modss)
smooth.spline(datGR0$Abundance_HT,datGR0$ninesum)
?gam
plot(modGR0)
modGR0 <- gam(Abundance_HT~s(`9daysperiod.sum`), data=newAB)
?gam

library(mgcv)
m1 <- gam(Abundance_HT~s(var,k=3, bs="ts"), data=newAB)
a <- summary(m1)
a$dev.expl
plot(m1)
summary(m1)
# lm1 <- lm(counts ~ `9daysperiod.mean`, data=filtdataab)
lm2 <-  lm(counts ~ `9daysperiod.sum`, data=filtdatagr)
abline(lm2, col="green")
summary(lm2)
confint(lm2)
summary(lm2)
plot(lm2)

plot(GRDF$Abundance_HT,GRDF$`9daysperiod.sum`, col=alpha("blue",.7), pch=19)

plot(lm(GRDF$Abundance_HT~GRDF$`9daysperiod.sum`))

lines(smooth.spline(GRDF$Abundance_HT,GRDF$`9daysperiod.sum`,spar=1), lty=2)
abline(a=0,b=1, col="red")
abline(a=0.011,b=0.42030 , col="red", lty=3, lwd=2)
# weighted regression 


# xtabs(Abundance_HT~station,data=filtdataab)
zeros <- c("G1","G2","T2-1","T2-2")
filtdataab <- filter(ABDF, species=="ROEDMUS" & trapseason > 1 & !(station %in% zeros))
filtdatagr <- filter(GRDF, species=="ROEDMUS" & trapseason > 2 & !(station %in% zeros))

modcoefs_RM <- matrix(NA,ncol=3,nrow=14)
for(i in 1:14)
{
  newAB <- filtdataab[,c(1:5,5+i)]
  newGR <- filtdatagr[,c(1:5,5+i)]
  
  modcoefs_RM[i,1] <- colnames(newAB)[6]
  
  colnames(newAB)[6] = colnames(newGR)[6] = "var"
  
  modAB <- lm(counts~var, data=newAB)
  modGR <- lm(counts~var, data=newGR)
  summary(modAB)
  modcoefs_RM[i,2] <- summary(modAB)$r.squared
  summary(modAB)$coef[2]
  modcoefs_RM[i,3] <- summary(modGR)$r.squared
  
  
}


# 
# 
# plot(lm1)
# mdf2 <- data.frame(var=modcoefs_RM[,1],ABcoef=as.numeric(modcoefs_RM[,2]),
#                   GRcoef=as.numeric(modcoefs_RM[,3]))
# 
# tibble(bind_cols(mdf,mdf2[,2:3]))
# tibble(mdf2)
# 
# GRDF
# 
# plot(ABDF$Abundance_HT,ABDF$`9daysperiod.mean`)
# lines(smooth.spline(ABDF$Abundance_HT,ABDF$`9daysperiod.mean`,spar=1))
# 
# plot(GRDF$Abundance_HT,GRDF$`9daysperiod.sum`, col="blue")
# lines(smooth.spline(GRDF$Abundance_HT,GRDF$`9daysperiod.sum`,spar=1))
# 
# lines(smooth.spline(GRDF$Abundance_HT,GRDF$`9daysperiod.mean`,spar=1))
# abline(a=0,b=1, col="red")
# abline(a=5.4,b=0.26, col="red")

# plot(ABDF$Abundance_HT,ABDF$`9daysperiod.sum`)
# ?smooth.spline
# library(INLA)
# 
# 
# 
# 
# grinla <-
#   inla(
#     Abundance_HT ~ f(experimdays.sum, model="rw1", scale.model = TRUE,
#                      hyper = list(theta = list(prior="pc.prec", param=c(u=.5,0.1))))  + f(station, model = "iid"),
#     control.predictor = list(compute = TRUE),control.compute=list(config = TRUE),
#     data = filtdatagr
#   )
# 
# plot(grinla)
# 
# 
# plot(grinla$summary.random$experimdays.sum$ID,grinla$summary.random$sum$mean)
# lines(grinla$summary.random$experimdays.sum$ID,grinla$summary.random$sum$`0.025quant`, lty=2)
# lines(grinla$summary.random$experimdays.sum$ID,grinla$summary.random$sum$`0.975quant`, lty=2)
# 
# cor(grdf1$Abundance_HT,grdf1$sum)
# 
# syncmod1 <- inla(mean.res ~ f(distance, model="rw2", scale.model = TRUE,
#                               hyper = list(theta = list(prior="pc.prec", param=c(u=.5,0.1))))
#                  , family="gaussian", control.predictor = list(compute=TRUE)
#                  ,data=dat)
# 
# abline(a=0,b=1)
