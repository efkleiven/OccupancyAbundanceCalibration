

ABDF <- readRDS("data/cameratrap/porsanger/processed/regression_data_porsanger.rds")
ABDF_m <- readRDS("data/cameratrap/porsanger/processed/regression_data_porsanger_mean.rds")
unique(ABDF$station)
GRDF <- readRDS("data/cameratrap/porsanger/processed/GR_regression_data_porsanger.rds")
GRDF_m <- readRDS("data/cameratrap/porsanger/processed/GR_regression_data_porsanger_mean.rds")

getwd()

### REGRESSION ####
library(dplyr)

# for trap season 1, we have missing values for all the photos taken before the trapping
# for the GR dataset, trapseason 2 correponds to season 2 - season 1, thus it needs to be removed


filtdataab <- filter(ABDF, species=="GRAASIDEMUS" & trapseason > 1 & !(station == "G19" & trapseason <= 6) & station != "G4")
filtdatagr <- filter(GRDF, species=="GRAASIDEMUS" & trapseason > 2 & !(station == "G19" & trapseason <= 7) & station != "G4")

filtdataab_m <- filter(ABDF_m, species=="GRAASIDEMUS" & trapseason > 2 & !(station == "G19" & trapseason <= 7) & station != "G4")
filtdatagr_m <- filter(GRDF_m, species=="GRAASIDEMUS" & trapseason > 2 & !(station == "G19" & trapseason <= 7) & station != "G4")

plot_pors <- function(dataAB = filtdataab,dataGR = filtdatagr,var = "GR",smethod = "OCV", return=FALSE)
  {
    modcoefs <- matrix(NA, ncol = 5, nrow = 8)
    modlistAB <- list()
    modlistGR <- list()
    plotlab <-
      c(
        "Previous Day",
        "Experiment Days",
        "Previous 3 Days",
        "Previous 5 Days",
        "Previous 10 Days",
        "+-1 Days Interval",
        "+-3 Days Interval",
        "+-8 Days Interval"
      )
    par(mfrow = c(2, 4))
    for (i in 1:nrow(modcoefs))
    {
      newAB <- dataAB[, c(1:5, 5 + i)]
      newGR <- dataGR[, c(1:5, 5 + i)]
      
      modcoefs[i, 1] <- colnames(newAB)[6]
      
      colnames(newAB)[6] = colnames(newGR)[6] = "var"
      
      #modAB <- gam(Abundance_HT~s(var), data=newAB)
      #modGR <- gam(Abundance_HT~s(var), data=newGR)
      
      modGR <- npreg::ss(newGR$Abundance_HT, newGR$var, method = smethod)
      modAB <- npreg::ss(newAB$Abundance_HT, newAB$var, method = smethod)
      
      lmodGR <- lm(newGR$Abundance_HT~ newGR$var, method = smethod)
      lmodAB <- lm(newAB$Abundance_HT, newAB$var, method = smethod)
      
      modlistAB[[i]] <- modAB
      modlistGR[[i]] <- modGR
      
      modcoefs[i, 2] <- summary(lmodAB)$r.squared
      modcoefs[i, 3] <- modAB$df
      
      modcoefs[i, 4] <- summary(lmodGR)$r.squared
      modcoefs[i, 5] <- modGR$df

      # PlOT GR
      if(var=="GR"){
        print("GR")
        plot(modGR, main=plotlab[i], ylim=c(-4,4),xlim=c(-4,4), ylab="CT Density Index (GR)",
           xlab="CR Density (GR)")
        points(newGR$Abundance_HT,newGR$var, pch=19, col=scales::alpha(2,.4))
        abline(0,1,lty=2)
        r2x <- round(summary(modGR)$adj.r.squared,3)
        mtext(bquote(adj~R^2==.(r2x)), cex=.8)
      }
      
      # PLOT Density
      if(var!="GR"){
        print("Density")
        plot(modAB,main = plotlab[i],xlab = "CR Density",
             ylab = "CT Density Index",ylim = c(0, 12))
        points(newAB$Abundance_HT,newAB$var,
             pch = 19,col = scales::alpha(2, .4))
      abline(0, 1, lty = 2)
      r2x <- round(summary(modAB)$adj.r.squared, 3)
      mtext(bquote(adj~R ^ 2 == .(r2x)), cex = .8)
      }
      
    }
    if(return == TRUE){
    mdf <- data.frame(var=modcoefs[,1],AB_R2=as.numeric(modcoefs[,2]),AB_DF=as.numeric(modcoefs[,3]),
                      GR_R2=as.numeric(modcoefs[,4]), GR_DF=as.numeric(modcoefs[,5]))
    return(list(modcoefs=mdf, modelsAB=modlistAB,modelsGR=modlistGR))}
  }
plot_pors(dataAB=filtdataab_m,dataGR=filtdatagr_m,
          var="Ab", smethod="REML")

plot_pors(dataAB=filtdataab_m,dataGR=filtdatagr,
          var="GR", smethod="REML")


# xtabs(Abundance_HT~station,data=filtdataab)
zeros <- c("G1","G2","T2-1","T2-2")
filtdataab <- filter(ABDF_m, species=="ROEDMUS" & trapseason > 1 & !(station %in% zeros))
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
