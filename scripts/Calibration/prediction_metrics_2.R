# functions ------
cal_4apply <- function(y0,mod, mean.res=FALSE, interval="Wald")
{
  x0 <- investr::calibrate(mod,y0,interval=interval, mean.response = mean.res)
  return(x0[1:3])
}

growthinterval <- function(x0,y0, threshold=1.1) #threshold is in the real population growth scale 
{
  cond_a = abs(x0) < log(threshold) & abs(y0) < log(threshold)
  cond_b = x0 >= log(threshold) & y0 >= log(threshold)
  cond_c = x0 <= log(1/threshold) & y0 <= log(1/threshold)
  
  value <- ifelse(cond_a | cond_b | cond_c == TRUE, 1, 0)
  return(value)
}

summary(lHakmean)
#.40 , .75 for Haak / #1.5 and 3.2 for POR
densityinterval <- function(x0,y0, min=.40, max=.75) 
{
  cond_a = abs(x0) < min & abs(y0) < min
  cond_b = x0 > max & y0 > max
  cond_c = (x0 >= min & y0 >= min) & (x0 <= max & y0 <= max)
  
  value <- ifelse(cond_a | cond_b | cond_c == TRUE, 1, 0)
  return(value)
}


pred_general <-  function(dataset,ustation,mean.res=FALSE, gr=TRUE)
  # takes dataset, removes station and predicts for that station
{
  #start_time <- Sys.time()
  colnames(dataset)[4] <- "abundance"

  traindata <- filter(dataset, station != ustation)
  testdata <- filter(dataset, station == ustation)
  
  #fit model to traindata
  mod0 <- lm(var~abundance, data=traindata)
  predicted_x0 <- matrix(unlist(t(sapply(testdata$var,cal_4apply, mod=mod0))),ncol=3)
  #cbind(predicted_x0,testdata$abundance)

  coverage <- testdata$abundance<predicted_x0[,3] & testdata$abundance>predicted_x0[,2]
  if(gr==TRUE) cov_interval <- mapply(growthinterval,x0=predicted_x0[,1],y0=testdata$abundance)
  if(gr==FALSE) cov_interval <- mapply(densityinterval,x0=predicted_x0[,1],y0=testdata$abundance)
  
  Bias <- testdata$abundance-predicted_x0[,1]
  
  result <- data.frame(coverage=coverage*1, cov_interval=cov_interval,bias=Bias)
  
  return(result)
  
}


## read in data -----

## mean log densities data ----
### Porsanger-----
ABDF <- readRDS("data/cameratrap/porsanger/processed/regression_data_porsanger_sum.rds")

PorD <- filter(ABDF, species=="GRAASIDEMUS" & trapseason > 1 & !(station == "G19" & trapseason <= 6) & station != "G4")
summary(log(PorD$Abundance_HT+1))
ndaysP <- c(1,3,3,5,10,5,8,19) # number of days in the interval var

nvarP <- c("previousday", "experimdays.sum", "previous3day.sum",  "previous5day.sum","previous10day.sum",
          "5daysperiod.sum",   "9daysperiod.sum", "19daysperiod.sum")
lPormean <- PorD
for(v in 1:length(nvarP))
{lPormean[,nvarP[v]] <- log((lPormean[,nvarP[v]]+1)/ndaysP[v])}
lPormean$Abundance <- lPormean$Abundance_HT
lPormean$Abundance_HT <- log(lPormean$Abundance+1)
colnames(lPormean)[4] <- "logAb"

saveRDS(lPormean,"data/calibration/regression_porsanger_mean_logplus1_ab.rds")
View(lPormean)
### Haakoya ----
ABDF <- readRDS("data/calibration/Haakoya/regression_data_haakoya_ab_sum.rds")
ndaysH <- c(1,3,3,5,10,5,8,19)
vars1 <- c("trapsession","station","D25","D50","DT")
nvarH <- c("previousday", "experimdays.sum", "previous3day.sum",  "previous5day.sum",  
           "previous10day.sum", "int1day.sum" ,  "int3day.sum" ,"int8day.sum")

ABDF2 <- ABDF[,c(vars1,nvarH)]

lHakmean <- ABDF2
for(v in 1:length(nvarH))
{lHakmean[,nvarH[v]] <- log((lHakmean[,nvarH[v]]+1)/ndaysH[v])}

lHakmean$Abundance_D50 <- lHakmean$D50
lHakmean$D50 <- log(lHakmean$Abundance_D50+1)
colnames(lHakmean)[4] <- "logAb"
saveRDS(lHakmean,"data/calibration/regression_haakoya_mean_logplus1_ab.rds")

## log mean density predictions ------
lHakmean <- readRDS("data/calibration/regression_haakoya_mean_logplus1_ab.rds")

lPormean <- readRDS("data/calibration/regression_porsanger_mean_logplus1_ab.rds")
summary(lPormean)
originaldata <- lHakmean
imax=8
unstations <- unique(originaldata$station)
for(i in 1:imax)
{
  
  newGR <- originaldata[, c(1:5, 5 + i)]
  print(colnames(newGR)[6])
  
  colnames(newGR)[6] = "var"
  
  predDens <- foreach::foreach(s=1:length(unique(unstations)),.combine=rbind) %dopar% 
    pred_general(dataset=newGR,ustation=unstations[s],gr=FALSE)
  
  print(apply(predDens,2,mean))
  print(sqrt(mean(predDens$bias^2)))
  
}

## GR -----
lHakGR <- readRDS("data/calibration/regression_haakoya_mean_logplus1_gr.rds")

lPorGR <- readRDS("data/calibration/regression_porsanger_mean_logplus1_gr.rds")

originaldata <- lHakGR
imax=8
unstations <- unique(originaldata$station)
for(i in 1:imax)
{
  
  newGR <- originaldata[, c(1:5, 5 + i)]
  print(colnames(newGR)[6])
  
  colnames(newGR)[6] = "var"
  
  predDens <- foreach::foreach(s=1:length(unique(unstations)),.combine=rbind) %dopar% 
    pred_general(dataset=newGR,ustation=unstations[s],gr=TRUE)
  
  print(apply(predDens,2,mean))
  print(sqrt(mean(predDens$bias^2)))
  
}

# prediction plots ####

## log-Densities ----




 if (region=="porsanger")
# lPorAb <- readRDS("data/calibration/regression_porsanger_mean_logplus1_ab.rds")
# lPorGR <- readRDS("data/calibration/regression_porsanger_mean_logplus1_gr.rds")
# 
# vars = c("9daysperiod.sum","19daysperiod.sum")
# varlab <- c(
#   #"± 1 day interval CT-counts (Mean)",
#           "± 3 day interval CT-counts (Mean)",
#           "± 8 day interval CT-counts (Mean)")
# mt <- "CR-Abundance"
# par(mfrow=c(1,2))


# #  if (region=="hakoya")
  lHakab <- readRDS("data/calibration/regression_haakoya_mean_logplus1_ab.rds")
  originaldata <- lHakab
  vars = c("previousday","experimdays.sum")
  varlab <- c("Previous day CT-counts",
              "Previous 3 days CT-counts (Mean)")
  mt <- "CR-Abundance (per trap)"
  par(mfrow=c(1,2))

# pdf("Plots/pred_porsanger_2var.pdf", height=6*1.2,width=8*1.2)
# par(mfrow=c(2,2), mar=c(5, 5, 4, 2) + 0.1)
#   
pdf("Plots/pred_haakoya.pdf", height=6*1.2,width=8*1.2)
par(mfrow=c(2,2), mar=c(5, 5, 4, 2) + 0.1)
 
for(v in 1:length(vars))
{
  originaldata <- lHakab
  # originaldata <- lPorAb
  
  # position of variable
  varpos <- which(names(originaldata)==vars[v])
  # change name of variable
  colnames(originaldata)[varpos] <- "var"
  # fit model
  modv <- lm(var~logAb, data=originaldata)
  # predict grid
  seqx <- seq(min(originaldata$var)-.1, log(50), 0.01)
  
  predicted_v <- matrix(unlist(t(sapply(seqx,cal_4apply, mod=modv))),ncol=3)
  #
  expredv <- exp(predicted_v)
  expseq <- exp(seqx)
  
  plot(expseq,expredv[,1], type="l", ylab="",
       xlab=""
       , ylim=c(0,10), xlim=c(0,20)
       # xlim=c(0,1) # Pors
       )
  
  
  mtext(mt, side=2, line=2.5, font=1)
  #mtext("(log Growth rates)",side=2, line=2.5, cex=0.8)
  mtext(varlab[v], side=1, line=2.5, font=1)
  #mtext("(log Growth rates)",side=1, line=3.5, cex=0.8)
  
  polygon(c(expseq,c(rev(expseq))),c(expredv[,2], rev(expredv[,3])),
          col = scales::alpha("gold",0.8), border = NA)
  lines(expseq,exp(predicted_v[,1]), type="l", lty=1)
  lines(expseq,exp(predicted_v[,2]), type="l", lty=2)
  lines(expseq,exp(predicted_v[,3]), type="l", lty=2)
  points(exp(originaldata$var),exp(originaldata$logAb),pch=19, col=alpha("gray10",.8))
}

for(v in 1:length(vars))
{
  originaldata <- lHakab
  # originaldata <- lPorGR
  
  # position of variable
  varpos <- which(names(originaldata)==vars[v])
  # change name of variable
  colnames(originaldata)[varpos] <- "var"
  # fit model
  modv <- lm(var~logAb, data=originaldata)
  # predict grid
  seqx <- seq(min(originaldata$var)-.1, max(originaldata$var)+.1, 0.01)
  
  predicted_v <- matrix(unlist(t(sapply(seqx,cal_4apply, mod=modv))),ncol=3)
  #
  expredv <- predicted_v
  expseq <- seqx
  
  plot(expseq,expredv[,1], type="l", ylab="",
       xlab=""
       # ,ylim=c(-6,6), xlim=c(-5,5) Por
       , ylim=c(-3,3.5), xlim=c(-1.2,4)
       )
  
  mtext(mt, side=2, line=3.5, font=1)
  #mtext("(log Growth rates)",side=2, line=2.5, cex=0.8)
  mtext(varlab[v], side=1, line=2.5, font=1)
  #mtext("(log Growth rates)",side=1, line=3.5, cex=0.8)
  
  polygon(c(expseq,c(rev(expseq))),c(expredv[,2], rev(expredv[,3])),
          col = scales::alpha("gold",0.8), border = NA)
  lines(expseq,predicted_v[,1], type="l", lty=1)
  lines(expseq,predicted_v[,2], type="l", lty=2)
  lines(expseq,predicted_v[,3], type="l", lty=2)
  points(originaldata$var,originaldata$logAb,pch=19, col=alpha("gray10",.8))
}

dev.off()
# Aggregated Håkøya Plots -----


pdf("Plots/Hakoya_pooled_lm.pdf", height=6*1.2,width=8*1.2)
par(mfrow=c(1,2))
## Log-densities ----
totalldh <- aggregate(cbind(logAb,previousday,previous3day.sum)~trapsession,data=lHakab, mean)

mod2 <- lm(previousday~logAb, data=totalldh)
#mod2x <- lm(previous3day.sum~logAb, data=totalldh)

s2 <- summary(mod2)
#s2x <- summary(mod2x)

plot(totalldh$previousday~totalldh$logAb, pch=19, col=2, main="Abundance",
     xlab="log CR-Abundance", ylab="log Previous Day CT-counts")
seqpred <- seq(min(totalldh$logAb)-.2,max(totalldh$logAb)+.2,.1)
pdf <- as.data.frame(predict(mod2,newdata=data.frame(logAb=seqpred), interval = "confidence"))
polygon(c(seqpred,c(rev(seqpred))),c(pdf$lwr, rev(pdf$upr)),
        col = scales::alpha("gray50",0.5), border = NA)
abline(mod2, lwd=2.5)
points(totalldh$previousday~totalldh$logAb, pch=19, col=scales::alpha(2,.6))
mtext(bquote(R^2== .(round(s2$r.squared,2))), cex=1)



## GR ----
HKGR <- readRDS("data/calibration/regression_haakoya_mean_logplus1_gr.rds")

totalgrh <- aggregate(cbind(logAb,previousday,previous3day.sum)~trapsession,data=HKGR, mean)

mod1 <- lm(previousday~logAb, data=totalgrh)
#mod1x <- lm(previous3day.sum~D50, data=totalgrh)


plot(totalgrh$previousday~totalgrh$logAb, pch=19, col=2, main="Growth rates",
     xlab="log CR-Abundance (GR)", ylab="log Previous Day CT-counts (GR)")
seqpred <- seq(min(totalgrh$logAb)-.2,max(totalgrh$logAb)+.2,.1)
pdf <- as.data.frame(predict(mod1,newdata=data.frame(logAb=seqpred), interval = "confidence"))
polygon(c(seqpred,c(rev(seqpred))),c(pdf$lwr, rev(pdf$upr)),
        col = scales::alpha("gray50",0.5), border = NA)
abline(mod1, lwd=2.5)
points(totalgrh$previousday~totalgrh$logAb, pch=19, col=scales::alpha(2,.6))
mtext(bquote(R^2== .(round(s1$r.squared,2))), cex=1)


## devoff ####
dev.off()

# Porsanger ----
pdf("Plots/Porsanger_pooled_lm.pdf", height=6*1.2,width=8*1.2)
par(mfrow=c(1,2))
## Log-densities ----
lPORab <- readRDS("data/calibration/regression_porsanger_mean_logplus1_ab.rds")

totalldh <- aggregate(cbind(logAb,`9daysperiod.sum`)~trapseason,data=lPORab, mean)

mod2 <- lm(`9daysperiod.sum`~logAb, data=totalldh)
#mod2x <- lm(previous3day.sum~logAb, data=totalldh)

s2 <- summary(mod2)
#s2x <- summary(mod2x)

plot(totalldh$`9daysperiod.sum`~totalldh$logAb, pch=19, col=2, main="Abundance",
     xlab="log CR-Abundance", ylab="log +- 3 Day Interval CT-counts")
seqpred <- seq(min(totalldh$logAb)-.2,max(totalldh$logAb)+.2,.1)
pdf <- as.data.frame(predict(mod2,newdata=data.frame(logAb=seqpred), interval = "confidence"))
polygon(c(seqpred,c(rev(seqpred))),c(pdf$lwr, rev(pdf$upr)),
        col = scales::alpha("gray50",0.5), border = NA)
abline(mod2, lwd=2.5)
points(totalldh$`9daysperiod.sum`~totalldh$logAb, pch=19, col=scales::alpha(2,.6))
mtext(bquote(R^2== .(round(s2$r.squared,2))), cex=1)



## GR ----
PORGR <- readRDS("data/calibration/regression_porsanger_mean_logplus1_gr.rds")

totalgrh <- aggregate(cbind(logAb,`9daysperiod.sum`)~trapseason,data=PORGR, mean)

mod1 <- lm(`9daysperiod.sum`~logAb, data=totalgrh)
#mod1x <- lm(previous3day.sum~D50, data=totalgrh)
plot(totalgrh$`9daysperiod.sum`~totalgrh$logAb, pch=19, col=2, main="Growth rates",
     xlab="log CR-Abundance (GR)", ylab="log +- 3 Day Interval CT-counts (GR)")
seqpred <- seq(min(totalgrh$logAb)-.2,max(totalgrh$logAb)+.2,.1)
pdf <- as.data.frame(predict(mod1,newdata=data.frame(logAb=seqpred), interval = "confidence"))
polygon(c(seqpred,c(rev(seqpred))),c(pdf$lwr, rev(pdf$upr)),
        col = scales::alpha("gray50",0.5), border = NA)
abline(mod1, lwd=2.5)
points(totalgrh$previousday~totalgrh$logAb, pch=19, col=scales::alpha(2,.6))
mtext(bquote(R^2== .(round(sm1$r.squared,2))), cex=1)


## devoff ####
dev.off()

totalpor <- aggregate(cbind(Abundance_HT,`9daysperiod.sum`)~trapseason,data=PorGR, mean)
summary(lm(totalpor$Abundance_HT~totalpor$`9daysperiod.sum`))
totalpor2 <- aggregate(cbind(Abundance_HT,`9daysperiod.sum`)~trapseason,data=PorD, mean)
summary(lm(totalpor2$Abundance_HT~totalpor2$`9daysperiod.sum`))

plot(totalpor$Abundance_HT~totalpor$`9daysperiod.sum`)
abline(lm(totalpor$Abundance_HT~totalpor$`9daysperiod.sum`))
plot(totalpor2$Abundance_HT~totalpor2$`9daysperiod.sum`)
abline(lm(totalpor2$Abundance_HT~totalpor2$`9daysperiod.sum`))
