# Out of date 
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

#.4 , .1 for Haak / #log(3.5+1) and log(24+1) for POR
densityinterval <- function(x0,y0, min=log(3.5+1), max=log(24+1)) 
{
  cond_a = abs(x0) < min & abs(y0) < min
  cond_b = x0 > max & y0 > max
  cond_c = (x0 >= min & y0 >= min) & (x0 <= max & y0 <= max)
  
  value <- ifelse(cond_a | cond_b | cond_c == TRUE, 1, 0)
  return(value)
}

dataset <- newGR
ustation=unstations[4]
pred_GR <-  function(dataset,ustation,mean.res=FALSE)
  # takes dataset, removes station and predicts for that station
{
  #start_time <- Sys.time()

  traindata <- filter(dataset, station != ustation)
  testdata <- filter(dataset, station == ustation)

  #fit model to traindata
  mod <- lm(Abundance_HT~var, data=traindata)
  summary(mod)
  predicted_x0 <- matrix(unlist(t(sapply(testdata$var,cal_4apply, mod=mod))),ncol=3)
  # cbind(predicted_x0,testdata$Abundance_HT)
  # ydat <- ifelse(rep(gr,nrow(testdata))==TRUE, testdata$Abundance_HT,log(testdata$Abundance_HT+1))
  # xdat <- ifelse(rep(gr,nrow(testdata))==TRUE, testdata$var,log(testdata$var+1))
  
  coverage <- testdata$Abundance_HT<predicted_x0[,3] & testdata$Abundance_HT>predicted_x0[,2]
  cov_interval <- mapply(growthinterval,x0=predicted_x0[,1],y0=testdata$Abundance_HT)
  Bias <- testdata$Abundance_HT-predicted_x0[,1]
  
  result <- data.frame(coverage=coverage*1, cov_interval=cov_interval,bias=Bias)
  
  return(result)

}

predGR <- foreach::foreach(s=1:length(unique(unstations)),.combine=rbind) %dopar% 
  pred_GR(dataset=grdata,ustation=unstations[s])

# ddata <- newGR
dataset <- newGR
pred_DENS <-  function(dataset,ustation,mean.res=FALSE)
  # takes dataset, removes station and predicts for that station
{
  
  #start_time <- Sys.time()
  
  dataset$logdens <- log(dataset$Abundance_HT+1)
  dataset$logvar <- log(dataset$var+1)
  
  traindata <- filter(dataset, station != ustation)
  testdata <- filter(dataset, station == ustation)
  
  #fit model to traindata
  mod <- lm(logvar~logdens, data=traindata)
  
  predicted_x0 <- matrix(unlist(t(sapply(testdata$logvar,cal_4apply, mod=mod))),ncol=3)
  
  # ydat <- ifelse(rep(gr,nrow(testdata))==TRUE, testdata$Abundance_HT,log(testdata$Abundance_HT+1))
  # xdat <- ifelse(rep(gr,nrow(testdata))==TRUE, testdata$var,log(testdata$var+1))
  # cbind(predicted_x0,testdata$logdens)
  
  coverage <- testdata$logdens<predicted_x0[,3] & testdata$logdens>predicted_x0[,2]
  cov_interval <- mapply(densityinterval,x0=predicted_x0[,1],y0=testdata$logdens)
  Bias <- testdata$logdens-predicted_x0[,1]
  
  result <- data.frame(coverage=coverage*1, cov_interval=cov_interval,bias=Bias)
  
  return(result)
  
}

predDens <- foreach::foreach(s=1:length(unique(unstations)),.combine=rbind) %dopar% 
  pred_DENS(dataset=newGR,ustation=unstations[s])
summary(predDens)


unique(PorD$station)


####
ABDF <- readRDS("data/cameratrap/porsanger/processed/regression_data_porsanger_sum.rds")
PorD <- filter(ABDF, species=="GRAASIDEMUS" & trapseason > 1 & !(station == "G19" & trapseason <= 6) & station != "G4")
names(PorD)
GRDF <- readRDS("data/cameratrap/porsanger/processed/GR_regression_data_porsanger_sum.rds")
PorGR <- filter(GRDF, species=="GRAASIDEMUS" & trapseason > 2 & !(station == "G19" & trapseason <= 7) & station != "G4")
# saveRDS(PorGR,"data/calibration/regression_porsanger_sum_logplus1_GR.rds")

ABDF <- readRDS("data/calibration/Haakoya/regression_data_haakoya_ab_sum.rds")
GRDF <- readRDS("data/calibration/Haakoya/GR_regression_data_haakoya_sum.rds")

summary(PorD)

HKD <- ABDF[,c("trapsession","station","D25","D50","DT",
                   "previousday", "experimdays.sum",   
                   "previous3day.sum",  "previous5day.sum",  
                   "previous10day.sum", "int1day.sum" ,     
                   "int3day.sum" ,"int8day.sum")]

HKGR <- GRDF[c("trapsession","station","D25","D50","DT",
                    "previousday", "experimdays.sum",   
                    "previous3day.sum",  "previous5day.sum",  
                    "previous10day.sum", "int1day.sum" ,     
                    "int3day.sum" ,"int8day.sum")]

originaldata <- PorGR

colnames(HKGR)[4] <- "Abundance_HT"
# saveRDS(HKGR,"data/calibration/regression_haakoya_sum_logplus1_GR.rds")

originaldata <- HKGR

unstations <- unique(originaldata$station)
for(i in 1:imax)
{
  newGR <- originaldata[, c(1:5, 5 + i)]
  print(colnames(newGR)[6])
  colnames(newGR)[6] = "var"
  predGR <- foreach::foreach(s=1:length(unique(unstations)),.combine=rbind) %dopar% 
    pred_GR(dataset=newGR,ustation=unstations[s])
  print(apply(predGR,2,mean))
  print(sqrt(mean(predGR$bias^2)))
  
}

originaldata <- PorD

colnames(HKD)[4] <- "Abundance_HT"
originaldata <- HKD

unstations <- unique(originaldata$station)
for(i in 1:imax)
{

  newGR <- originaldata[, c(1:5, 5 + i)]
  
  print(colnames(newGR)[6])
  
  colnames(newGR)[6] = "var"
  
  predDens <- foreach::foreach(s=1:length(unique(unstations)),.combine=rbind) %dopar% 
    pred_DENS(dataset=newGR,ustation=unstations[s])
  
  print(apply(predDens,2,mean))
  print(sqrt(mean(predDens$bias^2)))
  
}

# prediction plots ####

pdf("Plots/pred_porsanger.pdf", height=6*1.2,width=8*1.2)
par(mfrow=c(2,3), mar=c(5, 5, 4, 2) + 0.1)

## Porsanger log-Densities ----

originaldata <- PorD

mod1 <- lm(log(Abundance_HT+1)~log(`5daysperiod.sum`+1), data=originaldata)
mod2 <- lm(log(Abundance_HT+1)~log(`9daysperiod.sum`+1), data=originaldata)
mod3 <- lm(log(Abundance_HT+1)~log(`19daysperiod.sum`+1), data=originaldata)

mod1 <- lm(log(Abundance_HT+1)~log(interval_1day.mean+1), data=originaldata)


seqx <- seq(log(min(originaldata$previous3day.sum)+1),log(max(originaldata$previous10day.sum)+1),.01)
predicted_1 <- matrix(unlist(t(sapply(seqx,cal_4apply, mod=mod1))),ncol=3)
predicted_2 <- matrix(unlist(t(sapply(seqx,cal_4apply, mod=mod2))),ncol=3)
predicted_3 <- matrix(unlist(t(sapply(seqx,cal_4apply, mod=mod3))),ncol=3)
p_list <- list(predicted_1,predicted_2,predicted_3)

expred1 <- exp(predicted_1)-1
expred2 <- exp(predicted_2)-1
expred3 <- exp(predicted_3)-1
expseq <- exp(seqx)-1

### 5 days interval ####
plot(exp(seqx)-1,exp(predicted_1[,1])-1, type="l", ylim=c(0,max(PorD$Abundance_HT)), ylab="",
     xlab="")

mtext("CR Densities", side=2, line=3.5, font=2)
#mtext("(log Growth rates)",side=2, line=2.5, cex=0.8)
mtext("± 3 day interval CT counts", side=1, line=2.5, font=2)
#mtext("(log Growth rates)",side=1, line=3.5, cex=0.8)

polygon(c(expseq,c(rev(expseq))),c(expred1[,2], rev(expred1[,3])),
        col = scales::alpha("gold",0.8), border = NA)
lines(expseq,exp(predicted_1[,1])-1, type="l", lty=1)
lines(expseq,exp(predicted_1[,2])-1, type="l", lty=2)
lines(expseq,exp(predicted_1[,3])-1, type="l", lty=2)
points(PorD$`5daysperiod.sum`,PorD$Abundance_HT,pch=19)

### 9 days interval ####
plot(exp(seqx)-1,exp(predicted_2[,1])-1, type="l", ylim=c(0,max(PorD$Abundance_HT)), ylab="",
     xlab="")
polygon(c(expseq,c(rev(expseq))),c(expred2[,2], rev(expred2[,3])),
        col = scales::alpha("gold",0.8), border = NA)
lines(expseq,exp(predicted_2[,1])-1, type="l", lty=1)
lines(expseq,exp(predicted_2[,2])-1, type="l", lty=2)
lines(expseq,exp(predicted_2[,3])-1, type="l", lty=2)

mtext("CR Densities", side=2, line=3.5, font=2)
#mtext("(log Growth rates)",side=2, line=2.5, cex=0.8)
mtext("± 5 day interval CT counts", side=1, line=2.5, font=2)
#mtext("(log Growth rates)",side=1, line=3.5, cex=0.8)

points(PorD$`9daysperiod.sum`,PorD$Abundance_HT,pch=19)

### 19 days interval ####
plot(expseq,exp(predicted_3[,1])-1, type="l", ylim=c(0,max(PorD$Abundance_HT)), ylab="",
     xlab="")
polygon(c(expseq,c(rev(expseq))),c(expred3[,2], rev(expred3[,3])),
        col = scales::alpha("gold",0.8), border = NA)
lines(expseq,exp(predicted_3[,1])-1, type="l", lty=1)
lines(expseq,exp(predicted_3[,2])-1, type="l", lty=2)
lines(expseq,exp(predicted_3[,3])-1, type="l", lty=2)
points(PorD$`19daysperiod.sum`,PorD$Abundance_HT,pch=19)

mtext("CR Densities", side=2, line=3.5, font=2)
# mtext("(Normal )",side=2, line=2.5, cex=0.8)
mtext("± 8 day interval CT counts", side=1, line=2.5, font=2)
# mtext("(log Growth rates)",side=1, line=3.5, cex=0.8)


## GR pred plots #####
## Porsanger GR ----

originaldata <- PorGR

mod1 <- lm(Abundance_HT~`5daysperiod.sum`, data=originaldata)
mod2 <- lm(Abundance_HT~`9daysperiod.sum`, data=originaldata)
mod3 <- lm(Abundance_HT~`19daysperiod.sum`, data=originaldata)

plot(mod2)
shapiro.test(residuals(mod3))


seqx <- seq(-6,6,.01)
predicted_1 <- matrix(unlist(t(sapply(seqx,cal_4apply, mod=mod1))),ncol=3)
predicted_2 <- matrix(unlist(t(sapply(seqx,cal_4apply, mod=mod2))),ncol=3)
predicted_3 <- matrix(unlist(t(sapply(seqx,cal_4apply, mod=mod3))),ncol=3)


head(expred1)
#par(mfrow=c(2,3))

### 5 days interval ####
plot(seqx,predicted_1[,1], type="l", ylab="",
     ylim=c(-4,4), xlim=c(-6,6), xlab="")

mtext("CR Densities", side=2, line=3.5, font=2)
mtext("(log Growth rates)",side=2, line=2.5, cex=0.8)
mtext("± 3 day interval CT counts", side=1, line=2.5, font=2)
mtext("(log Growth rates)",side=1, line=3.5, cex=0.8)

polygon(c(seqx,c(rev(seqx))),c(predicted_1[,2], rev(predicted_1[,3])),
        col = scales::alpha("gold",0.8), border = NA)
lines(seqx,predicted_1[,1], type="l", lty=1)
lines(seqx,predicted_1[,2], type="l", lty=2)
lines(seqx,predicted_1[,3], type="l", lty=2)
points(originaldata$`5daysperiod.sum`,originaldata$Abundance_HT,pch=19)

### 9 days interval ####
plot(seqx,predicted_2[,1], type="l", ylab="",
     ylim=c(-4,4), xlim=c(-6,6), xlab="")

mtext("CR Densities", side=2, line=3.5, font=2)
mtext("(log Growth rates)",side=2, line=2.5, cex=0.8)
mtext("± 5 day interval CT counts", side=1, line=2.5, font=2)
mtext("(log Growth rates)",side=1, line=3.5, cex=0.8)

polygon(c(seqx,c(rev(seqx))),c(predicted_2[,2], rev(predicted_2[,3])),
        col = scales::alpha("gold",0.8), border = NA)
lines(seqx,predicted_2[,1], type="l", lty=1)
lines(seqx,predicted_2[,2], type="l", lty=2)
lines(seqx,predicted_2[,3], type="l", lty=2)
points(originaldata$`9daysperiod.sum`,originaldata$Abundance_HT,pch=19)

### 19 days interval ####
plot(seqx,predicted_3[,1], type="l", ylab="",
     ylim=c(-4,4), xlim=c(-6,6), xlab="")

mtext("CR Densities", side=2, line=3.5, font=2)
mtext("(log Growth rates)",side=2, line=2.5, cex=0.8)
mtext("± 8 day interval CT counts", side=1, line=2.5, font=2)
mtext("(log Growth rates)",side=1, line=3.5, cex=0.8)

polygon(c(seqx,c(rev(seqx))),c(predicted_3[,2], rev(predicted_3[,3])),
        col = scales::alpha("gold",0.8), border = NA)
lines(seqx,predicted_3[,1], type="l", lty=1)
lines(seqx,predicted_3[,2], type="l", lty=2)
lines(seqx,predicted_3[,3], type="l", lty=2)
points(originaldata$`19daysperiod.sum`,originaldata$Abundance_HT,pch=19)

### devoff ####
dev.off()

pdf("Plots/pred_haakoya.pdf", height=6*1.2,width=8*1.2)
par(mfrow=c(2,2), mar=c(5, 5, 4, 2) + 0.1)

## Håkøya log-Densities ----

originaldata <- HKD

mod1 <- lm(log(D50+1)~log(previousday+1), data=originaldata)
plot(mod1)
mod2 <- lm(log(D50+1)~log(previous3day.sum+1), data=originaldata)


seqx <- seq(log(min(originaldata$previousday)+1),log(max(originaldata$previous3day.sum)+1),.01)
predicted_1 <- matrix(unlist(t(sapply(seqx,cal_4apply, mod=mod1))),ncol=3)
predicted_2 <- matrix(unlist(t(sapply(seqx,cal_4apply, mod=mod2))),ncol=3)

expred1 <- (exp(predicted_1))
expred2 <- (exp(predicted_2))
expseq <- (exp(seqx)-1)

### previous day ####
plot(expseq,expred1[,1]-1, type="l", ylab="", xlab="", ylim=c(0,4), xlim=c(0,30))

mtext("CR Densities", side=2, line=3.5, font=2)
#mtext("(log Growth rates)",side=2, line=2.5, cex=0.8)
mtext("previous day CT counts", side=1, line=2.5, font=2)
#mtext("(log Growth rates)",side=1, line=3.5, cex=0.8)


polygon(c(expseq,c(rev(expseq))),c(expred1[,2], rev(expred1[,3])),
        col = scales::alpha("gold",0.8), border = NA, ylim=c(0,10))
lines(expseq,expred1[,1], type="l", lty=1)
lines(expseq,expred1[,2], type="l", lty=2)
lines(expseq,expred1[,2], type="l", lty=2)
points(HKD$previousday,HKD$D50,pch=19)

### 3 previous days  ####
plot(expseq,expred2[,1]-1, type="l", ylab="",
     xlab="", ylim=c(0,40), xlim=c(0,30))
mtext("CR Densities", side=2, line=3.5, font=2)
#mtext("(log Growth rates)",side=2, line=2.5, cex=0.8)
mtext("3 previous days CT counts", side=1, line=2.5, font=2)
#mtext("(log Growth rates)",side=1, line=3.5, cex=0.8)

polygon(c(expseq,c(rev(expseq))),c(expred2[,2], rev(expred2[,3])),
        col = scales::alpha("gold",0.8), border = NA, ylim=c(0,10))
lines(expseq,expred2[,1], type="l", lty=1)
lines(expseq,expred2[,2], type="l", lty=2)
lines(expseq,expred2[,2], type="l", lty=2)
points(HKD$previous3day.sum,HKD$D50*10,pch=19)
max(HKD$D50)

## GR pred plots #####
## Haakoya GR ----

originaldata <- HKGR

mod1 <- lm(D50~previousday, data=originaldata)
mod2 <- lm(D50~previous3day.sum, data=originaldata)

seqx <- seq(-6,6,.01)
predicted_1 <- matrix(unlist(t(sapply(seqx,cal_4apply, mod=mod1))),ncol=3)
predicted_2 <- matrix(unlist(t(sapply(seqx,cal_4apply, mod=mod2))),ncol=3)
### previous day GR ####
plot(seqx,predicted_1[,1], type="l", ylab="", xlab="", ylim=c(-1,1), xlim=c(-4,4))
mtext("CR Densities", side=2, line=3.5, font=2)
mtext("(log Growth rates)",side=2, line=2.5, cex=0.8)
mtext("previous day CT counts", side=1, line=2.5, font=2)
mtext("(log Growth rates)",side=1, line=3.5, cex=0.8)
polygon(c(seqx,c(rev(seqx))),c(predicted_1[,2], rev(predicted_1[,3])),
        col = scales::alpha("gold",0.8), border = NA)
lines(seqx,predicted_1[,1], type="l", lty=1)
lines(seqx,predicted_1[,2], type="l", lty=2)
lines(seqx,predicted_1[,3], type="l", lty=2)
points(originaldata$previousday,originaldata$D50,pch=19)

### Previous 3 Days sum ####
plot(seqx,predicted_2[,1], type="l", ylab="",
     xlab="", ylim=c(-1,1), xlim=c(-4,4), sub="log-GR")
mtext("CR Densities", side=2, line=3.5, font=2)
mtext("(log Growth rates)",side=2, line=2.5, cex=0.8)
mtext("previous 3 days CT counts", side=1, line=2.5, font=2)
mtext("(log Growth rates)",side=1, line=3.5, cex=0.8)

polygon(c(seqx,c(rev(seqx))),c(predicted_2[,2], rev(predicted_2[,3])),
        col = scales::alpha("gold",0.8), border = NA)
lines(seqx,predicted_2[,1], type="l", lty=1)
lines(seqx,predicted_2[,2], type="l", lty=2)
lines(seqx,predicted_2[,3], type="l", lty=2)
points(originaldata$previous3day.sum,originaldata$D50,pch=19)

## devoff ####
dev.off()




# Aggregated Håkøya Plots -----


pdf("Plots/Hakoya_pooled_lm.pdf", height=6*1.2,width=8*1.2)
par(mfrow=c(2,2))
## Log-densities ----
totalldh <- aggregate(cbind(D50,previousday,previous3day.sum)~trapsession,data=HKD, mean)
totalldh[,2:4] <- log(totalldh[,2:4]+1)

mod2 <- lm(D50~previousday, data=totalldh)
mod2x <- lm(D50~previous3day.sum, data=totalldh)

s2 <- summary(mod2)
s2x <- summary(mod2x)

plot(totalldh$D50~totalldh$previousday, pch=19, col=2, ylim=c(0,1.25), main="Previous Day",
     ylab="log CR Density", xlab="log CT counts")
seqpred <- seq(min(totalldh$previousday)-.2,max(totalldh$previousday)+.2,.1)
pdf <- as.data.frame(predict(mod2,newdata=data.frame(previousday=seqpred), interval = "confidence"))
polygon(c(seqpred,c(rev(seqpred))),c(pdf$lwr, rev(pdf$upr)),
        col = scales::alpha("gray50",0.5), border = NA)
polygon(c(seqpred,c(rev(seqpred))),c(pdf$fit-.01, rev(pdf$fit+.01)),
        col = "black", border = NA)
points(totalldh$D50~totalldh$previousday, pch=19, col=scales::alpha(2,.6))
mtext(bquote("adj."~R^2== .(round(s2$adj.r.squared,2))), cex=.8)

plot(totalldh$D50~totalldh$previous3day.sum, ylim=c(0,1.25), pch=19, col=2, main="Previous 3 Days",
     ylab="log CR Density", xlab="log CT counts")
seqpred <- seq(min(totalldh$previous3day.sum)-.2,max(totalldh$previous3day.sum)+.2,.1)
pdf <- as.data.frame(predict(mod2x,newdata=data.frame(previous3day.sum=seqpred), interval = "confidence"))
polygon(c(seqpred,c(rev(seqpred))),c(pdf$lwr, rev(pdf$upr)),
        col = scales::alpha("gray50",0.5), border = NA)
polygon(c(seqpred,c(rev(seqpred))),c(pdf$fit-.01, rev(pdf$fit+.01)),
        col = "black", border = NA)
points(totalldh$D50~totalldh$previous3day.sum, pch=19, col=scales::alpha(2,.6))
mtext(bquote("adj."~R^2== .(round(s2x$adj.r.squared,2))), cex=.8)


## GR ----
totalgrh <- aggregate(cbind(D50,previousday,previous3day.sum)~trapsession,data=HKGR, mean)

mod1 <- lm(D50~previousday, data=totalgrh)
mod1x <- lm(D50~previous3day.sum, data=totalgrh)

s1 <- summary(mod1)
s1x <- summary(mod1x)

plot(totalgrh$D50~totalgrh$previousday, ylim=c(-.7,.7),xlim=c(-1.5,2), pch=19, col=2, main="Previous Day",
     ylab="log CR Density (GR)", xlab="log CT counts (GR)")
seqpred <- seq(min(totalgrh$previousday)-.2,max(totalgrh$previousday)+.2,.1)
pdf <- as.data.frame(predict(mod1,newdata=data.frame(previousday=seqpred), interval = "confidence"))
polygon(c(seqpred,c(rev(seqpred))),c(pdf$lwr, rev(pdf$upr)),
        col = scales::alpha("gray50",0.5), border = NA)
polygon(c(seqpred,c(rev(seqpred))),c(pdf$fit-.01, rev(pdf$fit+.01)),
        col = "black", border = NA)
points(totalgrh$D50~totalgrh$previousday, pch=19, col=scales::alpha(2,.6))
mtext(bquote("adj."~R^2== .(round(s1$adj.r.squared,2))), cex=.8)

plot(totalgrh$D50~totalgrh$previous3day.sum, ylim=c(-.7,.7),xlim=c(-1.5,1.7), pch=19, col=2, main="Previous 3 Days",
     ylab="log CR Density (GR)", xlab="log CT counts (GR)")
seqpred <- seq(min(totalgrh$previous3day.sum)-.2,max(totalgrh$previous3day.sum)+.2,.1)
pdf <- as.data.frame(predict(mod1x,newdata=data.frame(previous3day.sum=seqpred), interval = "confidence"))
polygon(c(seqpred,c(rev(seqpred))),c(pdf$lwr, rev(pdf$upr)),
        col = scales::alpha("gray50",0.5), border = NA)
polygon(c(seqpred,c(rev(seqpred))),c(pdf$fit-.01, rev(pdf$fit+.01)),
        col = "black", border = NA)
points(totalgrh$D50~totalgrh$previous3day.sum, pch=19, col=scales::alpha(2,.6))
mtext(bquote("adj."~R^2== .(round(s1x$adj.r.squared,2))), cex=.8)

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
