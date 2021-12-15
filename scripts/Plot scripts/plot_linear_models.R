# Plots Linear Models

plotlab <-c(
    "Previous Day",
    "Experiment Days",
    "Previous 3 Days",
    "Previous 5 Days",
    "Previous 10 Days",
    "±1 Days Interval",
    "±3 Days Interval",
    "±8 Days Interval"
  )

lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

## Håkøya ----
ABDF <- readRDS("data/calibration/regression_haakoya_mean_logplus1_ab.rds")
# GRDF <- readRDS("data/calibration/regression_haakoya_sum_logplus1_GR.rds")
GRDF <- readRDS("data/calibration/regression_haakoya_mean_logplus1_GR.rds")

### Growth rates -----
DX <- 2
originaldata <- GRDF
summary(GRDF)
r2.vec <- p.vec <- b.vec <- se.vec.b <- se.vec.int <- int.vec <- c()
pdf("plots/lm_haakoya_gr_mean.pdf", height=6*1.2,width=8*1.2)
par (mfrow=c(2,4))
i=1
for(i in 1:8) 
{

  newGR <- originaldata[, c(1:5, 5 + i)]
  varname <- names(newGR)[6]
  colnames(newGR)[6] = "var"
  colnames(newGR)[2+DX] = "Abundance_HT"
  
  m1 <- lm(var~Abundance_HT,data=newGR)
  s0 <- summary(m1)
  print(s0)
  print(paste0("R^2=",round(s0$r.squared,3)))
  print(paste0("F=",round(s0$fstatistic[1],3)))
  
  plot(newGR$var~newGR$Abundance_HT, pch=19, col=scales::alpha(2,.8), 
       main=paste0(plotlab[i]), ylab="CT-counts (GR)", xlab="CR-Abundance (GR)" ,
       xlim=c(min(newGR$Abundance_HT-.1),max(newGR$Abundance_HT+.1)), 
       ylim=c(-2.6,4.8))
  
  mtext(bquote(R^2== .(round(s0$r.squared,2))), cex=.8)
  
  seqpred <- seq(min(newGR$Abundance_HT)-.1,max(newGR$Abundance_HT)+.1,.1)
  pdf <- as.data.frame(predict(m1,newdata=data.frame(Abundance_HT=seqpred), interval = "confidence"))
  polygon(c(seqpred,c(rev(seqpred))),c(pdf$lwr, rev(pdf$upr)),
          col = scales::alpha("gray50",0.5), border = NA)
  abline(m1, lty=1, lwd=1.5)
  # abline(b=1,a=0, lty=2)
  r2.vec <- c(r2.vec,s0$r.squared)
  p.vec <- c(p.vec,lmp(m1))  
  b.vec <- c(b.vec,summary(m1)$coefficients[2,1] ) 
  se.vec.b <- c(se.vec.b,summary(m1)$coefficients[2,2] ) 
  se.vec.int <- c(se.vec.int,summary(m1)$coefficients[1,2] ) 
  int.vec <- c(int.vec,summary(m1)$coefficients[1,1] ) 
  #abline(b=0,a=0, lty=2)
  #abline(a=0,b=1, lty=2)
  print(i)
}

dev.off()

(df_gr <- data.frame(int=paste0(round(int.vec,2),"±",round(se.vec.int,2)),
                     coef=paste0(round(b.vec,2),"±",round(se.vec.b,2)),
                     R2=round(r2.vec,3),P=round(p.vec,3)))

### Density -----
originaldata <- ABDF    
summary(originaldata)
r2.vec <- p.vec <- b.vec <- se.vec.b <- se.vec.int <- int.vec <- c()
pdf("plots/lm_haakoya_logab_mean.pdf", height=6*1.2,width=8*1.2)
par(mfrow=c(2,4))

for(i in 1:8) 
{
  
  newGR <- originaldata[, c(1:5, 5 + i)]
  varname <- names(newGR)[6]
  colnames(newGR)[6] = "logvar"

  # fit model to log vars
  m1 <- lm(logvar~logAb,data=newGR)
  s0 <- summary(m1)
  print(s0)
  
  # plot 
  plot(newGR$logvar~newGR$logAb, pch=19, col=scales::alpha(2,.8), 
       main=paste0(plotlab[i]), ylab="Mean log CT-counts", xlab= "Mean log CT-abundance",
       xlim=c(min(newGR$logAb-mean(newGR$logAb)*.75),max(newGR$logAb+mean(newGR$logAb)*.75)), 
#       ylim=c(-0.2,1.5))
       ylim=c(-2.5,3.8))
       
  mtext(bquote(R^2== .(round(s0$r.squared,2))), cex=.8)
  
  seqpred <- seq(min(newGR$logAb)-.3,max(newGR$logAb)+.3,.1)
  pdf <- as.data.frame(predict(m1,newdata=data.frame(logAb=seqpred), interval = "confidence"))
  polygon(c(seqpred,c(rev(seqpred))),c(pdf$lwr, rev(pdf$upr)),
          col = scales::alpha("gray50",0.5), border = NA)

  abline(m1, lty=1, lwd=1.5)
  # abline(b=1,a=0)
  
  r2.vec <- c(r2.vec,s0$r.squared)
  p.vec <- c(p.vec,lmp(m1))  
  b.vec <- c(b.vec,summary(m1)$coefficients[2,1] ) 
  se.vec.b <- c(se.vec.b,summary(m1)$coefficients[2,2] ) 
  se.vec.int <- c(se.vec.int,summary(m1)$coefficients[1,2] ) 
  int.vec <- c(int.vec,summary(m1)$coefficients[1,1] ) 
  #abline(a=0,b=1, lty=2)
  
}

dev.off()
(df_ab <- data.frame(int=paste0(round(int.vec,2),"±",round(se.vec.int,2)),
                           coef=paste0(round(b.vec,2),"±",round(se.vec.b,2)),
                           R2=round(r2.vec,3),P=round(p.vec,3)))
## Porsanger -----
#ABDF <- readRDS("data/cameratrap/porsanger/processed/regression_data_porsanger_sum.rds")
ABDF <- readRDS("data/calibration/regression_porsanger_mean_logplus1_ab.rds")
#GRDF <- readRDS("data/cameratrap/porsanger/processed/GR_regression_data_porsanger_sum.rds")
GRDF <- readRDS("data/calibration/regression_porsanger_mean_logplus1_GR.rds")

#ABDF <- filter(ABDF, species=="GRAASIDEMUS" & trapseason > 1 & !(station == "G19" & trapseason <= 6) & station != "G4")
#filtdatagr <- filter(GRDF, species=="GRAASIDEMUS" & trapseason > 2 & !(station == "G19" & trapseason <= 7) & station != "G4")
### Growth rates -----

originaldata <- GRDF
r2.vec <- p.vec <- b.vec <- se.vec.b <- se.vec.int <- int.vec <- c()
pdf("plots/lm_porsanger_gr_mean.pdf", height=6*1.2,width=8*1.2)
par(mfrow=c(2,4))
for(i in 1:8) 
{
  
  newGR <- originaldata[, c(1:5, 5 + i)]
  varname <- names(newGR)[6]
  colnames(newGR)[6] = "logvar"

  m1 <- lm(logvar~logAb,data=newGR)
  s0 <- summary(m1)
  print(s0)
  print(paste0("R^2=",round(s0$r.squared,3)))
  print(paste0("F=",round(s0$fstatistic[1],3)))
  
  plot(newGR$logvar~newGR$logAb, pch=19, 
       xlim=c(min(newGR$logAb-.1),max(newGR$logAb+.1)), 
       ylim=c(-4,4),
  col=scales::alpha(2,.8), 
       main=paste0(plotlab[i]), 
  ylab="Mean CT-counts (GR)", xlab="Mean CR-Abundance (GR)" ,
  )
  mtext(bquote(R^2== .(round(s0$r.squared,2))), cex=.8)
  
  seqpred <- seq(min(newGR$logAb)-.1,max(newGR$logAb)+.1,.1)
  pdf <- as.data.frame(predict(m1,newdata=data.frame(logAb=seqpred), interval = "confidence"))
  polygon(c(seqpred,c(rev(seqpred))),c(pdf$lwr, rev(pdf$upr)),
          col = scales::alpha("gray50",0.5), border = NA)
  abline(m1, lty=1, lwd=1.5)
  
  # polygon(c(seqpred,c(rev(seqpred))),c(pdf$fit-.04, rev(pdf$fit+.04)),
  #         col = "black", border = NA)
  r2.vec <- c(r2.vec,s0$r.squared)
  p.vec <- c(p.vec,lmp(m1))  
  b.vec <- c(b.vec,summary(m1)$coefficients[2,1] ) 
  se.vec.b <- c(se.vec.b,summary(m1)$coefficients[2,2] ) 
  se.vec.int <- c(se.vec.int,summary(m1)$coefficients[1,2] ) 
  int.vec <- c(int.vec,summary(m1)$coefficients[1,1] ) 
  #abline(b=0,a=0, lty=2)
}
dev.off()
(df_gr <- data.frame(int=paste0(round(int.vec,2),"±",round(se.vec.int,2)),
                     coef=paste0(round(b.vec,2),"±",round(se.vec.b,2)),
                     R2=round(r2.vec,3),P=round(p.vec,3)))

### Density -----
pdf("plots/lm_porsanger_mean.pdf", height=6*1.2,width=8*1.2)
par(mfrow=c(2,4))
originaldata <- ABDF
summary(originaldata)
r2.vec <- p.vec <- b.vec <- se.vec.b <- se.vec.int <- int.vec <- c()
for(i in 1:8) 
{

  newGR <- originaldata[, c(1:5, 5 + i)]
  varname <- names(newGR)[6]
  colnames(newGR)[6] = "logvar"
  colnames(newGR)[4] = "logAb"
  
  
  # fit model to log vars
  m1 <- lm(logvar~logAb,data=newGR)
  s0 <- summary(m1)
  print(s0)
  
  # plot 
  plot(newGR$logvar~newGR$logAb, pch=19, col=scales::alpha(2,.8), 
       main=paste0(plotlab[i]), 
       ylab="Mean log CT-counts", xlab="Mean log CR-Abundance" ,
       xlim=c(min(newGR$logAb-mean(newGR$logAb)*.25),max(newGR$logAb+mean(newGR$logAb)*.25)), 
       ylim=c(-3,3)
       ) 
  mtext(bquote(R^2== .(round(s0$r.squared,2))), cex=.8)
  
  seqpred <- seq(min(newGR$logAb),max(newGR$logAb),.1)
  pdf <- as.data.frame(predict(m1,newdata=data.frame(logAb=seqpred), interval = "confidence"))
  polygon(c(seqpred,c(rev(seqpred))),c(pdf$lwr, rev(pdf$upr)),
          col = scales::alpha("gray50",0.5), border = NA)
  abline(m1, lty=1, lwd=1.5)
  
  r2.vec <- c(r2.vec,s0$r.squared)
  p.vec <- c(p.vec,lmp(m1))  
  b.vec <- c(b.vec,summary(m1)$coefficients[2,1] ) 
  se.vec.b <- c(se.vec.b,summary(m1)$coefficients[2,2] ) 
  se.vec.int <- c(se.vec.int,summary(m1)$coefficients[1,2] ) 
  int.vec <- c(int.vec,summary(m1)$coefficients[1,1] ) 
  #abline(b=1,a=0, lty=2)
}
dev.off()
(df_ab <- data.frame(int=paste0(round(int.vec,2),"±",round(se.vec.int,2)),
                     coef=paste0(round(b.vec,2),"±",round(se.vec.b,2)),
                     R2=round(r2.vec,3),P=round(p.vec,3)))
### Håkøya aggregated ----

par(mfrow=c(1,1))
for(i in 1:4)
{
  pdat <- filter(ABDF, station==i)
  if(i==1) plot(pdat$trapsession, pdat$DT, type="l",col=i, ylim=c(0,5))
  else lines (pdat$trapsession, pdat$DT, type="l",col=i)
}

hakk <- aggregate(cbind(DT,previousday,experimdays.sum,int8day.sum)~trapsession, data=ABDF, mean)

summary(lm(ABDF$DT~ABDF$previousday+factor(ABDF$station)))

