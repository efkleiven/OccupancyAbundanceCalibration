# Linear Models - only abundance - Porsanger

# Porsanger 
PorD <- readRDS("data/porsanger_mean_intervals.rds")
pormean <- filter(PorD, species=="GRAASIDEMUS" & trapseason > 1 & !(station == "G19" & trapseason <= 6))
View(pormean)
unique(pormean$station)
pormean$Abundance_HT  <- pormean$Abundance_HT/16 # divide by number of traps
pormean[,4:ncol(pormean)]<- log(pormean[,4:ncol(pormean)]+1)
r2.vec <- p.vec <- b.vec <- se.vec.b <- se.vec.int <- int.vec <- c()
mods <- list()
for(i in 1:13) 
{
  datacol <- pormean[, c(1:5, 5 + i)]
  varname <- names(datacol)[6]
  colnames(datacol)[6] = "var"

  m1 <- lm(var~Abundance_HT,data=datacol)
  s0 <- summary(m1)
  
  mods[[i]] <- m1
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

(porint <- data.frame(int=paste0(round(int.vec,2),"±",round(se.vec.int,2)),
                     coef=paste0(round(b.vec,2),"±",round(se.vec.b,2)),
                     R2=round(r2.vec,3),P=round(p.vec,3),
                     varnames=names(PorD)[6:ncol(PorD)]))
plot(c(1,seq(2,22,2)),porint$R2[1:12], xlab= "Time Window (days)",ylab=bquote(R^2), pch=19, col="gray50", type="b",lty=2, ylim=c(0,.6))
points(4,porint$R2[3], col=4, pch=19, cex=1.3)
plot(pormean$int_4~pormean$Abundance_HT, pch=19, col=scales::alpha(4,.8), 
     main=paste0("4-day interval"), ylab="log mean CT-counts", xlab="log mean CR-Abundance")

mtext(bquote(R^2== .(round(r2.vec[4],2))), cex=.8)

seqpred <- seq(min(pormean$Abundance_HT)-.1,max(pormean$Abundance_HT)+.1,.1)
pdf <- as.data.frame(predict(mods[[3]],newdata=data.frame(Abundance_HT=seqpred), interval = "confidence"))
polygon(c(seqpred,c(rev(seqpred))),c(pdf$lwr, rev(pdf$upr)),
        col = scales::alpha("gray50",0.5), border = NA)
abline(mods[[3]], lty=1, lwd=1.5)

# Previous days ----- 
PorD2 <- readRDS("data/porsanger_mean_intervals_prewindow.rds")
porpre <- filter(PorD2, species=="GRAASIDEMUS" & trapseason > 1 & !(station == "G19" & trapseason <= 6))
unique(porpre$station)
porpre$Abundance_HT  <- porpre$Abundance_HT/16 # divide by number of traps
porpre[,4:ncol(porpre)]<- log(porpre[,4:ncol(porpre)]+1)
r2.vec <- p.vec <- b.vec <- se.vec.b <- se.vec.int <- int.vec <- c()
mods <- list()
for(i in 1:11) 
{
  datacol <- porpre[, c(1:5, 5 + i)]
  varname <- names(datacol)[6]
  colnames(datacol)[6] = "var"
  
  m1 <- lm(var~Abundance_HT,data=datacol)
  s0 <- summary(m1)
  
  mods[[i]] <- m1
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

(porpredf <- data.frame(int=paste0(round(int.vec,2),"±",round(se.vec.int,2)),
                     coef=paste0(round(b.vec,2),"±",round(se.vec.b,2)),
                     R2=round(r2.vec,4),P=round(p.vec,3),
                     varnames=names(PorD)[6:ncol(PorD2)]))
lines(1:12,porpredf$R2[1:12], xlab= "Time Window (days)",ylab=bquote(R^2), pch=19, col="orange", type="b",lty=2)
points(4,porpredf$R2[4], col=4, pch=19, cex=1.3)
plot(porpre$int_4~porpre$Abundance_HT, pch=19, col=scales::alpha(4,.8), 
     main=paste0("4-day interval"), ylab="log mean CT-counts", xlab="log mean CR-Abundance")

mtext(bquote(R^2== .(round(r2.vec[4],2))), cex=.8)

seqpred <- seq(min(porpre$Abundance_HT)-.1,max(porpre$Abundance_HT)+.1,.1)
pdf <- as.data.frame(predict(mods[[3]],newdata=data.frame(Abundance_HT=seqpred), interval = "confidence"))
polygon(c(seqpred,c(rev(seqpred))),c(pdf$lwr, rev(pdf$upr)),
        col = scales::alpha("gray50",0.5), border = NA)
abline(mods[[3]], lty=1, lwd=1.5)

par(mfrow=c(1,2))
plot(c(1,seq(2,22,2)),porint$R2[1:12], main="Porsanger",xlab= "Time Window (days)",ylab=bquote(R^2), pch=19, col=alpha(2,.7), type="b",lty=2, ylim=c(0,.6))
points(4,porint$R2[3], col=alpha(2,1), pch=15, cex=1.5)

lines(1:12,porpredf$R2[1:12], xlab= "Time Window (days)",ylab=bquote(R^2), pch=19, col=alpha(4,.7), type="b",lty=2)
points(4,porpredf$R2[4], col=alpha(4,1), pch=15, cex=1.5)

