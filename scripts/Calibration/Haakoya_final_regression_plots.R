# Linear Models - only abundance - Haakoya

# Porsanger 
hakmean <- readRDS("data/haakoya_mean_intervals.rds")
str(hakmean)
unique(hakmean$station)
hakmean[,3:ncol(hakmean)]<- log(hakmean[,3:ncol(hakmean)]+1)
r2.vec <- p.vec <- b.vec <- se.vec.b <- se.vec.int <- int.vec <- c()
mods <- list()
for(i in 1:12) 
{
  datacol <- hakmean[, c(1:5, 5 + i)]
  varname <- names(datacol)[6]
  colnames(datacol)[6] = "var"
  
  m1 <- lm(var~D50,data=datacol)
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
par(mfrow=c(2,2))
par(mfrow=c(1,1))

(hakintdf <- data.frame(int=paste0(round(int.vec,2),"±",round(se.vec.int,2)),
                     coef=paste0(round(b.vec,2),"±",round(se.vec.b,2)),
                     R2=round(r2.vec,3),P=round(p.vec,3),
                     varnames=names(hakmean)[6:(ncol(hakmean)-1)]))
plot(c(1,seq(3,23,2)),hakintdf$R2[c(1:12)], xlab= "Time Window (days)",ylab=bquote(R^2), pch=19, col=alpha("gray20",.5), type="b",lty=2,
     , ylim=c(0,.6))
# points(5,hakintdf$R2[3], col=4, pch=19, cex=1.3)
plot(hakmean$int_4~hakmean$D50, pch=19, col=scales::alpha(4,.8), 
     main=paste0("4-day interval"), ylab="log mean CT-counts", xlab="log mean CR-Abundance")

mtext(bquote(R^2== .(round(r2.vec[4],2))), cex=.8)

seqpred <- seq(min(hakmean$D50)-.1,max(hakmean$D50)+.1,.1)
pdf <- as.data.frame(predict(mods[[3]],newdata=data.frame(D50=seqpred), interval = "confidence"))
polygon(c(seqpred,c(rev(seqpred))),c(pdf$lwr, rev(pdf$upr)),
        col = scales::alpha("gray50",0.5), border = NA)
abline(mods[[3]], lty=1, lwd=1.5)

## # Linear Models - only abundance - Haakoya

# Porsanger 
hakpre <- readRDS("data/haakoya_mean_intervals_prewindow.rds")
str(hakpre)
unique(hakpre$station)
hakpre[,3:ncol(hakpre)]<- log(hakpre[,3:ncol(hakpre)]+1)
r2.vec <- p.vec <- b.vec <- se.vec.b <- se.vec.int <- int.vec <- c()
mods <- list()
for(i in 1:11) 
{
  datacol <- hakpre[, c(1:5, 5 + i)]
  varname <- names(datacol)[6]
  colnames(datacol)[6] = "var"
  
  m1 <- lm(var~D50,data=datacol)
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

(hakpredf <- data.frame(int=paste0(round(int.vec,2),"±",round(se.vec.int,2)),
                     coef=paste0(round(b.vec,2),"±",round(se.vec.b,2)),
                     R2=round(r2.vec,3),P=round(p.vec,3),
                     varnames=names(hakpre)[6:ncol(hakpre)]))
lines(1:22,hakpredf$R2, xlab= "Time Window (days)",ylab=bquote(R^2), pch=19, col=alpha("orange2",.7), type="b",lty=2)
# plot(1,hakpredf$R2[1], col=alpha(6,.5), pch=19, cex=1.3)


plot(hakpre$int_0~hakpre$D50, pch=19, col=scales::alpha(5,.8), 
     main=paste0("1-day interval"), ylab="log mean CT-counts", xlab="log mean CR-Abundance")

mtext(bquote(R^2== .(round(max(r2.vec),2))), cex=.8)

seqpred <- seq(min(hakpre$D50)-.1,max(hakpre$D50)+.1,.1)
pdf <- as.data.frame(predict(mods[[3]],newdata=data.frame(D50=seqpred), interval = "confidence"))
polygon(c(seqpred,c(rev(seqpred))),c(pdf$lwr, rev(pdf$upr)),
        col = scales::alpha("gray50",0.5), border = NA)
abline(mods[[3]], lty=1, lwd=1.5)



plot(c(1,seq(3,23,2)),hakintdf$R2[c(1:12)], xlab= "Time Window (days)",ylab=bquote(R^2), pch=19, col=alpha(2,.7), type="b",lty=2,
     , ylim=c(0,.6), main = "Håkøya")
points(3,hakintdf$R2[2], col=alpha(2,1), pch=15, cex=1.5)

lines(1:22,hakpredf$R2, xlab= "Time Window (days)",ylab=bquote(R^2), pch=19, col=alpha(4,.7), type="b",lty=2)
points(1,hakpredf$R2[1], col=alpha(4,1), pch=15, cex=1.5)

legend("topright", c("Pre-CR","CR-centered window","Peak"), pch=c(19,19,15), 
       col=c(4,2,"gray50"), bty="n")
?legend

