# Calibration Plot 
library(dplyr)
library(scales)

# Porsanger Analysis -----
library(dplyr)
library(scales)
# Linear Models - only abundance - Porsanger
colorBlindBlack8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                       "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

# Porsanger 
PorD <- readRDS("data/porsanger_mean_intervals.rds")
pormean <- dplyr::filter(PorD, species=="GRAASIDEMUS" & trapseason > 1 & !(station == "G19" & trapseason <= 6))
pormean$Abundance_HT  <- pormean$Abundance_HT/16 # divide by number of traps
pormean[,4:ncol(pormean)]<- log(pormean[,4:ncol(pormean)]+1)
summary(pormean)
sd(pormean$int_4)
sd(pormean$Abundance_HT)

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

(porint <- data.frame(int=paste0("$",round(int.vec,2),"pm",round(se.vec.int,2),"$"),
                      coef=paste0("$",round(b.vec,2),"pm",round(se.vec.b,2),"$"),
                      R2=round(r2.vec,3),P=round(p.vec,3),
                      varnames=names(PorD)[6:ncol(PorD)]))

## Previous days ----- 
PorD2 <- readRDS("data/porsanger_mean_intervals_prewindow.rds")
porpre <- dplyr::filter(PorD2, species=="GRAASIDEMUS" & trapseason > 1 & !(station == "G19" & trapseason <= 6))
unique(porpre$station)
porpre$Abundance_HT  <- porpre$Abundance_HT/16 # divide by number of traps
porpre[,4:ncol(porpre)]<- log(porpre[,4:ncol(porpre)]+1)
r2.vecX <- p.vecX <- b.vecX <- se.vec.bX <- se.vec.intX <- int.vecX <- c()
modsX <- list()
for(i in 1:(ncol(porpre)-5)) 
{
  datacol <- porpre[, c(1:5, 5 + i)]
  varname <- names(datacol)[6]
  colnames(datacol)[6] = "var"
  
  m1 <- lm(var~Abundance_HT,data=datacol)
  s0 <- summary(m1)
  
  modsX[[i]] <- m1
  r2.vecX <- c(r2.vecX,s0$r.squared)
  p.vecX <- c(p.vecX,lmp(m1))  
  b.vecX <- c(b.vecX,summary(m1)$coefficients[2,1] ) 
  se.vec.bX <- c(se.vec.bX,summary(m1)$coefficients[2,2] ) 
  se.vec.intX <- c(se.vec.intX,summary(m1)$coefficients[1,2] ) 
  int.vecX <- c(int.vecX,summary(m1)$coefficients[1,1] ) 
  #abline(b=0,a=0, lty=2)
  #abline(a=0,b=1, lty=2)
  print(i)
}



(porpredf <- data.frame(int=paste0("$",round(int.vecX,2),"pm",round(se.vec.intX,2),"$"),
                        coef=paste0("$",round(b.vecX,2),"pm",round(se.vec.bX,2),"$"),
                        R2=round(r2.vecX,4),P=round(p.vecX,3),
                        varnames=names(PorD2)[6:ncol(PorD2)]))
# Hakoya Analysis and Plots (abundance) -----

# Linear Models - only abundance - Porsanger
colorBlindBlack8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                       "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Hakoya 
hakmean <- readRDS("data/haakoya_mean_intervals.rds")
hakmean[,3:ncol(hakmean)]<- log(hakmean[,3:ncol(hakmean)]+1)
r2.vec <- p.vec <- b.vec <- se.vec.b <- se.vec.int <- int.vec <- c()
modsE <- list()
for(i in 1:13) 
{
  datacol <- hakmean[, c(1:5, 5 + i)]
  varname <- names(datacol)[6]
  colnames(datacol)[6] = "var"
  
  m1 <- lm(var~D50,data=datacol)
  s0 <- summary(m1)
  
  modsE[[i]] <- m1
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

(hakint <- data.frame(varnames=names(PorD)[6:ncol(PorD)],int=paste0("$",round(int.vec,2),"pm",round(se.vec.int,2),"$"),
                      coef=paste0("$",round(b.vec,2),"pm",round(se.vec.b,2),"$"),
                      R2=round(r2.vec,3),P=round(p.vec,3)))

# Previous days ----- 
Hakpre <- readRDS("data/haakoya_mean_intervals_prewindow.rds")
Hakpre[,3:ncol(Hakpre)]<- log(Hakpre[,3:ncol(Hakpre)]+1)
r2.vecX <- p.vecX <- b.vecX <- se.vec.bX <- se.vec.intX <- int.vecX <- c()
modsX2 <- list()
i=1
for(i in 1:(ncol(Hakpre)-5)) 
{
  datacol <- Hakpre[, c(1:5, 5 + i)]
  varname <- names(datacol)[6]
  colnames(datacol)[6] = "var"
  
  m1 <- lm(var~D50,data=datacol)
  s0 <- summary(m1)
  
  modsX2[[i]] <- m1
  r2.vecX <- c(r2.vecX,s0$r.squared)
  p.vecX <- c(p.vecX,lmp(m1))  
  b.vecX <- c(b.vecX,summary(m1)$coefficients[2,1] ) 
  se.vec.bX <- c(se.vec.bX,summary(m1)$coefficients[2,2] ) 
  se.vec.intX <- c(se.vec.intX,summary(m1)$coefficients[1,2] ) 
  int.vecX <- c(int.vecX,summary(m1)$coefficients[1,1] ) 
  #abline(b=0,a=0, lty=2)
  #abline(a=0,b=1, lty=2)
  print(i)
}



(hakpredf <- data.frame(int=paste0("$",round(int.vecX,2),"pm",round(se.vec.intX,2),"$"),
                        coef=paste0("$",round(b.vecX,2),"pm",round(se.vec.bX,2),"$"),
                        R2=round(r2.vecX,4),P=round(p.vecX,3),
                        varnames=names(PorD2)[6:ncol(PorD2)]))
# ACTUAL PLOT #####


# ACTUAL PLOT #####
pdf("Plots/R2_GSV_TV.pdf", width=8.77,height=9)
par(mfrow=c(2,2),oma=c(0.5, 4, .5, .5))

## GSV ----
plot(c(1,seq(3,23,2)),porint$R2[1:12], main="",
     xlab= "Time Window (days)",ylab=bquote(R^2), pch=19, col=alpha("gray70"), 
     type="b", ylim=c(0,.6), lty=2)
points(5,porint$R2[3], col=colorBlindBlack8[6], pch=19, cex=1.3)
mtext("Gray-sided Vole", cex=1.5, side=2, line = 5.5)

lines(1:12,porpredf$R2[1:12], xlab= "Time Window (days)",ylab=bquote(R^2), 
      pch=19, col="gray40", type="b", lty=2)
# points(4,porpredf$R2[4], col=alpha(4,1), pch=15, cex=1.5)

plot(pormean$int_4~pormean$Abundance_HT, pch=19, col=alpha(1,.8), 
     main=paste0(""), ylab="CT-index", xlab="CMR-Abundance")

mtext(bquote(R^2== .(round(porint$R2[3],2))), cex=1, line=0.3)

seqpred <- seq(min(pormean$Abundance_HT)-.1,max(pormean$Abundance_HT)+.1,.1)
pdf <- as.data.frame(predict(mods[[3]],newdata=data.frame(Abundance_HT=seqpred), interval = "confidence"))
polygon(c(seqpred,c(rev(seqpred))),c(pdf$lwr, rev(pdf$upr)),
        col = scales::alpha("gray50",0.5), border = NA)
abline(mods[[3]], lty=1, lwd=1.5)

## TV ----

plot(c(1,seq(3,23,2)),hakint$R2[1:12], main="",
     xlab= "Time Window (days)",ylab=bquote(R^2), pch=19, col=alpha("gray70"), 
     type="b", ylim=c(0,.6), lty=2)

lines(1:12,hakpredf$R2[1:12], xlab= "Time Window (days)",ylab=bquote(R^2), 
      pch=19, col="gray40", type="b", lty=2)
points(1,hakpredf$R2[1], col=colorBlindBlack8[3], pch=19, cex=1.5)
mtext("Tundra Vole", cex=1.5, side=2, line = 5.5)

plot(Hakpre$int_0~Hakpre$D50, pch=19, col=alpha(1,.8), 
     main=paste0(""), ylab="CT-index", xlab="CMR-Abundance")

mtext(bquote(R^2== .(round(hakpredf$R2[1],2))), cex=1, line=0.3)

seqpredx <- seq(min(hakmean$D50)-.1,max(hakmean$D50)+.3,.1)
pdf <- as.data.frame(predict(modsX2[[1]],newdata=data.frame(D50=seqpredx), interval = "confidence"))
polygon(c(seqpredx,c(rev(seqpredx))),c(pdf$lwr, rev(pdf$upr)),
        col = scales::alpha("gray50",0.5), border = NA)
abline(modsX2[[1]], lty=1, lwd=1.5)
## ----
dev.off()
