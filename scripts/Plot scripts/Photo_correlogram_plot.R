# Correlogram plot ####
GSVdata <- readRDS("data/cameratrap/porsanger/processed/GS_photos_porsanger.rds")
TVdata <- readRDS("data/cameratrap/haakoya/processed/tv_photos_haakoya.rds")
View(TVdata)

# cameras were not in place yet
GSVdata[GSVdata$trapseason==1,][,4:13] <- NA

# create distance matrix (time distance matrix)
DMAT <-outer(-10:10, -10:10, '-')
DMAT2 <- DMAT[lower.tri(DMAT, diag = FALSE)]
rownames(DMAT) <- paste0("d",-10:10)
colnames(DMAT) <- paste0("d",-10:10)
diag(DMAT)
DMAT[upper.tri(DMAT, diag = TRUE)]
m <- data.frame(t(combn(rownames(DMAT),2)), dist=DMAT2)


# voledata <- GSVdata[,4:24]
#voledata <- TVdata[,3:23]

#CR2 <- cor(voledata, use = "pairwise.complete.obs")
#CR2[upper.tri(CR2)] <- NA
#diag(CR2) <- NA
#nCR <- reshape2::melt(CR2, varnames = c('X2', 'X1'), na.rm = TRUE)
#mat2 <- dplyr::left_join(m,nCR)


# plot the effect of the experiment

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

  #plot all data points 
  plot(mat2$dist,mat2$value, ylim=c(-.2,1), xlab = "Days apart", ylab="Correlation in Number of Photos", 
       main="With Experimental days", col=2, pch=19)
  lines(smooth.spline(mat2$dist,mat2$value), col=3)
  abline(h=0.5,lty=2)

  # remove experimental days
  mat2 <- filter(mat2,!( X1 %in% exp.days) & !( X2 %in% exp.days))
  
  # plot without experimental days
  plot(mat2$dist,mat2$value, ylim=c(-.2,1), xlab = "Days apart", ylab="Correlation in Number of Photos", 
       main="Without Experimental days", col=2, pch=19)
  lines(smooth.spline(mat2$dist,mat2$value), col=3)
  abline(h=0.5,lty=2)
  
# For Porsanger
  
  voledata <- GSVdata[,4:24]
  
  par(mfrow=c(1,2))
  
  CR2 <- cor(voledata, use = "pairwise.complete.obs")
  CR2[upper.tri(CR2)] <- NA
  diag(CR2) <- NA
  nCR <- reshape2::melt(CR2, varnames = c('X2', 'X1'), na.rm = TRUE)
  mat2 <- dplyr::left_join(m,nCR)
  
  #plot all data points 
  plot(mat2$dist,mat2$value, ylim=c(-.2,1), xlab = "Days apart", ylab="Correlation in Number of Photos", 
       main="With Experimental days", col=2, pch=19)
  lines(smooth.spline(mat2$dist,mat2$value), col=3)
  abline(h=0.5,lty=2)
  
  # remove experimental days
  mat2 <- filter(mat2,!( X1 %in% exp.days) & !( X2 %in% exp.days))
  
  # plot without experimental days
  plot(mat2$dist,mat2$value, ylim=c(-.2,1), xlab = "Days apart", ylab="Correlation in Number of Photos", 
       main="Without Experimental days", col=2, pch=19)
  lines(smooth.spline(mat2$dist,mat2$value), col=3)
  abline(h=0.5,lty=2)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# plot the different trapping seasons separate
# for Håkøya
tseasons <- unique(TVdata$trapsession)
tdf <- data.frame(trapsession=1:15,month=rep(1:5,3))
TVdata2 <- dplyr::left_join(TVdata,tdf)

month <- c("May","June","July","August","September")
i=1
par(mfrow=c(2,3))
stationsRJ <- c("Rolf","Rolf","Rolf","Jon")
for(i in 1:5)
{
  plotdata <- filter(TVdata2, month == i)

  voledata <- plotdata[,3:23]
  
  CR2 <- cor(voledata, use = "pairwise.complete.obs")
  CR2[upper.tri(CR2)] <- NA
  diag(CR2) <- NA
  nCR <- reshape2::melt(CR2, varnames = c('X2', 'X1'), na.rm = TRUE)
  mat2 <- dplyr::left_join(m,nCR)
  # mat2 <- filter(mat2,!( X1 %in% exp.days) & !( X2 %in% exp.days))

  
  plot(mat2$dist,mat2$value, ylim=c(-.2,1), xlab = "Days apart", ylab="Correlation in Number of Photos", 
       main=month[i], col=2, pch=19)
  lines(smooth.spline(mat2$dist,mat2$value), col=3)
  
  abline(h=0.5,lty=2)
}


# for porsanger
tseasons <- unique(GSVdata$trapseason)
tdf <- data.frame(trapseason=1:9,month=rep(1:3,3))
GSdata2 <- dplyr::left_join(GSVdata,tdf)

month <- c("Spring","Summer","Autumn")
i=1
par(mfrow=c(1,3))
stationsRJ <- c("Rolf","Rolf","Rolf","Jon")
for(i in 1:3)
{
  plotdata <- filter(GSdata2, month == i)
  
  voledata <- plotdata[,4:24]
  
  CR2 <- cor(voledata, use = "pairwise.complete.obs")
  CR2[upper.tri(CR2)] <- NA
  diag(CR2) <- NA
  nCR <- reshape2::melt(CR2, varnames = c('X2', 'X1'), na.rm = TRUE)
  mat2 <- dplyr::left_join(m,nCR)
  # mat2 <- filter(mat2,!( X1 %in% exp.days) & !( X2 %in% exp.days))
  
  
  plot(mat2$dist,mat2$value, ylim=c(-.2,1), xlab = "Days apart", ylab="Correlation in Number of Photos", 
       main=month[i], col=2, pch=19)
  lines(smooth.spline(mat2$dist,mat2$value), col=3)
  
  abline(h=0.5,lty=2)
}

## Correlogram for all stations ----
par(mfrow=c(1,2))

# Pors
voledata <- GSVdata[,4:24]
CR2 <- cor(voledata, use = "pairwise.complete.obs")
CR2[upper.tri(CR2)] <- NA
diag(CR2) <- NA
nCR <- reshape2::melt(CR2, varnames = c('X2', 'X1'), na.rm = TRUE)
mat2 <- dplyr::left_join(m,nCR)

plot(mat2$dist,mat2$value, ylim=c(0,1), xlab = "Days apart", ylab="Correlation in Number of Photos", 
     main="Porsanger", pch=19)
lines(smooth.spline(mat2$dist,mat2$value), col=2)
abline(h=0.5,lty=2)

# Haak
voledata <-  TVdata[,3:23]
CR2 <- cor(voledata, use = "pairwise.complete.obs")
CR2[upper.tri(CR2)] <- NA
diag(CR2) <- NA
nCR <- reshape2::melt(CR2, varnames = c('X2', 'X1'), na.rm = TRUE)
mat2 <- dplyr::left_join(m,nCR)

plot(mat2$dist,mat2$value, ylim=c(0,1), xlab = "Days apart", ylab="Correlation in Number of Photos", 
     main="Håkøya", pch=19, col=1)
lines(smooth.spline(mat2$dist,mat2$value), col=2)
abline(h=0.5,lty=2)





