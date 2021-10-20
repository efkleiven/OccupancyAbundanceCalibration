# Correlogram plot ####
GSVdata <- readRDS("data/cameratrap/porsanger/processed/GS_photos_porsanger.rds")
TVdata <- readRDS("data/cameratrap/haakoya/processed/tv_photos_haakoya.rds")

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
voledata <- TVdata[,3:23]

CR2 <- cor(voledata, use = "pairwise.complete.obs")
CR2[upper.tri(CR2)] <- NA
diag(CR2) <- NA
nCR <- reshape2::melt(CR2, varnames = c('X2', 'X1'), na.rm = TRUE)
mat2 <- dplyr::left_join(m,nCR)

plot(mat2$dist,mat2$value)
lines(smooth.spline(mat2$dist,mat2$value), col=2)


i=1
par(mfrow=c(2,3))
stationsRJ <- c("Rolf","Rolf","Rolf","Jon")
for(i in 1:4)
{
  plotdata <- filter(TVdata, station==i)

  voledata <- plotdata[,3:23]
  
  CR2 <- cor(voledata, use = "pairwise.complete.obs")
  CR2[upper.tri(CR2)] <- NA
  diag(CR2) <- NA
  nCR <- reshape2::melt(CR2, varnames = c('X2', 'X1'), na.rm = TRUE)
  mat2 <- dplyr::left_join(m,nCR)
  
  plot(mat2$dist,mat2$value, ylim=c(-.2,1), xlab = "Days apart", ylab="Correlation in Number of Photos", 
       main=paste0(stationsRJ[i]," (Station ", i,")"))
  lines(smooth.spline(mat2$dist,mat2$value), col=2)
  
  abline(h=0.5,lty=2)
}

voledata <- GSVdata[,4:24]
CR2 <- cor(voledata, use = "pairwise.complete.obs")
CR2[upper.tri(CR2)] <- NA
diag(CR2) <- NA
nCR <- reshape2::melt(CR2, varnames = c('X2', 'X1'), na.rm = TRUE)
mat2 <- dplyr::left_join(m,nCR)

plot(mat2$dist,mat2$value, ylim=c(-0.2,1), xlab = "Days apart", ylab="Correlation in Number of Photos", 
     main="Porsanger All Stations")
lines(smooth.spline(mat2$dist,mat2$value), col=2)

