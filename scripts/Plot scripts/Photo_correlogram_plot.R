# Correlogram plot ####

cameradata <- readRDS("data/cameratrap/porsanger/processed/porsanger_camera_processing1.rds")
cameradata$DateTimeOriginal <- as.POSIXct(cameradata$DateTimeOriginal)
DT <- data.table(cameradata)
cameradata$date <- as.Date(format(cameradata$DateTimeOriginal,format='%Y/%m/%d'))
cameradata$count <- 1
unique(cameradata$species)
cameracounts <- aggregate(count ~ date+station+species, data= cameradata, FUN=sum)
GSVcounts <- arrange(filter(cameracounts, species=="G"),date,station)


GSVdata <- readRDS("data/GS_photos_porsanger.rds")
GSVdata[GSVdata$trapseason==1,][,4:13] <- NA



cor(c(0,0,7,0),c(0,1,0,0))

sync_df5 <- function(res, round=0, dist_mat=gps$cumdist, method="pearson")
  # residuals as a smooth function of distance
{
  distmat <- stats :: dist(dist_mat, upper = TRUE, diag = TRUE)
  m <- data.frame(t(combn(as.numeric(rownames(gps)),2)), dist=as.numeric(distmat))
  
  CR2 <- cor(res, method=method)
  CR2[upper.tri(CR2)] <- NA
  diag(CR2) <- NA
  nCR <- reshape2::melt(CR2, varnames = c('X2', 'X1'), na.rm = TRUE)
  colnames(nCR)[3] <- "corres"
  
  mat2 <- dplyr::left_join(m,nCR)
  mat3 <- arrange(mat2, dist)
  mat3$dist <- round(mat3$dist, round)
  mat4 <- aggregate(corres~dist, data=mat3, FUN=mean)
  
  return(mat4)
}

DMAT <-outer(-10:10, -10:10, '-')
DMAT2 <- DMAT[lower.tri(DMAT, diag = FALSE)]
rownames(DMAT) <- paste0("d",-10:10)
colnames(DMAT) <- paste0("d",-10:10)
diag(DMAT)
DMAT[upper.tri(DMAT, diag = TRUE)]
m <- data.frame(t(combn(rownames(DMAT),2)), dist=DMAT2)

cor(GSVdata[,4:24], use = "pairwise.complete.obs")

CR2 <- cor(GSVdata[,4:24], use = "pairwise.complete.obs")
CR2[upper.tri(CR2)] <- NA
diag(CR2) <- NA
nCR <- reshape2::melt(CR2, varnames = c('X2', 'X1'), na.rm = TRUE)
colnames(nCR)[3] <- "corres"
mat2 <- dplyr::left_join(m,nCR)

plot(mat2$dist,mat2$value)
lines(smooth.spline(mat2$dist,mat2$value), col=2)
