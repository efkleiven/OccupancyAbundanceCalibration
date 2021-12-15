# Prediction script


## functions 4 prediction -----
cal_4apply <- function(y0,mod, mean.res=FALSE)
{
  x0 <- investr::calibrate(mod,y0,interval="Wald", mean.response = mean.res)
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

densityinterval <- function(x0,y0, min=2, max=9) 
{
  cond_a = abs(x0) < min & abs(y0) < min
  cond_b = x0 > max & y0 > max
  cond_c = (x0 >= min & y0 >= min) & (x0 <= max & y0 <= max)
  
  value <- ifelse(cond_a | cond_b | cond_c == TRUE, 1, 0)
  return(value)
}

funlist <- list(growthinterval,densityinterval)

x=0.5;y=5 # x is number of photos and y is density

int_fun <- function(x,y, quantile_df, operation="growth", gr=TRUE)
{
  x <- round(x,1)
  pos <- match(x,round(quantile_df$x,1))
  row <- quantile_df[pos,]
  
  # function depends on being gr or not
  f <- ifelse(gr==TRUE,1,2)
  func <- funlist[[f]]
  
  if(operation=="coverage") value <- ifelse(y > row$q025 & y < row$q975, 1, 0)
  if(operation=="sign") value <- sign(y)==sign(row$q50)*1
  if(operation=="bias") value <- row$q50-y
  if(operation=="bands") value <- func(row$q50,y)
  
  return(value)
}

invest_cal <- function(stdata, mean.res=FALSE, gr=NA)
{
  if(gr==TRUE) 
    {
    seqx=seq(-6,6,.1)
    mod0 <- lm(Abundance_HT~var, data = stdata)
    xx0 <- t(sapply(seqx,cal_4apply, mod=mod0, mean.res=mean.res ))
    xmat <- matrix(unlist(xx0),ncol=3)
  }
    
  if(gr==FALSE)   # with abundance
    {
    stdata$logab <- log(stdata$Abundance_HT+1)
    stdata$logvar <- log(stdata$var+1)
    seqx=seq(0,log(305),.1)
    mod0 <- lm(logab ~ logvar, data = stdata)
    xx0 <- t(sapply(seqx,mod=mod0,FUN=cal_4apply, mean.res=mean.res))
    xmat <- matrix(unlist(xx0),ncol=3)
  }
  # apply into function to obtain vector

  qdf <- data.frame(x=seqx, q025=xmat[,2],q50=xmat[,1],q975=xmat[,3])

  return(qdf)
}

pred_cal <-  function(dataset,ustation,mean.res=FALSE,gr=NA)
  # takes dataset, removes station and predicts for that station
{
  
  #start_time <- Sys.time()
  traindata <- filter(dataset, station != ustation)
  testdata <- filter(dataset, station == ustation)
  ib <- invest_cal(traindata,mean.res=mean.res, gr=gr)
  
  ydat <- ifelse(rep(gr,nrow(testdata))==TRUE, testdata$Abundance_HT,log(testdata$Abundance_HT+1))
  xdat <- ifelse(rep(gr,nrow(testdata))==TRUE, testdata$var,log(testdata$var+1))
  
  qcov <- mapply(FUN=int_fun,x=xdat,y=ydat, MoreArgs=list(quantile_df=ib,operation="coverage"), gr=gr)
  qsign <- mapply(FUN=int_fun,x=xdat,y=ydat, MoreArgs=list(quantile_df=ib,operation="sign"), gr=gr)*1
  qbias <- mapply(FUN=int_fun,x=xdat,y=ydat, MoreArgs=list(quantile_df=ib,operation="bias"), gr=gr)
  qgrowth <- mapply(FUN=int_fun,x=xdat,y=ydat, MoreArgs=list(quantile_df=ib,operation="bands"),
                    gr=gr)
  
  qlist <- list(qcov=qcov,qsign=qsign,qbias=qbias, qgrowth=qgrowth)
  #end_time <- Sys.time()
  #print(end_time - start_time)
  return(qlist)
}

### test ####
stdata <- ABDF
i=8
newGR <- stdata[, c(1:5, 5 + i)]
colnames(newGR)[6] = "var"
stdata <- newGR
View(newGR)
a <- invest_cal(stdata, gr=TRUE, mean.res = FALSE)
b <- exp(a)

plot(b$q50,b$x, type="l")
lines(b$q025,b$x, lty=2)
lines(b$q975,b$x, lty=2)
points(stdata$var,stdata$Abundance_HT)

plot(a$q50,a$x, type="l")
lines(a$q025,a$x, lty=2)
lines(a$q975,a$x, lty=2)
points(stdata$logvar,stdata$logab)


## set dataset ----
##Porsanger ----
ABDF <- readRDS("data/cameratrap/porsanger/processed/regression_data_porsanger_sum.rds")
#ABDF_m <- readRDS("data/cameratrap/porsanger/processed/regression_data_porsanger_mean.rds")

GRDF <- readRDS("data/cameratrap/porsanger/processed/GR_regression_data_porsanger_sum.rds")
#GRDF_m <- readRDS("data/cameratrap/porsanger/processed/GR_regression_data_porsanger_mean.rds")

filtdataab <- filter(ABDF, species=="GRAASIDEMUS" & trapseason > 1 & !(station == "G19" & trapseason <= 6) & station != "G4")
filtdatagr <- filter(GRDF, species=="GRAASIDEMUS" & trapseason > 2 & !(station == "G19" & trapseason <= 7) & station != "G4")
## prediction -----
### GR ----
originaldata <- filtdatagr
unstations <- unique(originaldata$station)
imax=8
predlist <- list()
predlabels <- colnames(originaldata)[6:(5+imax)]

for(i in 1:imax)
{
  print(i)
  newGR <- originaldata[, c(1:5, 5 + i)]
  colnames(newGR)[6] = "var"
  #time
  start_time <- Sys.time()
  predL <- foreach::foreach(s=1:length(unique(unstations))) %dopar% 
    pred_cal(dataset=newGR,ustation=unstations[s], gr=TRUE)
  attr(predL, "label") <- predlabels[i]
  predlist[[i]] <- predL
  # time
  end_time <- Sys.time()
  print(end_time - start_time)
}

gr_dl_p <- predlist

### AB ----
originaldata <- filtdataab

unstations <- unique(originaldata$station)

imax=8
i=6
predlabels <- colnames(originaldata)[6:(5+imax)]
predlist <- list()
for(i in 1:imax)
{
  print(i)
  newGR <- originaldata[, c(1:5, 5 + i)]
  colnames(newGR)[6] = "var"

  #time
  start_time <- Sys.time()
  predL <- foreach::foreach(s=1:length(unique(unstations))) %dopar% 
    pred_cal(dataset=newGR,ustation=unstations[s], gr=FALSE)

  attr(predL, "label") <- predlabels[i]
  predlist[[i]] <- predL
  # time
  end_time <- Sys.time()
  print(end_time - start_time)
}

ab_dl_p <- predlist

pred_cal(dataset=newGR,ustation=unstations[1], gr=FALSE)

## print results -----
predlist <- ab_dl
predlist <- gr_dl_p
predlist <- predL
coverageL <- rmseL <- grL <- c() 
for(i in 1:length(predlist))
{
  vari <- predlist[[i]]
  print(attributes(vari))
  
  cov1 <- mean(unlist(lapply(vari, `[[`, 1), recursive = TRUE, use.names = TRUE))
  coverageL <- c(coverageL,mean(cov1))
  print(paste0("coverage ",round(cov1,3)))
  
  mse <- mean(unlist(lapply(vari, `[[`, 2), recursive = TRUE, use.names = TRUE)^2)
  rmseL <- c(rmseL,sqrt(mse))
  print(paste0("RMSE ",round(mse,3)))
  
  grb <- mean(unlist(lapply(vari, `[[`, 4), recursive = TRUE, use.names = TRUE))
  grL <- c(grL,grb)
  print(paste0("ChangeCov ",round(grb,3)))
  
}

plist <- predlist
coverageL <- rmseL <- grL <- c() 
for(l in 1:length(plist))
{
  print(predlabels[l])
  cov1 <- mean(unlist(lapply(plist[[l]], `[[`, 1), recursive = TRUE, use.names = TRUE))
  coverageL <- c(coverageL,cov1)
  print(paste0("cov ",round(cov1,3)))
  
  sign1 <- mean(unlist(lapply(plist[[l]], `[[`, 2), recursive = TRUE, use.names = TRUE))
  coverageL <- c(coverageL,cov1)
  print(paste0("Signcov ",round(sign1,3)))
  
  mse <- mean(unlist(lapply(plist[[l]], `[[`, 3), recursive = TRUE, use.names = TRUE)^2)
  rmseL <- c(rmseL,sqrt(mse))
  print(paste0("RMSE ",round(mse,3)))
  
  grb <- mean(unlist(lapply(plist[[l]], `[[`, 4), recursive = TRUE, use.names = TRUE))
  grL <- c(grL,grb)
  print(paste0("GRcov ",round(grb,3)))
  
}

## Håkøya ------

ABDF <- readRDS("data/calibration/Haakoya/regression_data_haakoya_ab_sum.rds")
GRDF <- readRDS("data/calibration/Haakoya/GR_regression_data_haakoya_sum.rds")
str(GRDF)

ABDF2 <- ABDF_m[,c("trapsession","station","D25","DT",
                 "previousday", "experimdays.mean",   
                 "previous3day.mean",  "previous5day.mean",  
                 "previous10day.mean", "int1day.mean" ,     
                 "int3day.mean" ,"int8day.mean")]

GRDF2 <- GRDF[,c("trapsession","station","D25","D50","DT",
                 "previousday", "experimdays.mean",   
                 "previous3day.mean",  "previous5day.mean",  
                 "previous10day.mean", "int1day.mean" ,     
                 "int3day.mean" ,"int8day.mean")]
names(GRDF)

unstations <- unique(originaldata$station)
imax=8
predlist <- list()
predlabels <- colnames(originaldata)[6:(5+imax)]
for(i in 1:imax)
{
  print(i)
  newGR <- originaldata[, c(1:4, 4 + i)]
  colnames(newGR)[5] = "var"
  colnames(newGR)[3] = "Abundance_HT"
  #time
  start_time <- Sys.time()
  predL <- foreach::foreach(s=1:length(unique(unstations))) %dopar% 
    pred_cal(dataset=newGR,ustation=unstations[s])
  attr(predL, "label") <- predlabels[i]
  predlist[[i]] <- predL
  # time
  end_time <- Sys.time()
  print(end_time - start_time)
}

i=1

ABDF2 <- ABDF_m[,c("trapsession","station","D25","D50","DT",
                   "previousday", "experimdays.mean",   
                   "previous3day.mean",  "previous5day.mean",  
                   "previous10day.mean", "int1day.mean" ,     
                   "int3day.mean" ,"int8day.mean")]
originaldata <- GRDF2
d=1
for(d in 1:3){
  print(names(originaldata)[d+2])
for(i in 1:7) 
{
  newGR <- originaldata[, c(1:5, 5 + i)]
  print(names(newGR)[6])
  colnames(newGR)[6] = "var"
  colnames(newGR)[2+d] = "Abundance_HT"
  
  m1 <- lm(Abundance_HT~var,data=newGR)

  #plot(newGR$var,newGR$Abundance_HT)
  #abline(m1)
  s0 <- summary(m1)

  print(paste0("adj R^2=",round(s0$adj.r.squared,3)))
  print(paste0("F=",round(s0$fstatistic[1],3)))
  }
}


ABDF <- readRDS("data/cameratrap/porsanger/processed/regression_data_porsanger.rds")
ABDF_m <- readRDS("data/cameratrap/porsanger/processed/regression_data_porsanger_mean.rds")

ABm <- filter(ABDF_m, species=="GRAASIDEMUS" & trapseason > 1 & !(station == "G19" & trapseason <= 6) & station != "G4")
AB0 <- filter(ABDF, species=="GRAASIDEMUS" & trapseason > 1 & !(station == "G19" & trapseason <= 6) & station != "G4")


pred_cal(dataset=newGR,ustation=unstations[s], gr=FALSE)

a_m <- lm(Abundance_HT~interval_3day.mean, data=ABm)
summary(a_m)
a_0 <- lm(log(AB0$Abundance_HT+1)~log(AB0$`9daysperiod.sum`+1)+I(log(AB0$`9daysperiod.sum`+1)^2))
summary(a_0)
b_m <- lm(log(ABm$Abundance_HT+1)~log(ABm$interval_3day.mean+1))
b_0 <- lm(log(AB0$Abundance_HT+1)~log(AB0$`9daysperiod.sum`+1))
summary(b_m)
library(investr)
calibrate(a_m,y0=5)
?calibrate
