ABDF <- readRDS("data/cameratrap/porsanger/processed/regression_data_porsanger.rds")
ABDF_m <- readRDS("data/cameratrap/porsanger/processed/regression_data_porsanger_mean.rds")
unique(ABDF$station)
GRDF <- readRDS("data/cameratrap/porsanger/processed/GR_regression_data_porsanger.rds")
GRDF_m <- readRDS("data/cameratrap/porsanger/processed/GR_regression_data_porsanger_mean.rds")

filtdataab <- filter(ABDF, species=="GRAASIDEMUS" & trapseason > 1 & !(station == "G19" & trapseason <= 6) & station != "G4")
filtdatagr <- filter(GRDF, species=="GRAASIDEMUS" & trapseason > 2 & !(station == "G19" & trapseason <= 7) & station != "G4")

i=6
newAB <- dataAB[, c(1:5, 5 + i)]
newGR <- dataGR[, c(1:5, 5 + i)]

colnames(newAB)[6] = colnames(newGR)[6] = "var"

par(mfrow=c(1,1))

hist(newGR$Abundance_HT, breaks=seq(-4,4,.5))
hist(exp(newGR$Abundance_HT), breaks=seq(0,20,1))
table(abs(round(exp(newGR$Abundance_HT),3))<0.8)




mod0 <- npreg::ss(newGR$Abundance_HT, newGR$var, method = smethod)
plot(mod0, add=TRUE)
points(newGR$var,newGR$Abundance_HT, pch=19)
mod0$spar
#modAB <- npreg::ss(newAB$Abundance_HT, newAB$var, method = smethod)


B=1000

set.seed(12345)
sf_list <- inv_list <- list()
params <- c()
bdata_list <- c()
for(i in 1:B)
{
  bdata_list[[i]] <- bootdata <- sample_frac(newGR, replace=TRUE)
  mod1 <- npreg::ss(bootdata$Abundance_HT,bootdata$var,method="REML", control.spar = list(lower=0.7))
  params[i] <- mod1$spar
  predy <- mod1$y
  predx <- mod1$x
  
  inv_list[[i]] <- splinefun(predy,predx, method = "monoH.FC")
  sf_list[[i]] <- splinefun(predx,predy, method = "monoH.FC")
  
}

# bootx <- bdata_list[[i]]
# points(bootx$Abundance_HT,bootx$var)
# mod00 <- npreg::ss(bootx$Abundance_HT,bootx$var,method="REML", control.spar = list(lower=0.5))
# plot(mod00)
# mod00$spar
# hist(params)
par(mfrow=c(1,1))
plot(10,10, ylim=c(-4,4),xlim=c(-4,4))
plot(mod1)
seqx=seq(-5,5,.1)
quantiledf <- invdf <- matrix(nrow=length(seqx),ncol=B)

for(i in 1:B)
{
  fun0 <- sf_list[[i]]
  inv0 <- inv_list[[i]]
  
  quantiledf[,i] <- fun0(seqx)
  invdf[,i] <- inv0(seqx)
  
  lines(seqx,fun0(seqx), col=scales::alpha(3,.1))
  lines(seqx,inv0(seqx), col=scales::alpha(2,.1))
  
  
}

qdf <- invdf
q025 <- apply(qdf,1, quantile, probs=0.025)
q50 <- apply(qdf,1, quantile, probs=0.5)
q975 <- apply(qdf,1, quantile, probs=0.975)

lines(seqx, q025, col=3)
lines(seqx, q50, col=3, lwd=2)

lines(seqx, q975, col=3)

?quantile

points(newGR$Abundance_HT,newGR$var, pch=19)

plot(predy,predx)
modpred <- npreg::ss(predy,predx,method="REML")
plot(modpred)
points(newGR$var,newGR$Abundance_HT, pch=19)
sf2 <- splinefun(newGR$Abundance_HT,newGR$var)
plot()
sf1(0)

# GCV
# for growth rates, how many times do we get the sign right

station1 <- filter(GRDF, station == "G1")

# General framework -----
# start with data 
GRDF <- readRDS("data/cameratrap/porsanger/processed/GR_regression_data_porsanger.rds")
filtdatagr <- filter(GRDF, species=="GRAASIDEMUS" & trapseason > 2 & !(station == "G19" & trapseason <= 7) & station != "G4")
newGR <- dataGR[, c(1:5, 5 + i)]
colnames(newGR)[6] = "var"
unstations <- unique(newGR$station)

# original model
mod0 <- npreg::ss(newGR$Abundance_HT, newGR$var, method = smethod)

# loop over number of stations
##  filter station 1 from data
###  bootstrap data 1000 times
####  fit model to remaining stations
####  approximate curve and inverse with spline.fun
###  get quantiles from the bootstrapped data
## perform prediction for each station
# compute average prediction

## FUNCTIONS ####

inv_boot_quantiles <- function(stdata, B=500, seed=12345, boot_data=TRUE)
  # non-parametric smoothing model
{
  set.seed(seed)
  params <- c()
  bdata_list <- c()
  seqx=seq(-5,5,.1)
  quantiledf <- invdf <- matrix(nrow=length(seqx),ncol=B)

  for(i in 1:B)
  {
    # bootstrap dataset
    # obtain boot quantiles
    bdata_list[[i]] <- bootdata <- dplyr :: sample_frac(stdata, replace=TRUE)
    # fit ss model and restrict smoothing
    mod1 <- npreg::ss(bootdata$Abundance_HT,bootdata$var,method="REML", control.spar = list(lower=0.7))
    # save parameter
    params[i] <- mod1$spar
    # save x and fitted values of predictor y 
    predy <- mod1$y
    predx <- mod1$x
    
    # approximate smoothing function 
    fun0 <- splinefun(predy,predx)
    inv0 <- splinefun(predx,predy)
    
    # get curves
    # quantiledf[,i] <- fun0(seqx)
    invdf[,i] <- inv0(seqx)
  }
  
  # obtain boot quantiles
  q025 <- apply(invdf,1, quantile, probs=0.025)
  q50 <- apply(invdf,1, quantile, probs=0.5)
  q975 <- apply(invdf,1, quantile, probs=0.975)
  
  if(boot_data==FALSE) bdata_list <- NA
  
  obj <- list(inv_quant=data.frame(x=seqx, q025=q025,q50=q50,q975=q975)
              # ,fun_quantiles=quantiledf, bootdatasets=bdata_list, spar_vector=params
              )
  return(obj)
}



cal_4apply <- function(mod,y0, mean.res=FALSE)
{
  
  x0 <- investr::calibrate(mod,y0,interval="Wald", mean.response = mean.res)
  return(x0[1:3])
  
}

invest_cal <- function(stdata, mean.res=FALSE)
{
  mod0 <- lm(Abundance_HT~var, data = stdata)
  # apply into function to obtain vector
  xx0 <- t(sapply(seqx,cal_4apply, mod=mod0, mean.res=mean.res ))
  xmat <- matrix(unlist(xx0),ncol=3)
  qdf <- data.frame(x=seqx, q025=xmat[,2],q50=xmat[,1],q975=xmat[,3])
  seqx=seq(-5,5,.1)
  return(qdf)
}

x0=-2;y0=2
growthchange <- function(x0,y0, threshold=1.1) 
{
  cond_a = abs(x0) & abs(y0) < log(threshold)
  cond_b = x0 >= log(1/0.8) & y0 >= log(threshold)
  cond_c = x0 <= log(1/0.8) & y0 <= log(threshold)
  
  value <- ifelse(cond_a | cond_b | cond_c == TRUE, 1, 0)
}

int_fun <- function(x,y, iq, operation="coverage")
{
  x <- round(x,1)
  pos <- match(x,round(iq$x,1))
  row <- iq[pos,]
  if(operation=="coverage") value <- ifelse(y > row$q025 & y < row$q975, 1, 0)
  if(operation=="sign") value <- sign(y)==sign(row$q50)*1
  if(operation=="bias") value <- row$q50-y
  if(operation=="growth") value <- growthchange(row$q50,y)
  
  return(value)
}

pred_metrics <-  function(dataset,ustation,boot=FALSE,B=100,mean.res=FALSE)
  # combines previous functions
{
  
  #start_time <- Sys.time()
  
  stdata <- filter(dataset, station != ustation)
  fcvdata <- filter(dataset, station == ustation)
  
  if(boot==TRUE){ib1 <- inv_boot_quantiles(stdata, boot_data = FALSE, B=B); ib=ib1$inv_quant}
  
  if(boot==FALSE) {ib <- invest_cal(dataset,mean.res=mean.res)}
  
  
  qcov <- mapply(FUN=int_fun,x=fcvdata$Abundance_HT,y=fcvdata$var, MoreArgs=list(iq=ib,operation="coverage"))
  qsign <- mapply(FUN=int_fun,x=fcvdata$Abundance_HT,y=fcvdata$var, MoreArgs=list(iq=ib,operation="sign"))*1
  qbias <- mapply(FUN=int_fun,x=fcvdata$Abundance_HT,y=fcvdata$var, MoreArgs=list(iq=ib,operation="bias"))
  qgrowth <- mapply(FUN=int_fun,x=fcvdata$Abundance_HT,y=fcvdata$var, MoreArgs=list(iq=ib,operation="growth"))
  
  qlist <- list(qcov=qcov,qsign=qsign,qbias=qbias, qgrowth=qgrowth)
  #end_time <- Sys.time()
  #print(end_time - start_time)
  return(qlist)
}

atest <- pred_metrics(dataset=newGR,ustation = unstations[6],B=10, boot=FALSE, mean.res=FALSE)

foreach_pred <- function(unstations)
  # change manually in pred_metrics functuin
{
  foreach::foreach(s=1:length(unique(unstations))) %dopar% 
    pred_metrics(dataset=newGR,ustation=unstations[s], B=1000, boot=FALSE, mean.res=FALSE)
}

all_comb_pred <- function(originaldata, imax=6, boot=FALSE, mean.res=FALSE, B=1000)
{

  unstations <- unique(originaldata$station)

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
              pred_metrics(dataset=newGR,ustation=unstations[s], B=1000, boot=FALSE, mean.res=TRUE)
    attr(predL, "label") <- predlabels[i]
    predlist[[i]] <- predL
    # time
    end_time <- Sys.time()
    print(end_time - start_time)
  }
  

  
  return(predlist)
  
}

lm_gr <- predlist


grlist <- all_comb_pred(filtdatagr,imax=8)

plist <- smoothbs
plist <- lmbs
plist <- lmbs_mean
plist <- lm_gr
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

newGR




modlm0 <- lm(Abundance_HT~var, data=newGR, subset = trapseason != 3)
summary(modlm0)

modlm000 <- lm(Abundance_HT~var, data=newGR2)
summary(modlm000)
modlm <- nlme::gls(Abundance_HT~1, data=newGR2)
modlme <- nlme::lme(Abundance_HT~var,random=~1|trapseason, data=newGR2)
summary(modlme)
plot(modlme)

library(mgcv)
modss0 <- npreg::ss(newGR2$Abundance_HT, newGR2$var, method = smethod)
plot(modss0)
lines(modss0$x,modss0$y, col=3)
points(newGR2$Abundance_HT,newGR2$var, pch=19)

par(mfrow=c(1,2))
modgam0 <- gam(Abundance_HT~s(var, bs="ts", k=10)+s(station,bs="re"), data=newGR2)
summary(modgam0)
plot(modgam0)
?gam
plot(modgam0, select=1, ylim=c(-4,4), xlim=c(-4,4))


plot(newGR$Abundance_HT,newGR$var)

newGR2$trapseason <- as.factor(newGR$trapseason)
modgam0 <- gam(Abundance_HT~s(var)+s(trapseason, bs="re"), data=newGR2)

summary(modgam0)
abline(modlm000, lty=2, col=2)

reslm <- newGR2$Abundance_HT-as.vector(fitted(modlm000))
res00 <- newGR2$Abundance_HT-as.vector(modlme$fitted[,1])
1-sum(reslm^2)/sum((newGR2$Abundance_HT-mean(newGR2$Abundance_HT))^2)

modlme$coefficients$random
anova(modlme, modlm)
summary(modlme)
newGR2 <- newGR
newGR2$station <- factor(newGR$station)

start_time <- Sys.time()
stest <- foreach::foreach(s=1:length(unique(unstations))) %dopar% 
  pred_metrics(dataset=newGR,ustation=unstations[s], B=1000, boot=TRUE)
end_time <- Sys.time()
print(end_time - start_time)

stest2 <- foreach::foreach(s=1:length(unique(unstations))) %dopar% 
  pred_metrics(dataset=newGR,ustation=unstations[s], B=1000, boot=FALSE,mean.res=FALSE)

bias1 <- unlist(lapply(stest, `[[`, 3), recursive = TRUE, use.names = TRUE)
bias2 <- unlist(lapply(stest2, `[[`, 3), recursive = TRUE, use.names = TRUE)
sqrt(mean(bias1^2))


## Bootstrap setup #####


## Function to compute estimate of x0
# x0.fun <- function(object, y) {
#   theta <- unname(coef(object))
#   exp((log(theta[1] / mean(y) - 1) - theta[2]) / theta[3])
# }
# y0 <- c(-1, 0, 1)
# mod1 <- npreg::ss(newGR$Abundance_HT,newGR$var,method="REML", control.spar = list(lower=0.7))
# 
# fittedvals <- predict(mod1,newGR$var)
# rawres <- newGR$Abundance_HT-fittedvals$y
# res <- rawres - mean(rawres) # center the residuals
# n <- length(res)
# boot.data <- data.frame(nasturtium, res = res, fit = fitted(mod))
# boot.fun <- function(data, i) {
#   boot.mod <- nls(fit + res[i] ~ theta1 / (1 + exp(theta2 + theta3 * log(conc))),
#                   start = list(theta1 = 1000, theta2 = -1, theta3 = 1), data = data)
# ## Make sure the original estimate also gets returned
# if (all(i == 1:n)) x0.fun(mod, y0) else x0.fun(boot.mod, y0)
# }
# 
# ## Run bootstrap simulation (takes about 50s on a standard laptop)
# set.seed(123) # for reproducibility
# res <- boot(boot.data, boot.fun, R = 9999) # collect 9,999 bootstrap samples
# boot.ci(res, type = "bca") # obtain BCa confidence interval for x0
