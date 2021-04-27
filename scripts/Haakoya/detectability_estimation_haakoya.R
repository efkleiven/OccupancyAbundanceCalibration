#################### Real Data Analysis #################### 
# --- 2. Estimate detectability using CR-INLA & CR-VGAM --- #

library(dplyr)
library(INLA)
library(VGAM)
# set wd

#### functions to estimate abundance ####
source("~/Documents/OccupancyAbundanceCalibration/scripts/2_2_1_functions.R")

#### read datasets ####
# count only data
# capture history dataset formatted for INLA
data4inla0 <- readRDS("~/Documents/OccupancyAbundanceCalibration/data/capture_recapture/haakoya/processed/haakoya_inlaformat2.rds")
# dataset with only observed capture histories
summary(data4inla0)
data4inla0$wgt[is.na(data4inla0$wgt)] <- mean(data4inla0$wgt, na.rm=TRUE)
data4inla0$sex[is.na(data4inla0$sex)] <- 0.5

head(data4inla0)

# check that count variable is correct
rawdata <- filter(data4inla0, count==1)
nrow(data4inla0)/7
# ------------- Model fitting -------------- #

#### CR INLA MODEL ####
### MOD 0 ###
# intercept model
library(INLA)
formula <- count ~ f(id, initial = -10, fixed=T)
mod00 <- inla(formula, family = "Poisson", data = data4inla, control.predictor = list(compute=T),
          control.compute = list(dic=T, waic=T))
mod00$dic$dic
mod00$waic$waic
modi0 <- mod00

### MOD 1 ###
# weight
formula <- count ~ f(id, initial = -10, fixed=T)+f(alt.id, wgt, fixed = T, constr = T)
modw <- inla(formula, family = "Poisson", data = data4inla0, control.predictor = list(compute=T),
              control.compute = list(dic=T,waic=T))
modw$dic$dic
modw$waic$waic
modi1 <- modw

### MOD 2 ###
# weight + sex
data4inla$alt.id3 <- data4inla$alt.id2

formula <- count ~ f(id, initial = -10, fixed=T)+f(alt.id, wgt, fixed = T, constr = T)+
  f(alt.id2, sex, fixed = T, constr = T)
modws <- inla(formula, family = "Poisson", data = data4inla0, control.predictor = list(compute=T),
              control.compute = list(dic=T,waic=T))
modws$dic$dic
modws$waic$waic
modi2 <- modws

### MOD 3 ###
# weight + sex + station
data4inla0$alt.id2 <- data4inla0$alt.id 
formula <- count ~ f(id, initial = -10, fixed=T)+f(alt.id, wgt, fixed = T, constr = T)+
  f(alt.id2, sex, fixed = T, constr = T)+f(station.i,model="iid")
modwss2 <- inla(formula, family = "Poisson", data = data4inla0, control.predictor = list(compute=T),
                control.compute = list(dic=T,waic=T))
modwss2$dic$dic
modwss2$waic$waic

modi3 <- modwss2

# compute p1 and p2 based on conditional probability ratios
# retrieve fitted values
mod <- modi3
modi3$summary.random$alt.id
# obtained fitted probabilities for different capture histories
fitval <- mod$summary.fitted.values$mean
length(fitval)
nrow(data4inla0)
# standardize fitted values and make them into data frame
prob_df0 <- matrix(fitval, ncol=7)
# scale the transposed matrix to divide rows by the row mean and transpose it back
prob_df <- data.frame(t(scale(t(prob_df0), center = FALSE, scale = rowSums(prob_df0))))

colnames(prob_df) <- paste0("X",data4inla0$chist[1:7])
head(prob_df)
# solve equation system according to Mth 


alpha011 <- prob_df$`X0,1,1`/prob_df$`X1,1,1` # p1
alpha101 <- prob_df$`X1,0,1`/prob_df$`X1,1,1` # p2
alpha110 <- prob_df$`X1,1,0`/prob_df$`X1,1,1` # p3

prob_df$p1_inla <- 1/(alpha011+1)
prob_df$p2_inla <- 1/(alpha101+1)
prob_df$p3_inla <- 1/(alpha110+1)

prob_df$p00_inla <- (1-prob_df$p1_inla)*(1-prob_df$p2_inla)*(1-prob_df$p3_inla)
hist(1-prob_df$p00_inla, col=alpha("red2",.7), probability = TRUE, breaks = seq(.3,1,.025), ylim=c(0,20))
abline(v=mean(1-prob_df$p00_inla))
hist(1-totalvgam$p00.est, col=alpha("blue2",.7), add=TRUE, probability = TRUE, breaks = seq(.3,1,.025))
abline(v=mean(1-totalvgam$p00.est))

# Plots
# quantile(prob_df$p00_inla, c(.025,.975))
# boxplot(prob_df[,c(4,5)], ylim=c(.2,.75))

# lump probabilities with individuals
totaldf <- data.frame(rawdata[,c(1:8,10:12)],p1.est=prob_df$p1_inla,
                      p2.est=prob_df$p2_inla, p3.est=prob_df$p3_inla,
                      p00.est=(prob_df$p00_inla))
inladf <- arrange(totaldf, trapsession,station, id)

# compute N using the HT estimator for each station and time point
# zeros already added
# different Ns for different radius
inlaframe <- partialN_radius(inladf, radius=TRUE)



#### CR VGAM MODEL ####

# # try out different models
# library(VGAM)
# mod0 <- vglm(cbind(c1,c2,c3) ~ 1, posbernoulli.t, data = rawdata)
# mod1 <- vglm(cbind(c1,c2,c3) ~ wgt, posbernoulli.t, data = rawdata)
# mod2 <- vglm(cbind(c1,c2,c3) ~ wgt+sex, posbernoulli.tb, data = rawdata)
# summary(mod2)
# 
# # get fitted values from the VGAM model
# totalvgam <- as.data.frame(cbind(rawdata,fitted(mod1)))
# 
# colnames(totalvgam)[(ncol(totalvgam)-2):(ncol(totalvgam))] <- c("p1","p2","p3")
# totalvgam$p00.est <- (1-totalvgam$p1)*(1-totalvgam$p2)*(1-totalvgam$p3)
# totalvgam$station <- factor(totalvgam$station, levels=unique(totalvgam$station))
# vgamdf <- arrange(totalvgam, trapsession,station, id)
# 
# vgamframe <- partialN(p00.st.tp=data.frame(vgamdf$p00.est,vgamdf$station,vgamdf$trapsession))
# 
# 
# # add zeros
# vgamts <- as.data.frame.table(xtabs(N.est~timepoint+station, data=vgamframe))
# vgamts <- arrange(vgamts,station,timepoint)
# 
# estimatedf2 <- left_join(estimatedf1,vgamts)
# tibble(estimatedf2)
# colnames(estimatedf2)[4] <- "vgamcr"
# colnames(estimatedf2)[1] <- "trapsession"
# estimatedf2$trapsession <- as.numeric(as.character(estimatedf2$trapsession))
# 
# 
# plot(estimatedf2$inlacr, type="l")
# lines(estimatedf2$vgamcr, type="l", col="blue")

##### RAW COUNTS ######
# add raw counts
# add zeros if any
rawts <- as.data.frame(xtabs(count~trapsession+station, data=rawdata))
rawts2 <- arrange(rawts,station,trapsession)
colnames(rawts2)[3] <- "counts"
rawts2 <- as.data.frame(apply(rawts2[,], 2, FUN=as.numeric))


estimatedf3 <- left_join(inlaframe,rawts2)
estimatedf4 <- tibble(estimatedf3)
saveRDS(estimatedf4, "~/Documents/OccupancyAbundanceCalibration/data/capture_recapture/haakoya/processed/haakoya_abundance_radius.rds")


### PLOT ###
# par(mfrow=c(2,2))
# for(i in 1:4)
# {
# 
#   plot(1:15,estimatedf5$inlacr[estimatedf5$station==i], 
#        col="red", type="l", main=i, ylim=c(0,50),
#        xlab="timepoint", ylab="count")
#   lines(1:15,estimatedf5$vgamcr[estimatedf5$station==i], 
#        col="blue", type="l", main=i, ylim=c(0,50),
#        xlab="timepoint", ylab="count")
#   lines(1:15,estimatedf5$counts[estimatedf5$station==i], 
#         col="black", type="l", main=i, ylim=c(0,50),
#         xlab="timepoint", ylab="count")
#   if(i==2) legend("topright",c("CRINLA","CRVGAM","RAW"), col=c("red","blue",1), lty=1)
# }

