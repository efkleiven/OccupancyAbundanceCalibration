#################### Real Data Analysis #################### 
# --- 2. Estimate detectability using CR-INLA & CR-VGAM --- #

library(dplyr)
library(INLA)
library(VGAM)
# set wd

#### functions to estimate abundance ####
source("/Users/pedronicolau/OccupancyAbundanceCalibration/data/scripts/2_2_1_functions.R")

#### read datasets ####
# count only data
# capture history dataset formatted for INLA
data4inla0 <- readRDS("/Users/pedronicolau/OccupancyAbundanceCalibration/data/porsanger_inlaformat.rds")
# dataset with only observed capture histories
data4inla <- filter(data4inla0, species == "GRAASIDEMUS" & covNA == 0)
head(rawdata_NA)
rawdata_noNA <- filter(data4inla0, count == 1 & species == "GRAASIDEMUS" & covNA == 0)
rawdata_NA <- filter(data4inla0, count == 1 & species == "GRAASIDEMUS" & covNA == 1)


# ------------- Model fitting -------------- #

#### CR INLA MODEL ####
### MOD 0 ###
# intercept model
formula <- count ~ f(id, initial = -10, fixed=T)
mod00 <- inla(formula, family = "Poisson", data = data4inla, control.predictor = list(compute=T),
          control.compute = list(dic=T, waic=T))
mod00$dic$dic
mod00$waic$waic
modi0 <- mod00

### MOD 1 ###
# weight
formula <- count ~ f(id, initial = -10, fixed=T)+f(alt.id, weight, fixed = T, constr = T)
modw <- inla(formula, family = "Poisson", data = data4inla, control.predictor = list(compute=T),
              control.compute = list(dic=T,waic=T))
modw$dic$dic
modw$waic$waic
modi1 <- modw0

### MOD 2 ###
# weight + sex
formula <- count ~ f(id, initial = -10, fixed=T)+f(alt.id, weight, fixed = T, constr = T)+
  f(alt.id2, sex, fixed = T, constr = T)
modws <- inla(formula, family = "Poisson", data = data4inla, control.predictor = list(compute=T),
              control.compute = list(dic=T,waic=T))
modws$dic$dic
modws$waic$waic
modi2 <- modws

data4inla$alt.id3 <- data4inla$alt.id2
### MOD 3 ###
# weight + sex + station
formula <- count ~ f(id, initial = -10, fixed=T)+f(alt.id, weight, fixed = T, constr = T)+
  f(alt.id2, sex, fixed = T, constr = T)+f(station.i,model="iid")
modwss2 <- inla(formula, family = "Poisson", data = data4inla, control.predictor = list(compute=T),
                control.compute = list(dic=T,waic=T))
modwss2$dic$dic
# modwss2$waic$waic
modi3 <- modwss2

# compute p1 and p2 based on conditional probability ratios
# retrieve fitted values
mod <- modi3
# obtained fitted probabilities for different capture histories
fitval <- mod$summary.fitted.values$mean
# standardize fitted values and make them into data frame
prob_df <- makedf(stdize(fitval))

# solve equation system according to Mth 
alpha01 <- prob_df$X0.1/prob_df$X1.1
alpha10 <- prob_df$X1.0/prob_df$X1.1
prob_df$p1_inla <- 1/(alpha01+1)
prob_df$p2_inla <- 1/(alpha10+1)
prob_df$p00_inla <- (1-prob_df$p2_inla)*(1-prob_df$p1_inla)

# Plots
# quantile(prob_df$p00_inla, c(.025,.975))
# boxplot(prob_df[,c(4,5)], ylim=c(.2,.75))

# lump probabilities with individuals
partialdf <- data.frame(rawdata_noNA,p1.est=prob_df$p1_inla,
                      p2.est=prob_df$p2_inla, p00.est=(1-prob_df$p1_inla)*(1-prob_df$p2_inla))
NAdf <- data.frame(rawdata_NA,p1.est=mean(partialdf$p1.est),
                      p2.est=mean(partialdf$p2.est), p00.est=mean(partialdf$p00.est))
totaldf <- rbind(partialdf,NAdf)
totaldf$station <- factor(totaldf$station)
inladf <- arrange(totaldf, trapseason,station, id)

# compute N using the HT estimator for each station and time point
inlaframe <- partialN(p00.st.tp=data.frame(inladf$p00.est,inladf$station,inladf$trapseason))
# add zero counts
estimatedf <- as.data.frame.table(xtabs(N.est~timepoint+station, data=inlaframe))
colnames(estimatedf)[3] <- "inlacr"
estimatedf1 <- arrange(estimatedf,station,timepoint)



#### CR VGAM MODEL ####

# try out different models
mod0 <- vglm(cbind(c1,c2) ~ 1, posbernoulli.t, data = rawdata_noNA)
mod1 <- vglm(cbind(c1,c2) ~ weight, posbernoulli.t, data = rawdata_noNA)
mod2 <- vglm(cbind(c1,c2) ~ weight+sex, posbernoulli.t, data = rawdata_noNA)


# get fitted values from the VGAM model
vgam_noNA <- as.data.frame(cbind(rawdata_noNA,fitted(mod1)))

colnames(vgam_noNA)[(ncol(vgam_noNA)-1):(ncol(vgam_noNA))] <- c("p1","p2")
vgam_noNA$p00.est <- (1-vgam_noNA$p1)*(1-vgam_noNA$p2)

vgam_NA <- data.frame(rawdata_NA,p1=mean(vgam_noNA$p1),
                      p2=mean(vgam_noNA$p2), p00.est=mean(vgam_noNA$p00.est))

totalvgam <- rbind(vgam_noNA, vgam_NA)

totalvgam$station <- factor(totalvgam$station, levels=unique(totalvgam$station))
vgamdf <- arrange(totalvgam, trapseason,station, id)

vgamframe <- partialN(p00.st.tp=data.frame(vgamdf$p00.est,vgamdf$station,vgamdf$trapseason))


# add zeros
vgamts <- as.data.frame.table(xtabs(N.est~timepoint+station, data=vgamframe))
vgamts <- arrange(vgamts,station,timepoint)

estimatedf2 <- left_join(estimatedf1,vgamts)
colnames(estimatedf2)[4] <- "vgamcr"
colnames(estimatedf2)[1] <- "trapseason"
estimatedf2$trapseason <- as.numeric(as.character(estimatedf2$trapseason))
saveRDS(estimatedf2, "/Users/pedronicolau/OccupancyAbundanceCalibration/data/estimated_abundance.rds")

plot(estimatedf2$inlacr, type="l")
lines(estimatedf2$vgamcr, type="l", col="blue")

# ###
# par(mfrow=c(2,3))
# for(i in unique(countdata$station))
# {
#   plot(as.numeric(countdata$timepoint[countdata$station==i]),as.numeric(countdata$count[countdata$station==i]), col="black", type="l", main=i, ylim=c(0,50),
#        xlab="timepoint", ylab="count")
#   lines(as.numeric(countdata$timepoint[countdata$station==i]),countdata$inlaCR[countdata$station==i], col="red")
#   #lines(as.numeric(countdata$timepoint[countdata$station==i]),countdata$VGAMCRt[countdata$station==i], col="green1")
#   lines(as.numeric(countdata$timepoint[countdata$station==i]),countdata$VGAMCRtb[countdata$station==i], col="green4")
# }
# saveRDS(countdata, "Data/processed data/estimatedcounts.rds")
