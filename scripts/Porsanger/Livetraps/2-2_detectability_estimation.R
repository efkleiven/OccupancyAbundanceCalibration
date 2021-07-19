#################### Real Data Analysis #################### 
# --- 2. Estimate detectability using CR-INLA & CR-VGAM --- #

library(dplyr)
library(INLA)
# set wd
source("scripts/Porsanger/Livetraps/2_2_1_functions.R")


#### read datasets ####
# count only data
# capture history dataset formatted for INLA
data4inla <- readRDS("data/capture_recapture/porsanger/CHist_porsanger_4INLA.rds")
# dataset with only observed capture histories

data4inla$weight[is.na(data4inla$weight)] <- mean(data4inla$weight, na.rm=TRUE)
data4inla$sex01[is.na(data4inla$sex01)] <- 0.5


### filter species ###
nspecies <- unique(data4inla$species)
specieslist <- list()
for(s in 1:length(nspecies))
{
  data4inla0 <- filter(data4inla, species==nspecies[s])
  
  rawdata <- filter(data4inla0, count == 1)
  nrow(data4inla0) / 3
  
  ### MOD 3 ###
  # weight + sex + station
  formula <-
    count ~ f(id, initial = -10, fixed = T) + f(alt.id, weight, fixed = T, constr = T) +
    f(alt.id2, sex01, fixed = T, constr = T) + f(station.i, model = "iid")
  modwss2 <-
    inla(
      formula,
      family = "Poisson",
      data = data4inla0,
      control.predictor = list(compute = T),
      control.compute = list(dic = T, waic = T)
    )
  modwss2$dic$dic
  # modwss2$waic$waic
  modi3 <- modwss2
  
  # compute p1 and p2 based on conditional probability ratios
  # retrieve fitted values
  mod <- modi3
  # obtained fitted probabilities for different capture histories
  fitval <- mod$summary.fitted.values$mean
  
  # standardize fitted values and make them into data frame
  prob_df0 <- matrix(fitval, ncol = 3)
  # scale the transposed matrix to divide rows by the row mean and transpose it back
  prob_df <-
    data.frame(t(scale(
      t(prob_df0), center = FALSE, scale = rowSums(prob_df0)
    )))
  
  # solve equation system according to Mth
  alpha01 <- prob_df$X1 / prob_df$X3 #X1 is 01; X2 is 02; X3 is 11
  alpha10 <- prob_df$X2 / prob_df$X3
  prob_df$p1_inla <- 1 / (alpha01 + 1)
  prob_df$p2_inla <- 1 / (alpha10 + 1)
  prob_df$p00_inla <- (1 - prob_df$p2_inla) * (1 - prob_df$p1_inla)
  
  # Plots
  print(nspecies[s])       
  print(quantile(prob_df$p00_inla, c(.025, 0.5, .975)))
  
  # boxplot(prob_df[,c(4,5)], ylim=c(.2,.75))
  
  # lump probabilities with individuals
  
  totaldf <- data.frame(
    rawdata[, c(1:10)],
    p1.est = prob_df$p1_inla,
    p2.est = prob_df$p2_inla,
    p00.est = (prob_df$p00_inla)
  )
  inladf <- tibble(arrange(totaldf, trapseason, station, id))
  
  
  # partialdf <- data.frame(
  #   rawdata_noNA,
  #   p1.est = prob_df$p1_inla,
  #   p2.est = prob_df$p2_inla,
  #   p00.est = (1 - prob_df$p1_inla) * (1 - prob_df$p2_inla)
  # )
  
  # NAdf <- data.frame(
  #   rawdata_NA,
  #   p1.est = mean(partialdf$p1.est),
  #   p2.est = mean(partialdf$p2.est),
  #   p00.est = mean(partialdf$p00.est)
  # )
  
  # totaldf <- rbind(partialdf, NAdf)
  # totaldf$station <- factor(totaldf$station)
  # inladf <- arrange(totaldf, trapseason, station, id)
  
  # compute N using the HT estimator for each station and time point
  
  inlaframe <- partialN(inladf)
  
  rawts <-
    as.data.frame(xtabs(count ~ trapseason + station, data = rawdata))
  rawts2 <- arrange(rawts, station, trapseason)
  colnames(rawts2)[3] <- "counts"
  rawts2[, c(1, 3)] <-
    as.data.frame(apply(rawts2[, c(1, 3)], 2, FUN = as.numeric))
  
  trapdates <- distinct(data4inla0, trapseason, datetime, species)
  
  estimatedf3 <- left_join(inlaframe, rawts2)
  estimatedf4 <- left_join(estimatedf3, trapdates)
  
  specieslist[[s]] <- estimatedf4
  
}

specab <- bind_rows(specieslist)

saveRDS(specab, "data/capture_recapture/porsanger/porsanger_abundance_perspecies.rds")



### other models
# ------------- Model fitting -------------- #

# #### CR INLA MODEL ####
# ### MOD 0 ###
# # intercept model
# formula <- count ~ f(id, initial = -10, fixed=T)
# mod00 <- inla(formula, family = "Poisson", data = data4inla, control.predictor = list(compute=T),
#           control.compute = list(dic=T, waic=T))
# mod00$dic$dic
# mod00$waic$waic
# modi0 <- mod00
# 
# ### MOD 1 ###
# # weight
# formula <- count ~ f(id, initial = -10, fixed=T)+f(alt.id, weight, fixed = T, constr = T)
# modw <- inla(formula, family = "Poisson", data = data4inla, control.predictor = list(compute=T),
#               control.compute = list(dic=T,waic=T))
# modw$dic$dic
# modw$waic$waic
# modi1 <- modw0
# 
# ### MOD 2 ###
# # weight + sex
# formula <- count ~ f(id, initial = -10, fixed=T)+f(alt.id, weight, fixed = T, constr = T)+
#   f(alt.id2, sex01, fixed = T, constr = T)
# modws <- inla(formula, family = "Poisson", data = data4inla, control.predictor = list(compute=T),
#               control.compute = list(dic=T,waic=T))
# modws$dic$dic
# modws$waic$waic
# modi2 <- modws
# 
# data4inla$alt.id3 <- data4inla$alt.id2
