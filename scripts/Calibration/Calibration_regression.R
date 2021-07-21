ABDF <- readRDS("data/cameratrap/porsanger/processed/regression_data_porsanger.rds")

GRDF <- readRDS("data/cameratrap/porsanger/processed/GR_regression_data_porsanger.rds")


### REGRESSION ####
library(dplyr)

# for trap season 1, we have missing values for all the photos taken before the trapping
# for the GR dataset, trapseason 2 correponds to season 2 - season 1, thus it needs to be removed
filtdataab <- filter(ABDF, species=="GRAASIDEMUS" & trapseason > 1)
filtdatagr <- filter(GRDF, species=="GRAASIDEMUS" & trapseason > 2)


modcoefs <- matrix(NA,ncol=3,nrow=13)
for(i in 1:13)
{
  newAB <- filtdataab[,c(1:5,5+i)]
  newGR <- filtdatagr[,c(1:5,5+i)]
  
  modcoefs[i,1] <- colnames(newAB)[6]
  
  colnames(newAB)[6] = colnames(newGR)[6] = "var"
  
  modAB <- lm(counts~var, data=newAB)
  modGR <- lm(counts~var, data=newGR)
  
  modcoefs[i,2] <- summary(modAB)$r.squared
  modcoefs[i,3] <- summary(modGR)$r.squared
  
  
}


filtdataab <- filter(ABDF, species=="ROEDMUS" & trapseason > 1)
filtdatagr <- filter(GRDF, species=="ROEDMUS" & trapseason > 2)

i=1
modcoefs_RM <- matrix(NA,ncol=3,nrow=13)
for(i in 1:13)
{
  newAB <- filtdataab[,c(1:5,5+i)]
  newGR <- filtdatagr[,c(1:5,5+i)]
  
  modcoefs_RM[i,1] <- colnames(newAB)[6]
  
  colnames(newAB)[6] = colnames(newGR)[6] = "var"
  
  modAB <- lm(counts~var, data=newAB)
  modGR <- lm(counts~var, data=newGR)
  
  modcoefs_RM[i,2] <- summary(modAB)$r.squared
  modcoefs_RM[i,3] <- summary(modGR)$r.squared
  
  
}

