ABDF <- readRDS("data/cameratrap/porsanger/processed/regression_data_porsanger.rds")

GRDF <- readRDS("data/cameratrap/porsanger/processed/GR_regression_data_porsanger.rds")


### REGRESSION ####
filtdataab <- filter(ABDF, species=="GRAASIDEMUS")
filtdatagr <- filter(GRDF, species=="GRAASIDEMUS")

i=1
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


filtdataab <- filter(jointset4, species=="ROEDMUS" & trapseason > 1)
filtdatagr <- filter(grdf2, species=="ROEDMUS" & trapseason > 2)

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

