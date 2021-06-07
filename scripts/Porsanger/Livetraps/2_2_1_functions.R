## SIMULATION SET FUNCTIONS ##
library(dplyr)
###### FUNCTIONS ####

# Standardize inla fitted set
# stdize <- function(matrix, multiple=3){
#   #set counter
#   fit2 <- matrix
#   j=1
#   for(i in seq(1,length(fit2),multiple)){ #matrix needs to be made of triplets
#     seqn <- i:(i+2)
#     total <- sum(fit2[seqn])
#     
#     for(j in seqn) fit2[j]<- fit2[j]/total
#     j=j+multiple
#   }
#   return(fit2)
# }
# 
# # Transform triplet sequence into data frame with 3 columns
# makedf <- function(df){
#   for(i in seq(1,length(df),3))
#   {
#     if (i == 1) {A <- c(df[i]); B <- c(df[i+1]); C <- c(df[i+2])}
#     else {A <- c(A, df[i]); B <- c(B,df[i+1]); C <- c(C,df[i+2])}
#   }
#   dataf <- data.frame("0,1"=A,"1,0"=B,"1,1"=C)
#   return(dataf)
# }

# Horvitz-Thompson Estimator
HT <- function(x) sum(1/(1-x)) #x is the probability of 0,0

partialN_radius <- function(df, radius=TRUE){

   # use HT estimator on pre-defined traps
  newdata0 <- aggregate(p00.est~station+trapsession, data=df, FUN=HT)
  estimatedf0 <- as.data.frame(xtabs(p00.est~trapsession+station, data=newdata0), stringsAsFactors = FALSE)
  colnames(estimatedf0)[3] <- "N.est"

  if(radius==TRUE)
  {
    # use HT estimator on <=25 m animals 
  df0 <- filter(df, s25==1)
  newdata <- aggregate(p00.est~station+trapsession, data=df0, FUN=HT)
  newdata2 <- as.data.frame(xtabs(p00.est~trapsession+station, data=newdata), stringsAsFactors = FALSE)
  colnames(newdata2)[3] <- "N25"
  
   # use HT estimator on <=50 m animals 
  df0 <- filter(df, s50==1)
  newdata3 <- aggregate(p00.est~station+trapsession, data=df0, FUN=HT)
  newdata4 <- as.data.frame(xtabs(p00.est~trapsession+station, data=newdata3), stringsAsFactors = FALSE)
  colnames(newdata4)[3] <- "N50"
  
   # use HT estimator on <=100  m animals 
  
  df0 <- filter(df, s100==1)
  newdata5 <- aggregate(p00.est~station+trapsession, data=df0, FUN=HT)
  newdata6 <- as.data.frame(xtabs(p00.est~trapsession+station, data=newdata5), stringsAsFactors = FALSE)
  colnames(newdata6)[3] <- "N100"
  
  estimatedf01 <- left_join(newdata2,newdata4)
  estimatedf02 <- left_join(estimatedf01,newdata6)
  estimatedf03 <- left_join(estimatedf02,estimatedf0)
  
  }
  if(radius==FALSE) estimatedf03 <- estimatedf0
  estimatedf03[,] <- as.data.frame(apply(estimatedf03[,], 2, FUN=as.numeric))
  
  return(estimatedf03)
}

partialN <- function(df){
  
  # use HT estimator on pre-defined traps

  newdata0 <- aggregate(p00.est~station+trapseason, data=df, FUN=HT)
  estimatedf0 <- as.data.frame(xtabs(p00.est~trapseason+station, data=newdata0), stringsAsFactors = FALSE)
  colnames(estimatedf0)[3] <- "N.est"

  estimatedf0[,c(1,3)] <- as.data.frame(apply(estimatedf0[,c(1,3)], 2, FUN=as.numeric))

  return(tibble(estimatedf0))
}
