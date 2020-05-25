## SIMULATION SET FUNCTIONS ##

###### FUNCTIONS ####

# Standardize inla fitted set
stdize <- function(matrix){
  #set counter
  fit2 <- matrix
  j=1
  for(i in seq(1,length(fit2),3)){ #matrix needs to be made of triplets
    seqn <- i:(i+2)
    total <- sum(fit2[seqn])
    
    for(j in seqn) fit2[j]<- fit2[j]/total
    j=j+3
  }
  return(fit2)
}

# Transform triplet sequence into data frame with 3 columns
makedf <- function(df){
  for(i in seq(1,length(df),3))
  {
    if (i == 1) {A <- c(df[i]); B <- c(df[i+1]); C <- c(df[i+2])}
    else {A <- c(A, df[i]); B <- c(B,df[i+1]); C <- c(C,df[i+2])}
  }
  dataf <- data.frame("0,1"=A,"1,0"=B,"1,1"=C)
  return(dataf)
}

# Horvitz-Thompson Estimator
HT <- function(x) sum(1/(1-x)) #x is the probability of 0,0

#Obtain Partial N sequence using the HT Estimator
# partialN <- function(p00.st.tp){
#   df <- p00.st.tp
#   colnames(df) <- c("p00","st","tp")
#   N <- c()
#   
#   sts <- sort(unique(df$st))
# 
#   for(s in 1:length(sts)){
#     print(sts[s])
#     df_st <- filter(df, st == sts[s])
#     
#     for (t in sort(unique(df_st$tp)))
#     {
#       df_tp <- filter(df_st, tp == t)
#       nt <- HT(df_tp$p00)
#       
#       N <- c(N,nt)
#       
#     }
#   }
#   
#   return(N)
# }

partialN <- function(p00.st.tp){
  df <- p00.st.tp
  colnames(df) <- c("p00","st","tp")
  
  newdata <- distinct(p00.st.tp[,2:3]) ; colnames(newdata) <- c("station","timepoint")
  newdata$N.est <- NA

  sts <- sort(unique(df$st))
  c <- 1
  for(s in 1:length(sts)){
    print(sts[s])
    df_st <- filter(df, st == sts[s])
  
    for (timep in sort(unique(df_st$tp)))
    {
      df_tp <- filter(df_st, tp == timep)
      nt <- HT(df_tp$p00)
      
      newdata$N.est[newdata$station==sts[s]&newdata$timepoint==timep] <- nt
    }
  }
  
  return(newdata)
}
