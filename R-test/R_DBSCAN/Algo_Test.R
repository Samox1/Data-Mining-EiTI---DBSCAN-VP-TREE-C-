### Test VP-Tree

rm(list = ls())         # Clear Environment
gc()                    # Free up memrory and report the memory usage.


library(tidyverse)
library(MASS)
library(ggplot2)
library(ggforce)


### Functions ###########################################################################

euc.dist <- function(x1, x2){
  sqrt(sum((x1 - x2) ^ 2))
}

dist_calc <- function(p, D){
  dist_all <- NULL
  for(i in 1:nrow(D)) dist_all[i] <- euc.dist(p, D[i,])
  #cat("Dist_all = " ,dist_all, "\n")
  
  return(dist_all)
}

Select_VP <- function(S){
  P <- S[sample(nrow(S), 10, replace = F), ]
  best_spread <- 0
  best_p <- 0
  
  for (px in c(1:nrow(P))) {
    #cat("Kappa px: ",px, "\n")
    p = as.data.frame(P[px,])
    #show(p)
    p_id <- as.integer(rownames(p))
    #cat(typeof(p_id))
    P_temp <- P[as.integer(rownames(P)) != p_id,]
    #show(P_temp)
    D <- P_temp[sample(nrow(P_temp), 3, replace = F), ]
    
    dist_all <- dist_calc(p,D)
    
    mu <- median(dist_all)
    #cat("MU = ", mu, "\n")
    
    spread_temp <- var(dist_all - mu)
      
    if (spread_temp > best_spread){
      best_spread <- spread_temp
      best_p <- p
    }
    
  }
  return(best_p)
}


### Main ###############################################################################

Dane <- as.data.frame(read.table("DataCpp-2D-1000.csv", header=TRUE,sep=","))
Dane <- Dane[1:nrow(Dane),2:ncol(Dane)]
colnames(Dane) <- c("x", "y")

node.p <- 0
node.p <- Select_VP(Dane)
Dane_temp <- Dane[as.integer(rownames(Dane)) != as.integer(rownames(node.p)),]
node.mu <- median(dist_calc(node.p, Dane_temp))

for(sx in c(1:nrow(Dane_temp))){
  if(euc.dist(node.p, Dane_temp[sx,])){
    
  }
}




