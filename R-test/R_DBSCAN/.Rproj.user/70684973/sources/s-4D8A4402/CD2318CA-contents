# Test

library(readr)
library(tidyverse)
library(dbscan)

# 
# forestfires <- read_csv("forestfires.csv")
# View(forestfires)
# 
# 
# #forestfires$ISI <- forestfires$ISI[which.max(forestfires$ISI < 30.0)]
# plot(forestfires$ISI, forestfires$temp, xlim=c(0,26))

# Live <- read_csv("Live.csv")
# View(Live)
# plot(Live$num_comments, Live$num_likes)


# ### Cambridge data
# Cambridge_Crime_Data_2009_2016 <- read_csv("Cambridge Crime Data 2009-2016.csv")
# #View(Cambridge_Crime_Data_2009_2016)
# 
# Cambridge_Crime_Data_2009_2016$Crime <- as.numeric(factor(Cambridge_Crime_Data_2009_2016$Crime))
# Cambridge_Crime_Data_2009_2016$`Crime Date Time` <- as.numeric(factor(Cambridge_Crime_Data_2009_2016$`Crime Date Time`))
# plot(Cambridge_Crime_Data_2009_2016$Crime, Cambridge_Crime_Data_2009_2016$`Crime Date Time`)
# 

# ## Example 1: use dbscan on the iris data set
# data(iris)
# iris <- as.matrix(iris[,1:4])
# ## find suitable eps parameter using a k-NN plot for k = dim + 1
# ## Look for the knee!
# kNNdistplot(iris, k = 5)
# abline(h=.5, col = "red", lty=2)
# res <- dbscan(iris, eps = .5, minPts = 5)
# show(res)
# 
# plot(iris[,1], iris[,3])


###################################################################################
# Plot some data for C++ project

# library(MASS)
# 
# S <- matrix(c(1,0,0,1),2,2)
# mt1 <- c(2,2)
# mt2 <- c(6,0)
# n1 <- 60
# n2 <- 60
# n <- n1 + n2
# 
# X1 <- mvrnorm(n1, mt1, S)
# X2 <- mvrnorm(n2, mt2, S)
# 
# DATA_Cpp <- rbind(X1, X2)
# kNN1 <- kNNdistplot(DATA_Cpp, k = 5)
# abline(h=.5, col = "red", lty=2)
# 
# # Write to CSV
# write.csv(DATA_Cpp, "DataCpp.csv")
# 
# 
# plot(X1, ylim = c(-5,5), xlim = c(-2,10), pch = 19, col = "blue", xlab = "X", ylab = "Y", font = 2, asp = 1)
# abline(v = 0, h = 0, col = "gray")
# points(X2, pch = 19, col = "orange")

library(readr)
# DataCpp <- read_csv("D:/Programming/Data-Mining-EiTI---DBSCAN-VP-TREE-C-/DataCpp.csv")
# #View(DataCpp)
# 
# plot(DataCpp[2:3], xlim=c(0,8), ylim=c(0,8))
# #symbols(x=DataCpp[10,2], y=DataCpp[10,3], circles=rep(0.5), add=T, inches=F)
# XX <- DataCpp[,2]
# YY <- DataCpp[,3]
# ZZ <- c(10,20,25,48,57,7,10,35,42,19,23,24,50,49,43,39,45,4,47,52,18,15,36,6,3)
# 
# for (val in ZZ){
#   plot(DataCpp[2:3], xlim=c(0,8), ylim=c(0,8))
#   symbols(x=DataCpp[val,2], y=DataCpp[val,3], circles=rep(0.5), add=T, inches=F)
# }
# 
# plot(DataCpp[2:3], xlim=c(0,8), ylim=c(0,8))
# for (val in ZZ){
#   symbols(x=DataCpp[val,2], y=DataCpp[val,3], circles=rep(0.5), add=T, inches=F)
# }


Data_Cluster <- as.data.frame(read.table("~/GitHub/DBSCAN_VP-TREE_Cpp/Data_Cluster.csv", header=FALSE,sep=","))
#Data_Cluster <- as.data.frame(read.table("D:/Programming/Data-Mining-EiTI---DBSCAN-VP-TREE-C-/Data_Cluster.csv", header=FALSE,sep=","))

cluster <- as.factor(Data_Cluster[,3])

library(ggplot2)
#sp <- ggplot(Data_Cluster[,1:2], aes(x=V1, y=V2, colour = cluster, size = cluster)) + geom_point() + geom_point(size = 50, pch = 1) + coord_cartesian(xlim =c(-1, 8), ylim = c(-1, 8))
sp <- ggplot(Data_Cluster[,1:2], aes(x=V1, y=V2, colour = cluster, size = cluster)) + geom_point() + geom_point(size = 50, pch = 1)
show(sp)

# cols <- c((-1)='red',1='blue',2='green',3='yellow');
# plot(Data_Cluster[,1:2], xlim=c(0,8), ylim=c(0,8),col=cols[Data_Cluster[3]])
