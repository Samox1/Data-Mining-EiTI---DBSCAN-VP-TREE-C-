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


Cambridge_Crime_Data_2009_2016 <- read_csv("Cambridge Crime Data 2009-2016.csv")
#View(Cambridge_Crime_Data_2009_2016)

Cambridge_Crime_Data_2009_2016$Crime <- as.numeric(factor(Cambridge_Crime_Data_2009_2016$Crime))
Cambridge_Crime_Data_2009_2016$`Crime Date Time` <- as.numeric(factor(Cambridge_Crime_Data_2009_2016$`Crime Date Time`))
plot(Cambridge_Crime_Data_2009_2016$Crime, Cambridge_Crime_Data_2009_2016$`Crime Date Time`)


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
