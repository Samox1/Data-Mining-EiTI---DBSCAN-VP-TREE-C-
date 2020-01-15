# Test

library(readr)
library(tidyverse)
library(dbscan)

###################################################################################
# Plot some data for C++ project

library(MASS)

S <- matrix(c(1,0,0,1),2,2)
mt1 <- c(0,0)
mt2 <- c(10,10)
mt3 <- c(12,12)
mt4 <- c(7, 11)
mt5 <- c(-5, 12)
mt6 <- c(-9, 12)

n1 <- 1000
n2 <- 1000
n3 <- 1000
n4 <- 1000
n5 <- 1000
n6 <- 1000
n <- n1 + n2 + n3 + n4 + n5 + n6

X1 <- mvrnorm(n1, mt1, S)
X2 <- mvrnorm(n2, mt2, S)
X3 <- mvrnorm(n3, mt3, S)
X4 <- mvrnorm(n4, mt4, S)
X5 <- mvrnorm(n5, mt5, S)
X6 <- mvrnorm(n6, mt6, S)


DATA_Cpp <- rbind(X1, X2, X3, X4, X5, X6)
kNN1 <- kNNdistplot(DATA_Cpp, k = 5)
abline(h=.1, col = "red", lty=2)
abline(h=.2, col = "red", lty=2)
abline(h=.3, col = "red", lty=2)
abline(h=.4, col = "red", lty=2)
abline(h=.5, col = "red", lty=2)

# Write to CSV
write.csv(DATA_Cpp, "DataCpp-2D-6000.csv")


plot(X1, ylim = c(-5,15), xlim = c(-10,15), pch = 19, col = "blue", xlab = "X", ylab = "Y", font = 2, asp = 1)
abline(v = 0, h = 0, col = "gray")
points(X2, pch = 19, col = "orange")
points(X3, pch = 19, col = "orange")
points(X4, pch = 19, col = "orange")
points(X5, pch = 19, col = "red")
points(X6, pch = 19, col = "red")

### Visualization
Data_Cluster <- as.data.frame(read.table("Clustered_1.csv", header=FALSE,sep=","))
Data_Cluster <- as.data.frame(read.table("Clustered_Test.csv", header=FALSE,sep=","))

Data_Cluster <- as.data.frame(read.table("Clustered-2D-2000-Eps-0-3_N-5_Rand-0-2.csv", header=FALSE,sep=","))
Data_Cluster <- as.data.frame(read.table("Data_Clustered_S.csv", header=FALSE,sep=","))
Data_Cluster <- as.data.frame(read.table("DataCpp.csv", header=FALSE,sep=","))
#Data_Cluster <- as.data.frame(read.table("D:/Programming/Data-Mining-EiTI---DBSCAN-VP-TREE-C-/Data_Cluster.csv", header=FALSE,sep=","))

cluster <- as.factor(Data_Cluster[,3])
cluster2 <- as.factor(Data_Cluster[,4])
cluster3 <- as.factor(Data_Cluster[,5])

for (x in c(1:length(Data_Cluster[,3]))) {
  if(Data_Cluster[x,3] != Data_Cluster[x,4] || Data_Cluster[x,4] != Data_Cluster[x,5] || Data_Cluster[x,3] != Data_Cluster[x,5]){
    cat(Data_Cluster[x,3], " | ", Data_Cluster[x,4], " | ", Data_Cluster[x,5], "\n")
  }
}


library(ggplot2)
library(ggforce)
#sp <- ggplot(Data_Cluster[,1:2], aes(x=V1, y=V2, colour = cluster, size = cluster)) + geom_point() + geom_point(size = 50, pch = 1) + coord_cartesian(xlim =c(-1, 8), ylim = c(-1, 8))

sp <- ggplot(Data_Cluster[,1:2], aes(x=V1, y=V2, colour = cluster3)) + geom_point() + scale_x_continuous() + scale_y_continuous() #+ theme(aspect.ratio = 1)
sp <- sp + coord_fixed(ratio = 1)
sp <- sp + geom_point(size=10, pch=1) #+ coord_cartesian(xlim = c(0,4), ylim = c(0,4))
#sp <- sp + geom_point(aes(x=Data_Cluster[63,1], y=Data_Cluster[63,2], colour = 'black'))
show(sp)

P_tab = c(0,2,10,15,16,18,19,22,23,24,30,34,45,59,61,63,64,79,86,94,97,100,102,107)
for (zz in 1:length(P_tab)){
  kappa = P_tab[zz]
  sp <- sp + geom_point(aes(x=Data_Cluster[P_tab[zz],1], y=Data_Cluster[P_tab[zz],2], colour = 'yellow'))
  sp <- sp + geom_point(size=10, pch=1)
}


x <- c(0,1,5)
y <- c(0,1,3)
kappa <- as.data.frame(x)
kappa <- cbind(kappa,y)
sp1 <- ggplot(kappa, aes(x=x, y=y)) + geom_point() + coord_fixed(ratio = 1) + geom_circle(aes(x0=x, y0=y, r= 10), data= kappa)
show(sp1)

  # cols <- c((-1)='red',1='blue',2='green',3='yellow');
# plot(Data_Cluster[,1:2], xlim=c(0,8), ylim=c(0,8),col=cols[Data_Cluster[3]])
#show(3^10)


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


#############################################################################################################################################################
### TRZEBA w C++ :
### 1) wyswietlic przypisywanie klastrow punktow dla DataCpp z 120 pkt
### 2) wyswietlic cale drzewo zeby zobaczyc listy + ewentualnie stworzyc mape drzewa w R (zarys okregow - mediany)


library(tidyverse)
library(MASS)
library(ggplot2)
library(ggforce)
library(dbscan)

### --- Funkcja z biblioteki DBSCAN --- ###
Data <- as.data.frame(read.table("Data_Clustered_4000_0-1-v2.csv", header=FALSE,sep=","))
dbsca <- dbscan(Data[,1:2], 0.3, 6, borderPoints = TRUE)
colur <- as.factor(dbsca[["cluster"]])

jpeg("R-DBSCAN.jpg", width = 2000, height = 2000, quality = 100)
sp <- ggplot(Data[,1:2], aes(x=Data[,1], y=Data[,2], colour = colur)) + geom_point() + scale_x_continuous() + scale_y_continuous() #+ theme(aspect.ratio = 1)
sp <- sp + coord_fixed(ratio = 1)
sp <- sp + geom_point(size=10, pch=1) #+ coord_cartesian(xlim = c(0,4), ylim = c(0,4))
#sp <- sp + geom_point(aes(x=Data_Cluster[63,1], y=Data_Cluster[63,2], colour = 'black'))
show(sp)
dev.off()


### --- Dane z C++ --- ###
# Data_Cluster <- as.data.frame(read.table("Data_Clustered_4000.csv", header=FALSE,sep=","))
Data_Cluster <- Data
cluster <- as.factor(Data_Cluster[,3])

jpeg("Cpp-DBSCAN.jpg", width = 2000, height = 2000, quality = 100)
sp <- ggplot(Data_Cluster[,1:2], aes(x=V1, y=V2, colour = cluster)) + geom_point() + scale_x_continuous() + scale_y_continuous() #+ theme(aspect.ratio = 1)
sp <- sp + coord_fixed(ratio = 1)
sp <- sp + geom_point(size=10, pch=1) #+ coord_cartesian(xlim = c(0,4), ylim = c(0,4))
#sp <- sp + geom_point(aes(x=Data_Cluster[63,1], y=Data_Cluster[63,2], colour = 'black'))
show(sp)
dev.off()


### --- Dane z C++ --> VP-TREE v1 --- ###
# Data_Cluster <- as.data.frame(read.table("Data_Clustered_20000.csv", header=FALSE,sep=","))
cluster <- as.factor(Data_Cluster[,4])

jpeg("Cpp-DBSCAN-VP-v1.jpg", width = 2000, height = 2000, quality = 100)
sp <- ggplot(Data_Cluster[,1:2], aes(x=V1, y=V2, colour = cluster)) + geom_point() + scale_x_continuous() + scale_y_continuous() #+ theme(aspect.ratio = 1)
sp <- sp + coord_fixed(ratio = 1)
sp <- sp + geom_point(size=10, pch=1) #+ coord_cartesian(xlim = c(0,4), ylim = c(0,4))
#sp <- sp + geom_point(aes(x=Data_Cluster[63,1], y=Data_Cluster[63,2], colour = 'black'))
show(sp)
dev.off()


### --- Dane z C++ --> VP-TREE v2 --- ###
# Data_Cluster <- as.data.frame(read.table("Data_Clustered_20000.csv", header=FALSE,sep=","))
cluster <- as.factor(Data_Cluster[,5])

jpeg("Cpp-DBSCAN-VP-v2.jpg", width = 2000, height = 2000, quality = 100)
sp <- ggplot(Data_Cluster[,1:2], aes(x=V1, y=V2, colour = cluster)) + geom_point() + scale_x_continuous() + scale_y_continuous() #+ theme(aspect.ratio = 1)
sp <- sp + coord_fixed(ratio = 1)
sp <- sp + geom_point(size=10, pch=1) #+ coord_cartesian(xlim = c(0,4), ylim = c(0,4))
#sp <- sp + geom_point(aes(x=Data_Cluster[63,1], y=Data_Cluster[63,2], colour = 'black'))
show(sp)
dev.off()

table(colur)
table(Data_Cluster[,3])
table(Data_Cluster[,4])
table(Data_Cluster[,5])


Data[Data[,4] == 5,]


##############################################################################################
### Badanie Drzewa z C++

Tree1 <- as.data.frame(read.table("Tree1.csv", header=TRUE ,sep=","))
Tree2 <- as.data.frame(read.table("Tree2.csv", header=TRUE ,sep=","))






