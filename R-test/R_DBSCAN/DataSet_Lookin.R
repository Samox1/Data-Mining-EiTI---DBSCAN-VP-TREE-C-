# Looking into some DataSets for Project

library(readr)
library(tidyverse)
library(dbscan)
library(MASS)
library(ggplot2)
library(ggforce)
library(igraph)
library(cowplot)
library(collapsibleTree)


gg_circle <- function(r, xc, yc, color="black", fill=NA, ...) {
  x <- xc + r*cos(seq(0, pi, length.out=100))
  ymax <- yc + r*sin(seq(0, pi, length.out=100))
  ymin <- yc + r*sin(seq(0, -pi, length.out=100))
  annotate("ribbon", x=x, ymin=ymin, ymax=ymax, color=color, fill=fill, ...)
}

Data120 <- as.data.frame(read.table("T48K_Out_3_10.0_20.txt", header=FALSE ,sep=","))
Data120 <- as.data.frame(read.table("Data_Out_2D_30.csv", header=FALSE ,sep=","))
Data120 <- as.data.frame(read.table("B2_Out_1_3_3.txt", header=FALSE ,sep=","))
Tree1 <- as.data.frame(read.table("Treee1_30.csv", header=TRUE ,sep=","))
Tree2 <- as.data.frame(read.table("Treee2_30.csv", header=TRUE ,sep=","))

Cole <- c("red", "green", "blue", "violet", "gold")
Radi <- 3
cluster <- as.factor(Data120[,3])
square <- ggplot(Data120[,1:2], aes(x=V1, y=V2, colour= cluster)) + scale_x_continuous(name = "X") + scale_y_continuous(name = "Y") + geom_point() + coord_fixed(ratio = 1)
square <- square + scale_x_continuous(name="X") + scale_y_continuous(name="Y")
for(x in 1:length(Data120[,1])){
  # square <- square + gg_circle(r=Tree1$Mediana[Tree1$VP.Index==x], xc=Data120[x,1], yc=Data120[x,2], color="black", alpha=0.2)      # Mediana
  square <- square + gg_circle(r=Radi, xc=Data120[x,1], yc=Data120[x,2], color=Cole[cluster[x]], alpha=0.2)                         # Epsilon
  # square <- square + gg_circle(r=Tree2$Left.Bound[Tree2$VP.Index==x], xc=Data120[x,1], yc=Data120[x,2], color="red", alpha=0.2)     # Left Bound
  # square <- square + gg_circle(r=Tree2$Right.Bound[Tree2$VP.Index==x], xc=Data120[x,1], yc=Data120[x,2], color="black", alpha=0.2)  # Right Bound
}
show(square)


### --- Grafy Drzewa
Tree2$ID_Parent[1] = NA
p <- collapsibleTree(Tree1, c("ID_Parent", "ID", c("L_kid", "R_kid")))
p <- collapsibleTreeNetwork(Tree2[,c(5,1)], c("ID_Parent", "ID"))
p
# save the widget
library(htmlwidgets)
saveWidget(p, file=paste0(getwd(),"dendrogram_interactive.html"))

wat <- warpbreaks

dat <- data.frame(parent=Tree1$ID_Parent, 
                  node=Tree1$ID, 
                  # text=as.character(Tree1$VP.Index))
                  text=as.character(Tree1$L_kid_N+Tree1$R_kid_N))
g <- graph.data.frame(dat)
lay = layout.reingold.tilford(g)
par(mar=rep(0,4), mfrow=c(1,2))
plot(g, layout=lay)

dat <- data.frame(parent=Tree2$ID_Parent, 
                  node=Tree2$ID, 
                  # text=as.character(Tree1$VP.Index))
                  text=as.character(Tree1$L_kid_N+Tree1$R_kid_N))
g <- graph.data.frame(dat)
lay = layout.reingold.tilford(g)
par(mar=rep(0,4), mfrow=c(1,2))
plot(g, layout=lay)

### ---- HELLO --- Lookin into Data Sets before runing in C++ project --- ###


### Europe: N = 169673 (Polylines) = wspolrzedne geograficzne

# Europe <- as.data.frame(read.table("europe.txt", header=FALSE ,sep=","))
# plot(Europe[,1:2])

# MINST <- as.data.frame(read.table("MINST_Hand_Digits.txt", header=FALSE ,sep=","))

Bio <- as.data.frame(read.table("KDDCUP04Bio.txt", header=FALSE ,sep=","))
kNNdistplot(Bio[,1:10])
plot(Bio[,c(1,3)])
abline(h=1000, col = "red", lty=2)

Cities <- as.data.frame(read.table("worldcities.csv", header=TRUE ,sep=","))
plot(x=Cities$lng, y=Cities$lat)


# S1 <- as.data.frame(read.table("S1.txt", header=FALSE ,sep=","))
# S1 <- S1[,2:3]
# plot(S1, xlim=c(3*10^5, 5*10^5))
# kNNdistplot(S1)
# abline(h=10000, col = "red", lty=2)
# write.csv(S1, "S1e.csv")
# 
# S1OUT <- as.data.frame(read.table("D:/Programming/Data-Mining-EiTI---DBSCAN-VP-TREE-C-/ZZ_S1-Obliczenia/S1_Out_1_20000_11.txt", header=FALSE ,sep=","))
# cluster <- as.factor(S1OUT[,3])
# square <- ggplot(S1OUT[,1:2], aes(x=V1, y=V2, colour= cluster)) + scale_x_continuous(name = "X") + scale_y_continuous(name = "Y") + geom_point() + coord_fixed(ratio = 1)
# show(square)


# T48K <- as.data.frame(read.table("t4.8k.txt", header=FALSE ,sep=","))
# plot(T48K, pch=20)
# kNNdistplot(T48K)
# abline(h=4, col = "red", lty=2)
# 
# T48K_VP2 <- as.data.frame(read.table("T48K_Out_3_10.0_20.txt", header=FALSE ,sep=","))
# cluster <- as.factor(T48K_VP2[,3])
# square <- ggplot(T48K_VP2[,1:2], aes(x=V1, y=V2, colour= cluster)) + scale_x_continuous(name = "X") + scale_y_continuous(name = "Y") + geom_point() + coord_fixed(ratio = 1)
# show(square)

Worms <- as.data.frame(read.table("worms_2d.txt", header=FALSE ,sep=","))
plot(Worms, pch=20)
plot(Worms, xlim=c(3000,3200), ylim=c(3200,3400), pch=20)
kNNdistplot(Worms, 35)
w1 <- kNNdist(Worms, 5)
hist(w1)
abline(h=10, col = "red", lty=2)

road_Worms <- dbscan(Worms, eps = 30, minPts = 25)
table(road_Worms$cluster)
hist(road_Worms$cluster)
road_Worms$cluster[road_Worms$cluster==0] = "red"
plot(Worms, pch=20, size=0.1)
square <- ggplot(Worms, aes(x=V1, y=V2, colour= road_Worms$cluster)) + scale_x_continuous(name = "X") + scale_y_continuous(name = "Y") + geom_point()
show(square)


B1 <- as.data.frame(read.table("birch1.txt", header=FALSE ,sep=","))
B1 <- B1[,2:3]
write.csv(B1, "B1Good.csv")
plot(B1, pch=20, xlim=c(350000, 360000), ylim=c(350000, 360000))


B2 <- as.data.frame(read.table("birch2.txt", header=FALSE ,sep=","))
B2 <- B2[,2:3]
write.csv(B2, "B2Good.csv")
plot(B2, pch=20,xlim=c(400000, 410000))

road <- dbscan(B2, eps = 750, minPts = 10)
square <- ggplot(B2, aes(x=V2, y=V3, colour= as.factor(road$cluster))) + scale_x_continuous(name = "X") + scale_y_continuous(name = "Y") + geom_point()
show(square)

B2OUT <- as.data.frame(read.table("B2_Out_1_750_10.txt", header=FALSE ,sep=","))
Cluster <- as.factor(B2OUT$V3)
square <- ggplot(B2OUT, aes(x=V1, y=V2, colour=Cluster)) + scale_x_continuous(name = "X") + scale_y_continuous(name = "Y") + geom_point() + theme(legend.position = "none") 
show(square)

# Spiral <- as.data.frame(read.table("spiral.txt", header=FALSE ,sep="\t"))
# Spiral <- Spiral[,1:2]
# write.csv(Spiral[,1:2], "Spiral.csv")
# plot(Spiral[,1:2], pch=20)
# road <- dbscan(Spiral[,1:2], eps = 3, minPts = 3)
# square <- ggplot(Spiral[,1:2], aes(x=V1, y=V2, colour= road$cluster)) + scale_x_continuous(name = "X") + scale_y_continuous(name = "Y") + geom_point() + coord_fixed(ratio = 1)
# show(square)
# 
# SpiralOUT <- Data120
# Cluster <- as.factor(SpiralOUT$V3)
# square <- ggplot(SpiralOUT, aes(x=V1, y=V2)) + scale_x_continuous(name = "X") + scale_y_continuous(name = "Y") + geom_point() + coord_fixed(ratio = 1)
# show(square)

