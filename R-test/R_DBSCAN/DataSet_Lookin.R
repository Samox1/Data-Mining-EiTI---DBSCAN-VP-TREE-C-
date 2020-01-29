# Looking into some DataSets for Project

library(readr)
library(tidyverse)
library(dbscan)
library(MASS)
library(ggplot2)
library(ggforce)
library(igraph)
library(cowplot)


gg_circle <- function(r, xc, yc, color="black", fill=NA, ...) {
  x <- xc + r*cos(seq(0, pi, length.out=100))
  ymax <- yc + r*sin(seq(0, pi, length.out=100))
  ymin <- yc + r*sin(seq(0, -pi, length.out=100))
  annotate("ribbon", x=x, ymin=ymin, ymax=ymax, color=color, fill=fill, ...)
}

Cole <- c("red", "green", "blue", "violet", "gold")
Radi <- 0.5
cluster <- as.factor(Data120[,5])
square <- ggplot(Data120[,1:2], aes(x=V1, y=V2, colour= cluster)) + geom_point()
for(x in 1:length(Data120[,1])){
  # square <- square + gg_circle(r=Tree1$Mediana[Tree1$VP.Index==x], xc=Data120[x,1], yc=Data120[x,2], color=, alpha=0.2)
  square <- square + gg_circle(r=Radi, xc=Data120[x,1], yc=Data120[x,2], color=Cole[cluster[x]], alpha=0.2)
}
show(square)


### ---- HELLO --- Lookin into Data Sets before runing in C++ project --- ###


### Europe: N = 169673 (Polylines) = wspolrzedne geograficzne

Europe <- as.data.frame(read.table("europe.txt", header=FALSE ,sep=","))
plot(Europe[,1:2])

MINST <- as.data.frame(read.table("MINST_Hand_Digits.txt", header=FALSE ,sep=","))

Bio <- as.data.frame(read.table("KDDCUP04Bio.txt", header=FALSE ,sep=","))
kNNdistplot(Bio)
abline(h=1000, col = "red", lty=2)

Cities <- as.data.frame(read.table("worldcities.csv", header=TRUE ,sep=","))
plot(x=Cities$lng, y=Cities$lat)

S1 <- as.data.frame(read.table("S1.txt", header=FALSE ,sep=","))
S1 <- S1[,2:3]
plot(S1)
kNNdistplot(S1)
abline(h=10000, col = "red", lty=2)
write.csv(S1, "S1e.csv")
