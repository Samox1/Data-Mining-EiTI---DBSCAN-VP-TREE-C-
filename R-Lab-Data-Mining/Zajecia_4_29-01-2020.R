#script SD dataminig lab4 2019Z

library(dbscan)
library(fpc)
library(cluster)
library(factoextra)

##### kmeans ##################
set.seed(7777)

data("iris")
summary(iris)
iris2 = iris[,-5]
iris2


?kmeans
iris2.kmeans = kmeans(iris2,3, iter.max = 20, nstart=20)

#getting information about clustering
print(iris2.kmeans)
print(iris2.kmeans$iter)
print(iris2.kmeans$centers)

#compare clusters with original class labels
table(iris$Species,iris2.kmeans$cluster)

#plot clusters
plot(iris2[,1:2], col = iris2.kmeans$cluster)
#add cluster centers
points(iris2.kmeans$centers[,1:2], col = 1:3, pch = 8, cex=2)


# Quality of the clustering            

km<-kmeans(iris2,3)

#alternative execution of kmeans
km_alt<-eclust(iris2, "kmeans", k=3, graph=FALSE)
fviz_silhouette(km_alt, palette="jco")

# From the output, avg.silwidth represents the average silhouette width. A silhouette is a measurement
# that considers how closely related objects are within the cluster and how clusters are separated
# from each other. The silhouette value usually ranges from 0 to 1; a value closer to 1 suggests
# the data is better clustered.

silinfo<-km_alt$silinfo
names(silinfo)

#silhouette length for each observation
head(silinfo$widths[,1:3],10)
#silhouette length for each cluster
silinfo$clus.avg.widths
#average silhouette length
silinfo$avg.width

# Rand index
# The corrected Rand index provides a measure for assessing the similarity between
# two partitions, adjusted for chance. Its range is -1 (no agreement) to 1 (perfect agreement).
species <- as.numeric(iris$Species)
clust_stats<-cluster.stats(d=dist(iris2), species, km_alt$cluster)
clust_stats$corrected.rand


#### data scaling
?scale
irisScale <- scale(iris2, center = FALSE)
str(irisScale)
iris2.kmeansS = kmeans(irisScale,3, iter.max = 20)
str(iris2.kmeansS)
table(iris$Species,iris2.kmeansS$cluster)

plot(irisScale[,3:4], col = iris2.kmeansS$cluster)
#add cluster centers
points(iris2.kmeansS$centers[,3:4], col = 1:3, pch = 8, cex=2)

###############################################################
# finding the optimal number of groups with the "elbow" method
###############################################################

wss <- vector(mode = "integer" ,length = 15)

#  1 to 15 clusters
for (i in 1:15) {
  kmeans.group <- kmeans(irisScale, centers = i, nstart=20)
  # total within-cluster sum of squares
  wss[i] <- kmeans.group$tot.withinss
}

# total within-cluster sum of squares per number of groups
plot(1:15, wss, type = "b", 
     xlab = "number of groups", 
     ylab = "total within-cluster sum of squares")


####################################
# PAM - partitioning around medoids#
####################################

# deciding on the optimal number of clusters
fviz_nbclust(iris2, pam, method = "silhouette")+theme_classic()

# division into 2 clusters
pam.res <- pam(iris2, 2)

# clustering results together with information on objects being cluster centers
print(pam.res)

#adding information on cluster assignment
iris_clus<-cbind(iris2, pam.res$cluster)
head(iris_clus)

#cluster centers
print(pam.res$medoids)

#cluster assignment
pam.res$clustering

#clustering visualization
fviz_cluster(pam.res,
             palette = c("#00AFBB", "#FC4E07"), # color palette
             ellipse.type = "t", # ellipse of concentration
             repel = TRUE, # avoid overlapping (slows down)
             ggtheme = theme_light() #background color
)


#### hierarchical clustering
#sample data
idx <- sample(1:nrow(iris), 50)
irisSample <- iris[idx,1:4]

?hclust
#calculation of a distance matrix
?dist
distM = dist(irisSample)
distT = as.matrix(distM)

dim(distT)
distT[1:5,1:5]

#hierarchical clustering for different linkage methods
iris2.hc_complete <- hclust(distM, method="complete")
iris2.hc_single <- hclust(distM, method="single")
iris2.hc <- hclust(distM, method="average")
iris2.hc_centroid <- hclust(distM, method="centroid")

#dendrograms for different clustering
?plot

par(mfrow=c(2,2))
plot(iris2.hc, hang = -1, labels=iris$Species[idx])
rect.hclust(iris2.hc, k=3, border=2:4)

plot(iris2.hc_complete, hang = -1, labels=iris$Species[idx])
rect.hclust(iris2.hc_complete, k=3, border=2:4)

plot(iris2.hc_single, hang = -1, labels=iris$Species[idx])
rect.hclust(iris2.hc_single, k=3, border=2:4)

plot(iris2.hc_centroid, hang = -1, labels=iris$Species[idx])
rect.hclust(iris2.hc_centroid, k=3, border=2:4)
par(mfrow=c(1,1))

#generates clusters
?cutree
iris2.hc.groups <- cutree(iris2.hc, k=3)
iris2.hc.groups

#compare clusters with original class labels
table(iris$Species[idx], iris2.hc.groups)

##### dbscan algorithm ##########################

?dbscan

# MinPts parameter estimation
# The idea is to calculate the average of the distances of every point to its k nearest
# neighbors. The value of k will be specified by the user and corresponds to MinPts.
# Next, these k-distances are plotted in an ascending order. The aim is to determine
# the âkneeâ, which corresponds to the optimal eps parameter.
# A knee corresponds to a threshold where a sharp change occurs along the k-distance
# curve.

dbscan::kNNdistplot(iris2, k=5)
abline(h=0.5, lty="dashed")

# dbscan alg. execution

iris2.dbscan <- dbscan(iris2, eps=0.5, MinPts=5)

#compare clusters with original class labels
#cluster 0 means noise
table(iris$Species, iris2.dbscan$cluster)

iris2.dbscan <- dbscan(iris2, eps=0.4, MinPts=5)
table(iris$Species, iris2.dbscan$cluster)

# plot clusters
plot(iris2.dbscan, iris2)
plot(iris2.dbscan, iris2[c(1,4)])


#algorithms comparison

km<-kmeans(iris2,3)
single_iris<-hclust(dist(iris2),method="single")
hc_single<-cutree(single_iris, k=3)

complete_iris<-hclust(dist(iris2),method="complete")
hc_complete<-cutree(complete_iris, k=3)

#cluster validation statistics from the fpc package
cs<-cluster.stats(dist(iris2), km$cluster)
cs[c("within.cluster.ss","avg.silwidth")]

#  the within.cluster.ss measurement stands for the within clusters sum of
# squares, and avg.silwidth represents the average silhouette width. The within.cluster.ss
# measurement shows how closely related objects are in clusters; the smaller the value, the more
# closely related objects are within the cluster. On the other hand, a silhouette is a measurement
# that considers how closely related objects are within the cluster and how clusters are separated
# from each other. The silhouette value usually ranges from 0 to 1; a value closer to 1 suggests
# the data is better clustered.

sapply(list(kmeans<-km$cluster, hc_single<-hc_single, hc_complete<-hc_complete), 
       function(c) cluster.stats(dist(iris2),c)[c("within.cluster.ss","avg.silwidth")])

###########################################################
# Case study - grupowanie danych z sieci społecznosciowej #
###########################################################

download.file('http://staff.ii.pw.edu.pl/~rbembeni/dane/social_network_data.csv','social_network_data.csv')
teens <- read.csv("social_network_data.csv")

## data preprocessing
####################################

str(teens)
View(teens)
summary(teens)

# missing values checking
table(teens$gender)
table(teens$gender, useNA = "ifany")



# statistics of age attribute
summary(teens$age)

teens$age <- ifelse(teens$age >= 13 & teens$age < 20, teens$age, NA)
summary(teens$age)

# cleaning values of gender attributes

teens$female <- ifelse(teens$gender == "F" & !is.na(teens$gender), 1, 0)
teens$no_gender <- ifelse(is.na(teens$gender), 1, 0)


table(teens$gender, useNA = "ifany")
table(teens$female, useNA = "ifany")
table(teens$no_gender, useNA = "ifany")

# average age
mean(teens$age, na.rm = TRUE) # dziala

# average age in groups
?aggregate
aggregate(data = teens, age ~ gradyear, mean, na.rm = TRUE)

# 
?ave
ave_age <- ave(teens$age, teens$gradyear,
               FUN = function(x) mean(x, na.rm = TRUE))

teens$age <- ifelse(is.na(teens$age), ave_age, teens$age)

summary(teens$age)

## clustering
#############################################

interests <- teens[5:40]
interests_z <- as.data.frame(lapply(interests, scale))

teen_clusters <- kmeans(interests_z, 5)

## evaluaton of clustering

teen_clusters$size
teen_clusters$centers

## Analysis of clustering
#################

teens$cluster <- teen_clusters$cluster
View(teens)

teens[1:5, c("cluster", "gender", "age", "friends")]
?aggregate

# statistics for clusters
aggregate(data = teens, age ~ cluster, mean)
aggregate(data = teens, female ~ cluster, mean)
aggregate(data = teens, friends ~ cluster, mean)

library("data.table")
?data.table
dt <- data.table(teens)
dt[,list(meanA=mean(age),meanF=mean(friends)),by=cluster]



########## Laboratory task ###################
#calculation of accuracy
accuracyCalc <- function(confTbl, startCol)
{
  corr = 0;
  for(i in startCol:ncol(confTbl))
  {
    corr = corr + max(confTbl[,i])  
  }
  accuracy = corr/sum(confTbl)
  accuracy  
}

#data set for the laboratory task
#http://archive.ics.uci.edu/ml/datasets/Cardiotocography 

download.file('http://staff.ii.pw.edu.pl/~gprotazi/dydaktyka/dane/cardioto_noClass_corr.csv','cardioto_noClass_corr.csv')
ctg_noClass <- read.csv("cardioto_noClass_corr.csv",row.names = 1)

download.file('http://staff.ii.pw.edu.pl/~gprotazi/dydaktyka/dane/cardioto_all_corr.csv','cardioto_all_corr.csv')
ctg_all <- read.csv("cardioto_all_corr.csv",row.names = 1)

#example
distC = dist(ctg_noClass)
card.kmeans = kmeans(distC,10)
reskmeans = table(ctg_all$CLASS,card.kmeans$cluster )
reskmeans
accuracyCalc(reskmeans,1)


kmeansss <- matrix()
for (xx in c(1:15)) {
  distC = dist(ctg_noClass)
  card.kmeans = kmeans(distC,xx)
  res3 = table(ctg_all$CLASS,card.kmeans$cluster)
  kmeansss[xx] <- accuracyCalc(res3,1)
}
kmeansss[10]

pamsss <- matrix()
for (xx in c(1:15)) {
  card.pam = pam(ctg_noClass, xx)
  res4 = table(ctg_all$CLASS,card.pam$cluster )
  pamsss[xx] <- accuracyCalc(res4,1)
}
pamsss[10]

epsilon <- seq(0.5,1,by=0.1)
minN <- c(5:10)
dbscansss <- matrix(0, nrow = length(epsilon), ncol = length(minN))
for (xx in c(1:length(epsilon))) {
  for (yy in c(1:length(minN))) {
    xx
    yy
    epsilon[xx]
    minN[yy]
    card.dbscan = dbscan(ctg_noClass, eps=epsilon[xx], MinPts=minN[yy])
    res5 = table(ctg_all$CLASS,card.dbscan$cluster)
    dbscansss[xx,yy] <- accuracyCalc(res5,2)                          # ERROR
  }
}
dbscansss


#wines
download.file('http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv', 'wine_red.csv');
download.file('http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv', 'wine_white.csv');
wineRed_ds = read.table("wine_red.csv", header = TRUE, sep=";", na.strings= "*")
wineRed_dsC <- wineRed_ds[,-12]

#example - wines
distC = dist(wineRed_dsC)
card.kmeans = kmeans(distC,6)
res3 = table(wineRed_ds$quality,card.kmeans$cluster )
res3
accuracyCalc(res3,1)


