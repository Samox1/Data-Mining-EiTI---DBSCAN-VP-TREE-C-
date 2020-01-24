# Zajecia nr 3
# Data: 8.01.2020
# Clasification

# R server - komenda "iir2" odpala przegladarke i laczy sie z serwerem "r2d3" 
# Strona prowadzacego = http://staff.ii.pw.edu.pl/~gprotazi/index.html

rm(list = ls())

#script SD dataminig lab3 2019Z

#Temat: Klasyfikacja danych
#1. Classification with usage of "C5.0" library
#2. Classification with usage of  rpart library
#3. Random forest


#Task --> DeadLine --> 27.01.2020, godzina 8:00
#Defining the classification problem and building appropriate classifier for:
# credit approval, data and description available at: #http://archive.ics.uci.edu/ml/datasets/Credit+Approval
download.file("http://archive.ics.uci.edu/ml/machine-learning-databases/credit-screening/crx.data",'credit.csv');
credit_ds = read.table("credit.csv", header = TRUE, sep=",", na.strings= "*")
#wines data and description available at: http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/
download.file('http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv', 'wine_red.csv');
download.file('http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv', 'wine_white.csv');
wineRed_ds = read.table("wine_red.csv", header = TRUE, sep=";", na.strings= "*")


library(gmodels) #results analysis
library(Hmisc) #results analysis
library(caret)
library(rpart) # rpart() - decision tree classifier
library(rpart.plot) 
library(e1071)
library(C50) # C5 classifer
library(randomForest)


################################################################
#    data preprocessing                                       #
################################################################

download.file('http://archive.ics.uci.edu/ml/machine-learning-databases/car/car.data', 'car.data')

#date reading
cars = read.csv("car.data", header = FALSE,
                col.names = c('buying', 'maint', 'doors', 'persons', 'lug_boot','safety', "category") )
summary(cars)

set.seed(7777)

#creating training and test datasets 
sam <- sample(2, nrow(cars), replace=TRUE, prob=c(0.7, 0.3))
sam
carTrain1 <- cars[sam==1,]
carTest1 <- cars[sam==2,]

#class distribution in sets
prop.table(table(carTrain1$category))
prop.table(table(carTest1$category))

#creating training and test datasets 
?createDataPartition
idTrainData <- unlist(createDataPartition(cars$category,p=0.7))
#str(idTrainData)

carTrain <-cars[idTrainData,]
carTest <-cars[-idTrainData,]

#class distribution in sets
prop.table(table(carTrain$category))
prop.table(table(carTest$category))

table(carTrain$category)
table(carTest$category)

################################################################
# 1.  C5.0 classifier                                         #
################################################################

?C5.0
#model building - a decision tree
car_C50 <- C5.0(carTrain[,-7], carTrain$category) 
summary(car_C50)
plot(car_C50)

#quality of classification for training data
car_c50_trainPred <- predict(car_C50, carTrain)

?CrossTable
CrossTable(car_c50_trainPred, carTrain$category, prop.chisq = FALSE,prop.c = FALSE, 
           prop.r = FALSE, dnn = c('predicted class', 'actual class'))

?confusionMatrix
confusionMatrix(car_c50_trainPred, carTrain$category, mode="everything")

#quality of classification for test data
car_c50_testPred <- predict(car_C50, carTest)
CrossTable(car_c50_testPred, carTest$category, prop.chisq = FALSE,prop.c = FALSE, 
           prop.r = FALSE, dnn = c('predicted class', 'actual class'))

confusionMatrix(car_c50_testPred, carTest$category, mode="everything")

#model building - rules
car_C50R <- C5.0(carTrain[,-7], carTrain$category,  rules = TRUE) 
summary(car_C50R)

#quality of classification for test data
car_c50_testPred <- predict(car_C50R, carTest)
CrossTable(car_c50_testPred, carTest$category, prop.chisq = FALSE,prop.c = FALSE, 
           prop.r = FALSE, dnn = c('predicted class', 'actual class'))

confusionMatrix(car_c50_testPred, carTest$category, mode="everything")

#Ensemble classifier (boosting)
?churn
data(churn)
summary(churnTrain)
summary(churnTest)

#tree
churn_C50 <- C5.0(churnTrain[, -20], churnTrain$churn) 
churn_C50_testPred =predict(churn_C50, churnTest)
confusionMatrix(churn_C50_testPred, churnTest$churn, mode="everything")

#ensemble tree
churn_C50B <- C5.0(churnTrain[, -20], churnTrain$churn,trials = 10) 
churn_C50B_testPred =predict(churn_C50B, churnTest)
confusionMatrix(churn_C50B_testPred, churnTest$churn, mode="everything")
summary(churn_C50B)


###################################################################3333333
#dane iris (https://archive.ics.uci.edu/ml/datasets/Iris)
data(iris)
#str(iris)
#View(iris)

idTrainData1 <- unlist(createDataPartition(iris$Species,p=0.7))
#str(idTrainData)

irisTrain <-iris[idTrainData1,]
irisTest <-iris[-idTrainData1,]

table(irisTest$Species)

#setting the class attribute
irisFormula <-  Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width

################################################################
# 2.  rpart - recursive partitioning trees                       #
################################################################

?rpart
# tree building
iris_rpart <- rpart(irisFormula,  method="class", data=irisTrain)
print(iris_rpart)

#  CP - complexity parameter: serves as a penalty to control the size of the tree;
#the greater the CP value, the fewer the number of splits there are
#  rel error represents the average deviance of the current tree divided 
#by the average deviance of the null tree
#  xerror value represents the relative error estimated by a 10-fold classification 
#  xstd stands for the standard error of the relative error

summary(iris_rpart)

?rpart.plot
#plot(iris_rpart, main="Classification for Iris")
#text(iris_rpart, use.n=TRUE, all=TRUE, cex=.7)
rpart.plot(iris_rpart, main="Classification for Iris")

?prp
prp(iris_rpart, faclen = 0, cex = NULL, extra = 1, main="Classification for Iris")

iris_rpart <- rpart(irisFormula,  method="class", data=irisTrain)
print(iris_rpart)


#training data classification - confusion matrix 
iris_rpat_trainPred = predict(iris_rpart,irisTrain,type = "class")
table(iris_rpat_trainPred, irisTrain$Species)

#test data classification - confusion matrix 
iris_rpat_testPred = predict(iris_rpart,irisTest,type = "class")
table(iris_rpat_testPred, irisTest$Species)

confusionMatrix(iris_rpat_testPred, irisTest$Species, mode="everything")

#Loss matrix - a row the actual class, a column - predicted class 
#(The loss matrix must have zeros on the diagonal and positive off-diagonal elements)
lossM=matrix(c(0,1,1,1,0,4,1,1,0), byrow=TRUE, nrow=3)
lossM
iris_rpartLM <-  rpart(irisFormula,  method="class", data=irisTrain, parms = list(loss = lossM ))

#training data classification - confusion matrix 
iris_rpatLM_trainPred = predict(iris_rpartLM,irisTrain,type = "class")
table(iris_rpatLM_trainPred, irisTrain$Species)

#test data classification - confusion matrix 
iris_rpatLM_testPred = predict(iris_rpartLM,irisTest,type = "class")
table(iris_rpatLM_testPred, irisTest$Species)

#changing the values of parameters
rpControl = rpart.control(minbucket =30, maxDepth = 1);
rpTree <- rpart(irisFormula,  method="class", data=irisTrain,
                control =rpControl,
                parms = list(split = "information" ))
rpart.plot(rpTree, main="Classification for Iris")

iris_rpartS = predict(rpTree,irisTrain,type = "class")
table(iris_rpartS, irisTrain$Species)


#tree pruning 

#The cost complexity pruning algorithm considers the cost complexity of a tree to be a function of
# the number of leaves in the tree and the error rate of the tree (where the error rate is the
# percentage of tuples misclassified by the tree). It starts from the bottom of the tree. For
# each internal node, N, it computes the cost complexity of the subtree at N, and the cost
# complexity of the subtree at N if it were to be pruned (i.e., replaced by a leaf node). The
# two values are compared. If pruning the subtree at node N would result in a smaller cost
# complexity, then the subtree is pruned. Otherwise, it is kept.
# A pruning set of class-labeled tuples is used to estimate cost complexity. This set is
# independent of the training set used to build the unpruned tree and of any test set used
# for accuracy estimation. The algorithm generates a set of progressively pruned trees. In
# general, the smallest decision tree that minimizes the cost complexity is preferred.

#the minimal  cross-validation error 
min(iris_rpartLM$cptable[,"xerror"])
which.min(iris_rpartLM$cptable[,"xerror"])
rpTree.cp=iris_rpartLM$cptable[3,"CP"]
rpTree.cp
?prune
iris_rpartLM_Pruned<- prune(iris_rpartLM, cp = rpTree.cp)


#iris_rpartLM_Pruned <- prune(iris_rpartLM, cp = rpTree$cptable[which.min(rpTree$cptable[,"xerror"]),"CP"])

rpart.plot(iris_rpartLM, main="Classification for Iris")
rpart.plot(iris_rpartLM_Pruned, main="Classification for Iris - pruned")

################################################################
#  3. randomForest                                             #
################################################################

?randomForest
car_Forest = randomForest(category~., data = carTrain, importance = TRUE, nodesize = 10, mtry = 4, ntree = 100 )
#nodesize = minimal number of objects in a node
#mtry - the number of randomly selected attributes for searching the best test split in nodes
#ntree -  number of trees in a forest
#importance - calculation of attriubte importance


print(car_Forest)
plot(car_Forest)

?importance
round(importance(car_Forest, type = 1),2)

car_Forest_testPred = predict (car_Forest, newdata = carTest[-7])
confusionMatrix(car_Forest_testPred, carTest$category, mode = "everything")


#looking for the best values of parameters by means of K-fold validation
?trainControl
trControl <- trainControl(method = "cv", number = 10, search = "grid")

#arguments
#- method = "cv": The method used to resample the dataset. 
#- number = n: Number of folders to create
#- search = "grid": Use the search grid method. For randomized method, use "grid"

?train
tuneGrid <- expand.grid(mtry = c(1:6))
tuneGrid
car_Frestores_mtry <- train(category~.,  data = carTrain,
                            method = "rf",
                            metric = "Accuracy",
                            tuneGrid = tuneGrid,
                            trControl = trControl,
                            importance = TRUE,    # randomForest function parameter
                            nodesize = 10,        # randomForest function parameter
                            ntree = 250)          ## randomForest function parameter
print(car_Frestores_mtry)



treesModels <- list()
for (nbTree in c(5,10,25, 50, 100, 250, 500)) {
  car_F_maxtrees <- train(category~.,  data = carTrain,
                          method = "rf",
                          metric = "Accuracy",
                          tuneGrid = tuneGrid,
                          trControl = trControl,
                          importance = TRUE,
                          nodesize = 10,
                          ntree = nbTree)
  key <- toString(nbTree)
  treesModels[[key]] <- car_F_maxtrees
}

?resamples
results_tree <- resamples(treesModels)
summary(results_tree)

#końcowy model
car_Forest2 = randomForest(category~., data = carTrain, importance = TRUE, mtry = 6, ntree = 250, nodesize = 10)

print(car_Forest2)
plot(car_Forest2)


car_Forest2_testPred = predict (car_Forest, newdata = carTest[-7])
confusionMatrix(car_Forest2_testPred, carTest$category, mode = "everything")

varImpPlot(car_Forest2)

################################################
#Comparision of classifiers
###############################################
car_rpart <- rpart(category~., data=carTrain)
car_rpart_testPred = predict(car_rpart, carTest, type = "class")

klasyfikator = c('C50', 'rpart',  'rForest')
dokladnosc = c( mean(car_c50_testPred == carTest$category), 
                mean(car_rpart_testPred == carTest$category),
                mean(car_Forest2_testPred == carTest$category))

acc_all <- data.frame(klasyfikator, dokladnosc)
View(acc_all)
