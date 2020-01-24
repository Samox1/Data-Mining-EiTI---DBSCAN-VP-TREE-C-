# Zad 2 - Klasyfikacja
# Deadline: 27.01.2020, godzina 8:00

# Defining the classification problem and building appropriate classifier for:
# credit approval, data and description available at: #http://archive.ics.uci.edu/ml/datasets/Credit+Approval
# download.file("http://archive.ics.uci.edu/ml/machine-learning-databases/credit-screening/crx.data",'credit.csv');
# credit_ds = read.table("credit.csv", header = TRUE, sep=",", na.strings= "*")
# wines data and description available at: http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/
# download.file('http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv', 'wine_red.csv');
# download.file('http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv', 'wine_white.csv');
# wineRed_ds = read.table("wine_red.csv", header = TRUE, sep=";", na.strings= "*")

# Zadanie nr 2:
# 1) Wybranie danych
# 2) Sprawdzenie klasyfikatorow podstawowych z pokazanych na laboratorium (C50, rpart(Tree), randomForest)
# 3) Wybranie jednego klasyfikatora i testowanie go z roznymi parametrami - tak aby wyszla najlepsza klasyfikacja


rm(list = ls())

library(gmodels) #results analysis
library(Hmisc) #results analysis
library(caret)
library(rpart) # rpart() - decision tree classifier
library(rpart.plot) 
library(e1071)
library(C50) # C5 classifer
library(randomForest)

### --- Import pliku i zapisanie go w srodomisku

# download.file("http://archive.ics.uci.edu/ml/machine-learning-databases/credit-screening/crx.data",'credit.csv');
credit_ds = read.table("credit.csv", header = FALSE, sep=",", na.strings= "*")
# download.file("http://archive.ics.uci.edu/ml/machine-learning-databases/credit-screening/crx.names",'credit.names');
# credit_names = read.table("credit.names", header = FALSE, sep=",", na.strings= "*")

credit_ds$V2 <- as.numeric(credit_ds$V2)
credit_ds$V14 <- as.numeric(credit_ds$V14)

### --- Podzial na zbiory Train i Test

# sam <- sample(2, nrow(credit_ds), replace=TRUE, prob=c(0.7, 0.3))
# Credit_Train_S <- credit_ds[sam==1,]
# Credit_Test_S <- credit_ds[sam==2,]
# 
# prop.table(table(Credit_Train_S[,16]))
# prop.table(table(Credit_Test_S[,16]))

idTrainData <- unlist(createDataPartition(credit_ds[,16],p=0.7))
Credit_Train_CDP <- credit_ds[idTrainData,]
Credit_Test_CDP <- credit_ds[-idTrainData,]
table(credit_ds[,16])
prop.table(table(Credit_Train_CDP[,16]))
prop.table(table(Credit_Test_CDP[,16]))


### --- Sprawdzenie podstawowych klasyfikatorow

# --- C5.0 ---
C50_model <- C5.0(Credit_Train_CDP[,-16], Credit_Train_CDP$V16)
C50_Pred_Train <- predict(C50_model, Credit_Train_CDP)
C50_Pred_Test <- predict(C50_model, Credit_Test_CDP)
C50_CMatrix_Train <- confusionMatrix(C50_Pred_Train, Credit_Train_CDP$V16, mode="everything")
C50_CMatrix_Test <- confusionMatrix(C50_Pred_Test, Credit_Test_CDP$V16, mode="everything")

cat("\n --- C5.0 --- \n")
cat("C5.0 | ACC on Train: ", C50_CMatrix_Train$overall[["Accuracy"]], "\n")
cat("C5.0 | ACC on Test: ", C50_CMatrix_Test$overall[["Accuracy"]], "\n")


# --- rpart ---
rpart_model <- rpart(V16 ~ .,  method="class", data=Credit_Train_CDP)
rpart_Pred_Train <- predict(rpart_model, Credit_Train_CDP, type = "class")
rpart_Pred_Test <- predict(rpart_model, Credit_Test_CDP, type = "class")
rpart_CMatrix_Train <- confusionMatrix(rpart_Pred_Train, Credit_Train_CDP$V16, mode="everything")
rpart_CMatrix_Test <- confusionMatrix(rpart_Pred_Test, Credit_Test_CDP$V16, mode="everything")

cat("\n --- rpart --- \n")
cat("rpart | ACC on Train: ", rpart_CMatrix_Train$overall[["Accuracy"]], "\n")
cat("rpart | ACC on Test: ", rpart_CMatrix_Test$overall[["Accuracy"]], "\n")


# --- rforest ---
rforest_model <- randomForest(V16~., data = Credit_Train_CDP, importance = TRUE, nodesize = 10, mtry = 4, ntree = 100)
#nodesize = minimal number of objects in a node
#mtry - the number of randomly selected attributes for searching the best test split in nodes
#ntree -  number of trees in a forest
#importance - calculation of attriubte importance
rforest_Pred_Train <- predict(rforest_model, Credit_Train_CDP)
rforest_Pred_Test <- predict(rforest_model, Credit_Test_CDP)
rforest_CMatrix_Train <- confusionMatrix(rforest_Pred_Train, Credit_Train_CDP$V16, mode="everything")
rforest_CMatrix_Test <- confusionMatrix(rforest_Pred_Test, Credit_Test_CDP$V16, mode="everything")

cat("\n --- rforest --- \n")
cat("rforest | ACC on Train: ", rforest_CMatrix_Train$overall[["Accuracy"]], "\n")
cat("rforest | ACC on Test: ", rforest_CMatrix_Test$overall[["Accuracy"]], "\n\n")


# --- Porownanie predykcji na Test - wszystkie klasyfikatory
klasyfikator <- c('C50', 'rpart', 'rForest')
dokladnosc <- c( C50_CMatrix_Test$overall[["Accuracy"]],
                 rpart_CMatrix_Test$overall[["Accuracy"]],
                 rforest_CMatrix_Test$overall[["Accuracy"]])
acc_all <- data.frame(klasyfikator, dokladnosc)
print(acc_all)







