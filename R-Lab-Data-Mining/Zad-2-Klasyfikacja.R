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
library(dplyr)

### --- Import pliku i zapisanie go w srodomisku

# download.file("http://archive.ics.uci.edu/ml/machine-learning-databases/credit-screening/crx.data",'credit.csv');
credit_ds_download <- read.table("credit.csv", header = FALSE, sep=",", na.strings= "*")
credit_ds <- credit_ds_download
# download.file("http://archive.ics.uci.edu/ml/machine-learning-databases/credit-screening/crx.names",'credit.names');
# credit_names = read.table("credit.names", header = FALSE, sep=",", na.strings= "*")
for(xx in col(credit_ds[1,])){
  cat("\nColumn: ", xx)
  print(which(credit_ds[,xx]=="?"))
  credit_ds[,xx][credit_ds[,xx] == "?"] <- NA 
  print(which(credit_ds[,xx]=="?"))
}

credit_ds <- na.omit(credit_ds)       # Zmniejszenie tablicy z 690 wierszy do 653
                                      # Od tej pory brak problemow z "?" lub NA
                                      # Trzeba poprawic kolumny na odpowiednie typy: factor/numeric
# levels(credit_ds$V13)               # Test - poziomow kolumn z factor 

credit_ds$V1 <- factor(credit_ds$V1)
credit_ds$V2 <- as.numeric(as.character(credit_ds$V2))
# credit_ds$V3 - jest numeric
credit_ds$V4 <- factor(credit_ds$V4)
credit_ds$V5 <- factor(credit_ds$V5)
credit_ds$V6 <- factor(credit_ds$V6)
credit_ds$V7 <- factor(credit_ds$V7)
# credit_ds$V8 - jest numeric
credit_ds$V9 <- factor(credit_ds$V9)    # dla pewnosci
credit_ds$V10 <- factor(credit_ds$V10)  # tez dla pewnosci
# credit_ds$v11 - jest numeric
credit_ds$V12 <- factor(credit_ds$V12)  # tez dla pewnosci
credit_ds$V13 <- factor(credit_ds$V13)  # tez dla pewnosci
credit_ds$V14 <- as.numeric(as.character(credit_ds$V14))
#credit_ds$V15 - jest numeric
credit_ds$V16 <- factor(credit_ds$V16)

credit_ds[is.na(credit_ds)]             # sprawdzenie czy sa NA


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
print("Klasy w calym zbiorze: ")
print(table(credit_ds[,16]))
print("Klasy w zbiorze TRAIN:")
print(table(Credit_Train_CDP[,16]))
prop.table(table(Credit_Train_CDP[,16]))
print("Klasy w zbiorze TEST:")
print(table(Credit_Test_CDP[,16]))
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
rforest_model <- randomForest(V16~., data = Credit_Train_CDP, importance = TRUE)
# rforest_model <- randomForest(V16~., data = Credit_Train_CDP, importance = TRUE, nodesize = 10, mtry = 4, ntree = 100)
#nodesize = minimal number of objects in a node
#mtry - the number of randomly selected attributes for searching the best test split in nodes
#ntree -  number of trees in a forest
#importance - calculation of attriubte importance
rforest_Pred_Train <- predict(rforest_model, Credit_Train_CDP)
rforest_Pred_Test <- predict(rforest_model, Credit_Test_CDP)
rforest_CMatrix_Train <- confusionMatrix(rforest_Pred_Train, Credit_Train_CDP$V16, mode="everything")
rforest_CMatrix_Test <- confusionMatrix(rforest_Pred_Test, Credit_Test_CDP$V16, mode="everything")
print(round(importance(rforest_model, type = 1),2))

cat("\n --- rforest --- \n")
cat("rforest | ACC on Train: ", rforest_CMatrix_Train$overall[["Accuracy"]], "\n")
cat("rforest | ACC on Test: ", rforest_CMatrix_Test$overall[["Accuracy"]], "\n\n")


# --- Porownanie predykcji na Test - wszystkie klasyfikatory
print("Porownanie klasyfikatorow na Test:")
klasyfikator <- c('C50', 'rpart', 'rForest')
dokladnosc <- c( C50_CMatrix_Test$overall[["Accuracy"]],
                 rpart_CMatrix_Test$overall[["Accuracy"]],
                 rforest_CMatrix_Test$overall[["Accuracy"]])
acc_all <- data.frame(klasyfikator, dokladnosc)
print(acc_all)
cat("\n")


### --- Porownanie predykcji na Test po 10 petlach - wszystkie klasyfikatory ------------------------------------------------------ ###
print("--- Petla (1:10) usredniajaca wynik ACC dla klasyfikatorow (rozne zbiory TRAIN (0.7) i TEST (0.3)) ---")
print("Najlepsza klasyfikacja dla:")

for (x in c(1:10)) {
  # cat("\n")
  rm(idTrainData)
  idTrainData <- unlist(createDataPartition(credit_ds[,16],p=0.7))
  Credit_Train_CDP <- credit_ds[idTrainData,]
  Credit_Test_CDP <- credit_ds[-idTrainData,]
  # print(Credit_Test_CDP[,16])                                       # Sprawdzenie czy tworza sie inne zbiory

  C50_model <- C5.0(Credit_Train_CDP[,-16], Credit_Train_CDP$V16)
  C50_Pred_Test <- predict(C50_model, Credit_Test_CDP)
  C50_CMatrix_Test <- confusionMatrix(C50_Pred_Test, Credit_Test_CDP$V16, mode="everything")
  # cat("C5.0 | ACC on Test: ", C50_CMatrix_Test$overall[["Accuracy"]], "\n")
  best_model <- list("C50", C50_CMatrix_Test$overall[["Accuracy"]])

  rpart_model <- rpart(V16 ~ .,  method="class", data=Credit_Train_CDP)
  rpart_Pred_Test <- predict(rpart_model, Credit_Test_CDP, type = "class")
  rpart_CMatrix_Test <- confusionMatrix(rpart_Pred_Test, Credit_Test_CDP$V16, mode="everything")
  # cat("rpart | ACC on Test: ", rpart_CMatrix_Test$overall[["Accuracy"]], "\n")
  if(best_model[[2]] < rpart_CMatrix_Test$overall[["Accuracy"]]){
    best_model <- list("rpart", rpart_CMatrix_Test$overall[["Accuracy"]])
  }

  rforest_model <- randomForest(V16~., data = Credit_Train_CDP, importance = TRUE)
  rforest_Pred_Test <- predict(rforest_model, Credit_Test_CDP)
  rforest_CMatrix_Test <- confusionMatrix(rforest_Pred_Test, Credit_Test_CDP$V16, mode="everything")
  # cat("rforest | ACC on Test: ", rforest_CMatrix_Test$overall[["Accuracy"]], "\n")
  # print(round(importance(rforest_model, type = 1),2))

  if(best_model[[2]] < rforest_CMatrix_Test$overall[["Accuracy"]]){
    best_model <- list("rForest", rforest_CMatrix_Test$overall[["Accuracy"]])
  }
  cat(x, ": ", best_model[[1]], ": ACC = ", best_model[[2]], "\n")

  acc_all$dokladnosc[1] <- (acc_all$dokladnosc[1] + C50_CMatrix_Test$overall[["Accuracy"]]) / 2
  acc_all$dokladnosc[2] <- (acc_all$dokladnosc[2] + rpart_CMatrix_Test$overall[["Accuracy"]]) / 2
  acc_all$dokladnosc[3] <- (acc_all$dokladnosc[3] + rforest_CMatrix_Test$overall[["Accuracy"]]) / 2


}
cat("\n\n")
print("Srednie ACC klasyfikatorow na Test (p=0.3) po 11 razach: (nowe zbiory w TRAIN i TEST w kazdej petli)")
print(acc_all)

### Komentarz:
### Na kilkanascie prob przeprowadzonych na powyzszym kodzie, najlepszym klasyfikatorem 
### (bez strojenia) w tym przypadku jest model: randomForest.
### Kolejnym punktem jest strojenie randomForest w celu uzyskania najlepszego wyniku ACC (predykcji).


### --- TUNING RandomForest --- ###
cat("\nn")
print("--- Tuning - RandomForest ---")
print("Podzial danych na Train i Test")
# Zacznijmy od ponownego podzialu danych na Train i Test && podstawowego klasyfikatora randomForest

rm(idTrainData)
idTrainData <- unlist(createDataPartition(credit_ds[,16],p=0.7))
Credit_Train_CDP <- credit_ds[idTrainData,]
Credit_Test_CDP <- credit_ds[-idTrainData,]

rforest_model <- randomForest(V16~., data = Credit_Train_CDP, importance = TRUE, nodesize = 10, mtry = 4, ntree = 100)
rforest_Pred_Test <- predict(rforest_model, Credit_Test_CDP)
rforest_CMatrix_Test <- confusionMatrix(rforest_Pred_Test, Credit_Test_CDP$V16, mode="everything")
cat("rforest v0 (default) | ACC on Test: ", rforest_CMatrix_Test$overall[["Accuracy"]], "\n")
wynik_v0 <- rforest_CMatrix_Test$overall[["Accuracy"]]

# Tuning v1 - Znalezienie najlepszej wartosci: mtry

# Arguments in trainCoontrol():
#- method = "cv": The method used to resample the dataset. 
#- number = n: Number of folders to create
#- search = "grid": Use the search grid method. For randomized method, use "grid"

trControl <- trainControl(method = "cv", number = 20, search = "grid")
tuneGrid <- expand.grid(mtry = c(1:10))
Credit_mtry <- train(V16 ~.,  data = Credit_Train_CDP,
                            method = "rf",
                            metric = "Accuracy",
                            tuneGrid = tuneGrid,
                            trControl = trControl,
                            importance = TRUE,    # randomForest function parameter
                            nodesize = 10,        # randomForest function parameter
                            ntree = 250)          ## randomForest function parameter
print(Credit_mtry)

rforest_model_v1 <- randomForest(V16~., data = Credit_Train_CDP, importance = TRUE, 
                              nodesize = Credit_mtry$finalModel$param$nodesize, 
                              mtry = Credit_mtry$finalModel$mtry, 
                              ntree = Credit_mtry$finalModel$ntree)
rforest_Pred_Test_v1 <- predict(rforest_model_v1, Credit_Test_CDP)
rforest_CMatrix_Test_v1 <- confusionMatrix(rforest_Pred_Test_v1, Credit_Test_CDP$V16, mode="everything")
cat("\n")
cat("rforest v1 (best mtry) | ACC on Test: ", rforest_CMatrix_Test_v1$overall[["Accuracy"]], "\n")
wynik_v1 <- rforest_CMatrix_Test_v1$overall[["Accuracy"]]


# Tuning v2 - Znalezienie najlepszej wartosci: nbTree

treesModels <- list()
# for (nbTree in seq(10,600, by=10)) {
for (nbTree in c(5,10,25, 50, 100, 250, 500)) {
  credit_maxTrees <- train(V16 ~.,  data = Credit_Train_CDP,
                          method = "rf",
                          metric = "Accuracy",
                          tuneGrid = tuneGrid,
                          trControl = trControl,
                          importance = TRUE,
                          nodesize = 10,
                          ntree = nbTree)
  key <- toString(nbTree)
  treesModels[[key]] <- credit_maxTrees
  cat(nbTree, ".. ")
}

results_tree <- resamples(treesModels)
summ_v2 <- summary(results_tree)
max_acc <- max(summ_v2$statistics$Accuracy[,4])
nbTree_v2 <- rownames(summ_v2$statistics$Accuracy)[summ_v2$statistics$Accuracy[,4] == max_acc]
cat("\nInfo from search for best nTree: nbTree_v2 = ", nbTree_v2, "| mtry = ", treesModels[[nbTree_v2]][["finalModel"]][["mtry"]])

rforest_model_v2 <- randomForest(V16~., data = Credit_Train_CDP, importance = TRUE, 
                                 nodesize = treesModels[[nbTree_v2]][["finalModel"]][["param"]][["nodesize"]], 
                                 mtry = treesModels[[nbTree_v2]][["finalModel"]][["mtry"]], 
                                 ntree = as.numeric(nbTree_v2))
rforest_Pred_Test_v2 <- predict(rforest_model_v2, Credit_Test_CDP)
rforest_CMatrix_Test_v2 <- confusionMatrix(rforest_Pred_Test_v2, Credit_Test_CDP$V16, mode="everything")
cat("\n")
cat("Model info (v2): ", "nodesize = ",treesModels[[nbTree_v2]][["finalModel"]][["param"]][["nodesize"]], 
    "| mtry = ", treesModels[[nbTree_v2]][["finalModel"]][["mtry"]], 
    "| ntree = ", as.numeric(nbTree_v2), "\n")
cat("rforest v2 (best mtry+ntree) | ACC on Test: ", rforest_CMatrix_Test_v2$overall[["Accuracy"]], "\n")
wynik_v2 <- rforest_CMatrix_Test_v2$overall[["Accuracy"]]


# Tuning v3 - Model z palca
nodesize_v3 = 10
mtry_v3 = 7
ntree_v3 = 250

rforest_model_v3 <- randomForest(V16~., data = Credit_Train_CDP, importance = TRUE, 
                                 nodesize = nodesize_v3,#treesModels[[nbTree_v2]][["finalModel"]][["param"]][["nodesize"]], 
                                 mtry = mtry_v3, # treesModels[[nbTree_v2]][["finalModel"]][["mtry"]], 
                                 ntree = ntree_v3) # as.numeric(nbTree_v2))
rforest_Pred_Test_v3<- predict(rforest_model_v3, Credit_Test_CDP)
rforest_CMatrix_Test_v3 <- confusionMatrix(rforest_Pred_Test_v3, Credit_Test_CDP$V16, mode="everything")
cat("\n")
cat("Model info (v3): ", "nodesize = ", nodesize_v3, "| mtry = ", mtry_v3, "| ntree = ", ntree_v3, "\n")
cat("rforest v3 | ACC on Test: ", rforest_CMatrix_Test_v3$overall[["Accuracy"]], "\n")
wynik_v3 <- rforest_CMatrix_Test_v3$overall[["Accuracy"]]


# Tuning v4 - Model przy pomocy "tune"
tune1 <- tune(randomForest, V16 ~ . , data = Credit_Train_CDP, validation.x = Credit_Test_CDP[,-16], validation.y = Credit_Test_CDP[,16])
rforest_Pred_Test_v4<- predict(tune1$best.model, Credit_Test_CDP)
rforest_CMatrix_Test_v4 <- confusionMatrix(rforest_Pred_Test_v4, Credit_Test_CDP$V16, mode="everything")
cat("\n")
cat("rforest v4 | ACC on Test: ", rforest_CMatrix_Test_v4$overall[["Accuracy"]], "\n")
wynik_v4 <- rforest_CMatrix_Test_v4$overall[["Accuracy"]]

# Tuning v5 - czyste randomForest
rforest_model_v5 <- randomForest(V16~., data = Credit_Train_CDP, importance = TRUE)
rforest_Pred_Test_v5<- predict(rforest_model_v5, Credit_Test_CDP)
rforest_CMatrix_Test_v5 <- confusionMatrix(rforest_Pred_Test_v5, Credit_Test_CDP$V16, mode="everything")
cat("\n")
cat("rforest v5 | ACC on Test: ", rforest_CMatrix_Test_v5$overall[["Accuracy"]], "\n")
wynik_v5 <- rforest_CMatrix_Test_v5$overall[["Accuracy"]]


# Podsumowanie: 

wyniki <- as.matrix(c("v0=default", "v1=mtry(grid)", "v2=nbtree+mtry", "v3=obserwacja", "v4=tune", "v5=randomForest"))
wyniki <- cbind(wyniki, c(wynik_v0, wynik_v1, wynik_v2, wynik_v3, wynik_v4, wynik_v5))
print(wyniki)

### Komentarz: Najlepsze wyniki wychodza dla funkcji randomForest bez sprecyzowanych zmiennych. 


