# CSZ PROJECT
# Ultra-short-term forecasting of Total Load or something else


# https://stackoverflow.com/questions/43880823/subset-dataframe-based-on-posixct-date-and-time-greater-than-datetime-using-dply
# https://stackoverflow.com/questions/38396516/dplyr-filter-based-on-another-column
# https://data.library.virginia.edu/working-with-dates-and-time-in-r-using-the-lubridate-package/
# https://www.r-bloggers.com/how-to-filter-in-r-a-detailed-introduction-to-the-dplyr-filter-function/
# https://blog.exploratory.io/filter-data-with-dplyr-76cf5f1a258e
# https://www.guru99.com/r-decision-trees.html


library(readr)
library(MASS)
library(e1071)
library(rpart)
library(randomForest)
library(rpart.plot)
library(tidyverse)
library(lubridate)
library(data.table)
library(stringr)
library(caret)
library(FNN)
library(plotly)

rm(list = ls())            # Czyszczenie Globalnego Srodowiska 

HU_Data_ALL <- as.data.frame(read.table("HU_TS_15_min.csv",header=TRUE,sep=","))
HU_Data_ALL$utc_timestamp <- as.data.frame(ymd_hms(HU_Data_ALL$utc_timestamp, tz = "UTC"))

start1 <- lubridate::ymd_hms('2014-12-22 23:45:00', tz = "UTC")
rok_start1 <- lubridate::ymd_hms('2017-04-30 22:45:00', tz = "UTC")
end1 <- lubridate::ymd_hms('2018-04-30 23:00:00', tz = "UTC")


rok_start2 <- lubridate::ymd_hms('2018-04-30 22:45:00', tz = "UTC")
end2 <- lubridate::ymd_hms('2019-04-30 23:00:00', tz = "UTC")

HU_Data <- HU_Data_ALL %>% filter(utc_timestamp[[1]] > rok_start1-days(2) & utc_timestamp[[1]] < end2+minutes(15))
colnames(HU_Data[,1]) <- ""
HU_Data <- HU_Data[, c("utc_timestamp" , "HU_load_actual_entsoe_transparency")]

NA_sum <- sum(is.na(HU_Data$HU_load_actual_entsoe_transparency))
levele <- as.integer(max(HU_Data$HU_load_actual_entsoe_transparency) - min(HU_Data$HU_load_actual_entsoe_transparency))
minMW <- min(HU_Data$HU_load_actual_entsoe_transparency)
maxMW <- max(HU_Data$HU_load_actual_entsoe_transparency)


Daty_Prognozowane <- seq(rok_start1, end1, by = '15 min')

# --- Utworzenie nowej tablicy pod drzewo --- #
Temp <- HU_Data %>% filter(utc_timestamp > (rok_start1) & utc_timestamp < end1)
Temp$godzina <- hour(Temp$utc_timestamp)

Min15Before <- HU_Data %>% filter(utc_timestamp > (rok_start1-minutes(15)) & utc_timestamp < end1-minutes(15)) %>% select(HU_load_actual_entsoe_transparency)
Temp$Min15B <- cbind(as.matrix(Min15Before))
rm(Min15Before)

Min30Before <- HU_Data %>% filter(utc_timestamp > (rok_start1-minutes(30)) & utc_timestamp < end1-minutes(30)) %>% select(HU_load_actual_entsoe_transparency)
Temp$Min30B <- cbind(as.matrix(Min30Before))
rm(Min30Before)

Min45Before <- HU_Data %>% filter(utc_timestamp > (rok_start1-minutes(45)) & utc_timestamp < end1-minutes(45)) %>% select(HU_load_actual_entsoe_transparency)
Temp$Load_Min45 <- cbind(as.matrix(Min45Before))
rm(Min45Before)

Day1Before <- HU_Data %>% filter(utc_timestamp > (rok_start1-days(1)) & utc_timestamp < end1-days(1)) %>% select(HU_load_actual_entsoe_transparency)
Temp$Day1B <- cbind(as.matrix(Day1Before))
rm(Day1Before)

Day1Before15Min <- HU_Data %>% filter(utc_timestamp > (rok_start1-days(1)-minutes(15)) & utc_timestamp < end1-days(1)-minutes(15)) %>% select(HU_load_actual_entsoe_transparency)
Temp$Load_Day1B15min <- cbind(as.matrix(Day1Before15Min))
rm(Day1Before15Min)

Day1Beforep15Min <- HU_Data %>% filter(utc_timestamp > (rok_start1-days(1)+minutes(15)) & utc_timestamp < end1-days(1)+minutes(15)) %>% select(HU_load_actual_entsoe_transparency)
Temp$Load_Day1Bp15min <- cbind(as.matrix(Day1Beforep15Min))
rm(Day1Beforep15Min)

# Temp$Class <- as.integer(Temp$HU_load_actual_entsoe_transparency - minMW)
Temp$Class <- Temp$HU_load_actual_entsoe_transparency
# Temp$Class <- factor(Temp$Class)
# Temp$Class <- factor(as.integer(Temp$HU_load_actual_entsoe_transparency))

colnames(Temp) <- c("Time", "Load_Now", "Hour", "Load_Min15", "Load_Day1B", "Class", "Load_Min30")
Temp$Hour <- as.factor(Temp$Hour)

# --- Testy na Drzewie --- #
# Tree <- rpart(Class ~ Load_Min15 + Load_Day1B, Temp, method = "class", minsplit = 1, minbucket = 1, cp=0.000001)
Tree_Anova <- rpart(Load_Now ~ Load_Min15 + Load_Day1B, Temp, minsplit = 1, minbucket = 1, cp=0.000001)
Tree_Anova_30 <- rpart(Load_Now ~ Load_Min15 + Load_Day1B + Load_Min30, Temp, minsplit = 1, minbucket = 1, cp=0.000001)
Tree_Anova_H <- rpart(Load_Now ~ Load_Min15 + Load_Day1B + Hour, Temp, minsplit = 1, minbucket = 1, cp=0.000001)
Tree_Anova_H30 <- rpart(Load_Now ~ Load_Min15 + Load_Min30 + Load_Day1B + Hour, Temp, minsplit = 1, minbucket = 1, cp=0.000001)
Tree_Anova_3045 <- rpart(Load_Now ~ Load_Min15 + Load_Min30 + Load_Min45 + Load_Day1B, Temp, minsplit = 1, minbucket = 1, cp=0.000001)
Tree_Anova_1Day <- rpart(Load_Now ~ Load_Min15 + Load_Day1B + Load_Day1B15min + Load_Day1Bp15min, Temp, minsplit = 1, minbucket = 1, cp=0.000001)

# printcp(Tree)
# plotcp(Tree)
# rpart.plot(Tree, type=1, extra=1)

# --- Random Forest --- #                                                                     # Sprawdzic z godzinami - H
rf <- randomForest(Load_Now ~ Load_Min15 + Load_Day1B, data = Temp, ntree = 10)
rf100 <- randomForest(Load_Now ~ Load_Min15 + Load_Day1B, data = Temp, ntree = 100)           # -------------------------

rf_H <- randomForest(Load_Now ~ Load_Min15 + Load_Day1B + Hour, data = Temp, ntree = 10)      # + Hour
rf100_H <- randomForest(Load_Now ~ Load_Min15 + Load_Day1B + Hour, data = Temp, ntree = 100)  # + Hour

rf_H30 <- randomForest(Load_Now ~ Load_Min15 + Load_Min30 + Load_Day1B + Hour, data = Temp, ntree = 10)
rf100_H30 <- randomForest(Load_Now ~ Load_Min15 + Load_Min30 + Load_Day1B + Hour, data = Temp, ntree = 100)  # + Hour

rf_30 <- randomForest(Load_Now ~ Load_Min15 + Load_Min30 + Load_Day1B, data = Temp, ntree = 10)
rf100_30 <- randomForest(Load_Now ~ Load_Min15 + Load_Min30 + Load_Day1B, data = Temp, ntree = 100)  # + Hour

rf_3045 <- randomForest(Load_Now ~ Load_Min15 + Load_Min30 + Load_Min45 + Load_Day1B, data = Temp, ntree = 10)
rf100_3045 <- randomForest(Load_Now ~ Load_Min15 + Load_Min30 + Load_Min45 + Load_Day1B, data = Temp, ntree = 100)  # + Hour

rf_1Day <- randomForest(Load_Now ~ Load_Min15 + Load_Day1B + Load_Day1B15min + Load_Day1Bp15min, data = Temp, ntree = 10)
rf100_1Day <- randomForest(Load_Now ~ Load_Min15 + Load_Day1B + Load_Day1B15min + Load_Day1Bp15min, data = Temp, ntree = 100)  # + Hour


# rf.pred <- predict(rf, newdata = newdata)                                                   # -------------------------
# newdata$RFpred <- predict(rf, newdata = newdata)                                            # -------------------------
# newdata$RF100pred <- predict(rf100, newdata = newdata)                                      # -------------------------

# --- SVM --- #                                                                                                                                     # Zrobic SVM
# SVM_Data_v1 <- svm(Load_Now ~ Load_Min15 + Load_Day1B, data = Temp, kernel="radial", probability=TRUE, gamma=0.00005, cost=2000, scale = F)         # <-- v1: MAE=22.416 | MAPE=0.474
# SVM_Data_v2 <- svm(Load_Now ~ Load_Min15 + Load_Day1B, data = Temp, kernel="radial", probability=TRUE, gamma=0.00001, cost=2000, scale = F)         # <-- v2: MAE=17.422 | MAPE=0.371
# SVM_Data_v3 <- svm(Load_Now ~ Load_Min15 + Load_Day1B, data = Temp, kernel="radial", probability=TRUE, gamma=0.000005, cost=5000, scale = F)        # <-- v3: MAE=15.547 | MAPE=0.333                                                                                                                                                 
# SVM_Data_v4 <- svm(Load_Now ~ Load_Min15 + Load_Day1B, data = Temp, kernel="radial", probability=TRUE, gamma=0.000001, cost=10000, scale = F)       # <-- v4: MAE=14.258 | MAPE=0.308
# SVM_Data_v5 <- svm(Load_Now ~ Load_Min15 + Load_Day1B, data = Temp, kernel="radial", probability=TRUE, gamma=0.0000005, cost=10000, scale = F)      # <-- v5: MAE=14.047 | MAPE=0.303
# SVM_Data_v6 <- svm(Load_Now ~ Load_Min15 + Load_Day1B, data = Temp, kernel="radial", probability=TRUE, gamma=0.0000001, cost=100000, scale = F)     # <-- v6: MAE=13.707 | MAPE=0.297
# SVM_Data_v7 <- svm(Load_Now ~ Load_Min15 + Load_Day1B, data = Temp, kernel="radial", probability=TRUE, gamma=0.00000001, cost=100000, scale = F)    # <-- v7: MAE=12.071 | MAPE=0.261
# SVM_Data_v8 <- svm(Load_Now ~ Load_Min15 + Load_Day1B, data = Temp, kernel="radial", probability=TRUE, gamma=0.000000001, cost=1000000, scale = F)  # <-- v8: MAE=11.579 | MAPE=0.246
# SVM_Data_v9 <- svm(Load_Now ~ Load_Min15 + Load_Day1B, data = Temp, kernel="radial", probability=TRUE, gamma=0.000000001, cost=100000, scale = F)   # <-- v9: MAE=10.132 | MAPE=0.216
# SVM_Data_v10 <- svm(Load_Now ~ Load_Min15 + Load_Day1B, data = Temp, kernel="radial", probability=TRUE, gamma=0.000000001, cost=10000, scale = F)   # <-- v10: MAE=8.103 | MAPE=0.171
# SVM_Data_v11 <- svm(Load_Now ~ Load_Min15 + Load_Day1B, data = Temp, kernel="radial", probability=TRUE, gamma=0.000000001, cost=1000, scale = F)    # <-- v11: MAE=8.184 | MAPE=0.169
# SVM_Data_v12 <- svm(Load_Now ~ Load_Min15 + Load_Day1B, data = Temp, kernel="radial", probability=TRUE, gamma=0.0000000001, cost=10000, scale = F)  # <-- v12: MAE=7.980 | MAPE=0.165
# SVM_Data_v13 <- svm(Load_Now ~ Load_Min15 + Load_Day1B, data = Temp, kernel="radial", probability=TRUE, gamma=0.00000000001, cost=10000, scale = F) # <-- v13: MAE=24.763 | MAPE=0.503

gammas <- c(0.00005, 0.00001, 0.000005,0.000001,0.0000005,0.0000001,0.00000001,0.000000001,0.000000001,0.000000001,0.000000001,0.0000000001)
costs <- c(2000,2000,5000,10000,10000,100000,100000,1000000,100000,10000,1000,10000)
MAE_C <- c(MAE_SVM_v1,MAE_SVM_v2,MAE_SVM_v3,MAE_SVM_v4,MAE_SVM_v5,MAE_SVM_v6,MAE_SVM_v7,MAE_SVM_v8,MAE_SVM_v9,MAE_SVM_v10,MAE_SVM_v11,MAE_SVM_v12)
MAPE_C <- c(MAPE_SVM_v1,MAPE_SVM_v2,MAPE_SVM_v3,MAPE_SVM_v4,MAPE_SVM_v5,MAPE_SVM_v6,MAPE_SVM_v7,MAPE_SVM_v8,MAPE_SVM_v9,MAPE_SVM_v10,MAPE_SVM_v11,MAPE_SVM_v12)

p <- plot_ly(x=gammas, y=costs, z=MAPE_C)
p

SVM_Tune_v1 <- tune(svm, Load_Now ~ Load_Min15 + Load_Day1B, data = Temp, validation.x = newdata[,4:5], validation.y = newdata$Load_Now,
            ranges = list(gamma = c(10^(-12:-5),(10^(-12:-5))/2), cost = 10^(2:8)),
            tunecontrol = tune.control(sampling = "fix", performances = TRUE ))

SVM_H_Tune_v1 <- tune(svm, Load_Now ~ Load_Min15 + Load_Day1B + Hour, data = Temp, validation.x = newdata[,3:5], validation.y = newdata$Load_Now,
                    ranges = list(gamma = c(10^(-12:-5),(10^(-12:-5))/2), cost = 10^(2:8)),
                    tunecontrol = tune.control(sampling = "fix", performances = TRUE ))


# SVM_Data_H <- svm(Load_Now ~ Load_Min15 + Load_Day1B + Hour, data = Temp, kernel="radial", probability=TRUE, gamma=0.0000001, cost=10000, scale = F)      # <-- Hv1: MAE=13.522 | MAPE=0.272
# SVM_Data_H <- svm(Load_Now ~ Load_Min15 + Load_Day1B + Hour, data = Temp, kernel="radial", probability=TRUE, gamma=0.0000001, cost=100000, scale = F)     # <-- Hv2: MAE=15.388 | MAPE=0.328
# SVM_Data_H <- svm(Load_Now ~ Load_Min15 + Load_Day1B + Hour, data = Temp, kernel="radial", probability=TRUE, gamma=0.00000001, cost=10000, scale = F)     # <-- Hv3: MAE=11.772 | MAPE=0.257
# SVM_Data_H <- svm(Load_Now ~ Load_Min15 + Load_Day1B + Hour, data = Temp, kernel="radial", probability=TRUE, gamma=0.0000000001, cost=10000, scale = F)   # <-- Hv4: MAE=7.985 | MAPE=0.165
# SVM_Data_H <- svm(Load_Now ~ Load_Min15 + Load_Day1B + Hour, data = Temp, kernel="radial", probability=TRUE, gamma=0.0000000001, cost=100000, scale = F)  # <-- Hv5: MAE=7.446 | MAPE=0.155
# SVM_Data_H <- svm(Load_Now ~ Load_Min15 + Load_Day1B + Hour, data = Temp, kernel="radial", probability=TRUE, gamma=0.0000000001, cost=1000, scale = F)    # <-- Hv6: MAE=7.981 | MAPE=0.165

# SVM_Data_H30 <- svm(Load_Now ~ Load_Min15 + Load_Day1B + Hour + Load_Min30, data = Temp, kernel="radial", probability=TRUE, gamma=0.0000001, cost=10000, scale = F)     # <-- H30v1: MAE=32.393 | MAPE=0.685
# SVM_Data_H30 <- svm(Load_Now ~ Load_Min15 + Load_Day1B + Hour + Load_Min30, data = Temp, kernel="radial", probability=TRUE, gamma=0.0000001, cost=100000, scale = F)    # <-- H30v2: MAE=32.459 | MAPE=0.686
# SVM_Data_H30 <- svm(Load_Now ~ Load_Min15 + Load_Day1B + Hour + Load_Min30, data = Temp, kernel="radial", probability=TRUE, gamma=0.00000001, cost=10000, scale = F)    # <-- H30v3: MAE=32.479 | MAPE=0.685
# SVM_Data_H30 <- svm(Load_Now ~ Load_Min15 + Load_Day1B + Hour + Load_Min30, data = Temp, kernel="radial", probability=TRUE, gamma=0.0000000001, cost=10000, scale = F)  # <-- H30v4: MAE=17.254 | MAPE=0.356
# SVM_Data_H30 <- svm(Load_Now ~ Load_Min15 + Load_Day1B + Hour + Load_Min30, data = Temp, kernel="radial", probability=TRUE, gamma=0.0000000001, cost=100000, scale = F) # <-- H30v5: MAE=31.348 | MAPE=0.649
# SVM_Data_H30 <- svm(Load_Now ~ Load_Min15 + Load_Day1B + Hour + Load_Min30, data = Temp, kernel="radial", probability=TRUE, gamma=0.0000000001, cost=1000, scale = F)   # <-- H30v6: MAE=24.361 | MAPE=0.496

SVM_Data_30 <- svm(Load_Now ~ Load_Min15 + Load_Day1B + Load_Min30, data = Temp, kernel="radial", probability=TRUE, gamma=0.0000001, cost=10000, scale = F)      # <-- 30v1: MAE= | MAPE=
SVM_Data_30 <- svm(Load_Now ~ Load_Min15 + Load_Day1B + Load_Min30, data = Temp, kernel="radial", probability=TRUE, gamma=0.0000001, cost=100000, scale = F)     # <-- 30v2: MAE= | MAPE=
SVM_Data_30 <- svm(Load_Now ~ Load_Min15 + Load_Day1B + Load_Min30, data = Temp, kernel="radial", probability=TRUE, gamma=0.00000001, cost=10000, scale = F)     # <-- 30v3: MAE= | MAPE=
SVM_Data_30 <- svm(Load_Now ~ Load_Min15 + Load_Day1B + Load_Min30, data = Temp, kernel="radial", probability=TRUE, gamma=0.0000000001, cost=1000, scale = F)    # <-- 30v4: MAE= | MAPE=
SVM_Data_30 <- svm(Load_Now ~ Load_Min15 + Load_Day1B + Load_Min30, data = Temp, kernel="radial", probability=TRUE, gamma=0.0000000001, cost=10000, scale = F)   # <-- 30v5: MAE= | MAPE=
SVM_Data_30 <- svm(Load_Now ~ Load_Min15 + Load_Day1B + Load_Min30, data = Temp, kernel="radial", probability=TRUE, gamma=0.0000000001, cost=100000, scale = F)  # <-- 30v6: MAE= | MAPE=

SVM_Data_3045 <- svm(Load_Now ~ Load_Min15 + Load_Day1B + Load_Min30 + Load_Min45, data = Temp, kernel="radial", probability=TRUE, gamma=0.0000001, cost=10000, scale = F)      # <-- 3045v1: MAE= | MAPE=
SVM_Data_3045 <- svm(Load_Now ~ Load_Min15 + Load_Day1B + Load_Min30 + Load_Min45, data = Temp, kernel="radial", probability=TRUE, gamma=0.0000001, cost=100000, scale = F)     # <-- 3045v2: MAE= | MAPE=
SVM_Data_3045 <- svm(Load_Now ~ Load_Min15 + Load_Day1B + Load_Min30 + Load_Min45, data = Temp, kernel="radial", probability=TRUE, gamma=0.00000001, cost=10000, scale = F)     # <-- 3045v3: MAE= | MAPE=
SVM_Data_3045 <- svm(Load_Now ~ Load_Min15 + Load_Day1B + Load_Min30 + Load_Min45, data = Temp, kernel="radial", probability=TRUE, gamma=0.0000000001, cost=1000, scale = F)    # <-- 3045v4: MAE= | MAPE=
SVM_Data_3045 <- svm(Load_Now ~ Load_Min15 + Load_Day1B + Load_Min30 + Load_Min45, data = Temp, kernel="radial", probability=TRUE, gamma=0.0000000001, cost=10000, scale = F)   # <-- 3045v5: MAE= | MAPE=
SVM_Data_3045 <- svm(Load_Now ~ Load_Min15 + Load_Day1B + Load_Min30 + Load_Min45, data = Temp, kernel="radial", probability=TRUE, gamma=0.0000000001, cost=100000, scale = F)  # <-- 3045v6: MAE= | MAPE=

# 1Day + 1Day-15Min + 1Day+15Min
SVM_Data_1Day1 <- svm(Load_Now ~ Load_Min15 + Load_Day1B + Load_Day1B15min + Load_Day1Bp15min, data = Temp, kernel="radial", probability=TRUE, gamma=0.0000001, cost=10000, scale = F)      # <-- 1Dayv1: MAE= | MAPE=
SVM_Data_1Day2 <- svm(Load_Now ~ Load_Min15 + Load_Day1B + Load_Day1B15min + Load_Day1Bp15min, data = Temp, kernel="radial", probability=TRUE, gamma=0.0000001, cost=100000, scale = F)     # <-- 1Dayv2: MAE= | MAPE=
SVM_Data_1Day3 <- svm(Load_Now ~ Load_Min15 + Load_Day1B + Load_Day1B15min + Load_Day1Bp15min, data = Temp, kernel="radial", probability=TRUE, gamma=0.00000001, cost=10000, scale = F)     # <-- 1Dayv3: MAE= | MAPE=
SVM_Data_1Day4 <- svm(Load_Now ~ Load_Min15 + Load_Day1B + Load_Day1B15min + Load_Day1Bp15min, data = Temp, kernel="radial", probability=TRUE, gamma=0.0000000001, cost=1000, scale = F)    # <-- 1Dayv4: MAE= | MAPE=
SVM_Data_1Day5 <- svm(Load_Now ~ Load_Min15 + Load_Day1B + Load_Day1B15min + Load_Day1Bp15min, data = Temp, kernel="radial", probability=TRUE, gamma=0.0000000001, cost=10000, scale = F)   # <-- 1Dayv5: MAE= | MAPE=
SVM_Data_1Day6 <- svm(Load_Now ~ Load_Min15 + Load_Day1B + Load_Day1B15min + Load_Day1Bp15min, data = Temp, kernel="radial", probability=TRUE, gamma=0.0000000001, cost=100000, scale = F)  # <-- 1Dayv6: MAE= | MAPE=


# --- kNN --- #
kNN_wyniki <- data.frame()
for(zz in c(40:45)){
  kNN_model <- knn.reg(cbind(Temp$Load_Min15, Temp$Load_Day1B), newdata[,4:5] ,y=Temp$Load_Now, k=zz)
  newdata$kNN_Pred_Shift <- c(as.numeric(as.character(kNN_model$pred[2:length(kNN_model$pred)])), as.numeric(as.character(kNN_model$pred[length(kNN_model$pred)])))
  MAE_kNN <- sum(abs(newdata$Load_Now - newdata$kNN_Pred_Shift)) / length(newdata$Load_Now)
  MAPE_kNN <- sum(abs(newdata$Load_Now - newdata$kNN_Pred_Shift) / newdata$Load_Now) / length(newdata$Load_Now) * 100
  kNN_wyniki <- rbind(kNN_wyniki, c(zz, MAE_kNN, MAPE_kNN))
  cat(zz,">")
}
colnames(kNN_wyniki) <- c("k", "MAE", "MAPE")
View(kNN_wyniki)                                    ### Komentarz: Best kNN dla k=c(40:45) ==> k=43 --> MAE=21.241 | MAPE=0.443


# --- Nowe dane --- #
newdata <- HU_Data %>% filter(utc_timestamp > (rok_start2) & utc_timestamp < end2)
newdata$godzina <- hour(newdata$utc_timestamp)

Min15Before <- HU_Data %>% filter(utc_timestamp > (rok_start2-minutes(15)) & utc_timestamp < end2-minutes(15)) %>% select(HU_load_actual_entsoe_transparency)
newdata$Load_Min15 <- cbind(as.matrix(Min15Before))
rm(Min15Before)

Min30Before <- HU_Data %>% filter(utc_timestamp > (rok_start2-minutes(30)) & utc_timestamp < end2-minutes(30)) %>% select(HU_load_actual_entsoe_transparency)
newdata$Load_Min30 <- cbind(as.matrix(Min30Before))
rm(Min30Before)
names(newdata)[37] <- "Load_Day1Bp15min"


Min45Before <- HU_Data %>% filter(utc_timestamp > (rok_start2-minutes(45)) & utc_timestamp < end2-minutes(45)) %>% select(HU_load_actual_entsoe_transparency)
newdata$Load_Min45 <- cbind(as.matrix(Min45Before))   # Columna 31
rm(Min45Before)

Day1Before <- HU_Data %>% filter(utc_timestamp > (rok_start2-days(1)) & utc_timestamp < end2-days(1)) %>% select(HU_load_actual_entsoe_transparency)
newdata$Day1B <- cbind(as.matrix(Day1Before))
# newdata$Day1B <- as.matrix(newdata$Day1B)
rm(Day1Before)

Day1BeforeM15Min <- HU_Data %>% filter(utc_timestamp > (rok_start2-days(1)-minutes(15)) & utc_timestamp < end2-days(1)-minutes(15)) %>% select(HU_load_actual_entsoe_transparency)
newdata$Load_Day1B15min <- cbind(as.matrix(Day1BeforeM15Min))      # Column: 36
# newdata$Day1B <- as.matrix(newdata$Day1B)
rm(Day1BeforeM15Min)

Day1BeforeP15Min <- HU_Data %>% filter(utc_timestamp > (rok_start2-days(1)+minutes(15)) & utc_timestamp < end2-days(1)+minutes(15)) %>% select(HU_load_actual_entsoe_transparency)
newdata$Load_Day1Bp15min <- cbind(as.matrix(Day1BeforeP15Min))     # Column: 37
# newdata$Day1B <- as.matrix(newdata$Day1B)
rm(Day1BeforeP15Min)

# newdata$Class <- as.integer(newdata$HU_load_actual_entsoe_transparency - minMW)
newdata$Class <- newdata$HU_load_actual_entsoe_transparency
# newdata$Class <- as.factor(newdata$Class)
# newdata$Class <- factor(as.integer(newdata$HU_load_actual_entsoe_transparency))

# colnames(newdata) <- c("Time", "Load_Now", "Hour", "Load_Min15", "Load_Day1B", "Class", "ClassPredict", "ClassPre_Shift")
colnames(newdata) <- c("Time", "Load_Now", "Hour", "Load_Min15", "Load_Day1B", "Class")
# newdata[,4:5]
newdata$Hour <- as.factor(newdata$Hour)



# --- Predykcja na nowym roku --- #
newdata$ClassPredictAnova <- predict(Tree_Anova, newdata[,4:5])
newdata$ClassPredictAnova30 <- predict(Tree_Anova_30, newdata[,c(4,5,26)])
newdata$ClassPredictAnovaH <- predict(Tree_Anova_H, newdata[,3:5])
newdata$ClassPredictAnovaH30 <- predict(Tree_Anova_H30, newdata[,c(3:5,26)])
newdata$ClassPredictAnova3045 <- predict(Tree_Anova_3045, newdata[,c(4,5,26,31)])
newdata$ClassPredictAnova1Day <- predict(Tree_Anova_1Day, newdata[,c(4,5,36,37)])


newdata$RFpred <- predict(rf, newdata = newdata[,4:5])                 # Load_Min15 + Load_Day1B
newdata$RF100pred <- predict(rf100, newdata = newdata[,4:5])           # Load_Min15 + Load_Day1B
newdata$RFpred_H <- predict(rf_H, newdata = newdata[,3:5])             # Load_Min15 + Load_Day1B + Hour
newdata$RF100pred_H <- predict(rf100_H, newdata = newdata[,3:5])       # Load_Min15 + Load_Day1B + Hour
newdata$RFpred_H30 <- predict(rf_H30, newdata[,c(3:5,26)])             # Load_Min15 + Load_Day1B + Hour + Load_Min30
newdata$RF100pred_H30 <- predict(rf100_H30, newdata[,c(3:5,26)])       # Load_Min15 + Load_Day1B + Hour + Load_Min30
newdata$RFpred_30 <- predict(rf_30, newdata[,c(4:5,26)])               # Load_Min15 + Load_Day1B + Load_Min30
newdata$RF100pred_30 <- predict(rf100_30, newdata[,c(4:5,26)])         # Load_Min15 + Load_Day1B + Load_Min30
newdata$RFpred_3045 <- predict(rf_3045, newdata[,c(4,5,26,31)])               # Load_Min15 + Load_Day1B + Load_Min30
newdata$RF100pred_3045 <- predict(rf100_3045, newdata[,c(4,5,26,31)])         # Load_Min15 + Load_Day1B + Load_Min30
newdata$RFpred_1Day <- predict(rf_1Day, newdata[,c(4,5,36,37)])               # Load_Min15 + Load_Day1B + Load_Min30
newdata$RF100pred_1Day <- predict(rf100_1Day, newdata[,c(4,5,36,37)])         # Load_Min15 + Load_Day1B + Load_Min30


newdata$SVM <- predict(SVM_Data, newdata[,4:5])                       # Load_Min15 + Load_Day1B
newdata$SVM_Tune_v1 <- predict(SVM_Tune_v1$best.model, newdata[,4:5])                       # Load_Min15 + Load_Day1B

newdata$SVM_H <- predict(SVM_Data_H, newdata[,3:5])                   # Load_Min15 + Load_Day1B + Hour
newdata$SVM_H30 <- predict(SVM_Data_H30, newdata[,c(3:5,26)])         # Load_Min15 + Load_Day1B + Hour + Load_Min30

newdata$SVM_30 <- predict(SVM_Data_30, newdata[,c(4,5,26)])           # Load_Min15 + Load_Day1B + Load_Min30
newdata$SVM_3045 <- predict(SVM_Data_3045, newdata[,c(4,5,26,31)])    # Load_Min15 + Load_Day1B + Load_Min30 + Load_Min45

newdata$SVM_1Day <- predict(SVM_Data_1Day6, newdata[,c(4,5,36,37)])    # Load_Min15 + Load_Day1B + Load_Min30 + Load_Min45


# --- Shift predykcji o jedno w górę -> lepsza korelacja --- #
# newdata$ClassPre_Shift <- c(as.numeric(as.character(newdata$ClassPredict[2:length(newdata$ClassPredict)])), as.numeric(as.character(newdata$ClassPredict[length(newdata$ClassPredict)])))
newdata$ClassPreAnova_Shift <- c(as.numeric(as.character(newdata$ClassPredictAnova[2:length(newdata$ClassPredictAnova)])), as.numeric(as.character(newdata$ClassPredictAnova[length(newdata$ClassPredictAnova)])))
newdata$ClassPreAnova30_Shift <- c(as.numeric(as.character(newdata$ClassPredictAnova30[2:length(newdata$ClassPredictAnova30)])), as.numeric(as.character(newdata$ClassPredictAnova30[length(newdata$ClassPredictAnova30)])))
newdata$ClassPreAnovaH_Shift <- c(as.numeric(as.character(newdata$ClassPredictAnovaH[2:length(newdata$ClassPredictAnovaH)])), as.numeric(as.character(newdata$ClassPredictAnovaH[length(newdata$ClassPredictAnovaH)])))
newdata$ClassPreAnovaH30_Shift <- c(as.numeric(as.character(newdata$ClassPredictAnovaH30[2:length(newdata$ClassPredictAnovaH30)])), as.numeric(as.character(newdata$ClassPredictAnovaH30[length(newdata$ClassPredictAnovaH30)])))
newdata$ClassPreAnova3045_Shift <- c(as.numeric(as.character(newdata$ClassPredictAnova3045[2:length(newdata$ClassPredictAnova3045)])), as.numeric(as.character(newdata$ClassPredictAnova3045[length(newdata$ClassPredictAnova3045)])))
newdata$ClassPreAnova1Day_Shift <- c(as.numeric(as.character(newdata$ClassPredictAnova1Day[2:length(newdata$ClassPredictAnova1Day)])), as.numeric(as.character(newdata$ClassPredictAnova1Day[length(newdata$ClassPredictAnova1Day)])))


newdata$RFPre_Shift <- c(as.numeric(as.character(newdata$RFpred[2:length(newdata$RFpred)])), as.numeric(as.character(newdata$RFpred[length(newdata$RFpred)])))
newdata$RF100Pre_Shift <- c(as.numeric(as.character(newdata$RF100pred[2:length(newdata$RF100pred)])), as.numeric(as.character(newdata$RF100pred[length(newdata$RF100pred)])))
newdata$RFPre_H_Shift <- c(as.numeric(as.character(newdata$RFpred_H[2:length(newdata$RFpred_H)])), as.numeric(as.character(newdata$RFpred_H[length(newdata$RFpred_H)])))
newdata$RF100Pre_H_Shift <- c(as.numeric(as.character(newdata$RF100pred_H[2:length(newdata$RF100pred_H)])), as.numeric(as.character(newdata$RF100pred_H[length(newdata$RF100pred_H)])))
newdata$RFPre_H30_Shift <- c(as.numeric(as.character(newdata$RFpred_H30[2:length(newdata$RFpred_H30)])), as.numeric(as.character(newdata$RFpred_H30[length(newdata$RFpred_H30)])))
newdata$RF100Pre_H30_Shift <- c(as.numeric(as.character(newdata$RF100pred_H30[2:length(newdata$RF100pred_H30)])), as.numeric(as.character(newdata$RF100pred_H30[length(newdata$RF100pred_H30)])))
newdata$RFPre_30_Shift <- c(as.numeric(as.character(newdata$RFpred_30[2:length(newdata$RFpred_30)])), as.numeric(as.character(newdata$RFpred_30[length(newdata$RFpred_30)])))
newdata$RF100Pre_30_Shift <- c(as.numeric(as.character(newdata$RF100pred_30[2:length(newdata$RF100pred_30)])), as.numeric(as.character(newdata$RF100pred_30[length(newdata$RF100pred_30)])))
newdata$RFPre_3045_Shift <- c(as.numeric(as.character(newdata$RFpred_3045[2:length(newdata$RFpred_3045)])), as.numeric(as.character(newdata$RFpred_3045[length(newdata$RFpred_3045)])))
newdata$RF100Pre_3045_Shift <- c(as.numeric(as.character(newdata$RF100pred_3045[2:length(newdata$RF100pred_3045)])), as.numeric(as.character(newdata$RF100pred_3045[length(newdata$RF100pred_3045)])))
newdata$RFPre_1Day_Shift <- c(as.numeric(as.character(newdata$RFpred_1Day[2:length(newdata$RFpred_1Day)])), as.numeric(as.character(newdata$RFpred_1Day[length(newdata$RFpred_1Day)])))
newdata$RF100Pre_1Day_Shift <- c(as.numeric(as.character(newdata$RF100pred_1Day[2:length(newdata$RF100pred_1Day)])), as.numeric(as.character(newdata$RF100pred_1Day[length(newdata$RF100pred_1Day)])))


newdata$SVM_Shift <- c(as.numeric(as.character(newdata$SVM[2:length(newdata$SVM)])), as.numeric(as.character(newdata$SVM[length(newdata$SVM)])))
newdata$SVM_Tune_v1_Shift <- c(as.numeric(as.character(newdata$SVM_Tune_v1[2:length(newdata$SVM_Tune_v1)])), as.numeric(as.character(newdata$SVM_Tune_v1[length(newdata$SVM_Tune_v1)])))

newdata$SVM_H_Shift <- c(as.numeric(as.character(newdata$SVM_H[2:length(newdata$SVM_H)])), as.numeric(as.character(newdata$SVM_H[length(newdata$SVM_H)])))
newdata$SVM_H30_Shift <- c(as.numeric(as.character(newdata$SVM_H30[2:length(newdata$SVM_H30)])), as.numeric(as.character(newdata$SVM_H30[length(newdata$SVM_H30)])))

newdata$SVM_30_Shift <- c(as.numeric(as.character(newdata$SVM_30[2:length(newdata$SVM_30)])), as.numeric(as.character(newdata$SVM_30[length(newdata$SVM_30)])))
newdata$SVM_3045_Shift <- c(as.numeric(as.character(newdata$SVM_3045[2:length(newdata$SVM_3045)])), as.numeric(as.character(newdata$SVM_3045[length(newdata$SVM_3045)])))

newdata$SVM_1Day_Shift <- c(as.numeric(as.character(newdata$SVM_1Day[2:length(newdata$SVM_1Day)])), as.numeric(as.character(newdata$SVM_1Day[length(newdata$SVM_1Day)])))


# --- MAL i MAE -> sprawdzenie odchylenia prognozy --- #
# MAE <- sum(abs(newdata$Load_Now - newdata$ClassPre_Shift)) / length(newdata$Load_Now)
# MAPE <- sum(abs(newdata$Load_Now - newdata$ClassPre_Shift) / newdata$Load_Now) / length(newdata$Load_Now) * 100

MAE_Anova <- sum(abs(newdata$Load_Now - newdata$ClassPreAnova_Shift)) / length(newdata$Load_Now)
MAPE_Anova <- sum(abs(newdata$Load_Now - newdata$ClassPreAnova_Shift) / newdata$Load_Now) / length(newdata$Load_Now) * 100
MAE_Anova30 <- sum(abs(newdata$Load_Now - newdata$ClassPreAnova30_Shift)) / length(newdata$Load_Now)
MAPE_Anova30 <- sum(abs(newdata$Load_Now - newdata$ClassPreAnova30_Shift) / newdata$Load_Now) / length(newdata$Load_Now) * 100
MAE_Anova3045 <- sum(abs(newdata$Load_Now - newdata$ClassPreAnova3045_Shift)) / length(newdata$Load_Now)
MAPE_Anova3045 <- sum(abs(newdata$Load_Now - newdata$ClassPreAnova3045_Shift) / newdata$Load_Now) / length(newdata$Load_Now) * 100
MAE_Anova1Day <- sum(abs(newdata$Load_Now - newdata$ClassPreAnova1Day_Shift)) / length(newdata$Load_Now)
MAPE_Anova1Day <- sum(abs(newdata$Load_Now - newdata$ClassPreAnova1Day_Shift) / newdata$Load_Now) / length(newdata$Load_Now) * 100

MAE_AnovaH <- sum(abs(newdata$Load_Now - newdata$ClassPreAnovaH_Shift)) / length(newdata$Load_Now)
MAPE_AnovaH <- sum(abs(newdata$Load_Now - newdata$ClassPreAnovaH_Shift) / newdata$Load_Now) / length(newdata$Load_Now) * 100
MAE_AnovaH30 <- sum(abs(newdata$Load_Now - newdata$ClassPreAnovaH30_Shift)) / length(newdata$Load_Now)
MAPE_AnovaH30 <- sum(abs(newdata$Load_Now - newdata$ClassPreAnovaH30_Shift) / newdata$Load_Now) / length(newdata$Load_Now) * 100

### Random Forest ###
MAE_RF <- sum(abs(newdata$Load_Now - newdata$RFPre_Shift)) / length(newdata$Load_Now)
MAPE_RF <- sum(abs(newdata$Load_Now - newdata$RFPre_Shift) / newdata$Load_Now) / length(newdata$Load_Now) * 100
MAE_RF100 <- sum(abs(newdata$Load_Now - newdata$RF100Pre_Shift)) / length(newdata$Load_Now)
MAPE_RF100 <- sum(abs(newdata$Load_Now - newdata$RF100Pre_Shift) / newdata$Load_Now) / length(newdata$Load_Now) * 100
# --- + Hour --- #
MAE_RF_H <- sum(abs(newdata$Load_Now - newdata$RFPre_H_Shift)) / length(newdata$Load_Now)
MAPE_RF_H <- sum(abs(newdata$Load_Now - newdata$RFPre_H_Shift) / newdata$Load_Now) / length(newdata$Load_Now) * 100
MAE_RF100_H <- sum(abs(newdata$Load_Now - newdata$RF100Pre_H_Shift)) / length(newdata$Load_Now)
MAPE_RF100_H <- sum(abs(newdata$Load_Now - newdata$RF100Pre_H_Shift) / newdata$Load_Now) / length(newdata$Load_Now) * 100
MAE_RF_H30 <- sum(abs(newdata$Load_Now - newdata$RFPre_H30_Shift)) / length(newdata$Load_Now)
MAPE_RF_H30 <- sum(abs(newdata$Load_Now - newdata$RFPre_H30_Shift) / newdata$Load_Now) / length(newdata$Load_Now) * 100
MAE_RF100_H30 <- sum(abs(newdata$Load_Now - newdata$RF100Pre_H30_Shift)) / length(newdata$Load_Now)
MAPE_RF100_H30 <- sum(abs(newdata$Load_Now - newdata$RF100Pre_H30_Shift) / newdata$Load_Now) / length(newdata$Load_Now) * 100
MAE_RF_30 <- sum(abs(newdata$Load_Now - newdata$RFPre_30_Shift)) / length(newdata$Load_Now)
MAPE_RF_30 <- sum(abs(newdata$Load_Now - newdata$RFPre_30_Shift) / newdata$Load_Now) / length(newdata$Load_Now) * 100
MAE_RF100_30 <- sum(abs(newdata$Load_Now - newdata$RF100Pre_30_Shift)) / length(newdata$Load_Now)
MAPE_RF100_30 <- sum(abs(newdata$Load_Now - newdata$RF100Pre_30_Shift) / newdata$Load_Now) / length(newdata$Load_Now) * 100
MAE_RF_3045 <- sum(abs(newdata$Load_Now - newdata$RFPre_3045_Shift)) / length(newdata$Load_Now)
MAPE_RF_3045 <- sum(abs(newdata$Load_Now - newdata$RFPre_3045_Shift) / newdata$Load_Now) / length(newdata$Load_Now) * 100
MAE_RF100_3045 <- sum(abs(newdata$Load_Now - newdata$RF100Pre_3045_Shift)) / length(newdata$Load_Now)
MAPE_RF100_3045 <- sum(abs(newdata$Load_Now - newdata$RF100Pre_3045_Shift) / newdata$Load_Now) / length(newdata$Load_Now) * 100
MAE_RF_1Day <- sum(abs(newdata$Load_Now - newdata$RFPre_1Day_Shift)) / length(newdata$Load_Now)
MAPE_RF_1Day <- sum(abs(newdata$Load_Now - newdata$RFPre_1Day_Shift) / newdata$Load_Now) / length(newdata$Load_Now) * 100
MAE_RF100_1Day <- sum(abs(newdata$Load_Now - newdata$RF100Pre_1Day_Shift)) / length(newdata$Load_Now)
MAPE_RF100_1Day <- sum(abs(newdata$Load_Now - newdata$RF100Pre_1Day_Shift) / newdata$Load_Now) / length(newdata$Load_Now) * 100


### SVM ###
MAE_SVM_v13 <- sum(abs(newdata$Load_Now - newdata$SVM_Shift)) / length(newdata$Load_Now)
MAPE_SVM_v13 <- sum(abs(newdata$Load_Now - newdata$SVM_Shift) / newdata$Load_Now) / length(newdata$Load_Now) * 100
MAE_SVM_Tune_v1_1 <- sum(abs(newdata$Load_Now - newdata$SVM_Tune_v1_Shift)) / length(newdata$Load_Now)
MAPE_SVM_Tune_v1_1 <- sum(abs(newdata$Load_Now - newdata$SVM_Tune_v1_Shift) / newdata$Load_Now) / length(newdata$Load_Now) * 100


# --- + Hour --- #
MAE_SVM_H_v6 <- sum(abs(newdata$Load_Now - newdata$SVM_H_Shift)) / length(newdata$Load_Now)
MAPE_SVM_H_v6 <- sum(abs(newdata$Load_Now - newdata$SVM_H_Shift) / newdata$Load_Now) / length(newdata$Load_Now) * 100

# --- + Hour + Load_Min30 --- #
MAE_SVM_H30_v6 <- sum(abs(newdata$Load_Now - newdata$SVM_H30_Shift)) / length(newdata$Load_Now)
MAPE_SVM_H30_v6 <- sum(abs(newdata$Load_Now - newdata$SVM_H30_Shift) / newdata$Load_Now) / length(newdata$Load_Now) * 100


# --- + Load_Min30 --- #
MAE_SVM_30_v6 <- sum(abs(newdata$Load_Now - newdata$SVM_30_Shift)) / length(newdata$Load_Now)
MAPE_SVM_30_v6 <- sum(abs(newdata$Load_Now - newdata$SVM_30_Shift) / newdata$Load_Now) / length(newdata$Load_Now) * 100


# --- + Load_Min30 + Load_Min45 --- #
MAE_SVM_3045_v6 <- sum(abs(newdata$Load_Now - newdata$SVM_3045_Shift)) / length(newdata$Load_Now)
MAPE_SVM_3045_v6 <- sum(abs(newdata$Load_Now - newdata$SVM_3045_Shift) / newdata$Load_Now) / length(newdata$Load_Now) * 100

# --- + Day1B15Min + Day1BP15Min --- #
MAE_SVM_1Day_v6 <- sum(abs(newdata$Load_Now - newdata$SVM_1Day_Shift)) / length(newdata$Load_Now)
MAPE_SVM_1Day_v6 <- sum(abs(newdata$Load_Now - newdata$SVM_1Day_Shift) / newdata$Load_Now) / length(newdata$Load_Now) * 100



# --- Prognoza Naiwna --- #
Naiwna <- HU_Data %>% filter(utc_timestamp > (rok_start2) & utc_timestamp < end2)
Naiwna$godzina <- hour(Naiwna$utc_timestamp)
Naiwna$Predict <- HU_Data %>% filter(utc_timestamp > (rok_start2-minutes(15)) & utc_timestamp < end2-minutes(15)) %>% select(HU_load_actual_entsoe_transparency)
Naiwna$Predict <- as.matrix(Naiwna$Predict)
colnames(Naiwna) <- c("Time", "Load_Now", "Hour", "Predict")

MAE_Naiwna <- sum(abs(Naiwna$Load_Now - Naiwna$Predict)) / length(Naiwna$Load_Now)
MAPE_Naiwna <- sum(abs(Naiwna$Load_Now - Naiwna$Predict) / Naiwna$Load_Now) / length(Naiwna$Load_Now) * 100




  
  