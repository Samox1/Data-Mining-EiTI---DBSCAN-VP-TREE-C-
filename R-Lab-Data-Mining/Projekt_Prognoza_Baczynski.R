# Projekt na przedmiot: Wybrane zagadnienia uczenia maszyn (CSZ)
# Tytuł projektu: 
# Autor: Szymon Baczyński 
# Indeks: KD-158

# Ze wzgledu na czas wykonywanych obliczen, niektore linie kodu 
# zostaly zakomentowane w celu ograniczenia czasu wykonywania skryptu.
# Wybrane zostaly najlepsze modele jako reprezentanci swoich klasyfikatorow.

library(readr)
library(MASS)
library(e1071)
library(rpart)
library(randomForest)
library(rpart.plot)
library(dplyr)
library(lubridate)
library(data.table)
library(stringr)
library(caret)
library(plotly)

# Import danych:
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
names(HU_Data)[2] <- "Load_Now"

# Wstepna obrobka danych:
# --- Utworzenie nowej tablicy pod drzewo --- #
Temp <- HU_Data %>% filter(utc_timestamp > (rok_start1) & utc_timestamp < end1)

Temp$Hour <- hour(Temp$utc_timestamp)
Temp$Hour <- as.factor(Temp$Hour)

Min15Before <- HU_Data %>% filter(utc_timestamp > (rok_start1-minutes(15)) & utc_timestamp < end1-minutes(15)) %>% select(Load_Now)
Temp$Load_Min15 <- cbind(as.matrix(Min15Before))
rm(Min15Before)

Min30Before <- HU_Data %>% filter(utc_timestamp > (rok_start1-minutes(30)) & utc_timestamp < end1-minutes(30)) %>% select(Load_Now)
Temp$Load_Min30 <- cbind(as.matrix(Min30Before))
rm(Min30Before)

Min45Before <- HU_Data %>% filter(utc_timestamp > (rok_start1-minutes(45)) & utc_timestamp < end1-minutes(45)) %>% select(Load_Now)
Temp$Load_Min45 <- cbind(as.matrix(Min45Before))
rm(Min45Before)

Day1Before <- HU_Data %>% filter(utc_timestamp > (rok_start1-days(1)) & utc_timestamp < end1-days(1)) %>% select(Load_Now)
Temp$Load_Day1B <- cbind(as.matrix(Day1Before))
rm(Day1Before)

Day1Before15Min <- HU_Data %>% filter(utc_timestamp > (rok_start1-days(1)-minutes(15)) & utc_timestamp < end1-days(1)-minutes(15)) %>% select(Load_Now)
Temp$Load_Day1B15min <- cbind(as.matrix(Day1Before15Min))
rm(Day1Before15Min)

Day1Beforep15Min <- HU_Data %>% filter(utc_timestamp > (rok_start1-days(1)+minutes(15)) & utc_timestamp < end1-days(1)+minutes(15)) %>% select(Load_Now)
Temp$Load_Day1Bp15min <- cbind(as.matrix(Day1Beforep15Min))
rm(Day1Beforep15Min)


# --- Przygotowanie danych testowych:
newdata <- HU_Data %>% filter(utc_timestamp > (rok_start2) & utc_timestamp < end2)
newdata$Hour <- hour(newdata$utc_timestamp)
newdata$Hour <- as.factor(newdata$Hour)

Min15Before <- HU_Data %>% filter(utc_timestamp > (rok_start2-minutes(15)) & utc_timestamp < end2-minutes(15)) %>% select(Load_Now)
newdata$Load_Min15 <- cbind(as.matrix(Min15Before))
rm(Min15Before)

Min30Before <- HU_Data %>% filter(utc_timestamp > (rok_start2-minutes(30)) & utc_timestamp < end2-minutes(30)) %>% select(Load_Now)
newdata$Load_Min30 <- cbind(as.matrix(Min30Before))
rm(Min30Before)

Min45Before <- HU_Data %>% filter(utc_timestamp > (rok_start2-minutes(45)) & utc_timestamp < end2-minutes(45)) %>% select(Load_Now)
newdata$Load_Min45 <- cbind(as.matrix(Min45Before))
rm(Min45Before)

Day1Before <- HU_Data %>% filter(utc_timestamp > (rok_start2-days(1)) & utc_timestamp < end2-days(1)) %>% select(Load_Now)
newdata$Load_Day1B <- cbind(as.matrix(Day1Before))
rm(Day1Before)

Day1BeforeM15Min <- HU_Data %>% filter(utc_timestamp > (rok_start2-days(1)-minutes(15)) & utc_timestamp < end2-days(1)-minutes(15)) %>% select(Load_Now)
newdata$Load_Day1B15min <- cbind(as.matrix(Day1BeforeM15Min))
rm(Day1BeforeM15Min)

Day1BeforeP15Min <- HU_Data %>% filter(utc_timestamp > (rok_start2-days(1)+minutes(15)) & utc_timestamp < end2-days(1)+minutes(15)) %>% select(Load_Now)
newdata$Load_Day1Bp15min <- cbind(as.matrix(Day1BeforeP15Min))
rm(Day1BeforeP15Min)



# Przygotowanie modelu:

Tree_Anova_Tune <- tune(rpart, Load_Now ~ Load_Min15 + Load_Day1B, data = Temp, validation.x = newdata, validation.y = newdata$Load_Now,ranges = list(minsplit=c(1:5), minbucket=c(1:5), cp=10^(c(-4):(-10))))
newdata$ClassPredictAnova <- predict(Tree_Anova_Tune$best.model, newdata)
newdata$ClassPreAnova_Shift <- c(as.numeric(as.character(newdata$ClassPredictAnova[2:length(newdata$ClassPredictAnova)])), as.numeric(as.character(newdata$ClassPredictAnova[length(newdata$ClassPredictAnova)])))
MAE_Anova <- sum(abs(newdata$Load_Now - newdata$ClassPreAnova_Shift)) / length(newdata$Load_Now)
MAPE_Anova <- sum(abs(newdata$Load_Now - newdata$ClassPreAnova_Shift) / newdata$Load_Now) / length(newdata$Load_Now) * 100

Tree_Anova_Tune <- tune(rpart, Load_Now ~ Load_Min15 + Load_Min30 + Load_Day1B, data = Temp, validation.x = newdata, validation.y = newdata$Load_Now,ranges = list(minsplit=c(1:5), minbucket=c(1:5), cp=10^(c(-4):(-10))))
newdata$ClassPredictAnova <- predict(Tree_Anova_Tune$best.model, newdata)
newdata$ClassPreAnova_Shift <- c(as.numeric(as.character(newdata$ClassPredictAnova[2:length(newdata$ClassPredictAnova)])), as.numeric(as.character(newdata$ClassPredictAnova[length(newdata$ClassPredictAnova)])))
MAE_Anova_30 <- sum(abs(newdata$Load_Now - newdata$ClassPreAnova_Shift)) / length(newdata$Load_Now)
MAPE_Anova_30 <- sum(abs(newdata$Load_Now - newdata$ClassPreAnova_Shift) / newdata$Load_Now) / length(newdata$Load_Now) * 100

Tree_Anova_Tune <- tune(rpart, Load_Now ~ Load_Min15 + Load_Min30 + Load_Min45 + Load_Day1B, data = Temp, validation.x = newdata, validation.y = newdata$Load_Now,ranges = list(minsplit=c(1:5), minbucket=c(1:5), cp=10^(c(-4):(-10))))
newdata$ClassPredictAnova <- predict(Tree_Anova_Tune$best.model, newdata)
newdata$ClassPreAnova_Shift <- c(as.numeric(as.character(newdata$ClassPredictAnova[2:length(newdata$ClassPredictAnova)])), as.numeric(as.character(newdata$ClassPredictAnova[length(newdata$ClassPredictAnova)])))
MAE_Anova_3045 <- sum(abs(newdata$Load_Now - newdata$ClassPreAnova_Shift)) / length(newdata$Load_Now)
MAPE_Anova_3045 <- sum(abs(newdata$Load_Now - newdata$ClassPreAnova_Shift) / newdata$Load_Now) / length(newdata$Load_Now) * 100

Tree_Anova_Tune <- tune(rpart, Load_Now ~ Load_Min15 + Load_Day1B + Load_Day1B15min + Load_Day1Bp15min, data = Temp, validation.x = newdata, validation.y = newdata$Load_Now,ranges = list(minsplit=c(1:5), minbucket=c(1:5), cp=10^(c(-4):(-10))))
newdata$ClassPredictAnova <- predict(Tree_Anova_Tune$best.model, newdata)
newdata$ClassPreAnova_Shift <- c(as.numeric(as.character(newdata$ClassPredictAnova[2:length(newdata$ClassPredictAnova)])), as.numeric(as.character(newdata$ClassPredictAnova[length(newdata$ClassPredictAnova)])))
MAE_Anova_1Day <- sum(abs(newdata$Load_Now - newdata$ClassPreAnova_Shift)) / length(newdata$Load_Now)
MAPE_Anova_1Day <- sum(abs(newdata$Load_Now - newdata$ClassPreAnova_Shift) / newdata$Load_Now) / length(newdata$Load_Now) * 100


