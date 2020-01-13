# Zajecia nr 2
# Data: 8.01.2020
# Sequential Rules

# Prowadzacy: Grzegorz Protaziuk
# script SD dataminig lab2 2019Z
#1. Data reading and analysis
#2. Frequent sequences discovery
#3. Sequential rules discovery

#setting working directory - adjust a path to your directory with a dataset

rm(list = ls())

#setwd('/home/staff/gprotazi/');
library(arules)
library(arulesSequences)

#read data with sequences - tags assigned by users to items
download.file('http://staff.ii.pw.edu.pl/~gprotazi/dydaktyka/dane/tags.data','tags.data')
?read_baskets
dataSeq <- read_baskets(con = file("tags.data", "r"), info = c("sequenceID","eventID","SIZE"))

#summary
summary(dataSeq)
str(dataSeq)

#presenting data in data.frame form
frameS =   as(dataSeq,"data.frame")
View(frameS)

#information about data concerning times 
?timeFrequency
timeSeq  = as(dataSeq,"timedsequences")
freqT = timeFrequency(timeSeq, "times")
freqT

spanT= timeFrequency(timeSeq, "span")
spanT

#calculation of frequency of items
?itemFrequency
freqItem = itemFrequency(dataSeq)
#str(freqItem)
freqItem = sort(freqItem, decreasing = TRUE )

head(freqItem,20)
str(dataSeq)

#ids <- unique(dataSeq@itemsetInfo$sequenceID)
#transaction for in a given sequence
trans109 <- dataSeq[dataSeq@itemsetInfo$sequenceID == 109]
inspect(trans109)

#frequent sequences discovery
#parameters of Spade algorithm
?'SPparameter-class'
seqParam = new ("SPparameter",support = 0.003, maxsize = 5, mingap=1, maxgap = 3, maxlen = 5)
print(seqParam)

#execution of the algorithm
?cspade
patSeq= cspade(dataSeq,seqParam, control = list(verbose = TRUE, tidLists = FALSE, summary= TRUE))

#information about discoverd sequences
summary(patSeq)
#length(patSeq)

inspect(head(patSeq,100))
#patterns with more than one element
seq2elem <- patSeq[size(patSeq)>1]
length(seq2elem)
inspect(seq2elem)


#patters supported by a given sequence of transactions(without time constraints))
inspect(patSeq[which(support(patSeq,trans109,type='absolute') >0)])

#searching for patterns with given items
?subset
?match

seqI = subset(patSeq, x %ein% c('design','art'))
inspect(seqI)
View(as(seqI,"data.frame"))

#execution the algorithm with different parameters
seqParam1 = new ("SPparameter",support = 0.001, maxsize = 5, mingap=1, maxgap = 3, maxlen = 5)
patSeq1= cspade(dataSeq,seqParam1, control = list(verbose = TRUE, tidLists = TRUE, summary= TRUE))

#selecting new discovered sequences
seqdiff = patSeq1[which(!(patSeq1 %in% patSeq))]
length(patSeq1)
length(seqdiff)

#discovery sequential rules
?ruleInduction
seqRules = ruleInduction(patSeq1,confidence = 0.8)

length(seqRules)
#summary of set of rules
summary(seqRules)
#view of rules and sequneces
inspect(seqRules)
#arulesSequences::size(seqRules)
size(lhs(seqRules)) 
#sequences in the left part of rules
inspect(lhs(seqRules))
#sequences in the right part of rules
inspect(rhs(seqRules))

#items in rules
seqRules@elements@items@itemInfo

#all patterns included in rules (from left and right parts of rules)
allSeq <- c(rhs(seqRules),lhs(seqRules))
allSeq <- unique(allSeq)
inspect(allSeq)
str(allSeq)

#selecting interesting rules
rulesI = subset(seqRules, rhs(seqRules) %in% c('design','webdesign'))
inspect(rulesI)
View(as(rulesI,"data.frame"))

rulesI = subset(seqRules, lhs(seqRules) %ein% c('design','webdesign'))
inspect(rulesI)

rulesI = subset(seqRules, lhs(seqRules) %ein% c('design','webdesign') & rhs(seqRules) %in% c('webdesign'))
inspect(rulesI)


#------------------------------------------------------------------------------------------------------------
# Laboratory task ################################################## http://staff.ii.pw.edu.pl/~gprotazi/index.html
#To find out if occurrence of hypoglycaemic symptoms may be predicted basedon other events.
#The value associated with the blood glucose measurement event should be discretized according to 
#information available on https://en.wikipedia.org/wiki/Blood_sugar_level#Normal_values
#or https://www.medicinenet.com/normal_blood_sugar_levels_in_adults_with_diabetes/article.htm

rm(list = ls())

# https://archive.ics.uci.edu/ml/datasets/diabetes
# Szukamy co wywoluje wydarzenie --> ID 65 = Hypoglycemic symptoms

# ------------------------------------------------------------------------------------------------------------------------------------
# Komentarz prowadzacego:
# 1) Potrzebujemy "id_xx" lub "id_xx + value" --> samo "value" nie mowi o zdarzeniu (najlepiej to zrobic na poczatku)
# 2) Dyskretyzacja wartosci podczas pomiarow --> jezeli ID 48-64 to zamienic to na np. 3 klasy - za niskie, w normie i za wysokie
# 3) Czas - nie obchodzi zdarzenie ktore wydarzylo sie miesiac wczesniej - ustalic maxgap na np. tydzien
# 4) Support - w tym przypadku minimalny support to przynajmniej kilka osob, u ktorych powtorzyla sie sekwencja zdarzen,
#                np. 2-3 osoby (czyli unikatowych "patient_id")
# ------------------------------------------------------------------------------------------------------------------------------------

download.file('http://staff.ii.pw.edu.pl/~gprotazi/dydaktyka/dane/diab_trans.data','diab_trans.data')
#reading data - into dataframe
diab.df <- read.csv("diab_trans.data", header=TRUE, stringsAsFactors = FALSE)
View(diab.df)
#example of saving data into a file  - removing the header line
write.table(diab.df, "diab_trans2.data", sep = "," , row.names = FALSE, col.names = FALSE )

#reading data in transactional form
diabSeq <- read_baskets(con = "diab_trans2.data", sep =",", info = c("sequenceID","eventID"))
View(as(diabSeq,"data.frame"))

summary(diabSeq)

#setting parameters
#time(eventid) in the diab_trans.data set is given as a number of seconds from some date.

seqParam = new ("SPparameter",support = 0.1, maxsize = 4, mingap=600, maxgap =172800, maxlen = 3 )
patSeq= cspade(diabSeq,seqParam, control = list(verbose = TRUE, tidLists = FALSE, summary= TRUE))

#discovery of sequential rules
seqRules = ruleInduction(patSeq,confidence = 0.8)

length(seqRules)
#summary for the set of rules
summary(seqRules)
#view of of rules
# inspect(head(seqRules,100))
kepa <- rhs(seqRules)
View(as(kepa,"data.frame"))

rulesI = subset(seqRules, lhs(seqRules) %ein% c('"id_65"') & rhs(seqRules) %in% c('"id_65"'))
inspect(rulesI)
View(as(rulesI,"data.frame"))


library(tidyverse)
# diab.df$glukoza <- diab.df %>% filter(code == "id_48" | code == "id_49" | code == "id_50" | code == "id_51" |
#                                       code == "id_52" | code == "id_53" | code == "id_54" | code == "id_55" |
#                                       code == "id_56" | code == "id_57" | code == "id_58" | code == "id_59" |
#                                       code == "id_60" | code == "id_61" | code == "id_62" | code == "id_63" |
#                                       code == "id_64") %>% select(value)
# 
# diab.df$glukoza <- diab.df 



