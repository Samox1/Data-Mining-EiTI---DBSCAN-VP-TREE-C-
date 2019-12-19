# Data Mining - EiTI
# 18.12.2019 - wtorek (10:15-14:00)
# Zajecia nr 1

# R server - komenda "iir2" odpala przegladarke i laczy sie z serwerem "r2d3" 
# Strona prowadzacego = http://staff.ii.pw.edu.pl/~gprotazi/index.html

rm(list = ls())

#Data mining 2019z Lab1.1
#WstÄpne przetwarzenia i analiza danych

#pakiety
library(arules)

#####################################
#wstÄpne przetwarzanie danych
#####################################
#uzupeĹnianie danych

#przykĹadowy wektor liczb losowych - funkcja runif
?runif
wLos = runif(100,1,10)
wLos
#pseudolowy wybĂłr 10 liczb naturlany - funkcja sample
#ustawienie ziarana generatora
set.seed(1234)
?sample
wek2 = sample(100,10)

#dane niekompletne
#przypisanie wybranym pozycjom wektora wartoĹci specjalnej NA
wLos[wek2] = NA 
wLos
#lista pozycji wektora ze specjalnÄ wartoĹciÄ NA  - uĹźycie funkcji which oraz is.na
#?which
#?is.na
which(is.na(wLos) == TRUE)  
#which(wLos == NA)# nie daje oczekiwanego wyniku

#kopiowanie danych
wLos2 <- wLos
mean(wLos)
#?mean
#wyliczenie Ĺredniej na podstawie istniejÄcych wartoĹci

srednia = mean(na.omit(wLos)) 
#srednia1 = mean(wLos, na.rm = TRUE);
srednia

#uzupeĹnienie danych - zastÄpienie specjalnej wartoĹci NA ĹredniÄ 
wLos2[is.na(wLos2)==TRUE] <- mean(na.omit(wLos)) 

#sprawdzenie
which(is.na(wLos2))
wLos2
wLos2 == wLos

##########################################################
# przykĹadowyc zbiĂłr danych 
#zbiĂłr danych AdultUCI jest dostÄpny m.in. w bibliotece arules

rm(list = ls())

data("AdultUCI") 

dim(AdultUCI)
#View(AdultUCI)

###########################################################

# selekcja atrybutĂłw i wierszy

#ususniÄcie atrubutĂłw z jednÄ wartoĹciÄ
#?sapply
delOneValued <- function(inputData11)
{
  res <- c(which(sapply(inputData11, function(x) {length(unique(x))}) == 1));
  if(length(res) > 0)         
  {
    data11 <- inputData11[,-res];
  }   
  else
  {
    data11 <-inputData11;
  }
  data11 
}

#testowanie

which(sapply(AdultUCI, function(x) {length(unique(x))}) == 1);
AdultUCI <- delOneValued(AdultUCI)

#dodanie dwĂłch atrybutĂłw
AdultUCI$Att1 = 0
AdultUCI$Att2.Cat = 'cat1'
colnames(AdultUCI)

which(sapply(AdultUCI, function(x) {length(unique(x))}) == 1);
AdultUCI <- delOneValued(AdultUCI)
colnames(AdultUCI)

###########################################################
#ususniÄcie atrubutĂłw z tylko unikalnymi wartoĹciami

delUniqueValueAtt<- function(inputData11)
{
  res <- c(which(sapply(inputData11, function(x) {length(unique(x))}) == nrow(inputData11)));
  if(length(res) >0)         
  {
    data11 <- inputData11[,-res];
  }   
  else
  {
    data11 <-inputData11;
  }
  
  data11 
}

#testowanie
which(sapply(AdultUCI, function(x) {length(unique(x))}) == nrow(AdultUCI))

AdultUCI <- delUniqueValueAtt(AdultUCI)

#dodanie dwĂłch atrybutĂłw
AdultUCI$Att1 = sample(nrow(AdultUCI),nrow(AdultUCI));
AdultUCI$Att2.Cat = sample.int(nrow(AdultUCI))

which(sapply(AdultUCI, function(x) {length(unique(x))}) == nrow(AdultUCI))

AdultUCI <- delUniqueValueAtt(AdultUCI)
colnames(AdultUCI)

###########################################################
#usuwanie duplikatĂłw
#?duplicated
#?anyDuplicated

which(duplicated(AdultUCI) == TRUE)
length(which(duplicated(AdultUCI) == TRUE))

AdultUCI.U <- unique(AdultUCI)
dim(AdultUCI)
dim(AdultUCI.U)

################################################3
#wstÄpna analiza danych

#?summary
summary(AdultUCI)

#atrybut liczbowy
summary(AdultUCI$age)
#odchylenie standardowe
sd(AdultUCI$age)
#kwantyle
quantile(AdultUCI$age)

#decyle
#?quantile
quantile(AdultUCI$age, prob = seq(0, 1, length = 11), type = 5)
#histogram
hist(AdultUCI$age)

#atrybut nominalny
#liczba wystÄpieĹ poszczegĂłlnych wartoĹci dla danego atrybutu
table(AdultUCI$education) 
#wizualizacja pokazyjÄca rozkĹad wartoĹci
pie(table(AdultUCI$"education"))
barplot(table(AdultUCI$"education"), cex.names=0.7)
plot(table(AdultUCI$"education"),ylab = "lb wystÄpieĹ")

#zapis rysunku do pliku
#?png
#png("nazwa_pliku.png", res=80, height=800, width=2400) 
#pie(table(AdultUCI$"education"))
#dev.off(); 


##############################################
#dyskretyzacja  - funkcja cut
#?cut

#podziaĹ na przedziaĹy wg zadanych granic
AdultUCI$age.d  <- ordered(cut(AdultUCI[["age"]], c(15,25,45,65,100)), labels = c("Young", "Middle-aged", "Senior", "Old"))

str(AdultUCI["age.d"])

#podziaĹ na przedziaĹy z wykorzystaniem mediany
AdultUCI$"capital_gain.d" <- ordered(cut(AdultUCI[[ "capital-gain"]], c(-Inf, 0, median(AdultUCI[[ "capital-gain"]][AdultUCI["capital-gain"] > 0 ]),Inf)), labels = c("None", "Low", "High"))

summary(AdultUCI$capital_gain.d)


# tabela krzyĹźowa
xtabs(formula = ~ age.d + capital_gain.d, data = AdultUCI)

#####################################
#zamiana atrybutu nominalnego na atrubuty binarne
rm(list = ls())

#ustawienie katalogu roboczego
#setwd('/home/staff/gprotazi/DataScience');

#pobranie przykĹadowych danych
download.file('http://archive.ics.uci.edu/ml/machine-learning-databases/car/car.data', 'car.data')

#wczytanie danych
cars = read.csv("car.data", header = FALSE,
                col.names = c('buying', 'maint', 'doors', 'persons', 'lug_boot','safety', "category") )
#struktura danych
str(cars)
# pokazanie danych
View(cars)

#funkcja generujÄca dodatkowe atrybuty dla kaĹźdej wartoĹci podanego atrybutu nominalnego zadanej ramki danych
genNumericAttrsForFactor = function(dataF, attName)
{
  #sprawdzenie typĂłw danych wejĹciowych  
  if(is.data.frame(dataF) == FALSE)
  {
    print("podany obiekt nie jest ramkÄ danych (dataFrame)");
    return(dataF);
  }
  
  if(is.character(attName) == FALSE)
  {
    print("wartoĹÄ podana jako nazwa atrybutu nie jest napisem");
    return(dataF);    
  }
  
  if(is.factor(dataF[[attName]]) == FALSE)
  {
    print("podany atrybut nie jest atrybutem o wartoĹciach nominalnych");
    return(dataF);
  }
  
  #nazwy dla atrubutĂłw
  attsName = levels(dataF[[attName]]);
  
  #utworzenie nowych atrybutĂłw
  for(name1 in attsName)
  {    
    dataF[paste0(attName,'_',name1)] = 0;
  } 
  #aktualizacja danych  
  for( id in 1:nrow(dataF))
  {
    dataF[id,paste0(attName,'_',as.character(dataF[id,attName]))] = 1
  } 
  
  dataF
}

#sprawdzenie funkcji
cars2 = genNumericAttrsForFactor(cars,'maint')
View(cars)
View(cars2)


###########################################################################################################################
### KONIEC WPROWADZENIA ### KONIEC WPROWADZENIA ### KONIEC WPROWADZENIA ### KONIEC WPROWADZENIA ### KONIEC WPROWADZENIA ###
###########################################################################################################################


#Data mining sem 2019z lab 1.2

#Temat: Odkrywanie reguĹ asocjacyjnych

#ustawienie katalogu roboczego
#Ĺadowanie bibiliotek

rm(list = ls())

library(arules) #reguĹy asocjacyjne
library(arulesViz) # wizualizacja reguĹ

#zbiĂłr danych AdultUCI jest dostÄpny w bibliotece arules
#https://archive.ics.uci.edu/ml/datasets/Adult
data("AdultUCI")
dim(AdultUCI)

######################################
#przetwarzanie danych

#statystyki dotyczÄce datych
summary(AdultUCI)
#przykĹadowe rekordy - pierwszych 10
head(AdultUCI,10)


summary(AdultUCI)
#dyskretyzacja przy uĹźyciu funkcji discretize z pakietu arules
?discretize
discInterval = discretize(AdultUCI$age, method ='interval', breaks = 4)
summary(discInterval)

discFreq = discretize(AdultUCI$age, method= 'frequency', breaks = 4)
summary(discFreq);

#dane numeryczne
discFreq = discretize(AdultUCI$fnlwgt, method= 'frequency', breaks = 4)
summary(discFreq);


#dyskretyzacja atrybutĂłw ciÄgĹych przy uĹźyciu funkcji cut
#?ordered

AdultUCI[[ "age"]] <- ordered(cut(AdultUCI[[ "age"]], c(15,25,45,65,100)), labels = c("Young", "Middle-aged", "Senior", "Old"))

AdultUCI[[ "hours-per-week"]] <- ordered(cut(AdultUCI[[ "hours-per-week"]],c(0,25,40,60,168)),
                                         labels = c("Part-time", "Full-time", "Over-time", "Workaholic"))

AdultUCI[[ "capital-gain"]] <- ordered(cut(AdultUCI[[ "capital-gain"]], c(-Inf,0,median(AdultUCI[[ "capital-gain"]][AdultUCI[[ "capital-gain"]]>0]),Inf)), labels = c("None", "Low", "High"))

AdultUCI[[ "capital-loss"]] <- ordered(cut(AdultUCI[[ "capital-loss"]],
                                           c(-Inf,0, median(AdultUCI[[ "capital-loss"]][AdultUCI[[ "capital-loss"]]>0]),Inf)),
                                       labels = c("none", "low", "high"))

summary(AdultUCI)

#usuniÄcie zbÄdnych atrybutĂłw
AdultUCI[["fnlwgt"]] <- NULL
AdultUCI[["fnlwgt"]]

AdultUCI[["education-num"]] <-NULL

#konwersja danych z postacji relacyjnej (data.frame) na transakcyjnÄ (transactions)
#?as
adultTR <- as(AdultUCI, "transactions")
#informacja dotyczÄce struktury obiektu transakcje
#str(adultTR)
#class(adultTR)
#statystyki dotyczÄce danych
summary(adultTR)
length(which(is.na(AdultUCI)==T))

######################################
#wstÄpna analiza danych transakcyjnych

#czÄstoĹÄ wystÄpowania elementĂłw
?itemFrequency
freqTbl  = itemFrequency(adultTR, type = "relative")
str(freqTbl)
#class(freqTbl)
summary(freqTbl)
print(freqTbl)

#sortowanie elementĂłw wg wsparcia wzglÄdnego
#?sort
freqTbl = sort(freqTbl, decreasing= TRUE)

#pokazanie elementĂłw ze wsparciem > 20%
print(freqTbl[freqTbl>0.2])

#liczba elementĂłw ze wspaciem >= 5%
length(freqTbl[freqTbl>=0.05])

#wykres
#?itemFrequencyPlot
itemFrequencyPlot(adultTR, type ="relative", support= 0.2)

#################################################
# Wykrywanie zbiorĂłw czÄstych
#parametry funkcji apriori
?APparameter
aParam  = new("APparameter", "confidence" = 0.6, "support" =0.5, "minlen"= 1, maxtime = 20) 
print(aParam)
#korekta wartoĹci parametrĂłw
aParam@support <- 0.3
aParam@confidence <-0.8
aParam@target ="frequent itemsets"

#wykrywanie zbiorĂłw czÄstych - funkcja apriori
?apriori
asets <-apriori(adultTR,aParam)
str(asets)
#analiza wykrytych zbiorĂłw czÄstych
length(asets)
summary(asets)
#?inspect
inspect(head(sort(asets, by="support"),10))

size(asets)
inspect(asets[size(asets)>5])
#grafika
#plot(asets[1:10])
plot(asets[size(asets)>5], method = "graph")
plot(asets[size(asets)>4], method = "paracoord", control = list(reorder = TRUE))

#wyszukiwanie zbiorĂłw zawierajÄcych zadany element(tekst)
?subset
setsRace <- subset(asets, subset = items %pin% "race")
inspect(setsRace)

setsRace <- subset(asets, subset = items %in% "race=White")

setsRace <- subset(asets, subset = items %ain% c("race=White","sex=Male"))
inspect(setsRace)
?is.closed
#zbiory zamkniÄte
is.closed(asets)
#zbiory maksymalne
?is.maximal
maxSets <- asets[is.maximal(asets)==TRUE]
inspect(maxSets)
summary(maxSets)


#wykrywanie zbiorĂłw czÄstych - funkcja eclat
#parametry
ecParam  = new("ECparameter", "confidence" = 0.8, "support" = 0.3) 
print(ecParam)

#wykorzystanie funkcji eclat do wykrycia zbiorĂłw czÄstych 
fsets <- eclat(adultTR,ecParam)
length(fsets)

#seleckcja zbiorĂłw wykrytych przy uĹźyciu funkcji eclat, a nie wykrytych przy uĹźyciu funkcji apriori
inspect(fsets[which(!(fsets %in% asets))])
fsets[which(!(fsets %in% asets))]


#zmiana wartoĹci paramaterĂłw
ecParam@support = 0.4
print(ecParam)

#wykorzystanie funkcji eclat do wykrycia zbiorĂłw czÄstych - zmienione wartoĹci paramatrĂłw
fsets <- eclat(adultTR,ecParam)
length(fsets)

#seleckcja zbiorĂłw wykrytych przy uĹźyciu funkcji apriori, a nie wykrytych przy uĹźyciu funkcji eclat
print(inspect(asets[which(!(asets %in% fsets))]))

#################################################
#Wykrywanie reguĹ asocjacyjnych
#ustawienie parametrĂłw
aParam@target ="rules"
aParam@minlen = 2L
aParam@confidence =0.8

print(aParam)

#uĹźycie funkcji apriori do wykrycia reguĹ asocjacyjnych
aRules <-apriori(adultTR,aParam)
#statystyka dotyczÄca reguĹ
summary(aRules)
length(aRules)
str(aRules)
#przykĹadowe reguĹy
inspect(head(aRules))

plot(aRules, measure = c("support", "lift"), shading = "confidence")
plot(aRules, method = "matrix", engine = '3d')

# wykrycie reguĹ z zadanym nastÄpnikiem
rulesWithRHS <- apriori(adultTR, parameter = list(support=0.2, confidence = 0.5, minlen =2), 
                        appearance = list(rhs = c("capital-gain=None", "capital-loss=none"), default="lhs"))
inspect(rulesWithRHS[1:10])

# wykrycie reguĹ, ktĂłre nie zawierajÄ zadanych elementĂłw 
rulesNotItems <- apriori(adultTR, parameter = list(support=0.2, confidence = 0.5, minlen = 2), 
                         appearance = list(none = c("capital-gain=None", "capital-loss=none"), default="both"))
inspect(rulesNotItems[1:10])

#filtrowanie reguĹ - wybĂłr reguĹ interesujÄcych
#reguĹy, dla ktĂłrych wartoĹÄ parametru lift  >1.2
rulesLift1.2 <- subset(aRules, subset =  lift > 1.2)
inspect(head(rulesLift1.2,1))

#grafika prezentujÄca wybrane reguĹu
#size(rulesLift1.2)

plot(rulesLift1.2, shading="order", control=list(main = "Two-key plot" ))
plot(rulesLift1.2, method="matrix", measure="lift", engine = 'interactive')

#wybĂłr reguĹ z zadanym nastÄpnikiem
rulesInGivenConseq<- subset(aRules, subset = rhs %in% "relationship=Husband" & lift >=2.3)
inspect(rulesInGivenConseq)

#reguĹy oparte o czÄste zbiory maksymalne
maxRul <- aRules[is.maximal(aRules) == TRUE]
summary(maxRul)
inspect(maxRul[1:10])

#usuniÄcie reguĹ nadmiarowych (dla ktĂłrych istnieje prostsza reguĹa, ktĂłra ma wsparcie taki sam nastÄpnik i niemniejsze zaufanie)
notRedun <- aRules[is.redundant(aRules) == TRUE]
summary(notRedun)
inspect(notRedun[1:10])

#wybĂłr reguĹ na podstawie wybranego wskaĹşka jak bardzo dana reguĹa jest interesujÄca
?interestMeasure
#reguĹy dla ktĂłrych wspĂłĹczynnik poprawy jest wiÄkszy od 0,01
resTbl <- interestMeasure(aRules,"improvement", asets)
intres <- which(sapply(resTbl, function(x) {x > 0.01  && x <= 1 })==TRUE)

intersRule <- aRules[intres] 
inspect(intersRule)

#################################################
#Generowanie reguĹ na podstawie wczeĹniej utworzonego zbioru zbiorĂłw czÄstych
?ruleInduction

ecParam  = new("ECparameter", "confidence" = 0.8, "support" =0.3) 
#wykrywanie zbiorĂłw czÄstych
fsets <- eclat(adultTR,ecParam)
#generowanie reguĹ
iERules = ruleInduction(fsets, adultTR, confidence = 0.8,control=list(method ="ptree"))

#statystyki dotyczÄce reguĹ
summary(iERules)
length(iERules)
inspect(head(iERules))

########################################################################################
#Wykorzystanie hierarchii elementĂłw do odkrywania reguĹ asoscjacyjnych

data("Groceries")
str(Groceries)
View(Groceries@itemInfo)

#Hierarchia jest zapisana w skĹadowej iteminfo 
#level2 - pierwszy poziom hierarchii
#level1 - drugi poziom hierarchii

#utworzenie transakcji zawierajÄcych elementy z pierwszego poziomu hierarchii
?aggregate
grocTlevel1<- aggregate(Groceries, by = "level2")
summary(grocTlevel1)

## porĂłwnanie orygianalnych transkacji z transakcjami po agregacji
for( i in 1:10)
{
  inspect(Groceries[i])
  inspect(grocTlevel1[i])
  cat('\n')
}

#wykrywanie reguĹ z danych oryginalnych
rulesBasic <- apriori(Groceries, parameter=list(supp=0.01, conf=0.5, minlen=2))
summary(rulesBasic)
inspect(head(rulesBasic,15))

#agregacja reguĹ
rulesBasicAgg <- aggregate(rulesBasic, by = "level2")
summary(rulesBasicAgg)
inspect(head(rulesBasicAgg,10))

#wykrywanie reguĹ z danych po agregacji 
rulesAgg <- apriori(grocTlevel1, parameter=list(supp=0.01, conf=0.5, minlen=2))
summary(rulesAgg)
inspect(head(rulesAgg,10))
#reguĹy 5 elementowe
rulesAgg5 = rulesAgg[size(rulesAgg)>4] 
size(rulesAgg5)

inspect(head(rulesAgg5,10))

#wykrywanie reguĹ dotyczÄcych wielu poziomĂłw
?addAggregate
#poziom1
grocHier <- addAggregate(Groceries, "level2") 
summary(grocHier)
inspect(head(grocHier,30))
View(grocHier@itemInfo)

#dodanie poziomu nr 2
hLevel2 <- paste0(grocHier@itemInfo$level1, "_l2")  #zmiana nazwy elementĂłw na 2 poziomie hierarchii.
grocHier@itemInfo$myH <- as.factor(hLevel2)

grocTFullHier <- addAggregate(grocHier, "myH") 
View(grocTFullHier@itemInfo)

#odkrywanie reguĹ
rulesAggFH <- apriori(grocTFullHier, parameter=list(supp=0.01, conf=0.5, minlen=2)) 
summary(rulesAggFH)

rulesAggFH <- apriori(grocTFullHier, parameter=list(supp=0.1, conf=0.5, minlen=2)) 
summary(rulesAggFH)
inspect(rulesAggFH[300:320])

#usuwanie reguĹ potencjalnie nadmiarowych typu element -> poziom_X_hierarchii(element)
rulesAggFH_Filter <- filterAggregate(rulesAggFH)
summary(rulesAggFH_Filter)
inspect(rulesAggFH[70:95])


#################################################
#ZADANIE: Odkrywanie reguĹ asocjacyjnych w zbiorze AdultUCI, Groceries
# analiza wpĹywu sposobu przetwarzania wstÄpnego (np. dyskretyzacji,zastosowanie hierachii) na wyniki
# analiza wpĹywu zmiany wartoĹci parametrĂłw na wyniki - wsparcie, zaufanie, inne
#################################################


