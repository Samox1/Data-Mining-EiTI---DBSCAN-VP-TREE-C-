# Zadanie domowe #1 - Data Mining dla SD3
# Reguly asocjacyjne
# Szymon Baczyński KD-158

#######################################################################################################
#ZADANIE: Odkrywanie regul asocjacyjnych w zbiorze AdultUCI, Groceries
# analiza wplywu sposobu przetwarzania wstepnego (np. dyskretyzacji, zastosowanie hierachii) na wyniki
# analiza wplywu zmiany wartosci parametrow na wyniki - wsparcie, zaufanie, inne
#######################################################################################################


# Szczerze powiedziawszy nie wiem co trzeba bylo osiagnac, wiec zrobilem dla "Groceries":
# 1) Wykrywanie regul Apriori dla Oryginalnych danych dla roznych wartosci "supp" i "conf" (wsparcia i zaufania)
# 2) Wykrywanie regul Apriori dla Hierarchi poziomu 1 (level2) dla roznych wartosci "supp" i "conf"
# 3) Wykrywanie regul Apriori dla Hierarchi poziomu 2 (level1) dla roznych wartosci "supp" i "conf"
# 4) Wykrywanie regul dla Hierarchi poziomu 1 i 2 


rm(list = ls())

library(arules)
library(arulesViz)


data("Groceries")
View(Groceries@itemInfo)                                                                        # Wyswietlanie

#Hierarchia jest zapisana w skladowej @iteminfo
#level2 - pierwszy poziom hierarchii  - podzial na grupy kategorii
#level1 - drugi poziom hierarchii     - podzial na ogolne grupy

#utworzenie transakcji zawierajacych elementy z pierwszego poziomu hierarchii
GrocT_level2 <- aggregate(Groceries, by = "level1")
GrocT_level1 <- aggregate(Groceries, by = "level2")
# summary(GrocT_level2)                                                                         # Wyswietlanie


itemFrequencyPlot(Groceries, type ="relative", support= 0.05)
itemFrequencyPlot(GrocT_level1, type ="relative", support= 0.05)
itemFrequencyPlot(GrocT_level2, type ="relative", support= 0.05)

# --------------------------------------------------------------------------------------- #
# 1) Wykrywanie regul z oryginalnych danych - rozne wsparcie, zaufanie i inne

# a) Rozne wartosci Wsparcia
suppo <- seq(0.001, 0.020, by=0.004)
i = 0
Basics_suppo = list()

for (y in suppo) {
  i = i + 1
  rulesBasic <- apriori(Groceries, parameter=list(supp=y, conf=0.5, minlen=2))
  plot(rulesBasic, measure = c("support", "lift"), shading = "confidence")
  Basics_suppo[[i]] <- inspect(head(rulesBasic,15))
}

show(Basics_suppo[[1]])
show(Basics_suppo[[5]])

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# b) Rozne wartosci Zaufania 
confi <- seq(0.1, 0.55, by=0.1)
i = 0
Basics_confi = list()

for (x in confi) {
  i = i + 1
  rulesBasic <- apriori(Groceries, parameter=list(supp=0.01, conf=x, minlen=2))
  plot(rulesBasic, measure = c("support", "lift"), shading = "confidence")
  Basics_confi[[i]] <- inspect(head(rulesBasic,15))
}


show(Basics_confi[[1]])
show(Basics_confi[[5]])

plot(rulesBasic, method = "matrix", engine = '3d')

rulesBasicAgg <- aggregate(rulesBasic, by = "level2")
summary(rulesBasicAgg)
inspect(head(rulesBasicAgg,10))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# c) Rozne wartosci Wsparcia oraz Zaufania - eksport do png - 25 wykresow
# suppo <- seq(0.001, 0.020, by=0.004)  ==> {0.001, 0.005, 0.009, 0.013, 0.017}   ==> powyzej tych wartosci byl problem ze znalezieniem regul
# confi <- seq(0.1, 0.55, by=0.1)       ==> {0.1, 0.2, 0.3, 0.4, 0.5}             ==> powyzej tych wartosci byl problem ze znalezieniem regul

suppo <- seq(0.001, 0.020, by=0.0005)

SupConf <- matrix(data = 0, nrow = length(confi), ncol = length(suppo))
i = 1L
j = 1L

for (x in confi) {
  j = 1L
  for (y in suppo) {
    rulesBasic <- apriori(Groceries, parameter=list(supp=y, conf=x, minlen=2))
    
    SupConf[i,j] <-  length(rulesBasic@quality$count)
    
    # nazwa <- paste("c-",as.character(x),"_s-",as.character(y),".jpeg", sep = "")
    # jpeg(nazwa)
    # plot(rulesBasic, measure = c("support", "lift"), shading = "confidence")
    # dev.off()
    
    j = j + 1
  }
  i = i + 1
}

colnames(SupConf) <- c(as.character(suppo))
rownames(SupConf) <- c(as.character(confi))
show(SupConf)

plot(suppo ,SupConf[1,1:length(SupConf[5,])], col = "black", pch=19, t="o", main = "Ilosc regul w zaleznosci od Wsparcia dla roznego Zaufania", ylim = c(0,max(SupConf)), ylab = "Znalezionych regul", xlab = "Wsparcie" )
legend("topright", inset=.05, title="Zaufanie", as.character(confi), fill=c("black", "red", "blue","green","orange"), horiz=TRUE)
lines(suppo ,SupConf[2,1:length(SupConf[5,])], col = "red")
lines(suppo ,SupConf[3,1:length(SupConf[5,])], col = "blue")
lines(suppo ,SupConf[4,1:length(SupConf[5,])], col = "green")
lines(suppo ,SupConf[5,1:length(SupConf[5,])], col = "orange")


rulesBasic_1 <- apriori(Groceries, parameter=list(supp=0.01, conf=0.5, minlen=2))
rulesBasic_2 <- apriori(Groceries, parameter=list(supp=0.015, conf=0.5, minlen=2))
inspect(head(sort(rulesBasic_1, by="support"),10))
inspect(head(sort(rulesBasic_2, by="support"),10))

rulesBasic_1 <- apriori(Groceries, parameter=list(supp=0.005, conf=0.1, minlen=2))
rulesBasic_2 <- apriori(Groceries, parameter=list(supp=0.005, conf=0.7, minlen=2))
inspect(head(sort(rulesBasic_1, by="confidence"),10))
inspect(head(sort(rulesBasic_2, by="confidence"),10))

# WNIOSKI:
# Widac ze zmiana wspolczynnika Wsparcia i Zaufania rzutuje na ilosc wykrytych regul. Takim dzialaniem mozna odrzucic reguly, ktore pojawiaja sie rzadko. 
# Zmieniajac zaufanie mozna wykryc reguly, ktore pozwola na lepsze dopasowanie przyszlej reklamy do osob kpujacych przedmiot X, aby kupily przedmiot Y.


# --------------------------------------------------------------------------------------- #
# 2) Wykrywanie regul z Hierarchi poziomu 1 (level2)  - rozne wsparcie, zaufanie i inne

rulesAgg <- apriori(GrocT_level1, parameter=list(supp=0.01, conf=0.5, minlen=2))
summary(rulesAgg)
inspect(head(rulesAgg,10))

suppo <- seq(0.001, 0.040, by=0.0005)
SupConf_lvl1 <- matrix(data = 0, nrow = length(confi), ncol = length(suppo))
i = 1L
j = 1L

for (x in confi) {
  j = 1L
  for (y in suppo) {
    ruleslvl1 <- apriori(GrocT_level1, parameter=list(supp=y, conf=x, minlen=2))
    
    SupConf_lvl1[i,j] <-  length(ruleslvl1@quality$count)
    
    # nazwa <- paste("c-",as.character(x),"_s-",as.character(y),".jpeg", sep = "")
    # jpeg(nazwa)
    # plot(rulesBasic, measure = c("support", "lift"), shading = "confidence")
    # dev.off()
    
    j = j + 1
  }
  i = i + 1
}

colnames(SupConf_lvl1) <- c(as.character(suppo))
rownames(SupConf_lvl1) <- c(as.character(confi))

plot(suppo ,SupConf_lvl1[1,1:length(SupConf_lvl1[5,])], col = "black", pch=19, t="o", main = "Ilosc regul (LVL1) w zaleznosci od Wsparcia dla roznego Zaufania", ylim = c(0,max(SupConf_lvl1)), ylab = "Znalezionych regul", xlab = "Wsparcie" )
legend("topright", inset=.05, title="Zaufanie", as.character(confi), fill=c("black", "red", "blue","green","orange"), horiz=TRUE)
lines(suppo ,SupConf_lvl1[2,1:length(SupConf_lvl1[5,])], col = "red")
lines(suppo ,SupConf_lvl1[3,1:length(SupConf_lvl1[5,])], col = "blue")
lines(suppo ,SupConf_lvl1[4,1:length(SupConf_lvl1[5,])], col = "green")
lines(suppo ,SupConf_lvl1[5,1:length(SupConf_lvl1[5,])], col = "orange")



# --------------------------------------------------------------------------------------- #
# 3) Wykrywanie regul z Hierarchi poziomu 2 (level1)  - rozne wsparcie, zaufanie i inne

rulesAgg2 <- apriori(GrocT_level2, parameter=list(supp=0.01, conf=0.5, minlen=2))
summary(rulesAgg2)
inspect(head(rulesAgg2,10))

suppo <- seq(0.001, 0.040, by=0.0005)
SupConf_lvl2 <- matrix(data = 0, nrow = length(confi), ncol = length(suppo))
i = 1L
j = 1L

for (x in confi) {
  j = 1L
  for (y in suppo) {
    ruleslvl2 <- apriori(GrocT_level2, parameter=list(supp=y, conf=x, minlen=2))
    
    SupConf_lvl2[i,j] <-  length(ruleslvl2@quality$count)
    
    # nazwa <- paste("c-",as.character(x),"_s-",as.character(y),".jpeg", sep = "")
    # jpeg(nazwa)
    # plot(rulesBasic, measure = c("support", "lift"), shading = "confidence")
    # dev.off()
    
    j = j + 1
  }
  i = i + 1
}

colnames(SupConf_lvl2) <- c(as.character(suppo))
rownames(SupConf_lvl2) <- c(as.character(confi))

plot(suppo ,SupConf_lvl2[1,1:length(SupConf_lvl2[5,])], col = "black", pch=19, t="o", main = "Ilosc regul (lvl2) w zaleznosci od Wsparcia dla roznego Zaufania", ylim = c(0,max(SupConf_lvl2)), ylab = "Znalezionych regul", xlab = "Wsparcie" )
legend("topright", inset=.05, title="Zaufanie", as.character(confi), fill=c("black", "red", "blue","green","orange"), horiz=TRUE)
lines(suppo ,SupConf_lvl2[2,1:length(SupConf_lvl2[5,])], col = "red")
lines(suppo ,SupConf_lvl2[3,1:length(SupConf_lvl2[5,])], col = "blue")
lines(suppo ,SupConf_lvl2[4,1:length(SupConf_lvl2[5,])], col = "green")
lines(suppo ,SupConf_lvl2[5,1:length(SupConf_lvl2[5,])], col = "orange")


# --------------------------------------------------------------------------------------- #
# 4) Wykrywanie regul z Hierarchi LVL 1 + 2 

GrocHier <- addAggregate(Groceries, "level2") 
# summary(GrocHier)
# inspect(head(GrocHier,30))
# View(GrocHier@itemInfo)

#dodanie poziomu nr 2
hLevel2 <- paste0(GrocHier@itemInfo$level1, "_l2")  #zmiana nazwy elementĂłw na 2 poziomie hierarchii.
GrocHier@itemInfo$myH <- as.factor(hLevel2)
GrocTFullHier <- addAggregate(GrocHier, "myH") 
# View(GrocTFullHier@itemInfo)


#odkrywanie regul
rulesAggFH <- apriori(GrocTFullHier, parameter=list(supp=0.01, conf=0.5, minlen=2)) 
# summary(rulesAggFH)
inspect(head(rulesAggFH,15))

rulesAggFH <- apriori(GrocTFullHier, parameter=list(supp=0.1, conf=0.5, minlen=2)) 
# summary(rulesAggFH)
inspect(head(rulesAggFH,15))

#usuwanie regul potencjalnie nadmiarowych typu element -> poziom_X_hierarchii(element)
rulesAggFH_Filter <- filterAggregate(rulesAggFH)
summary(rulesAggFH_Filter)
inspect(head(rulesAggFH_Filter,15))

plot(rulesAggFH_Filter, shading="order", control=list(main = "Two-key plot" ))
plot(rulesAggFH_Filter, method="matrix", measure="lift", engine = 'interactive')




# WNIOSKI: 
# Wykorzystanie Hierarchizacji pozwala na poznanie korelacji miedzy poszczegolnymi grupami. Wykorzystanie kategorii przedmiotu daje mozliwosc wykrycia regul, ktore dla pojedycznych przedmiotow nie mialy by sensu.




# ??? nie wiem co dalej


