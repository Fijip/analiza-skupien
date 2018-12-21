library(e1071)#zawiera cmeans
library(readr)#do czytania plików

kwadrat<- matrix(c(0,0,2,2,1,0,2,0,2,1),ncol=2)
plot(kwadrat)
Kkwadrat<-kmeans(kwadrat,2)
plot(kwadrat, col=Kkwadrat$cluster)

Ckwadrat<-cmeans(kwadrat,2)
plot(kwadrat, col=Ckwadrat$cluster)
print(Kkwadrat)
print(Ckwadrat)

#odczyt pliku, nasze dane
grupa <- read_csv("grupa.csv")

#wykres z uznanie dwóch zmiennych
plot(Pensja_Netto  ~ czas_pracy_w_tygodniu, grupa)
with(grupa,text(Pensja_Netto  ~ wiek, labels=Imie , pos=4, cex=.8))

#kmeans
kgrupa <-kmeans(grupa[2:9], 4)
plot(grupa[2:9],col=kgrupa$cluster)
#with(grupa, text(Pensja_Netto  ~ wiek, labels=Imie , pos=4, cex=.8))
print(kgrupa)

#Normalizacja

grupa2 <- grupa[1:9]
for (i in c(1:26)){
  grupa2[i,2]=(exp(grupa[i,2])-1)/(exp(grupa[i,2]))
}#nie dzia³a wiêc trzeba innaczej

for (j in c(2:9)){
  for(i in c(1:26)){
    grupa2[i,j] <- (grupa[i,j]-min(grupa[j]))/(max(grupa[j])-min(grupa[j]))
  }
}

#klasteryzja po zmianach

kgrupa2 <-append(grupa2,kmeans(grupa2[2:9], 10))
plot(grupa[2:3],col=kgrupa2$cluster)
with(grupa, text(czas_pracy_w_tygodniu~ Pensja_Netto , labels=Imie , pos=4, cex=.8))
print(kgrupa2[.2:3])

#cmeans
cgrupa2<-append(grupa2,cmeans(grupa2[2:9], 10))
plot(grupa[2:9],col=cgrupa2$cluster)
with(grupa, text(czas_pracy_w_tygodniu~ Pensja_Netto , labels=Imie , pos=4, cex=.8))
print(cgrupa2,digits=3)


#hclust i Dendrogram
#dystans miêdzy obserwacjami
dystans<-dist(grupa2)
hgrupa<-hclust(dystans,method="complete")#median, single, average
plot(hgrupa,labels=grupa2$Imie,hang=-1)
print(dystans,digits=1)

#tylko wzrost i waga
grupa3 <- grupa2[5:6]
kgrupa3 <-append(grupa2[1],kmeans(grupa2[5:6], 5))
plot(grupa[5:6],col=kgrupa3$cluster)
with(grupa, text(waga~ wzrost , labels=Imie , pos=4, cex=.8))
print(kgrupa3)
