#test verisine boþ bir Survived kolonunun eklenmesý
titanic_test$Survived=NA 

#test ve eðitim setlerinin birleþtirilmesi
titanic_tamVeri=rbind(titanic_egitim,titanic_test) #test ve eðitim setlerinin birleþtirilmesi

#verinin genel görünümüne bakýþ atmak
str(titanic_tamVeri)


#veri türlerinin düzenlenmesi

library(dplyr)
titanic_tamVeri = titanic_tamVeri %>% mutate(Survived=as.factor(Survived),
                                             Pclass=as.factor(Pclass),
                                             Sex=as.factor(Sex))


#verinin görselleþtirilmesi

library(ggplot2) 

grafik1=ggplot(titanic_tamVeri[1:891,]) + 
  geom_bar(mapping = aes(x = Pclass, fill = Survived)) + 
  theme(legend.position = "none")

grafik2=ggplot(titanic_tamVeri[1:891,]) + 
  geom_freqpoly(mapping = aes(x = Age, color = Survived), bins=50)

grafik3=ggplot(titanic_tamVeri[1:891,]) + 
  geom_freqpoly(mapping = aes(x = Fare, color = Survived), bins=50) + 
  theme(legend.position = "none")

grafik4=ggplot(titanic_tamVeri[1:891,]) + 
  geom_bar(mapping = aes(x = SibSp+Parch, fill = Survived), position = "fill") + 
  theme(legend.position = "none")


#birden fazla grafiði ayný ekranda göstermemizi saðlayan paket

library(ggpubr)
ggarrange(grafik1,grafik2,grafik3,grafik4,ncol = 2, nrow = 2)



#boþ deðerleri tespit etmek
bos_deger=is.na(titanic_tamVeri$Age)

#boþ deðerlerin toplam sayýsýný bulmak için sum fonksiyonu kullanýlýr. 
sum(is.na(titanic_tamVeri$Age))

#tüm sütunlardaki boþ deðerleri bulmak için apply fonksiyonunu kullanmamýz gerekir. 
apply(titanic_tamVeri, 2 , function(x) sum(is.na(x)))

#kýsa yoldan kayýp deðerleri görmek için;
summary(titanic_tamVeri)

#eksik deðerleri kaldýrmak için;
titanic_bosyok=subset(titanic_tamVeri,Age!=is.na(Age))

#summary fonksiyonu ile boþ deðerler kaldýrýldýmý kontrol edelim.
summary(titanic_bosyok)

#eksik deðerleri kaldýrmanýn kolay yolu;yalnýzca sayýlarý döndürür. 
titanic_bosyok2=na.omit(titanic_tamVeri$Age)

#veriyi daha ayrýntýlý göstermek için;
install.packages("psych")
library(psych)
describe(titanic_bosyok)


#hmisc paketi eksik ve tekil olan deðerleri görmek için kullanýlabilir. 
install.packages("Hmisc")
library(Hmisc)
describe(titanic_bosyok)


#eksik deðerleri ortalama ile doldurmak

yas=titanic_tamVeri$Age #Age sütununun yas deðiþkenine atanmasý

yas[is.na(yas)]=mean(yas,na.rm=TRUE) #bod deðerlere ortalama atanmasý

is.na(yas) #bos deðer kaldýmý kontrolü yapalým.

#olusturdugumuz yas deðiþkeninin gerçek verimize atanmasý

titanic_tamVeri$Age=yas


#aykýrý deðerleri görmek için hmisc paketindeki describe fonksiyonunu kullanabiliriz.
describe(titanic_tamVeri$Age)

#~tespit ettiðimiz aykýrý deðerleri temizlemek için aþaðýdaki gibi subset fonksiyonu kullanýlýr. 
aykiri_temizlendi=subset(titanic_tamVeri,Age>1 & Age<70)

#eðitim ve test veri setlerine ayýrmak

egitim_son=titanic_tamVeri[1:891,]
test_son=titanic_tamVeri[892:1309,]


#randomforest algoritmasý- karar aðacý algoritmasýdýr.ÝStediðimiz kadar fazla karar aðacý oluþturulur ve bu karar aðaçlarýnýn ortalamasý alýnarak tahminleme yapýlýr. 

install.packages("randomForest")
library(randomForest)


rf_model=randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare, 
                data = egitim_son, 
                mtry = 3, 
                ntree = 1000)


tahminler = predict(rf_model, test_son[, c(3,5,6,7,8,10)])

#yolcu numarasý sonuç deðiþkenine atandý.
sonuc=test_son$PassengerId


#sonuc degiskeni üzerinde daha iyi iþlem yapabilmek adýna data.frame e dönüþtürüldü.
sonuc=as.data.frame(sonuc)

#sütun ismi atandý.
colnames(sonuc)=c("PassengerId")

#sonucta Survived deðiþkeni oluþturularak tahmin sonuçlarý buraya aktarýldý

sonuc$Survived=tahminler

#verimizde tahmin edilemeyen deðerler varmý kontrol etmek için describe fonksiyonu kullanýlýr. 
describe(sonuc)

#verilerin excele aktarýlmasý
write.csv(sonuc,"C:/Users/Meltem/Desktop/sonuc.csv",row.names = F)






