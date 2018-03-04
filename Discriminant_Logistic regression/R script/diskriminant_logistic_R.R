##############################################
######                                   #####
###### R SCRIPT: Discriminant Analysis   #####
######           Logistic Regression     #####
######                                   ##### 
######             Nihan Acar-Denizli    #####
######                 Asst. Prof. Dr.   #####
##############################################

#install.packages("Rcmdr")
#library(Rcmdr)

#SPSS'ten veri çekme
library(foreign)
skulls<-read.spss("skulls.sav",to.data.frame="TRUE")

View(skulls)
str(skulls)
attach(skulls)

#iki kategori için Diskriminant Analizi (lda fonksiyonu kullanilarak) 
?lda
install.packages("MASS")
library(MASS)
skullda<-lda(type~lenght+width+height+faceheight+facewidth, data=skulls)
skullda
skullda$prior
names(skullda)

#Grup tahmini yapilmasi
?predict
skullpred<-predict(skullda)
names(skullpred)
#Sinif Tahminleri 
skullpred$class
#Gözlemlere ait Sonsal Olasiliklar 
skullpred$posterior

#Çapraz Çizelge Tablosunun olusturulmasi
tablo<-table(skulls$type,skullpred$class)
tablo

#Dogru siniflanma orani
classrate<-sum(diag(tablo))/sum(tablo)
classrate

#Gruplara göre dogru siniflama oranlari (satırlar üzerinden olasılık hesabı)
diag(prop.table(tablo, 1))

#Toplam doğru sınıflama oranı 
sum(diag(prop.table(tablo)))

#Nisbi sans kriteri p1^2+p^2
skullda$prior[1]^2 + skullda$prior[2]^2

#Orjinal gruplar ile Tahmin edilen grupların karşılaştırılması
?cbind
comp<-cbind(skulls$type,skullpred$class)
comp

##Stepwise Classification
#install.packages(klaR)
library(klaR)
?stepclass
#method="lda", direction=c("forward", "backward","both"), criterion=c("CR","AC"),start.vars="faceheigth" olabilir.
skullstep<-stepclass(skulls[,2:6],skulls[,1],method="lda",direction="forward", criterion="CR") 
skullstep
summary(skullstep)
skullstep$process
skullstep$model
plot(skullstep)
skullstep$result.pm

#######################Çok Gruplu Diskriminant###############################

#Noktanın Eksik Gözlem olarak tanımlanması gerek!
install.packages("readxl")
library(readxl)
araba <- read_excel("cars.xls")
View(araba)
araba$Origin<-as.factor(araba$Origin)
summary(araba)

#Değişken içinde bulunan noktanın Eksik Gözlem olarak tanımlanması gerek!
araba <- read_excel("cars.xls",na=".") #,rownames=FALSE, header=TRUE, na=".", sheet="Sayfa1", stringsAsFactors=TRUE)
View(araba)
head(araba)
araba$Origin<-as.factor(araba$Origin)
summary(araba)

araba<-na.omit(araba)
summary(araba)
attach(araba)

carlda<-lda(Origin~MPG+Engine+Horse+Weight+Accel, data=araba)
carlda
carlda$prior

#Grup tahmini yapilmasi
carpred<-predict(carlda)
names(carpred)
#Sinif Tahminleri 
carpred$class
carpred$posterior


#Çapraz Çizelge Tablosunun olusturulmasi

tablo<-table(araba$Origin,carpred$class)
tablo

#Dogru siniflanma orani
classrate<-sum(diag(tablo))/sum(tablo)
classrate

#Gruplara göre dogru siniflama oranlari
diag(prop.table(tablo, 1))

#Toplam doğru sınıflama oranı
sum(diag(prop.table(tablo)))

#Nisbi sans kriteri p1^2+p^2
carlda$prior[1]^2 + carlda$prior[2]^2+carlda$prior[3]^2

#Orjinal gruplar ile Tahmin edilen grupların karşılaştırılması
comp<-cbind(araba$Origin,carpred$class)
comp

##Stepwise Classification
#method="lda", direction=c("forward", "backward","both"), criterion=c("CR","AC"),start.vars="faceheigth" olabilir.

carstep<-stepclass(araba[,1:5],araba[,7],method="lda",direction="backward", criterion="CR") 
carstep
summary(carstep)
carstep$process
carstep$model
carstep$result.pm


####################################################Lojistik Regresyon###############################################
##GLM fonksiyonu ile Lojistik Regresyon
?glm

skull.glm<-glm(type~lenght+width+height+faceheight+facewidth,method="glm.fit",family=binomial, data=skulls)
summary(skull.glm)
names(skull.glm)
skull.glm$coefficients


#Ki-kare istatistginin hesabi
skull.glm$deviance
skull.glm$null.deviance
kikare<- skull.glm$null.deviance-skull.glm$deviance
kikare

#serbestlik derecesi hesabi
skull.glm$df.null
skull.glm$df.residual
df<-skull.glm$df.null-skull.glm$df.residual

#Ki kare istatistigine ait p degerinin hesabi (p<0.05 ise eklenen degiskenlerin modele katkisi anlamlidir.)
kikare.p<- 1 - pchisq(kikare,df)
kikare.p

#Hoshmer Lemeshov hesabi (p>0.05 ise model anlamlidir)

HL<-kikare/skull.glm$null.deviance
HL

#Modelin R^2 degerlerinin hesabi (Field, Andy. Discovering Statistics Using R)
N<-length(skull.glm$fitted.values)
N
#Cox and Snell R^2
R.cs <- 1 - exp ((skull.glm$deviance - skull.glm$null.deviance) /N)
R.cs
#Nagelkerke R^2
R.n<-R.cs /(1-(exp(-(skull.glm$null.deviance/N))))
R.n

#Model katsayilarinin exponential alinmis hali ve güven araliklari
exp(coef(skull.glm))
#exp(confint(coef(skull.glm)))#parm="faceheight ile tek bir degiskene ait güven araligi hesaplanabilir.

#Atama Tablosu
typepred<-fitted(skull.glm)
thresh  <- 0.5
typefac<- cut(typepred, breaks=c(-Inf, thresh, Inf), labels=c("A", "B"))
cTab <- table(skulls$type, typefac)
cTab
#Toplam Dogru Atanma Yüzdesi
sum(diag(cTab)) / sum(cTab)

#Stepwise Lojistik Regresyon
?step
sglm<-step(skull.glm)
sglm<-step(skull.glm,direction="both",trace=0) #direction=c("backward","forward","both")
summary(sglm)
names(sglm)
exp(sglm$coefficients)

typepreds<-fitted(sglm)
thresh  <- 0.5
typefacs<- cut(typepreds, breaks=c(-Inf, thresh, Inf), labels=c("A", "B"))
cTabs <- table(skulls$type, typefacs)
cTabs

##################################### Plasma data örnegi ("A Handbook of Statistical Analyses Using R-Brian Everitt")#########################
install.packages("HSAUR2")
data("plasma",package="HSAUR2")
layout(matrix(1:2, ncol=2))
cdplot(ESR~fibrinogen,data=plasma)
cdplot(ESR~globulin,data=plasma)

plasma_glm<-glm(ESR~fibrinogen, data=plasma, family=binomial())
plasma_glm
summary(plasma_glm)
confint(plasma_glm,parm="fibrinogen")

exp(coef(plasma_glm))
exp(confint(plasma_glm,parm="fibrinogen"))

plasma2<-glm(ESR~fibrinogen+globulin,data=plasma,family=binomial())
summary(plasma2)
?anova
anova(plasma_glm,plasma2,test="Chisq")

############# Multinomial Lojistik Regresyon Siniflama##########
#View(araba)
#araba<-na.omit(araba)
attach(araba)

#summary(araba)
araba$Origin<-as.factor(araba$Origin)
araba$MPG<-as.numeric(araba$MPG)
araba$Horse<-as.numeric(araba$Horse)


install.packages("nnet")
library(nnet)
?multinom

araba$Origin <- relevel(araba$Origin, ref = "3")
loj_car<-multinom(formula=Origin ~ MPG+Engine+Horse,data=araba)
names(loj_car)
#loj_car$coefnames
summary(loj_car)

#Standardized coefficients
z <- summary(loj_car)$coefficients/summary(loj_car)$standard.errors
z

p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

#Model katsayilarinin exponential alinmis hali 
exp(coef(loj_car))


###Arabaların lojistik regresyona göre sınıflandırılması
#Perform classification
?predict.multinom
pred_org<- predict(loj_car, araba[,1:3])
car_tab<- table(araba$Origin, pred_org)
car_tab

#Gruplara göre dogru siniflama oranlari
diag(prop.table(car_tab, 1))

#Toplam doğru sınıflama oranı
sum(diag(prop.table(car_tab)))






