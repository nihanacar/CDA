###############################################################
######                                                    #####
###### SCRIPT:   Multivariate Analysis of Variance        #####
#######          (MANOVA)                                 #####
######                                                    #####
######       Nihan Acar-Denizli, Asst. Prof.              #####
######       Mimar Sinan Güzel Sanatlar Üniversitesi      #####
###############################################################

###Baska formatta veri okumak için R Commander kullanilanilabilir.###
### Use R Commander to read different format data. ####
#install.packages("Rcmdr")
library(Rcmdr)

#### Excel verisi okumak için "readxl" paketi kullanilabilir. #####
#### Use "readxl" package to read xls/xlsx files  #####
#install.packages("readxl")
library(readxl)
?read_excel
tel_isletim<-read_excel("telefon_isletim.xls")

#### Spss verisi okumak için "foreign" paketi kullanilabilir. #####
#### Use "foreign" package to read spss files #####
library(foreign)
toprak<-read.spss("Manova_toprak.sav",to.data.frame="TRUE")

#### ÖRNEK 1: "Toprak" Veri Seti ####
####Veri seti genel bilgi (The structure of the data frame) #####
str(toprak)
head(toprak)
attach(toprak)

####Faktör degisken kodlamalari (The levels of categorical variables) ####
levels(ilce)
levels(Bitkituru)

### Çok Degiskenli Normallik Testleri (Multivariate Normality Tests) ####
#install.packages("mvnormtest")
library(mvnormtest)
deg<-toprak[,1:4]
str(deg)
mshapiro.test(t(deg))
deg1<-toprak[,7:9]
mshapiro.test(t(deg1))

###Normallik Testleri (Normality of single variable) ###
shapiro.test(Potasyum)
shapiro.test(Fosfor)
shapiro.test(Kalsiyum)

shapiro.test(lnpot)
shapiro.test(lnfosfor)
shapiro.test(lnkalsiyum)
shapiro.test(lnmagnezyum)

#### Box M Varyans Homojenligi Testi (Box M Test of Homogeneity of Variances) #####
install.packages("biotools")
library("biotools")
?boxM
boxM(deg,ilce)

boxM(deg1,ilce)
boxM(deg1,bitkituru)

#### Levene Testi (Levene's Test) ####
library(car)
?leveneTest
leveneTest(lnpot ~ ilce*bitkituru, data=toprak)
leveneTest(lnfosfor ~ ilce*bitkituru, data=toprak)
leveneTest(lnkalsiyum ~ ilce*bitkituru, data=toprak)
leveneTest(lnmagnezyum ~ ilce*bitkituru, data=toprak) #varyans homojenliði saðlanmýyor!

### Degiskenlere Göre Ortalamalar ve Ortalama Çizimleri (The Mean Plots of the Variables) ########
means_ilce <- aggregate(deg1,list(ilce), mean)
means_ilce

means_bitki<- aggregate(deg1,list(Bitkituru), mean)
means_bitki

# Tek deðiþkene göre ortalama
meant<-tapply(deg1[,1],ilce,mean)
meant

#install.packages("gplots")
library(gplots)

#op<-par(mfrow=c(3,1))
plotmeans(lnpot~ilce,xlab="ilce",ylab="lnpotasyum", main="Mean Plot\nwith 95% CI")
plotmeans(lnfosfor~ilce, xlab="ilce",ylab="lnfosfor", main="Mean Plot\nwith 95% CI" )
plotmeans(lnkalsiyum~ilce, xlab="ilce",ylab="lnkalsiyum", main="Mean Plot\nwith 95% CI")
#par(op)

#op<-par(mfrow=c(3,1))
plotmeans(lnpot~Bitkituru,xlab="bitkituru",ylab="lnpotasyum", main="Mean Plot\nwith 95% CI")
plotmeans(lnfosfor~Bitkituru, xlab="bitkituru",ylab="lnfosfor", main="Mean Plot\nwith 95% CI" )
plotmeans(lnkalsiyum~Bitkituru, xlab="bitkituru",ylab="lnkalsiyum", main="Mean Plot\nwith 95% CI")
#par(op)

#### MANOVA ####

### Multivariate Tests ###
lntoprak_man <- manova(cbind(lnpot,lnfosfor,lnkalsiyum) ~ ilce*Bitkituru,data=toprak)
summary(lntoprak_man, test = "Hotelling-Lawley")
summary(lntoprak_man, test = "Wilks")
summary(lntoprak_man, test = "Pillai")
summary(lntoprak_man, test = "Roy")

### Test of Between Subjects ###
summary.aov(lntoprak_man)

########## Çoklu Karsilastirmalar (Multiple Comparisons) #################
#lnpotasyum için
lnpot_aov <- aov(lnpot ~ ilce, data = toprak)
TukeyHSD(lnpot_aov, "ilce")

lnpot_aov <- aov(lnpot ~ Bitkituru, data = toprak)
TukeyHSD(lnpot_aov, "bitkituru")

#lnkalsiyum için
lnkal_aov <- aov(lnkalsiyum~ ilce, data = toprak)
TukeyHSD(lnkal_aov, "ilce")

lnkal_aov <- aov(lnkalsiyum ~ Bitkituru, data = toprak)
TukeyHSD(lnkal_aov, "bitkituru")

### Bitki türü  ve ilce için Etkilesim Grafikleri (Interaction Plots) ###
?interaction.plot
?legend
interaction.plot(ilce,Bitkituru,lnpot, fun=mean, type="b", legend=TRUE)
interaction.plot(Bitkituru,ilce,lnkalsiyum, fun=mean, type="l", legend=TRUE)

##############################################################################
##############################################################################

#### ÖRNEK 2: Mevki-Departman Veri Seti ####

#### Mevki-Dep Veri Setinin Spss'ten R'a çekilmesi ####
mevkidep<- read.spss("mevki_departman.sav",to.data.frame=TRUE)

str(mevkidep)
levels(departman)
levels(Mevki)

### Mevki ve Dapertmana Gore Manova ###
mevkidep_man <- manova(cbind(tatmin, sahiplenme, kalicilik) ~ departman*mevki,data=mevkidep)
summary.aov(mevkidep_man)

###Coklu Karsilastirmalar (Multiple Comparisons) ###

##Mevki için
tatmin_mev <- aov(tatmin ~ mevki, data = mevkidep)
TukeyHSD(tatmin_mev, "mevki")

sahip_mev <- aov(sahiplenme ~ mevki, data = mevkidep)
TukeyHSD(sahip_mev, "mevki")

kalicilik_mev <- aov(kalicilik ~ mevki, data = mevkidep)
TukeyHSD(kalicilik_mev, "mevki")

###Departman için 
tatmin_dep <- aov(tatmin ~ departman, data = mevkidep)
TukeyHSD(tatmin_dep, "departman")

sahip_dep <- aov(sahiplenme ~ departman, data = mevkidep)
TukeyHSD(sahip_dep, "departman")

kalici_dep <- aov(kalicilik ~ departman, data = mevkidep)
TukeyHSD(kalici_dep, "departman")

### Mevki ve Departmana Göre Etkilesim Grafikleri ###
#op<-par(mfrow=c(3,1))
interaction.plot(mevki,departman, tatmin, fun=mean, type="l", legend=TRUE)
interaction.plot(mevki,departman,sahiplenme, fun=mean, type="l", legend=TRUE)
interaction.plot(mevki,departman,kalicilik, fun=mean, type="l", legend=TRUE)
#par(op)

interaction.plot(mevki,departman, tatmin, fun=mean, type="l", legend=TRUE, col=1:5)

#####################################################################################
