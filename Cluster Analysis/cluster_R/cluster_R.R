##### FOOD NUTRIENT VERI SETI #####
#install.packages("Rcmdr")
#library(Rcmdr)

#nutrient<-read.csv("food.csv",header=TRUE,",")

library(foreign)
nutrient<-read.spss("food nutrient data.sav",to.data.frame="TRUE")

# Verinin Hazirlanma Asamasi
View(nutrient)
nutrient <- na.omit(nutrient) # eksik g�zlemlerin �ikarilmasi
#nutrient<-nutrient[,-1]
#mydata <- scale(mydata) # gerekti�inde degiskenleri standardize etmek i�in

## K�me sayisinin belirlenmesi (R in Action (2nd ed): Chapter 16) 
##(Within groups sum of squares vs no of clusters)

wss <- (nrow(nutrient)-1)*sum(apply(nutrient,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(nutrient, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# Hiyerarsik K�meleme
d <- dist(nutrient, method = "euclidean") # uzaklik matrisi
?hclust
fit <- hclust(d, method="complete") 
plot(fit) # Dendogram �izimi
groups <- cutree(fit, k=5) # cut tree into k clusters
# k k�meyi belli eden dikd�rtgenlerin �izimi 
rect.hclust(fit, k=5, border="green")

#Ayk�r� de�erin ��kar�lmas� ile s�n�flar tekrar olu�turulabilir
nutrient<-nutrient[-25,]

# K-Means K�meleme Analizi
k=3 #k�me sayisi
?kmeans
fit <- kmeans(nutrient, k) # k k�me sayisi
names(fit)
# k�me ortalamalarinin hesabi 
aggregate(nutrient,by=list(fit$cluster),FUN=mean)
# G�zlemin atandigi sinifi ekliyor veri setine
nutrient <- data.frame(nutrient, fit$cluster)
View(nutrient)

#K means ile se�ilen k�me say�s�na ait g�zlemlerin s�n�flanmas�n� g�steriyor.
library(cluster)
names(fit)
?clusplot
clusplot(nutrient, fit$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0)

#ilk iki temel bile�en ile s�n�flama
install.packages("fpc")
library(fpc)
?plotcluster
plotcluster(nutrient, fit$cluster)

######### SARAP VERI SETI ##########

wine<-read.spss("sarap.sav",to.data.frame="TRUE")
View(wine)
wine<-wine[,1:5]

# Hiyerarsik K�meleme
d <- dist(wine, method = "euclidean") # uzaklik matrisi
winefit <- hclust(d, method="ward.D") 
plot(winefit) # Dendogram �izimi
groups <- cutree(winefit, k=5)# cut tree into k clusters
# K�meleri belli eden dikd�rtgenlerin �izimi 
rect.hclust(winefit, k=5, border="red")

# K-Means K�meleme Analizi
k=4 #k�me sayisi
fit <- kmeans(wine, k) # k k�me sayisi
names(fit)
fit$centers

# k�me ortalamalarinin hesabi 
#aggregate(wine,by=list(fit$cluster),FUN=mean)

# G�zlemin atandigi sinifi ekliyor veri setine
wine <- data.frame(wine, fit$cluster)
wine

wss <- (nrow(wine)-1)*sum(apply(wine,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(wine, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")