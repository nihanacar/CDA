##### FOOD NUTRIENT VERI SETI #####
#install.packages("Rcmdr")
#library(Rcmdr)

#nutrient<-read.csv("food.csv",header=TRUE,",")

library(foreign)
nutrient<-read.spss("food nutrient data.sav",to.data.frame="TRUE")

# Verinin Hazirlanma Asamasi
View(nutrient)
nutrient <- na.omit(nutrient) # eksik gözlemlerin çikarilmasi
#nutrient<-nutrient[,-1]
#mydata <- scale(mydata) # gerektiðinde degiskenleri standardize etmek için

## Küme sayisinin belirlenmesi (R in Action (2nd ed): Chapter 16) 
##(Within groups sum of squares vs no of clusters)

wss <- (nrow(nutrient)-1)*sum(apply(nutrient,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(nutrient, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# Hiyerarsik Kümeleme
d <- dist(nutrient, method = "euclidean") # uzaklik matrisi
?hclust
fit <- hclust(d, method="complete") 
plot(fit) # Dendogram çizimi
groups <- cutree(fit, k=5) # cut tree into k clusters
# k kümeyi belli eden dikdörtgenlerin çizimi 
rect.hclust(fit, k=5, border="green")

#Aykýrý deðerin çýkarýlmasý ile sýnýflar tekrar oluþturulabilir
nutrient<-nutrient[-25,]

# K-Means Kümeleme Analizi
k=3 #küme sayisi
?kmeans
fit <- kmeans(nutrient, k) # k küme sayisi
names(fit)
# küme ortalamalarinin hesabi 
aggregate(nutrient,by=list(fit$cluster),FUN=mean)
# Gözlemin atandigi sinifi ekliyor veri setine
nutrient <- data.frame(nutrient, fit$cluster)
View(nutrient)

#K means ile seçilen küme sayýsýna ait gözlemlerin sýnýflanmasýný gösteriyor.
library(cluster)
names(fit)
?clusplot
clusplot(nutrient, fit$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0)

#ilk iki temel bileþen ile sýnýflama
install.packages("fpc")
library(fpc)
?plotcluster
plotcluster(nutrient, fit$cluster)

######### SARAP VERI SETI ##########

wine<-read.spss("sarap.sav",to.data.frame="TRUE")
View(wine)
wine<-wine[,1:5]

# Hiyerarsik Kümeleme
d <- dist(wine, method = "euclidean") # uzaklik matrisi
winefit <- hclust(d, method="ward.D") 
plot(winefit) # Dendogram çizimi
groups <- cutree(winefit, k=5)# cut tree into k clusters
# Kümeleri belli eden dikdörtgenlerin çizimi 
rect.hclust(winefit, k=5, border="red")

# K-Means Kümeleme Analizi
k=4 #küme sayisi
fit <- kmeans(wine, k) # k küme sayisi
names(fit)
fit$centers

# küme ortalamalarinin hesabi 
#aggregate(wine,by=list(fit$cluster),FUN=mean)

# Gözlemin atandigi sinifi ekliyor veri setine
wine <- data.frame(wine, fit$cluster)
wine

wss <- (nrow(wine)-1)*sum(apply(wine,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(wine, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
