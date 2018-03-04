############################################
######                                 #####
###### SCRIPT:  TEMEL B�LE�ENLER(PCA)  #####
######          FAKT�R ANAL�Z�         #####             
######                                 #####
######             Nihan Acar-Denizli  #####
######                 Asst. Prof. Dr. #####
############################################

###Veri seti "Food-Price"###
library(Rcmdr)
View(food)
str(food)

library(foreign)
food<-read.spss("food_price_data.sav",to.data.frame="TRUE")

################################# TBA UYGULANAB�L�RL��� (BARTLETT TEST VE KMO INDEKSI)######################

###### Bartlett K�resellik Testi ####
###### Bartlett's Test of Spherecity####

install.packages("psych")
library(psych)
?cortest.bartlett
#Besin fiyatlar� korelasyon matrisi
R<-cor(food[,2:6])
print(R)
#G�zlem say�s� (n)
n<-nrow(food)
n
##Bartlett test sonucu###
cortest.bartlett(R,n)

###### Kaiser-Meyer-Olkin (KMO) testi ###

##KMO fonksiyonu!! (Prof. Shigenobu Aok) (http://minato.sip21c.org/swtips/factor-in-R.pdf)
kmo <- function(x)
{
  x <- subset(x, complete.cases(x))       # Omit missing values
  r <- cor(x)                             # Correlation matrix
  r2 <- r^2                               # Squared correlation coefficients
  i <- solve(r)                           # Inverse matrix of correlation matrix
  d <- diag(i)                            # Diagonal elements of inverse matrix
  p2 <- (-i/sqrt(outer(d, d)))^2          # Squared partial correlation coefficients
  diag(r2) <- diag(p2) <- 0               # Delete diagonal elements
  KMO <- sum(r2)/(sum(r2)+sum(p2))
  MSA <- colSums(r2)/(colSums(r2)+colSums(p2))
  return(list(KMO=KMO, MSA=MSA))
}

#KMO de�eri
kmo(food[,2:6])

####################################### TEMEL B�LE�ENLER ANAL�Z� ##########################################3
?princomp
pca<-princomp(food[,2:6],cor=TRUE,scores=TRUE)#,cutoff=0.01)

#Bile�enlerce A��klanan Varyans Oranlar�#
summary(pca)
names(summary(pca))
summary(pca)$loadings

#�zde�erlerin Hesaplanmas�#
s2<-(summary(pca)$sdev)^2
s2 #�zde�erler
sum(s2) #Korelasyon matrisi kullan�ld���nda �zde�erlerin toplam� de�i�ken say�s�na e�it (de�i�kenler standardize ediliyor!)

#Scree Plot#
plot(pca)
plot(pca,type="line")

pca$loadings

#PC1 vs. PC2#
biplot(pca)
pca$scores

which.min(pca$scores[,1])
which.min(pca$scores[,2])

### Pahal�l�k �ndeksi ###
indeks<- apply(pca$scores[,1:2],1,sum)
indeks
which.min(apply(pca$scores[,1:2],1,sum))

####################Kovaryans matrisi kullan�larak "princomp" ve "prcomp" fonksiyonlar�n�n sonu�lar�!#############
pca_cov<-princomp(food[,2:6],scores=TRUE) #Korelasyon matrisi i�in cor=TRUE eklenmeli!
summary(pca_cov)
names(pca_cov)
pca_cov$loadings ##0.1'den k���k g�stermiyor defaultta!
biplot(pca_cov)
pca_cov$scores

?prcomp
pca_pr_cov<-prcomp(food[,2:6],scores=TRUE) # scale=TRUE korelasyon matrisi sonucunu veriyor!
summary(pca_pr_cov)
names(pca_pr_cov)
pca_pr_cov$rotation
pca_pr_cov$x #scores
biplot(pca_pr_cov)

#Bile�enlere g�re en pahal� �ehirler (Pozitif y�kl� oldu�undan max score al�n�yor!)
which.max(pca_pr_cov$x[,1])
which.max(pca_pr_cov$x[,2])

### Pahal�l�k �ndeksi 2 (cov. matrisine g�re) ###
indeks2<- apply(pca_pr_cov$x[,1:2],1,sum)
indeks2
which.max(apply(pca_pr_cov$x[,1:2],1,sum))

########################## FAKT�R ANAL�Z� ##################################################

#domes<-read_excel("domes_factor.xls")
domes<-read.spss("domes_factor.sav",to.data.frame="TRUE")

veri<-domes[,5:13]

?factanal
fit <- factanal(veri,3,rotation="none") #eksik g�zlem oldu�undan hata veriyor

#eksik g�zlemler d��ar�da b�rak�larak korelasyon matrisi hesab�!
summary(veri)
?cor
R<-cor(veri,use="complete.obs")
R

#Fakt�r Analizi
fit <- factanal(veri, 3, covmat=R, rotation="none")
names(fit)
fit$factors #fakt�r say�s�
fit$loadings #de�i�kenlerin fakt�rdeki y�k�
print(fit, digits=2, cutoff=.3, sort=TRUE)

#Varimax D�n���m� ile
fit_var <- factanal(veri, 3, covmat=R, rotation="varimax")
yuk<-fit_var$loadings[,1:3] #De�i�kenlerin fakt�rdeki y�kleri
print(fit_var, digits=2, cutoff=.3, sort=TRUE) #0.3 de�erinden itibaren kesilmi� y�kler!

#De�i�kenlerin Fakt�r Y�k� Grafi�i
plot(yuk,type="n") # set up plot
text(yuk,labels=names(veri),cex=.7)

########################################################################################################
?KMO
KMO(R)



