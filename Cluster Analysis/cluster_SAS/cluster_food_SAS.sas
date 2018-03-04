proc cluster data=nutrient simple method=centroid RMSSTD RSQUARE nonorm outtree=centroid; 

/*method="average","complete", "single" ya da "centroid" */
/*SIMPLE basit istatistikelri verir,
  RMSSTD her k�me i�in root-mean-square standard deviation verir, 
  RSQUARE  k�me ��z�mleri i�in R2 and semipartial R2 de�erlerini verir.
  NONORM uzakl�klar� normalle�tirmeden verir. */ 

ID Food_Item; /*ID g�zlemlerin isimlerini g�r�nt�lemek i�in*/
VAR Calories Protein Fat Calcium Iron; /*Analize girecek de�i�kenlerin adlar�*/
run;

data nutrient2; /*Ayk�r� de�er olan Canned Sardine g�zleminin veri setinden ��kar�lmas�*/
set nutrient;
if Food_Item not in ('Canned sardines');
run;
proc cluster data=nutrient2 simple method=centroid RMSSTD RSQUARE nonorm out=tree; 
ID Food_Item; /*ID g�zlemlerin isimlerini g�r�nt�lemek i�in*/
VAR Calories Protein Fat Calcium Iron; /*Analize girecek de�i�kenlerin adlar�*/
run;

proc tree data=tree out=clust nclusters=3; /*dendogram �izimi prosed�r�*/
/*NCLUSTERS= OUT= i�inde olmas� istenen k�me say�s�*/
Id Food_Item;
copy Calories--Iron;
run;

proc sort; by cluster;
proc print; by cluster;
var Calories Protein Fat Calcium Iron;
title '3 K�me i�in ��z�m';
run;

proc means data=clust; /* De�i�kenlerin k�me ortalamalar� */
var Calories Protein Fat Calcium Iron;
by cluster;
run;
proc glm data=clust; /*De�i�kenlerin k�melemede anlaml� olup olmad���n� g�rmek i�in glm komutu kullan�l�yor */
class cluster;
model Calcium=cluster;
run;

proc glm data=clust; 
class cluster;
model Calories=cluster;
run;

proc glm data=clust; 
class cluster;
model Fat=cluster;
run;

proc glm data=clust; 
class cluster;
model Iron=cluster;
run;

