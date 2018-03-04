/****************************************************************/
 /*      SAS Script: CLUSTER ANALYSIS                           */
 /*                                                             */
 /*                                                             */
 /*                      Nihan Acar-Denizli, Asst. Prof.        */
 /*                      Mimar Sinan Güzel Sanatlar Ünv.        */
 /****************************************************************/

proc cluster data=nutrient simple method=centroid RMSSTD RSQUARE nonorm outtree=centroid; 

/*method="average","complete", "single" ya da "centroid" */
/*SIMPLE basit istatistikelri verir,
  RMSSTD her küme için root-mean-square standard deviation verir, 
  RSQUARE  küme çözümleri için R2 and semipartial R2 değerlerini verir.
  NONORM uzaklıkları normalleştirmeden verir. */ 

ID Food_Item; /*ID gözlemlerin isimlerini görüntülemek için*/
VAR Calories Protein Fat Calcium Iron; /*Analize girecek değişkenlerin adları*/
run;

data nutrient2; /*Aykırı değer olan Canned Sardine gözleminin veri setinden çıkarılması*/
set nutrient;
if Food_Item not in ('Canned sardines');
run;
proc cluster data=nutrient2 simple method=centroid RMSSTD RSQUARE nonorm out=tree; 
ID Food_Item; /*ID gözlemlerin isimlerini görüntülemek için*/
VAR Calories Protein Fat Calcium Iron; /*Analize girecek değişkenlerin adları*/
run;

proc tree data=tree out=clust nclusters=3; /*dendogram çizimi prosedürü*/
/*NCLUSTERS= OUT= içinde olması istenen küme sayısı*/
Id Food_Item;
copy Calories--Iron;
run;

proc sort; by cluster;
proc print; by cluster;
var Calories Protein Fat Calcium Iron;
title '3 Küme için Çözüm';
run;

proc means data=clust; /* Değişkenlerin küme ortalamaları */
var Calories Protein Fat Calcium Iron;
by cluster;
run;
proc glm data=clust; /*Değişkenlerin kümelemede anlamlı olup olmadığını görmek için glm komutu kullanılıyor */
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

