proc cluster data=nutrient simple method=centroid RMSSTD RSQUARE nonorm outtree=centroid; 

/*method="average","complete", "single" ya da "centroid" */
/*SIMPLE basit istatistikelri verir,
  RMSSTD her küme için root-mean-square standard deviation verir, 
  RSQUARE  küme çözümleri için R2 and semipartial R2 deðerlerini verir.
  NONORM uzaklýklarý normalleþtirmeden verir. */ 

ID Food_Item; /*ID gözlemlerin isimlerini görüntülemek için*/
VAR Calories Protein Fat Calcium Iron; /*Analize girecek deðiþkenlerin adlarý*/
run;

data nutrient2; /*Aykýrý deðer olan Canned Sardine gözleminin veri setinden çýkarýlmasý*/
set nutrient;
if Food_Item not in ('Canned sardines');
run;
proc cluster data=nutrient2 simple method=centroid RMSSTD RSQUARE nonorm out=tree; 
ID Food_Item; /*ID gözlemlerin isimlerini görüntülemek için*/
VAR Calories Protein Fat Calcium Iron; /*Analize girecek deðiþkenlerin adlarý*/
run;

proc tree data=tree out=clust nclusters=3; /*dendogram çizimi prosedürü*/
/*NCLUSTERS= OUT= içinde olmasý istenen küme sayýsý*/
Id Food_Item;
copy Calories--Iron;
run;

proc sort; by cluster;
proc print; by cluster;
var Calories Protein Fat Calcium Iron;
title '3 Küme için Çözüm';
run;

proc means data=clust; /* Deðiþkenlerin küme ortalamalarý */
var Calories Protein Fat Calcium Iron;
by cluster;
run;
proc glm data=clust; /*Deðiþkenlerin kümelemede anlamlý olup olmadýðýný görmek için glm komutu kullanýlýyor */
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

