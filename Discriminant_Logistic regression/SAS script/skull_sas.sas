/****************************************************************/
 /*      SAS Script: DISCRIMINANT ANALYSIS                      */
 /*                  LOGISTIC REGRESSION ANALYSIS               */
 /*                                                             */
 /*                      Nihan Acar-Denizli, Asst. Prof.        */
 /*                      Mimar Sinan Güzel Sanatlar Ünv.        */
 /****************************************************************/


proc discrim data=skulls pool=test simple manova wcov crossvalidate;
class type;
var lenght width height faceheight facewidth;
run;

proc stepdisc data=skulls sle=.05 sls=.05 ;
class type;
var lenght--facewidth;
run;

proc discrim data=skulls crossvalidate;
class type;
var faceheight;
run;

proc logistic data=skulls;
model type=lenght width height faceheight facewidth /expb;
run;

proc logistic data=skulls;
model type=lenght width height faceheight facewidth /expb rsq selection=stepwise;
run;


proc genmod data=skulls;
model type=lenght width height faceheight facewidth /dist=binomial;
run;
