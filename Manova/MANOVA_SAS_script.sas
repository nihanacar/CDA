
/****************************************************************/
 /*         MANOVA SAS Script                                   */
 /*                                                             */
 /*                                                             */
 /*                      Nihan Acar-Denizli, Asst. Prof.        */
 /*                      Mimar Sinan Güzel Sanatlar Ünv.        */
 /****************************************************************/


/* Telefon Ýþletim Sistemi MANOVA örneði */

/* Telefonlara ait bapýmlý deðiþkenler için Varyans-Kovaryans Matrisinin Homojenliði Testi */
proc discrim data=WORK.Tel pool=test;
class isletim;
var  Kullanim Hiz Sarj Memnuniyet;
run;

/* Telefon Ýþletim Sistemi MANOVA örneði1 (solution ile farklýlýklarýn incelenmesi)*/
PROC GLM DATA= WORK.Tel;
class isletim ;
model Kullanim Hiz Sarj Memnuniyet = isletim /solution ss3;
manova h = isletim;
run;

/* Telefon Ýþletim Sistemi MANOVA örneði2 (lsmeans prosedürü ile farklýlýklarýn incelenmesi)*/
PROC GLM DATA= WORK.Tel; 
class isletim ;
model Kullanim Hiz Sarj Memnuniyet = isletim /ss3;
manova h = isletim;
lsmeans isletim/ diff;
run;

/* Okul ders baþarýlaýrnýn karþýlaþtýrýlmasý */
PROC GLM DATA= WORK.okul;
class Program ;
model Sosyal_puan Fen_puan matrematik_puan = Program /ss3;
manova h = Program;
lsmeans Program/adjust=Tukey;
run;


/* Toprak verisi MANOVA örneði */

PROC GLM DATA= WORK.Toprak;
class ilce ;
model lnpot lnfosfor lnkalsiyum = ilce /ss3;
manova h = ilce;
/* Toprak mineral miktarýnýn ilcelere göre hangi gruplar arasýnda ne kadar farklýlýk gösterdiðinin hesabý için */ 
lsmeans ilce/ diff;
/* lsmeans ilce / adjust=Tukey; */ 
run;

/* Toprak ve bitki türlerine göre etkileþim terimli MANOVA modeli */

PROC GLM DATA= WORK.Toprak;
class ilce Bitkituru ;
model lnpot lnfosfor lnkalsiyum = ilce*Bitkituru /ss3;
manova h = ilce*Bitkituru;
lsmeans ilce Bitkituru ilce*Bitkituru/ diff;
/* lsmeans ilce / adjust=Tukey; */ 
run;


