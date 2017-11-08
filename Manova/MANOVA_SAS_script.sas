
/****************************************************************/
 /*         MANOVA SAS Script                                   */
 /*                                                             */
 /*                                                             */
 /*                      Nihan Acar-Denizli, Asst. Prof.        */
 /*                      Mimar Sinan G�zel Sanatlar �nv.        */
 /****************************************************************/


/* Telefon ��letim Sistemi MANOVA �rne�i */

/* Telefonlara ait bap�ml� de�i�kenler i�in Varyans-Kovaryans Matrisinin Homojenli�i Testi */
proc discrim data=WORK.Tel pool=test;
class isletim;
var  Kullanim Hiz Sarj Memnuniyet;
run;

/* Telefon ��letim Sistemi MANOVA �rne�i1 (solution ile farkl�l�klar�n incelenmesi)*/
PROC GLM DATA= WORK.Tel;
class isletim ;
model Kullanim Hiz Sarj Memnuniyet = isletim /solution ss3;
manova h = isletim;
run;

/* Telefon ��letim Sistemi MANOVA �rne�i2 (lsmeans prosed�r� ile farkl�l�klar�n incelenmesi)*/
PROC GLM DATA= WORK.Tel; 
class isletim ;
model Kullanim Hiz Sarj Memnuniyet = isletim /ss3;
manova h = isletim;
lsmeans isletim/ diff;
run;

/* Okul ders ba�ar�la�rn�n kar��la�t�r�lmas� */
PROC GLM DATA= WORK.okul;
class Program ;
model Sosyal_puan Fen_puan matrematik_puan = Program /ss3;
manova h = Program;
lsmeans Program/adjust=Tukey;
run;


/* Toprak verisi MANOVA �rne�i */

PROC GLM DATA= WORK.Toprak;
class ilce ;
model lnpot lnfosfor lnkalsiyum = ilce /ss3;
manova h = ilce;
/* Toprak mineral miktar�n�n ilcelere g�re hangi gruplar aras�nda ne kadar farkl�l�k g�sterdi�inin hesab� i�in */ 
lsmeans ilce/ diff;
/* lsmeans ilce / adjust=Tukey; */ 
run;

/* Toprak ve bitki t�rlerine g�re etkile�im terimli MANOVA modeli */

PROC GLM DATA= WORK.Toprak;
class ilce Bitkituru ;
model lnpot lnfosfor lnkalsiyum = ilce*Bitkituru /ss3;
manova h = ilce*Bitkituru;
lsmeans ilce Bitkituru ilce*Bitkituru/ diff;
/* lsmeans ilce / adjust=Tukey; */ 
run;


