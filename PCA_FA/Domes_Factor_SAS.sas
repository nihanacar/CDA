/* SAS SCRIPT: Factor Analysis     (data set: "domes") */
/*                  Nihan Acar-Denizli                 */
/*                  Asst. Prof. Dr.                    */


proc factor data=domes /*name of data file*/
 nobs=397 /*number of observations*/
 corr /*print correlation matrix*/
 /*priors=max  types of priors to be used*/
 method=principal /*method of extraction*/
 nfactors=3 /*number of factors to retain*/
 maxiter = 25 /*maximum number of iterations*/
 rotate=varimax  /*type of rotation */
 scree /*print of scree plot*/
 res /*display residual correlation matrix*/
 preplot
 plot
heywood;/*sets to 1 any communality greater than 1, allowing iterations to proceed*/
var provo protect mental caused save insane passion defend stable; /*variables to be included*/
run;
