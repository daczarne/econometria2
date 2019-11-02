/* #########################
PRÁCTICO 6 - EJERCICIO 1
########################## */

clear all
set more 1
set matsize 100

global ruta "C:\Users\dacza\Dropbox\UdelaR\FCEA\Semestre 7\Econometría II\Prácticos\Práctico 6 - Modelos truncados, censurados, y Heckman"

capture log close
capture log using "$ruta/taller6.log", replace

use "$ruta/per_2009_muj.dta", clear
keep if edad>=25 & edad<=60

/* #############
## PARTE I	##
############## */

/* ##### DESCRIPCIÓN DE LA BASE Y LAS VARIABLES RELEVANTES ##### */

tabstat	horas, s(mean p75)
count if horas==0 //cantidad de mujeres que trabajn 0 horas: 7953

tabstat yxhs, s(mean p75)
count if yxhs==0

/* ##### GENERO LAS VARIABLES QUE NO SE ENCUENTRAN EN LA BASE ##### */

g exp_pot=edad-aniosed-6
g exp_cuad=exp_pot^2
g lylabd_hom=ln(1+ylabd_hom)

* Construyo listas con las variables (esto me ahorra tener que escribirlas cuando corro los modelos
global lista1 "aniosed exp_pot exp_cua"	
global lista2 "cant_hijos"	
global lista3 "lylabd_hom"

/* ##### PREGUNTA 2 ##### */

* Me quedo unicamente con las observaviones para las que no existen NA's
g control = horas+aniosed+edad+cant_hijos+lylabd_hom
keep if control!=. //quedan 19919 observaciones

/* ### MCO utilizando todas las observaciones ### */

reg horas $lista1  $lista2 

/* ### TOBIT utilizando todas las observaciones ### */

tobit horas $lista1  $lista2, robust ll(0)

/* # Efectos parciales en el modelo TOBIT # */

* Efectos parciales en el modelo latente (en y* según notación de clase)
margins, dydx($lista1 $lista2)

* Efectos parciales en la esperanza de Y condicional en X
margins, dydx($lista1 $lista2) predict(ystar(0,.))

* Efectos parciales en la esperanza de Y condicional en X y Y>0
margins, dydx($lista1 $lista2) predict(e(0,.))

********************** ALTERNATIVA **********************
{
* Efectos parciales en el tobit(manual)

tobit horas $lista1  $lista2, robust ll(0)
predict xb
matrix eb=e(b)
scalar sigma=el(eb,1,6)
scalar dir sigma
g xb_sigma=xb/sigma
g FIxbi=normal(xb_sigma)
sum FIxbi
scalar FIxb=r(mean)
g fixbi=normalden(xb_sigma)
g lambdai=fixbi/FIxbi
g c=1-(xb_sigma+lambdai)*lambdai
sum c
scalar factori=r(mean)

*en la esperanza de Y condicional en X 
matrix EP1=eb*FIxb
matrix list EP1

*en la esperanza de Y condicional en X  y Y>0
matrix EP2=eb*factori
matrix list EP2



*borramos la variable lambdai, ya que en Heckman calcularemos una variable con este mismo nombre 
* la definición de lambda será la misma (la densidad normal dividida la distribucion normal) pero el 
* indice xb cambiará
drop lambdai
}
*
************ FIN DE LA SOLUCIÓN ALTERNATIVA ************

/* ##### PREGUNTA 4 ##### */

* Estimacion MCO utilizando las observaciones no censuradas
reg horas $lista1  $lista2 if horas>0

* MODELO HECKMAN
g seleccion = (horas>0)

********************** ALTERNATIVA **********************
quiet{
* HECKMAN MANUAL
probit seleccion $lista1 $lista2 $lista3
predict Fi_zgammagorro
predict zgammagorro, index
g fi_zgammagorro=normalden(zgammagorro)
g lambdai=fi_zgammagorro/Fi_zgammagorro
reg horas $lista1  $lista2 lambdai if horas>0
}
*
************ FIN DE LA SOLUCIÓN ALTERNATIVA ************

* HECKMAN STATA
heckman horas  $lista1 $lista2, sel( seleccion = $lista1 $lista2 $lista3) two mills(lambda1i)

* sum lambdai lambda1i //Compara la estimación manual con la de Stata

/* ##### PREGUNTA 7 ##### */

* Efectos parciales en el modelo latente (en y* según notación de clase)
margins, dydx($lista1 $lista2)
* Efectos parciales en la esperanza de Y condicional en X
margins, dydx($lista1 $lista2) predict(ystar(0,.))
* Efectos parciales en la esperanza de Y condicional en X y Y>0
margins, dydx($lista1 $lista2) predict(e(0,.))

/* #############
##	PARTE II  ##
############## */

* MCO con observaciones no truncadas
g lsalario=ln(wage)
replace lsalario=0 if lsalario==.
reg lsalario  $lista1 if lsalario!=0

* REGRESION TRUNCADA
truncreg lsalario  $lista1, robust ll(0)
margins, dydx($lista1)
margins, dydx($lista1) predict(e(0,.))
margins, dydx($lista1) predict(ystar(0,.))

* MODELO HECKMAN

********************** ALTERNATIVA **********************
* HECKMAN MANUAL (requeire de la estimación manual previa
{ 
* el calculo de landai es igual que en el modelo anterior ya que la ecuación de selección es la misma 
* se observan horas y salarios por hora mayores que cero en aquellas mujeres que trabajaron al menos una hora

reg lsalario $lista1 lambdai if horas>0
}
*
************ FIN DE LA SOLUCIÓN ALTERNATIVA ************

* HECKMAN STATA
heckman lsalario  $lista1, sel( seleccion = $lista1 $lista2 $lista3) two mills(lambda2i)
margins, dydx($lista1)
margins, dydx($lista1) predict(e(0,.))
margins, dydx($lista1) predict(ystar(0,.))

* sum lambdai lambda2i

/* ##### FIN DEL EJERCICIO ##### */

log close

**********************************************************
***************** FIN DE LA PROGRAMACIÓN *****************
**********************************************************
