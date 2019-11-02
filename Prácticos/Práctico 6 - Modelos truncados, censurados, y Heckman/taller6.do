clear all
set more 1	
set matsize 100	


global ruta "H:\econometriaII\practicaVDLySS\2015"	

capture log close	
capture log using "$ruta/taller6.log", replace	
	
use "$ruta/per_2009_muj.dta", clear	
keep if edad>=25 & edad<=60 
	
tabstat	horas, s(mean p75)
count if horas==0

*tabstat wage, s(mean p75) 	
*count if wage==0	

g exp_pot=edad-aniosed-6
g exp_cuad=exp_pot^2
g lylabd_hom=ln(1+ylabd_hom)

* 1o. Modelo para las horas trabajadas	
global lista1 "aniosed exp_pot exp_cua"	
global lista2 "cant_hijos"	
global lista3 "lylabd_hom"

g control=horas+aniosed+edad+cant_hijos+lylabd_hom
keep if control!=. //quedan 19919 observaciones

tab horas

*Estimacion MCO utilizando todas laa observaciones
reg horas $lista1  $lista2 
*MODELO TOBIT
tobit horas $lista1  $lista2, robust ll(0)

* Efectos parciales en el modelo latente (en y* según notación de clase)
margins, dydx($lista1 $lista2)


* Efectos parciales en la esperanza de Y condicional en X
margins, dydx($lista1 $lista2) predict(ystar(0,.))

* Efectos parciales en la esperanza de Y condicional en X y Y>0
margins, dydx($lista1 $lista2) predict(e(0,.))


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


*Estimacion MCO utilizando las observaciones no censuradas
reg horas $lista1  $lista2 if horas>0

* MODELO HECKMAN
g seleccion=(horas>0)

* HECKMAN MANUAL
probit seleccion $lista1 $lista2 $lista3
predict Fi_zgammagorro
predict zgammagorro, index
g fi_zgammagorro=normalden(zgammagorro)
g lambdai=fi_zgammagorro/Fi_zgammagorro
reg horas $lista1  $lista2 lambdai if horas>0

* HECKMAN STATA
heckman horas  $lista1 $lista2, sel( seleccion = $lista1 $lista2 $lista3) two mills(lambda1i)
sum lambdai lambda1i
* Efectos parciales en el modelo latente (en y* según notación de clase)
margins, dydx($lista1 $lista2)
* Efectos parciales en la esperanza de Y condicional en X
margins, dydx($lista1 $lista2) predict(ystar(0,.))
* Efectos parciales en la esperanza de Y condicional en X y Y>0
margins, dydx($lista1 $lista2) predict(e(0,.))




* 2o. Modelo para los salarios	
*MCO con observaciones no truncadas
g lsalario=ln(yxhs)
replace lsalario=0 if lsalario==.
reg lsalario  $lista1 if lsalario!=0

*REGRESION TRUNCADA
truncreg lsalario  $lista1, robust ll(0)
margins, dydx($lista1)
margins, dydx($lista1) predict(e(0,.))
margins, dydx($lista1) predict(ystar(0,.))

* MODELO HECKMAN

* HECKMAN MANUAL

* el calculo de landai es igual que en el modelo anterior ya 
* que la ecuación de selección es la misma
* se observan horas y salarios por hora mayores que cero en aquellas
* mujeres que trabajaron al menos una hora

reg lsalario $lista1 lambdai if horas>0

* HECKMAN STATA
heckman lsalario  $lista1, sel( seleccion = $lista1 $lista2 $lista3) two mills(lambda2i)
margins, dydx($lista1)
margins, dydx($lista1) predict(e(0,.))
margins, dydx($lista1) predict(ystar(0,.))

sum lambdai lambda2i

log close

