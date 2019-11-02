* PROPUESTA DE EXAMEN 
* ECONOMETRÍA II - MARZO 2013
* TEMA: Censurados

capture clear
capture clear all
set more 1	
set matsize 100	
set memory 400m	
global ruta "C:\Users\matias\Desktop\Econometría\Econometría II - 2014\capítulo 4"
capture log close
log using "$ruta\Ejercicio_Censura.log", replace
use "$ruta\basetobit.dta"

**********************************************************************************

rename read lectura
rename math matematica

gen bac1 =0
replace bac1 = 1 if prog == 1
gen bac2 =0
replace bac2 = 1 if prog == 2
gen bac3 =0
replace bac3 = 1 if prog == 3

sum  apt lectura matematica 
tab prog
pwcorr apt lectura matematica bac1 bac2 bac3, star (0.05)

reg apt lectura matematica bac2 bac3
tobit apt lectura matematica bac2 bac3, ul(800)

margins, dydx(lectura matematica bac2 bac3)
margins, dydx(lectura matematica bac2 bac3) predict(e(.,800))
margins, dydx(lectura matematica bac2 bac3) predict(ystar(.,800))

histogram apt, normal

*valor de log-verosimilitud restringuida: -1135,545
