/* ###################################

* PRÁCTICA 3 ECONOMETRIA II AÑO 2016

######################################*/

capture clear all
capture log close
set more off

global ruta "G:\UdelaR\CCEEA\Semestre 7\Econometría II\Prácticos\Práctico 3 - Modelos no lineales"

/* #############################################################

* EJERCICIO 3: elasticidad ingreso del consumo de las viviendas

############################################################### */

use "$ruta\IngGtos_UY_1994.dta", clear

* ESTADÍSTICAS DESCRIPTIVAS

des
sum
pwcorr vivienda ingtot, star(0.05)
xtile Xingtot=ingtot, nq(5)
tabstat vivienda, s(mean sd) by(Xingtot)	// el gasto en vivienda tiene tendencia y varianza creciente con el ingreso

/* ##### PARTE 4 ##### */

nl (vivienda = {beta0}+{beta1}*(ingtot^{gamma}) ),variables(ingtot) initial(beta0 2 beta1 0.5 gamma 1) r
mfx

/* ##### PARTE 4 ##### */

test [gamma]_cons=1 						// test de linealidad: no se rechaza que gamma sea 1 al 5% de significación

/* ##### PARTE 7 ##### */

scalar SIGMA2U_NR=e(rmse) 
reg vivienda ingtot
scalar SIGMA2U_R=e(rmse) 
scalar RV_TEST=e(N)*ln(SIGMA2U_R/SIGMA2U_NR)
scalar pvalorRV=chi2tail(1,RV_TEST)
scalar dir RV_TEST pvalorRV					// no se rechaza que el modelo sea lineal al 5% de significación

/* ##### FIN DEL EJERCICIO ##### */

log close

***************** FIN DE LA PROGRAMACIÓN *****************
