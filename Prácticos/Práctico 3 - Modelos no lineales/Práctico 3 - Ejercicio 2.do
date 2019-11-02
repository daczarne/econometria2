/* ###################################

* PRÁCTICA 3 ECONOMETRIA II AÑO 2016

######################################*/

capture clear all
capture log close
set more off

global ruta "G:\UdelaR\CCEEA\Semestre 7\Econometría II\Prácticos\Práctico 3 - Modelos no lineales"

/* ###################################################

* EJERCICIO 2: estimación de un modelo Poisson

#################################################### */

use "$ruta\mus10data.dta", clear

/* ver la base */ browse
keep if year02 == 1 /* Me quedo solo con un año */
label var year02 "= 1 if year 2002"		// este label estaba mal puesto

/* ##### PARTE 1 ##### */

* ESTADÍSTICOS DESCRIPTIVOS

describe
sum
tab docvis
g docvis2 = docvis
replace docvis2 = 5 if docvis >= 5
tab docvis2
tab docvis2 private						// tabla con frecuencias absolutas
tab docvis2 private, col nofreq			// tabla con frecuencias relativas Opciones: 
												// la opción col establece que se armen las frecuencias relativas por columnas
												// la opción nofreq establece que no muestre las frecuencias absolutas, sólo las relativas

/* ##### PARTE 3 ##### */

* ESTIMACION POR MINIMOS CUADRADOS NO LINEALES

nl (docvis = exp({beta1}*private+{beta2}*chronic+{beta3}*female+{beta4}*income+{beta0})), variables(private chronic female income) vce(robust)

/* El comando habilita la introducción de una forma funcional que se desee, en este caso se utiliza la función exponencial dado que esa es la forma funcional 
de la función de cuantía de la Poisson. 
	Opciones:
		- variables no influye en los resultados pero es necesaria para luego correr el comando mfx que calcula efectos marginales. 
		- vce permite establecer el tipo de errores estándar que queremos. 

	Nótese que el modelo arroja resultados de varias iteraciones. Esto se debe a que la estimación por MCNL se obtiene linealizando la función utilizando el 
	desarrollo de Taylor, alrededor de un valor inicial de los parámetros que luego se modifica con cada iteración para optimizar los resultados. */

* EFECTOS PARCIALES

mfx										// arroja los efectos parciales de todas las variables
margins, dydx(*) atmeans cont			// calcula el promedio de los efectos marginales
margins, dydx(*)

* TEST DE SIGNIFICACIÓN INDIVIDUAL PARA LA VARIABLE income

test [beta4]_cons=0					// se rechaza la Ho: beta4 = 0, por lo que no se puede afirmar que el ingreso no afecte la cantidad de visitas al médico

/* ##### PARTE 5 ##### */

* ESTIMACION CON EL COMANDO POISSON

nl (docvis = exp({beta1}*private+{beta2}*chronic+{beta3}*female+{beta4}*income+{beta0})), variables(private chronic female income) vce(robust)
poisson docvis private chronic female income

* Las estimaciones por MCNL y Poisson no son iguales

/* ##### PARTE 5 ##### */

* EFECTOS PARCIALES (contiene la información sobre la estimación puntual y el intervalo de confianza)

nl (docvis = exp({beta1}*private+{beta2}*chronic+{beta3}*female+{beta4}*income+{beta0})), variables(private chronic female income) vce(robust)
mfx	

poisson docvis private chronic female income
mfx

* SIGNIFICACIÓN DE BETA4

* H0: beta4 = 0 versus H1: beta4 != 0

nl (docvis = exp({beta1}*private+{beta2}*chronic+{beta3}*female+{beta4}*income+{beta0})), variables(private chronic female income) vce(robust)
test [beta4]_cons = 0					// se rechaza la Ho: beta4 = 0

poisson docvis private chronic female income
test income=0							// se rechaza la Ho: beta4 = 0

/* ##### FIN DEL EJERCICIO ##### */

log close

***************** FIN DE LA PROGRAMACIÓN *****************
