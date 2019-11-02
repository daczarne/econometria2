/* ###################################

* PR�CTICA 3 ECONOMETRIA II A�O 2016

######################################*/

capture clear all
capture log close
set more off

global ruta "G:\UdelaR\CCEEA\Semestre 7\Econometr�a II\Pr�cticos\Pr�ctico 3 - Modelos no lineales"

/* ###################################################

* EJERCICIO 2: estimaci�n de un modelo Poisson

#################################################### */

use "$ruta\mus10data.dta", clear

/* ver la base */ browse
keep if year02 == 1 /* Me quedo solo con un a�o */
label var year02 "= 1 if year 2002"		// este label estaba mal puesto

/* ##### PARTE 1 ##### */

* ESTAD�STICOS DESCRIPTIVOS

describe
sum
tab docvis
g docvis2 = docvis
replace docvis2 = 5 if docvis >= 5
tab docvis2
tab docvis2 private						// tabla con frecuencias absolutas
tab docvis2 private, col nofreq			// tabla con frecuencias relativas Opciones: 
												// la opci�n col establece que se armen las frecuencias relativas por columnas
												// la opci�n nofreq establece que no muestre las frecuencias absolutas, s�lo las relativas

/* ##### PARTE 3 ##### */

* ESTIMACION POR MINIMOS CUADRADOS NO LINEALES

nl (docvis = exp({beta1}*private+{beta2}*chronic+{beta3}*female+{beta4}*income+{beta0})), variables(private chronic female income) vce(robust)

/* El comando habilita la introducci�n de una forma funcional que se desee, en este caso se utiliza la funci�n exponencial dado que esa es la forma funcional 
de la funci�n de cuant�a de la Poisson. 
	Opciones:
		- variables no influye en los resultados pero es necesaria para luego correr el comando mfx que calcula efectos marginales. 
		- vce permite establecer el tipo de errores est�ndar que queremos. 

	N�tese que el modelo arroja resultados de varias iteraciones. Esto se debe a que la estimaci�n por MCNL se obtiene linealizando la funci�n utilizando el 
	desarrollo de Taylor, alrededor de un valor inicial de los par�metros que luego se modifica con cada iteraci�n para optimizar los resultados. */

* EFECTOS PARCIALES

mfx										// arroja los efectos parciales de todas las variables
margins, dydx(*) atmeans cont			// calcula el promedio de los efectos marginales
margins, dydx(*)

* TEST DE SIGNIFICACI�N INDIVIDUAL PARA LA VARIABLE income

test [beta4]_cons=0					// se rechaza la Ho: beta4 = 0, por lo que no se puede afirmar que el ingreso no afecte la cantidad de visitas al m�dico

/* ##### PARTE 5 ##### */

* ESTIMACION CON EL COMANDO POISSON

nl (docvis = exp({beta1}*private+{beta2}*chronic+{beta3}*female+{beta4}*income+{beta0})), variables(private chronic female income) vce(robust)
poisson docvis private chronic female income

* Las estimaciones por MCNL y Poisson no son iguales

/* ##### PARTE 5 ##### */

* EFECTOS PARCIALES (contiene la informaci�n sobre la estimaci�n puntual y el intervalo de confianza)

nl (docvis = exp({beta1}*private+{beta2}*chronic+{beta3}*female+{beta4}*income+{beta0})), variables(private chronic female income) vce(robust)
mfx	

poisson docvis private chronic female income
mfx

* SIGNIFICACI�N DE BETA4

* H0: beta4 = 0 versus H1: beta4 != 0

nl (docvis = exp({beta1}*private+{beta2}*chronic+{beta3}*female+{beta4}*income+{beta0})), variables(private chronic female income) vce(robust)
test [beta4]_cons = 0					// se rechaza la Ho: beta4 = 0

poisson docvis private chronic female income
test income=0							// se rechaza la Ho: beta4 = 0

/* ##### FIN DEL EJERCICIO ##### */

log close

***************** FIN DE LA PROGRAMACI�N *****************
