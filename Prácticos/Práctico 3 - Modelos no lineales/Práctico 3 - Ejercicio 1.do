/* ##############################################

* PRACTICA 3 - MODELOS DE REGRESI�N NO LINEALES

############################################### */

capture clear all
capture log close
set more off

global ruta "G:\UdelaR\CCEEA\Semestre 7\Econometr�a II\Pr�cticos\Pr�ctico 3 - Modelos no lineales"

/* ###############

* EJERCICIO 1

############### */

* DATOS DE CAlonMa.dta

capture log close
log using "$ruta\Taller_3_2016_MNL.log", replace

use "$ruta\CAlonMA.dta", clear

/* Estad�stica descriptiva de la base de datos a utilizar */

describe
sum
pwcorr y n k, star(0.05)
xtile Xn=n, nq(5)					// genera una variable que establece en qu� quintil de la distribuci�n de la variable n, se encuentra cada observaci�n.
xtile Xk=k, nq(5)

tabstat y, s(mean sd) by(Xn)
tabstat y, s(mean sd) by(Xk)		// puede verse la tendencia creciente que tiene el output respecto tanto al trabajo como al capital usado

/* ##### PARTE 4 ##### */

* FUNCION DE PRODUCCI�N COBB-DOUGLAS

* Estimaci�n MCO de la funci�n de producci�n Cobb-Douglas

reg y n k				// notar que las variables est�n expresadas en logaritmos

* Contraste Retornos Constantes a Escala

test n+k=1				// se rechaza la existencia de retornos constantes a escala

* Estimaci�n de la funci�n de producci�n Cobb-Douglas errores robustos

reg y n k, robust

* Contraste Retornos Constantes a Escala Cobb Douglas, robustos

test n+k=1				// se rechaza la existencia de retornos constantes a escala


* FUNCION DE PRODUCCI�N CES

g N=exp(n)
g K=exp(k)
g Y=exp(y)

nl (y = {beta0}+(1/{r})*ln({a}*N^{r}+(1-{a})*K^{r}) ), variables(N K) initial(beta0 2 a 0.5 r 0.05) 

/* El comando nl habilita la introducci�n de una forma funcional que se desee. 

	Opciones:
		- la opci�n initial habilita a establecer valores iniciales de los par�metros. Sin una semilla en la opci�n initial no puede estimar.
		- la opci�n variables no influye en los resultados pero es necesaria para luego correr el comando mfx que calcula efectos marginales. */

/* ##### PARTE 5 ##### */

* FUNCION DE PRODUCCION COBB-DOUGLAS

* Estimaci�n MCO de la funci�n de producci�n Cobb-Douglas con errores robustos

reg y n k, robust

* Muestra la matrix de varianzas y covarianzas de los regresores

matrix list e(V)

*FUNCION DE PRODUCCION CES

* Estimaci�n con errores robustos

nl (y = {beta0}+(1/{r})*ln({a}*N^{r}+(1-{a})*K^{r}) ),  variables(N K) initial(beta0 2 a 0.5 r 0.05) r

* Muestra la matrix de varianzas y covarianzas de los regresores

matrix list e(V)

/* ##### PARTE 6 ##### */

* Efectos parciales

/* los efectos parciales de la funcion CD pueden interpretarse como como elasticidades debido a que es lineal en los par�metros

En cambio, la CES, al ser no lineal, requiere un tratamiento diferente */

nl (y = {beta0}+(1/{r})*ln({a}*N^{r}+(1-{a})*K^{r}) ),  variables(N K) initial(beta0 2 a 0.5 r 0.05) r

mfx
margins, dydx(N K) atmeans //efecto marginal evaluado en la media
margins, dydx(N K) //efecto marginal promedio

/* ##### PARTE 8 ##### */

/* Si r = 0, entonces la funci�n CES tiene, en el l�mite, a una funci�n Cobb-Douglas */

nl (y = {beta0}+(1/{r})*ln({a}*N^{r}+(1-{a})*K^{r}) ),  variables(N K) initial(beta0 2 a 0.5 r 0.05) r

test [r]_cons=0 

/* se rechaza la Ho r=0, por lo que la funci�n CES parece m�s adecuada que la Cobb-Douglas para estos datos. */

/* ##### FIN DEL EJERCICIO ##### */

log close

***************** FIN DE LA PROGRAMACI�N *****************
