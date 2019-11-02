/* ##############################################

* PRACTICA 3 - MODELOS DE REGRESIÓN NO LINEALES

############################################### */

capture clear all
capture log close
set more off

global ruta "G:\UdelaR\CCEEA\Semestre 7\Econometría II\Prácticos\Práctico 3 - Modelos no lineales"

/* ###############

* EJERCICIO 1

############### */

* DATOS DE CAlonMa.dta

capture log close
log using "$ruta\Taller_3_2016_MNL.log", replace

use "$ruta\CAlonMA.dta", clear

/* Estadística descriptiva de la base de datos a utilizar */

describe
sum
pwcorr y n k, star(0.05)
xtile Xn=n, nq(5)					// genera una variable que establece en qué quintil de la distribución de la variable n, se encuentra cada observación.
xtile Xk=k, nq(5)

tabstat y, s(mean sd) by(Xn)
tabstat y, s(mean sd) by(Xk)		// puede verse la tendencia creciente que tiene el output respecto tanto al trabajo como al capital usado

/* ##### PARTE 4 ##### */

* FUNCION DE PRODUCCIÓN COBB-DOUGLAS

* Estimación MCO de la función de producción Cobb-Douglas

reg y n k				// notar que las variables están expresadas en logaritmos

* Contraste Retornos Constantes a Escala

test n+k=1				// se rechaza la existencia de retornos constantes a escala

* Estimación de la función de producción Cobb-Douglas errores robustos

reg y n k, robust

* Contraste Retornos Constantes a Escala Cobb Douglas, robustos

test n+k=1				// se rechaza la existencia de retornos constantes a escala


* FUNCION DE PRODUCCIÓN CES

g N=exp(n)
g K=exp(k)
g Y=exp(y)

nl (y = {beta0}+(1/{r})*ln({a}*N^{r}+(1-{a})*K^{r}) ), variables(N K) initial(beta0 2 a 0.5 r 0.05) 

/* El comando nl habilita la introducción de una forma funcional que se desee. 

	Opciones:
		- la opción initial habilita a establecer valores iniciales de los parámetros. Sin una semilla en la opción initial no puede estimar.
		- la opción variables no influye en los resultados pero es necesaria para luego correr el comando mfx que calcula efectos marginales. */

/* ##### PARTE 5 ##### */

* FUNCION DE PRODUCCION COBB-DOUGLAS

* Estimación MCO de la función de producción Cobb-Douglas con errores robustos

reg y n k, robust

* Muestra la matrix de varianzas y covarianzas de los regresores

matrix list e(V)

*FUNCION DE PRODUCCION CES

* Estimación con errores robustos

nl (y = {beta0}+(1/{r})*ln({a}*N^{r}+(1-{a})*K^{r}) ),  variables(N K) initial(beta0 2 a 0.5 r 0.05) r

* Muestra la matrix de varianzas y covarianzas de los regresores

matrix list e(V)

/* ##### PARTE 6 ##### */

* Efectos parciales

/* los efectos parciales de la funcion CD pueden interpretarse como como elasticidades debido a que es lineal en los parámetros

En cambio, la CES, al ser no lineal, requiere un tratamiento diferente */

nl (y = {beta0}+(1/{r})*ln({a}*N^{r}+(1-{a})*K^{r}) ),  variables(N K) initial(beta0 2 a 0.5 r 0.05) r

mfx
margins, dydx(N K) atmeans //efecto marginal evaluado en la media
margins, dydx(N K) //efecto marginal promedio

/* ##### PARTE 8 ##### */

/* Si r = 0, entonces la función CES tiene, en el límite, a una función Cobb-Douglas */

nl (y = {beta0}+(1/{r})*ln({a}*N^{r}+(1-{a})*K^{r}) ),  variables(N K) initial(beta0 2 a 0.5 r 0.05) r

test [r]_cons=0 

/* se rechaza la Ho r=0, por lo que la función CES parece más adecuada que la Cobb-Douglas para estos datos. */

/* ##### FIN DEL EJERCICIO ##### */

log close

***************** FIN DE LA PROGRAMACIÓN *****************
