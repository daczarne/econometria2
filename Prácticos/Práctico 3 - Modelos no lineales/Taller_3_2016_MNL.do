* PRACTICA 3 ECONOMETRIA II A�O 2016

capture clear all
capture log close
set more off



global ruta "G:\UdelaR\CCEEA\Semestre 7\Econometr�a II\Pr�cticos\Pr�ctico 3 - Modelos no lineales"


*EJERCICIO 1: ESTIMACION DE FUNCIONES DE PRODUCCION UTILIZANDO LOS 
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

* FUNCION DE PRODUCCION COBB-DOUGLAS

* Estimaci�n MCO de la funci�n de producci�n Cobb-Douglas

reg y n k				// notar que las variables est�n expresadas en logaritmos

* Estimaci�n de la funci�n de producci�n Cobb-Douglas errores robustos

reg y n k, robust


*FUNCION DE PRODUCCION CES

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

nl (y = {beta0}+(1/{r})*ln({a}*N^{r}+(1-{a})*K^{r}) ), variables(N K) initial(beta0 2 a 0.5 r 0.05) 

* Muestra la matrix de varianzas y covarianzas de los regresores

matrix list e(V)

/* ##### PARTE 6 ##### */





* Contraste Retornos Constantes a Escala Cobb Douglas

test n+k=1				// se rechaza la existencia de retornos constantes 
						* a escala




* Contraste Retornos Constantes a Escala, robusto

test n+k=1				// se rechaza la existencia de retornos constantes 
						* a escala




mfx
margins, dydx(N K) atmeans //efecto marginal evaluado en la media
margins, dydx(N K) //efecto marginal promedio

* Errores est�ndar robustos

nl (y = {beta0}+(1/{r})*ln({a}*N^{r}+(1-{a})*K^{r}) ),  variables(N K) initial(beta0 2 a 0.5 r 0.05) r
mfx

*Se pide 8)

test [r]_cons=0 				// se rechaza la Ho r=0 por lo que la 
								* funci�n CES parece m�s adecuada que la 
								* Cobb-Douglas para estos datos.


*********************************************************************************************************

/* EJERCICIO 2: ESTIMACION DE UN MODELO POISSON
(PUEDE VERSE EL ARCHIVO TALLER3_MNL_COMPLEMENTO_PROGRAMACION 
AVANZADA PARA VER LA ESTIMACION POR MAXIMA VEROSIMILITUD) */

use "$ruta\mus10data.dta", clear

/* ver la base */ browse
keep if year02 == 1 /* Me quedo solo con un a�o */
label var year02 "= 1 if year 2002"		// este label estaba mal puesto

*Se pide 1)
*ESTAD�STICOS DESCRIPTIVOS

describe
sum
tab docvis
g docvis2 = docvis
replace docvis2 = 5 if docvis >= 5
tab docvis2
tab docvis2 private						// tabla con frecuencias 
										* absolutas
tab docvis2 private, col nofreq			// tabla con frecuencias relativas
										* (la opci�n col establece que se 
										* armen las frecuencias relativas 
										* por columnas
										// mientras que la opci�n nofreq 
										* establece que no muestre las 
										* frecuencias absolutas, s�lo las 
										* relativas


*Se pide 3)
*ESTIMACION POR MINIMOS CUADRADOS NO LINEALES

nl (docvis = exp({beta1}*private+{beta2}*chronic+{beta3}*female+{beta4}*income+{beta0})), variables(private chronic female income) vce(robust)

/* El comando habilita la introducci�n de una forma funcional que se 
desee, en este caso se utiliza la funci�n exponencial dado que esa es la 
forma funcional de la funci�n de cuant�a de la Poisson. La opci�n 
variables no influye en los resultados pero es necesaria para luego 
correr el comando mfx que calcula efectos marginales. La opci�n vce 
permite establecer el tipo de errores est�ndar que queremos. Notese que 
el modelo arroja resultados de varias iteraciones. Esto se debe a que la 
estimaci�n por MCNL se obtiene linealizando la funci�n utilizando el 
desarrollo de Taylor, alrededor de un valor inicial de los par�metros 
que luego se modifica con cada iteraci�n para optimizar los resultados.
*/

mfx										// arroja los efectos parciales 
										* de todas las variables
margins, dydx(*) atmeans cont				// calcula el promedio de los 
										* efectos marginales
margins, dydx(*)
test [beta4]_cons=0						// se rechaza la Ho: "el ingreso 
										* no afecta la cantidad de 
										* visitas de los m�dicos", por lo 
										* que no se puede afirmar que no 
										* afecte

*Se pide 5)																	
*ESTIMACION CON EL COMANDO POISSON

poisson docvis private chronic female income
mfx	
test income=0							// se rechaza la Ho: "el ingreso no afecta la cantidad de visitas de los m�dicos"

*Se pide 6)

*la estimaci�n de los efectos parciales es la que surge de los comandos mfx en cada caso

*los test para significaci�n de la variable de ingresos en la cantidad de visitas al m�dico est� detallada luego de cada estimaci�n en los puntos anteriores,
* y en ambos casos la hip�tesis de no afectaci�n del ingreso a las visitas se rechaza al 5% de significaci�n.


*********************************************************************************************************

* EJERCICIO 3: ELASTICIDAD INGRESO DEL CONSUMO DE VIVIENDA
use "$ruta\IngGtos_UY_1994.dta", clear
des
sum
pwcorr vivienda ingtot, star(0.05)
xtile Xingtot=ingtot, nq(5)
tabstat vivienda, s(mean sd) by(Xingtot)	// el gasto en vivienda tiene tendencia y varianza creciente con el ingreso

*Se pide 4)
nl (vivienda = {beta0}+{beta1}*(ingtot^{gamma}) ),variables(ingtot) initial(beta0 2 beta1 0.5 gamma 1) r
mfx

*Se pide 5)
test [gamma]_cons=1 						// test de linealidad: no se rechaza que gamma sea 1 al 5% de significaci�n

*Se pide 7)
scalar SIGMA2U_NR=e(rmse) 
reg vivienda ingtot
scalar SIGMA2U_R=e(rmse) 
scalar RV_TEST=e(N)*ln(SIGMA2U_R/SIGMA2U_NR)
scalar pvalorRV=chi2tail(1,RV_TEST)
scalar dir RV_TEST pvalorRV					// no se rechaza que el modelo sea lineal al 5% de significaci�n


*********************************************************************************************************

*EJERCICIO 5

*Se pide 2)
* Transformaci�n BOX-COX 1: 
*   - variable dependiente en logs
*   - regresores transformados seg�n transformaci�n Box-Cox (dos alternativas comandos nl y boxcox)

nl (y = {beta0}+{beta1}*(N^{lambda}-1)/{lambda}+{beta2}*(K^{lambda}-1)/{lambda} ), variables(N K) initial(lambda 0.8 beta0 0.5 beta1 0.5 beta2 0.5)
mfx

boxcox y N K, model(rhsonly)		// estima por MV los par�metros de la transformaci�n Box-Cox
									// la opci�n model(rhsonly) indica que se aplica Box-Cox s�lo del lado derecho, por lo que responde el se pide 2)
									
*N�tese que se rechazan las Ho lambda  =1 e =-1	, pero no se rechaza que lambda =0.									

*Se pide 4)
* Transformaci�n BOX-COX 2, todas las variables transformadas: 
boxcox Y N K, model(lambda)			// la opci�n model(lambda) indica que se aplica Box-Cox de ambos lados con el mismo par�metro, por lo que 
									// responde se pide 4)

*N�tese que se rechazan las Ho lambda =0, =1 e =-1			

***************************************************************************
*Funci�n de producci�n extra, no presentada en la letra del ejercicio
* Funci�n de producci�n trans-log
g nk=n*k
reg y n k nk, r
*efectos parciales
sum n
scalar meann=r(mean)
sum k
scalar meank=r(mean)
scalar EPn=_b[n]+_b[nk]*meank
scalar EPk=_b[k]+_b[nk]*meann
scalar dir EPn EPk

nl (y = {beta0}+{beta1}*n+{beta2}*k+{beta3}*n*k ), variables(n k)
mfx

*******************************************************************************							
									
log close
