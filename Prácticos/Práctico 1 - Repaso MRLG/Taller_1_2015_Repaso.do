* ECONOMETRÍA II AÑO 2016 - TALLER 1

* PRACTICO 1: REPASO MODELO DE REGRESION LINEAL GENERAL

/* Datos del Current Population Survey - 1997 (1000 trabajadores
norteamericanos), tomado de Hill, Griffiths y Lim, 2006. */

clear
set more 1
set matsize 100
capture log close
pause on

/* Cambio de directorio: Cambiar la ruta según la carpeta que estemos 
usando */

global ruta "C:\Users\dacza_000\Desktop\UdelaR\CCEEA\Semestre 7\Econometría II\Prácticos\Práctico 1 - Repaso MRLG"

*INICIAR ARCHIVO LOG: genera un log file final

log using "$ruta\Taller_1_2016.log", replace

/* ABRIR LA BASE DE DATOS: la abre desde la dirección que le entramos como ruta */

use "$ruta\taller1.dta", clear

/* ########################################################################## */

*PARTE I

*1. Estadisticas descriptivas.

rename wage log_wage
sum log_wage educ exper female

gen wage = exp(log_wage)
sum wage educ exper female

*Histogramas de los regresores discretos

hist educ, discrete
graph save "$ruta\histograma de educación.gph", replace

hist exper, discrete kdensity
graph save "$ruta\histograma de experiencia, con kernel.gph", replace

/* Correlaciones entre las variables (la estrella indica la significación al 5%) */

pwcorr log_wage educ exper female, star(0.05)

/* Estos signos son de correlaciones simples y no deben confundirse con los signos de los parámetros de la regresión lineal, pero brindan una idea de los 
posibles resultados esperados */

/* 2. Tabla de la variable WAGE, agrupando a los individuos segun sexo */

tabstat log_wage, by(female) s(mean var count)

* Regresión lineal 

/* Reg devuelve una regresión por MCO */

reg wage female

matrix alphas = e(b) /* Guardo la matriz de coefficientes asociados a los regresores */
scalar alpha_0=alphas[1,colsof(alphas)] /* Genero el beta_0 */
local aux=colsof(alphas)-1 /* Genero un local igual a la cantidad de coefs de regresores (sin contar la constante) */

/* Genero un loop que guarda el resto de los betas (beta_1, beta_2, ... ) */
forvalues i=1/`aux' {
scalar alpha_`i'=alphas[1,`i']
}
*

/* Cálculo de la varianza del modelo */

scalar SCR_1 = e(rss)
scalar var_1 = SCR_1 / e(df_r)

/* 3. Tabla de la variable WAGE, agrupando a los individuos años de 
educación */

tabstat log_wage, by(educ) s(mean sd count)

* Regresión lineal 

reg log_wage educ

matrix betas = e(b) /* Guardo la matriz de coefficientes asociados a los regresores */
scalar beta_0 = betas[1,colsof(betas)] /* Genero el beta_0 */
local aux = colsof(betas)-1 /* Genero un local igual a la cantidad de coefs de regresores (sin contar la constante) */

/* Genero un loop que guarda el resto de los betas (beta_1, beta_2, ... ) */
forvalues i=1/`aux' {
scalar beta_`i' = betas[1,`i']
}
*

scalar log_sal_12 = beta_0 + beta_1 * 12
scalar log_sal_15 = beta_0 + beta_1 * 15

scalar sal_12 = exp(log_sal_12)
scalar sal_15 = exp(log_sal_15)

display sal_12 
display sal_15

* 5. Varianza condicional en educación

g ceduc=0 if educ!=.
replace ceduc=1 if educ>0 & educ!=.
replace ceduc=2 if educ>6 & educ!=.
replace ceduc=3 if educ>9 & educ!=.
replace ceduc=4 if educ>12 & educ!=.
tabstat log_wage, by(ceduc) s(mean var)

/* ########################################################################## */

*PARTE II

/* Genero una variable que es igual a la suma de todas las variables a utilizar asi, si hay missing en alguna variable la variable "control" será también 
missing */

g control=wage+educ+exper+female

* Filtro la base eliminando aquellos casos con control = missing

drop if control==.

*Generar variable de experiencia al cuadrado

gen exper2=exper*exper

*REGRESION 1a: ESTIMACION MCO

reg log_wage educ exper exper2 female

matrix gammas = e(b) /* Guardo la matriz de coefficientes asociados a los regresores */
scalar gamma_0 = gammas[1,colsof(gammas)] /* Genero el beta_0 */
local aux = colsof(gammas)-1 /* Genero un local igual a la cantidad de coefs de regresores (sin contar la constante) */

/* Genero un loop que guarda el resto de los betas (beta_1, beta_2, ... ) */
forvalues i=1/`aux' {
scalar gamma_`i' = gammas[1,`i']
}
*

/* Pregunta 9 */

scalar log_sal_12_8_0 = gamma_0 + gamma_1 * 12 + gamma_2 * 8 + gamma_3 * 8^2 + gamma_4 * 0
scalar log_sal_12_8_1 = gamma_0 + gamma_1 * 12 + gamma_2 * 8 + gamma_3 * 8^2 + gamma_4 * 1

scalar sal_12_8_0 = exp(log_sal_12_8_0)
scalar sal_12_8_1 = exp(log_sal_12_8_1)

display sal_12_8_0
display sal_12_8_1

/* ########################################################################## */

*PARTE III
/* Contraste de la hipótesis “la experiencia no afecta los salarios una vez que se ha controlado por los años de educación y el sexo de la persona” */

*REGRESION 2.1: Modelo sin restringir

reg log_wage educ exper exper2 female 
scalar Vero_NoRes=e(ll)

* REGRESION 2.2: Modelo restringido

reg log_wage educ female 
scalar Vero_Res=e(ll)

* CONTRASTE ML (suponiendo que no hay heteroscedasticidad)
*1 - obtener los residuos del modelo restringido

reg log_wage educ female
predict residuos, resid

*2 - regresar los residuos del modelo restringido sobre todas las variables

reg residuos educ exper exper2 female

*3 - calcular el estadístico de ML del siguiente modo:

scalar ML = e(N)*e(r2)
scalar pvalor_ML = chi2tail(2,ML)
scalar dir ML pvalor_ML

/* CONCLUSIÓN: ML > chi => Rechazo H0 => exper es significativa a la hora de determinar los salarios */

* CONTRASTE DE WALD (suponiendo que no hay heteroscedasticidad):

reg log_wage educ exper exper2 female
test exper exper2

/* Notar que el STATA da el estadístico F(q,N-K) que equivale asintóticamente a una CHI2(q)/q, en este caso q=2 */

scalar WALD = r(F)*2
scalar pvalor_WALD = chi2tail(2,WALD)
scalar dir WALD pvalor_WALD

/* Notar: el pvalor_WALD es casi idéntico al que se obtuvo a través del comando "test", */

/* CONCLUSIÓN: WALD > chi => Rechazo H0 => exper es significativa a la hora de determinar los salarios */

/* CONTRASTE RATIO DE VEROSIMILITUDES (suponiendo que no hay heteroscedasticidad): */

scalar RV = -2*(Vero_Res-Vero_NoRes)
scalar pvalor_RV = chi2tail(2,RV)
scalar dir RV pvalor_RV

/* CONCLUSIÓN: RV > chi => Rechazo H0 => exper es significativa a la hora de determinar los salarios */

/* ########################################################################## */

*PARTE IV

*Crear variable dummy de "male"

gen male = 0
replace male = 1 if female == 0

*MODELO BASE

reg log_wage educ exper exper2 female

* Forma alternativa de escribir el modelo base

reg log_wage educ exper exper2 female male, nocons

* Modelo 2 (permitiendo que tanto la constante como los betas difieran entre hombres y mujeres

gen edu_fem = educ*female
gen exp_fem = exper*female
gen exp2_fem = exper2*female

reg log_wage educ exper exper2 edu_fem exp_fem exp2_fem female 

/* Notar que este podría ser un modelo "no restringido" frente al modelo base "restringido" */

* Forma alternativa de escribir el modelo 2

reg log_wage educ exper exper2 edu_fem exp_fem exp2_fem female male, nocons

*Otra forma alternativa de escribir el modelo 2

gen edu_male = educ*male
gen exp_male = exper*male
gen exp2_male = exper2*male

reg log_wage edu_male exp_male exp2_male edu_fem exp_fem exp2_fem female male, nocons

* Otra forma alternativa de escribir el modelo 2

reg log_wage educ exper exper2 if female == 1
reg log_wage educ exper exper2 if female == 0

* Algunos contrastes en el modelo 2
* Primero vuelvo a estimar el modelo (en este caso escrito como en la primera alternativa)

reg log_wage educ exper exper2 edu_fem exp_fem exp2_fem female 

/* Contraste 1: 
 Ho: Los retornos de la educación son iguales para hombres y mujeres
 Corresponde a un test de significación individual de la variable 
	edu_fem */

/* Contraste 2: 
   Ho: Los retornos de la experiencia son iguales para hombres y 
   mujeres
   Corresponde a un test de significación conjunta de las variables 
   exp_fem y exp2_fem */

test exp_fem exp2_fem
scalar WALD2 = r(F)*2
scalar pvalor_WALD2 = chi2tail(2,WALD2)
scalar dir WALD2 pvalor_WALD2

/* ########################################################################## */

* PARTE V: HETEROSCEDASTICIDAD

* REGRESION 1a: ESTIMACION MCO

reg log_wage educ exper exper2 female

* Análisis gráfico de los resitudos

rvfplot, yline(0) mcolor(purple) saving(log_wage, replace)
rvpplot educ, yline(0) mcolor(green) saving(educ, replace)
rvpplot exper, yline(0) mcolor(blue) saving(exper, replace)
rvpplot exper2, yline(0) mcolor(gold) saving(exper2, replace)
rvpplot female, yline(0) mcolor(pink) saving(female, replace)
graph combine log_wage.gph educ.gph exper.gph exper2.gph female.gph, rows(3) cols(3) holes(2)
graph save "$ruta\residuos x regresores.gph", replace

* Contraste de White
/* Este es un contraste genérico que indica si existe hetero. o no, pero no indica qué variable genera el problema */

estat imtest, white

/* el p valor de la prueba es 0.0006, por lo que rechazo H0 => afirmo que existe heteroscedasticidad */

* Contraste Breusch-Pagan

/* Primero busco hetero en el modelo */

estat hettest educ exper exper2

* Conclusión: existe hetero. Esto confirma la conclusión del test de White

/* Luego busco hetero en las variables específicas */

estat hettest /* Busca hetero en log_wage */
estat hettest educ 
estat hettest exper
estat hettest exper2


* REGRESION 1b: ESTIMACION MCO CON MATRIZ DE WHITE

reg log_wage educ exper exper2 female, robust

/* Las fórmulas de los estadísticos de contrastes de restricciones lineales (F, Wald y ML) cambian ante la presencia de heterocedasticidad la forma más 
sencilla de tratar con esto en STATA es hacer un contraste de Wald utilizando el comando "test" después de estimar el modelo con errores estándar robustos */

reg  log_wage educ exper exper2 female, robust
test exper exper2 

/* Comparar con contraste hecho anteriormente (cuando suponiamos que no hay heterocedasticidad) */

reg log_wage educ exper exper2 female
test exper exper2 

* Cierre del archivo log

capture log close

***** FIN DE LA PROGRAMACIÓN *****
