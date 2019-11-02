*ARCHIVO DO TALLER ECONOMETRÍA II 
* Usando datos de taller2_VI.dta

capture clear all
capture clear
capture log close
set memo 100m
set more off
set matsize 400

*Cambio de directorio: Cambiar la ruta según la computadora que estemos usando

global ruta "C:\Users\dacza_000\Desktop\UdelaR\CCEEA\Semestre 7\Econometría II\Prácticos\Práctico 2 - Omisión de variables, errores de medida, variables instrumentales"

*INICIAR ARCHIVO LOG: genera un log file final
log using "$ruta\practica2.log", replace

*ABRIR LA BASE DE DATOS: la abre desde la dirección que le entramos como ruta
use "$ruta\Ej4_regresoresendogenosyVI.dta", clear

*Estadisticas descriptivas
describe
sum 

*Renombrar variables
rename  ssiratio RatioSS
rename  age  Edad
rename female Mujer
rename lowincome IngresoBajo
rename income Ingreso
rename totchr Cronicas
rename hi_empunion Seguro
rename drugexp GtoMedicam 
* genero las variables en logaritmos
g lGtoMedicam= ln( GtoMedicam)
g lIngreso= ln( Ingreso)
*Estadisticas descriptivas para RatioSS
sum RatioSS

*Tabular la variable Edad
tab Edad

*Histograma de la variable Edad
histogram Edad, discrete frequency lcolor(black) normal title(Histograma)

*Tabular la variable Mujer
tab Mujer

*Tabular las variables GtoMedicam e Ingreso por media, desv. std, percentil 25 50 75
tabstat GtoMedicam Ingreso, s(mean sd p25 p50 p75)

*Idem que punto anterior pero filtrando por la variable Mujer
tabstat GtoMedicam Ingreso, s(mean sd p25 p50 p75) by(Mujer)

*Matriz de correlaciones indicando aquellas significativas al 5%
pwcorr lGtoMedicam lIngreso IngresoBajo Edad Mujer Cronicas Seguro RatioSS, star(0.05)

*Generar distribución del ingreso por quintiles
xtile xIngreso=Ingreso, nq(5)

*Tabular GtoMedicam por media y desv. std, filtrando por la variable Seguro
tabstat GtoMedicam, s(mean sd) by(Seguro)

*Tabular GtoMedicam por media y desv. std, filtrando por la variable Ingreso
tabstat GtoMedicam, s(mean sd) by(xIngreso)

*Estimación 1
reg lGtoMedicam Seguro Edad Cronicas Mujer lIngreso
estimates store MCO

*Estimación 2
reg Seguro RatioSS Edad Cronicas Mujer lIngreso

*Estimación 3
reg Seguro IngresoBajo Edad Cronicas Mujer lIngreso

*Estimación 4
ivreg lGtoMedicam Edad Cronicas Mujer lIngreso  ( Seguro = RatioSS)
estimates store VI_RATIOSS

*Estimación 5
ivreg lGtoMedicam Edad Cronicas Mujer lIngreso  ( Seguro = IngresoBajo)
estimates store VI_IngresoBajo

*Contraste 1
hausman VI_RATIOSS MCO

*Contraste 2
hausman VI_IngresoBajo MCO

*Estimación 6
ivreg lGtoMedicam Edad Cronicas Mujer lIngreso  ( Seguro = RatioSS IngresoBajo)
estimates store VI_2

*Contraste 3
hausman VI_2 MCO

