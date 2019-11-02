/* #########################

PRÁCTICO 5 - EJERCICIO 4

########################## */

* Se limpia y configura la memoria
capture clear all
set memory 80m	
set more off

* Se establece global con ruta de la carpeta de trabajo
global ruta "G:\UdelaR\CCEEA\Semestre 7\Econometría II\Prácticos\Práctico 5 - Modelos binarios"

* Se cierra cualquier archivo log abierto y se abre un nuevo log
capture log close
log using "$ruta\taller5_2015_EJ4.log", replace	

* Se abre la base MROZ y se ordenan las variables alfabéticamente
use "$ruta/CRIME.dta", clear	
aorder

* Generación de la lista de variables, la variable binaria y el punto de corte en relación a la proporción de unos. 
global lista "pcnv avgsen tottime ptime86 inc86 black hispan born60"	

/* ##### PARTE 1 ##### */

gen arr86 = 0
replace arr86 = 1 if narr86 > 0

egen propunos=mean(arr86)

*DESCRIPCION DE LA BASE Y LAS VARIABLES RELEVANTES

sum
tab arr86
tabstat $lista, by(arr86) s(mean)
sum propunos
pwcorr $lista,star(0.05)

*************************************
**  MODELO DE PROBABILIDAD LINEAL  **
*************************************

reg arr86 $lista
reg arr86 $lista, robust

*se guardan los valores ajustados en una variable prMPL (predicción de la probabilidad de cada individuo)
predict prMPL

*notar que existen estimaciones de probabilidad fuera del intervalo [0,1]
count if[prMPL<0]
count if[prMPL>1]

*predicciones de INLF utilizando como valor de corte la proporción muestral de unos
g UNOMPL=(prMPL>propunos)
*BONDAD DE AJUSTE: Tabla de proporción de 0 y 1 correctos
tab arr86 UNOMPL, col	

*predicciones de INLF utilizando como valor de corte 0,5
g UNOMPL2=(prMPL>0.5)
*BONDAD DE AJUSTE: Tabla de proporción de 0 y 1 correctos
tab arr86 UNOMPL2, col	

*Efecto de pasar en pcvn de 0.25 a 0.75
gen EPpcnv=_b[pcnv]*0.75 - _b[pcnv]*0.25
sum EPpcnv

*Prueba conjunta de avgsen y tottime
test avgsen tottime

*********************
**  MODELO PROBIT  **
*********************

probit arr86 $lista

*se guardan los valores ajustados en una variable prMPL (predicción de la probabilidad de cada individuo)
predict prPROBIT

*Bondad de ajuste 
* 1. Valor por defecto de STATA: 0.5
estat class
* 2. Usando proporción de unos en la muestra
sum propunos
local propunos=r(mean)
estat class, cutoff(`propunos')

*EFECTOS PARCIALES
*por defecto son en la media de todas las variables
mfx compute

*otras pruebas
mfx compute, at(black=1 hispan=0 born60=1)
*Efecto de cambio en pcnv de 0.25 a 0.75
mfx compute, at(black=1 hispan=0  born60=1 pcnv=0.75)
mfx compute, at(black=1 hispan=0 born60=1 pcnv=0.25)

*Relación entre probabilidad de arresto y pcnv

egen m1= mean(pcnv) 
egen m2= mean(avgsen) 
egen m3= mean(tottime) 
egen m4= mean(ptime86)
egen m5= mean(inc86)

gen gprobit1= normalden(_b[pcnv]*pcnv + _b[avgsen]*m2 + _b[tottime]*m3 + _b[ptime86]*m4 + _b[inc86]*m5 + _b[black] + _b[hispan] + _b[born60] + _b[_cons])
gen parcial_p_pcnv= gprobit1*_b[pcnv]
scatter parcial_p_pcnv pcnv


gen gprobit75= normal(_b[pcnv]*0.75 + _b[avgsen]*m2 + _b[tottime]*m3 + _b[ptime86]*m4 + _b[inc86]*m5 + _b[black]*1  + _b[born60]*1 + _b[_cons])
gen parcial_p_pcnv75= gprobit75*_b[pcnv]
gen gprobit25= normal(_b[pcnv]*0.25 + _b[avgsen]*m2 + _b[tottime]*m3 + _b[ptime86]*m4 + _b[inc86]*m5 + _b[black]*1  + _b[born60]*1 + _b[_cons])
gen parcial_p_pcnv25= gprobit25*_b[pcnv]
