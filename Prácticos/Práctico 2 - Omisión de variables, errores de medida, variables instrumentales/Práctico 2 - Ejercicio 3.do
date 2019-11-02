clear all
set mem 100m
set more off
set matsize 400

global ruta "C:\Users\dacza_000\Desktop\UdelaR\CCEEA\Semestre 7\Econometría II\Prácticos\Práctico 2 - Omisión de variables, errores de medida, variables instrumentales"

*ABRIR LA BASE DE DATOS: la abre desde la dirección que le entramos como ruta
use "$ruta\card.dta", clear

**Genero experiencia potencial al cuadrado sobre 100
gen exper2=exper*exper/100

**Estimación 1
reg lwage educ exper exper2 black reg661 reg662 reg663 reg664 reg665 reg666 reg667 reg668
est store MCO1

/* Reg endo: regresor correlacionado con el término de error. 

Fuentes de endo:
	omisión de variable relevante (por ej: habilidad - sesgo positivo)
	error de medida (sesgo negativo)
	existencia de retornos a la educ. variables entre las personas (ses. -)

*/

** Estimación 2

gen cercania=nearc4
reg educ cercania exper exper2 black reg661 reg662 reg663 reg664 reg665 reg666 reg667 reg668
est store MCO2

** Estimación 3

reg cercania educ exper exper2 black reg661 reg662 reg663 reg664 reg665 reg666 reg667 reg668
est store MCO3

** Estimación 4

ivreg lwage exper exper2 black reg661 reg662 reg663 reg664 reg665 reg666 reg667 reg668 (educ=cercania)
est store VI1

** Hausman

hausman VI1 MCO1

/* K del Hausman es el número de regresores sin contar la constante */


					***** FIN DE LA PROGRAMACIÓN *****
