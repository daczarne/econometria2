clear all
set mem 100m
set more off
set matsize 400

global ruta "C:\Users\dacza_000\Desktop\UdelaR\CCEEA\Semestre 7\Econometr�a II\Pr�cticos\Pr�ctico 2 - Omisi�n de variables, errores de medida, variables instrumentales"

*ABRIR LA BASE DE DATOS: la abre desde la direcci�n que le entramos como ruta
use "$ruta\card.dta", clear

**Genero experiencia potencial al cuadrado sobre 100
gen exper2=exper*exper/100

**Estimaci�n 1
reg lwage educ exper exper2 black reg661 reg662 reg663 reg664 reg665 reg666 reg667 reg668
est store MCO1

/* Reg endo: regresor correlacionado con el t�rmino de error. 

Fuentes de endo:
	omisi�n de variable relevante (por ej: habilidad - sesgo positivo)
	error de medida (sesgo negativo)
	existencia de retornos a la educ. variables entre las personas (ses. -)

*/

** Estimaci�n 2

gen cercania=nearc4
reg educ cercania exper exper2 black reg661 reg662 reg663 reg664 reg665 reg666 reg667 reg668
est store MCO2

** Estimaci�n 3

reg cercania educ exper exper2 black reg661 reg662 reg663 reg664 reg665 reg666 reg667 reg668
est store MCO3

** Estimaci�n 4

ivreg lwage exper exper2 black reg661 reg662 reg663 reg664 reg665 reg666 reg667 reg668 (educ=cercania)
est store VI1

** Hausman

hausman VI1 MCO1

/* K del Hausman es el n�mero de regresores sin contar la constante */


					***** FIN DE LA PROGRAMACI�N *****
