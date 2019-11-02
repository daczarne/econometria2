* ECONOMETRÍA II AÑO 2015 - TALLER 4

* PRACTICO 4: VI y MODELOS BINARIOS 

*Comandos iniciales
clear
set more 1
set matsize 100
capture log close
pause on

*Cambio de directorio: Cambiar la ruta según la carpeta que estemos usando
global ruta "C:\DOCENCIA\econIIccee\practicas\2015\practica4"

*INICIAR ARCHIVO LOG: genera un log file final
log using "$ruta\Taller_4_2015.log", replace

*ABRIR LA BASE DE DATOS: la abre desde la dirección que le entramos como ruta
use "$ruta\practica4.dta", clear

************************************************

sum cotizacion
*cotizacion: =1 si la persona registra haber tenido al menos una cotización en un empleo formal entre 2010 y 2011, y cotización=0 en otro caso
sum participa
*Participantes del Programa (Participa)	=1 si la persona participó efectivamente del programa, 0 en otro caso.
sum cprevia
*Cotización Previa (cprevia)	=1 si la persona registra haber tenido un empleo formal en 2007 o 2008 ; y 0 en otro caso
sum sorteo
*Sorteo	=1 si la persona salió sorteada para participar en el programa; =0 en otro caso
sum hombre
*Hombre	=1 si la persona es hombre; =0 en otro caso



sum sorteo
sum participa
tab sorteo
tab participa
*PARTE I
reg cotizacion participa, robust


*PARTE II
logit participa  cprevia, robust
logit participa , robust

di 1/(1+ exp(.5629194))

tabulate sorteo participa, row 
 
di 1/(1+ exp(.7101458 + .3642929  ))
tabulate cprevia participa, row
 
 * Efectos Parciales
 logit participa  cprevia
 mfx

 scalar efectomarginal = 1/(1+ exp(.7101458 + .3642929  )) - 1/(1+ exp(.3642929  ))
 
 *PARTE III
*Hausman
*Estimación 1
reg cotizacion  participa 
estimates store MCO
*Estimación 2
ivreg cotizacion  ( participa  = sorteo)
estimates store VI
*Hausman
hausman VI MCO

*prueba calidad de los instrumentos
 reg participa sorteo , robust
