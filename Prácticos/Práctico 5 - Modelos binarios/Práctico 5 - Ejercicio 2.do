/* #########################

PRÁCTICO 5 - EJERCICIO 2

########################## */

* Se limpia y configura la memoria
capture clear all
set memory 80m	
set more off

* Se establece global con ruta de la carpeta de trabajo
global ruta "G:\UdelaR\CCEEA\Semestre 7\Econometría II\Prácticos\Práctico 5 - Modelos binarios"

* Se cierra cualquier archivo log abierto y se abre un nuevo log
capture log close
log using "$ruta\taller5_2015.log", replace	

* Se abre la base MROZ y se ordenan las variables alfabéticamente
use "$ruta/mroz.dta", clear
aorder

/* ##### DESCRIPCIÓN DE LA BASE Y LAS VARIABLES RELEVANTES ##### */

sum
tab inlf
egen propunos = mean(inlf) // Genera una variable que contiene la proporción de unos 
sum propunos

* Estadísticas descriptivas
pwcorr inlf nwifeinc educ exper expersq age kidslt6 kidsge6,star(0.05)
tabstat nwifeinc educ exper expersq age kidslt6 kidsge6, by(inlf) s(mean)
graph box exper, by(inlf)

* Se hace un global con la lista de regresores
global lista "nwifeinc educ exper expersq age kidslt6 kidsge6"	

/*###################################
##  MODELO DE PROBABILIDAD LINEAL  ##
####################################*/

{
reg  inlf  $lista, robust

* Se guardan los valores ajustados en una variable prMPL (predicción de la probabilidad de cada individuo)
predict prMPL

* Notar que existen estimaciones de probabilidad fuera del intervalo [0,1]
count if[prMPL<0]
count if[prMPL>1]

* Efecto parcial del ingreso de los demás miembros del hogar
g EFNWmpl=_b[nwifeinc]

* Predicciones de INLF utilizando como valor de corte la proporción muestral de unos
g UNOMPL=(prMPL>propunos)
* BONDAD DE AJUSTE: Tabla de proporción de 0 y 1 correctos
tab inlf UNOMPL, col	

* Predicciones de INLF utilizando como valor de corte 0,5
g UNOMPL2=(prMPL>0.5)
* BONDAD DE AJUSTE: Tabla de proporción de 0 y 1 correctos
tab inlf UNOMPL2, col

/* ##### PARTE 6 ##### */

gen EPsp6mpl=_b[kidslt6]*3 - _b[kidslt6]*1
sum EPsp6mpl

* CONTRASTES DE SIGNIFICACION CONJUNTA (CONTRASTES DE EXCLUSION)
test kidslt6 kidsge6

// se puede crear una variable cantidad de hijos menores de 18 g hijos= kidslt6+kidsge6
test nwifeinc educ exper expersq age

}
*

/*####################################
##  		MODELO PROBIT 			##
#####################################*/

{
probit inlf $lista,

* Se guardan los valores ajustados en una variable prMPL (predicción de la probabilidad de cada individuo)
predict prPROBIT

* predicciones de INLF utilizando como valor de corte la proporción muestral de unos							
g UNOP=(prPROBIT>propunos)
* BONDAD DE AJUSTE: Tabla de proporción de 0 y 1 correctos
tab inlf UNOP, col

* predicciones de INLF utilizando como valor de corte 0,5
g UNOP2=(prPROBIT>0.5)
* BONDAD DE AJUSTE: Tabla de proporción de 0 y 1 correctos
tab  inlf UNOP2, col

********************** ALTERNATIVA **********************
{
* Comando de Stata que calcula proporción de predicciones correctas
* 1. Valor por defecto de STATA: 0.5
estat class
* 2. Usando proporción de unos en la muestra
sum propunos
local propunos=r(mean)
estat class, cutoff(`propunos')
* en cutoff se puede poner cualquier valor entre 0 y 1
//VER "sensitivity" y "specificity" 
//lsens, gencutoff(var1)   gensens(sensi) genspec(espec)

}
*
************ FIN DE LA SOLUCIÓN ALTERNATIVA ************

* EFECTOS PARCIALES
mfx compute //por defecto son en la media de todas las variables, eso genera problemas con las dummy

// Otras pruebas
mfx compute, at(educ=12)
mfx compute, at(educ=12 age=35 kidslt6=1 kidsge6=0)
mfx compute, at(educ=6 age=30 kidslt6=1 kidsge6=0)
mfx compute, at(nwifeinc=0 educ=6 age=30 kidslt6=1 kidsge6=0)

********************** ALTERNATIVA **********************
{
* CÁLCULO MANUAL DE LOS EFECTOS PARCIALES
* EFECTO PARCIAL DEL INGRESO DEL RESTO DE LOS MIEMBROS DEL HOGAR
/* obtenemos el efecto marginal del ingreso del hogar para cada individuo (es el efecto parcial estimado para cada individuo) y luego observamos la media*/
predict indice, index	
g gprobit=normalden(indice)	
g EFNWp=gprobit*_b[nwifeinc]

tabstat EFNWp, s(mean p10 p25 p50 p75 p90)	 //observamos la variación

xtile quint_ingre = nwifeinc, nq(5)
tabstat EFNWp, by(quint_ingre) s(mean)  //analizamos el efecto mg del ingreso por quintiles de ingreso

* Para reproducir a mano el efecto marginal de NWIFEINC en el vector de medias:
egen m1= mean(nwifeinc) 
egen m2= mean(educ) 
egen m3= mean(exper) 
egen m4= mean(expersq)
egen m5= mean(age) 
egen m6= mean(kidslt6) 
egen m7= mean(kidsge6)

gen gprobit1= normalden(_b[nwifeinc]*m1 + _b[educ]*m2 + _b[exper]*m3 + _b[expersq]*m4 + _b[age]*m5 + _b[kidslt6]*m6 + _b[kidsge6]*m7 + _b[_cons])
gen parcial_p_nwifeinc= gprobit1*_b[nwifeinc]
sum parcial_p_nwifeinc //como es esperable la media coincide exactamente con la obtenida con por "mfx compute"

* Para ver el efecto marginal de NWIFEINC dado todo lo demas constante y en la media
gen gprobit2= normalden(_b[nwifeinc]*nwifeinc + _b[educ]*m2 + _b[exper]*m3 + _b[expersq]*m4 + _b[age]*m5 + _b[kidslt6]*m6 + _b[kidsge6]*m7 + _b[_cons])
gen parcial2_p_nwifeinc= gprobit2*_b[nwifeinc]
scatter parcial2_p_nwifeinc nwifeinc //el efecto parcial (negativo) es menor en val abs cuando los ingresos están alejados de la media
sum parcial2_p_nwifeinc 

}
*
************ FIN DE LA SOLUCIÓN ALTERNATIVA ************

/* ##### PARTE 6 ##### */

/* En este caso no podemos usar "mfx" porque para una variable discreta no binaria Stata calcula el efecto parcial con la derivada, como si la variable 
kidslt6 fuera continua. Entonces tenemos que hacerlo a mano. Para ello hay que tomar la función de distribución Normal y restar su valor en uno y otro caso 
de los que interesa comparar. */
 
gen EPsp6probit=normal(_b[nwifeinc]*20 + _b[educ]*12 + _b[exper]*8 + _b[expersq]*64 + _b[age]*32 + _b[kidslt6]*3 + _b[kidsge6]*0 + _b[_cons])- normal(_b[nwifeinc]*20 + _b[educ]*12 + _b[exper]*8 + _b[expersq]*64 + _b[age]*32 + _b[kidslt6]*1 + _b[kidsge6]*0 + _b[_cons])
sum EPsp6probit

**************** ANEXO ****************
{
*****************************************************
** Comparación con efecto parcial usando binarias ***
*****************************************************

* Efecto parcial con la variable discreta "cantidad de hijos menores a 6" de pasar de 0 a 1
gen EPsp6probit1=normal(_b[nwifeinc]*20 + _b[educ]*12 + _b[exper]*8 + _b[expersq]*64 + _b[age]*32 + _b[kidslt6]*1 + _b[kidsge6]*0 + _b[_cons])- normal(_b[nwifeinc]*20 + _b[educ]*12 + _b[exper]*8 + _b[expersq]*64 + _b[age]*32 + _b[kidslt6]*0 + _b[kidsge6]*0 + _b[_cons])
sum EPsp6probit1

* Creación de una variable binaria para mujeres con hijos menores a 6 años
g kidslt6d = 0
replace kidslt6d = 1 if kidslt6 > 0

* Probit tomando binaria con hijos de menos de 6 o sin hijos mejores a 6
probit inlf nwifeinc educ exper expersq age kidslt6d kidsge6

* Efecto Parcial binaria
mfx compute

}
*
************ FIN DEL ANEXO ************

* CONTRASTES DE SIGNIFICACION CONJUNTA (CONTRASTES DE EXCLUSION)
test kidslt6 kidsge6

}
*

/*####################################
##  		MODELO LOGIT 			##
#####################################*/

{
logit inlf $lista

* Se guardan los valores ajustados en una variable prMPL (predicción de la probabilidad de cada individuo)
predict prLOGIT

* Predicciones de INLF utilizando como valor de corte la proporción muestral de unos	
g UNOL=(prLOGIT>propunos)	
* BONDAD DE AJUSTE: Tabla de proporción de 0 y 1 correctos
tab  inlf UNOL, col

* Predicciones de INLF utilizando como valor de corte 0,5
g UNOL2=(prLOGIT>0.5)	
* BONDAD DE AJUSTE: Tabla de proporción de 0 y 1 correctos
tab  inlf UNOL2, col	

********************** ALTERNATIVA **********************
{
* Comando de Stata que calcula proporción de predicciones correctas
* 1. Valor por defecto de STATA: 0.5
estat class
* 2. Usando proporción de unos en la muestra
sum propunos	
local propunos=r(mean)
estat class, cutoff(`propunos')
}
*
************ FIN DE LA SOLUCIÓN ALTERNATIVA ************

* EFECTOS PARCIALES
mfx compute
mfx compute, at(educ=12 age=35 kidslt6=1 kidsge6=0)
mfx compute, at(educ=6 age=30 kidslt6=1 kidsge6=0)
mfx compute, at(nwifeinc=0 educ=6 age=30 kidslt6=1 kidsge6=0)

********************** ALTERNATIVA **********************
{
* CÁLCULO MANUAL
* EFECTO PARCIAL DEL INGRESO DEL RESTO DE LOS MIEMBROS DEL HOGAR
g glogit=prLOGIT*(1-prLOGIT)	
g EFNWl=glogit*_b[nwifeinc]	
tabstat EFNWl, s(mean p10 p25 p50 p75 p90)	
tabstat EFNWl, by(quint_ingre) s(mean)

* Para replicar el efecto parcial de NWIFEINC en el vector de medias
gen glogit2=1/(1+exp(-(_b[nwifeinc]*m1+_b[educ]*m2+_b[exper]*m3+_b[expersq]*m4+_b[age]*m5+_b[kidslt6]*m6+_b[kidsge6]*m7+_b[_cons])))
gen efp_nwifeinc2= glogit2*(1-glogit2)*_b[nwifeinc]
sum efp_nwifeinc2 //coincide con lo obtenido con el comando "mfx"

* Para ver el efecto parcial de NWIFEINC dado todo lo demás constante y en la media
gen glogit3=1/(1+exp(-(_b[nwifeinc]*nwifeinc+_b[educ]*m2+_b[exper]*m3+_b[expersq]*m4+_b[age]*m5+_b[kidslt6]*m6+_b[kidsge6]*m7+_b[_cons])))
gen efp_nwifeinc3= glogit3*(1-glogit3)*_b[nwifeinc]
scatter efp_nwifeinc3 nwifeinc
sum efp_nwifeinc3

* Efecto parcial de la educ con el resto de las variables en la media
gen glogit4=1/(1+exp(-(_b[nwifeinc]*m1+_b[educ]*educ+_b[exper]*m3+_b[expersq]*m4+_b[age]*m5+_b[kidslt6]*m6+_b[kidsge6]*m7+_b[_cons])))
gen efp_educ= glogit4*(1-glogit4)*_b[educ]
scatter efp_educ educ

}
*
************ FIN DE LA SOLUCIÓN ALTERNATIVA ************

/* ##### PARTE 6 ##### */

gen EPsp6logit=1/(1+exp(-(_b[nwifeinc]*20 + _b[educ]*12 + _b[exper]*8 + _b[expersq]*64 + _b[age]*32 + _b[kidslt6]*3 + _b[kidsge6]*0 + _b[_cons])))- 1/(1+exp(-(_b[nwifeinc]*20 + _b[educ]*12 + _b[exper]*8 + _b[expersq]*64 + _b[age]*32 + _b[kidslt6]*1 + _b[kidsge6]*0 + _b[_cons])))
sum EPsp6logit

* CONTRASTES DE SIGNIFICACION CONJUNTA (CONTRASTES DE EXCLUSION)
test kidslt6 kidsge6
test nwifeinc educ exper expersq

}
*

/*########################################
## 		COMPARACIÓN DE RESULTADOS		##
#########################################*/

* COMPARACION EFECTO PARCIAL INGRESO RESTO DEL HOGAR EN LOS DISTINTOS MODELOS
tabstat EF*, s(p10 p25 p50 p75 p90) 

* COMPARACION PROBABILIDADES PREDICHAS SEGUN CANTIDAD DE HIJOS MENORES DE 6 AÑOS	
tabstat pr* inlf, by(kidslt6)	

* COMPARACION UNOS Y CEROS PREDICHOS SEGUN CANTIDAD DE HIJOS MENORES DE 6 AÑOS
tabstat UNO* inlf, by(kidslt6)	

* COMPARACION EFECTO PARCIAL DEL Se Pide 6
tabstat EPsp6*

**********************************************************
***************** FIN DE LA PROGRAMACIÓN *****************
**********************************************************
