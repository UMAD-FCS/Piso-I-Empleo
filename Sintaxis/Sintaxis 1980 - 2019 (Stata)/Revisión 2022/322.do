
*======================================================================================*
*                     **   EMPLEO, SALARIOS Y TRANSFERENCIAS   **
*                            MATRIZ PARA BASE WEB
*
*
* Creación:      28/10/2020 - rev 29/9/2022
* Institución:   UMAD, FCS
* Responsable:   Jimena Pandolfi
* Descripción:   Construcción de matriz para base web (Piso II - Motores)para 
*				 indicadores de dimiensión temática "Empleo, salarios y transferencias"
* Atención: 	 Esta sintaxis se modifica en 2022. Se adopta el SMN del año respectivo y no se 
*				 deflacta ingreso
*
*======================================================================================*


*======================================================================================*
*                           UBICACIÓN DE ARCHIVOS      
*======================================================================================*

	cd "C:\Users\Usuario\Dropbox\1. Unidad de Métodos y Acceso a Datos\1. Observatorio UMAD"
	global bases "C:\Users\Usuario\Dropbox\1. Unidad de Métodos y Acceso a Datos\1. Observatorio UMAD\PISO I_MICRODATOS\ECH\Microdatos\Compatibilizadas Iecon\En uso"
	global tabulados C:\Users\Usuario\Dropbox\1. Unidad de Métodos y Acceso a Datos\1. Observatorio UMAD\PISO II_MOTORES\Base web\3. Empleo, salarios y transferencias\Extracciones parciales

/*	cd "C:\Users\Usuario\Dropbox\1. Unidad de Métodos y Acceso a Datos\1. Observatorio UMAD"
	global bases "C:\Users\Usuario\Dropbox\1. Unidad de Métodos y Acceso a Datos\1. Observatorio UMAD\PISO I_MICRODATOS\ECH\Microdatos\Compatibilizadas Iecon\En uso"
	global tabulados C:\Users\Usuario\Dropbox\1. Unidad de Métodos y Acceso a Datos\1. Observatorio UMAD\PISO II_MOTORES\Base web\3. Empleo, salarios y transferencias\Extracciones parciales
*/

*======================================================================================*
*              CARACTERÍSTICAS GENERALES DE LA MATRIZ DE EXPORTACAIÓN      
*======================================================================================*

    global seieURB 1/19   // Años de serie para país urbano
	global seieTOT 6/19         // Años de serie para total país

	  

*======================================================================================*
*                 3.2. SALARIO     
*======================================================================================*

* INDICADOR: Porcentaje de ocupados que perciben ingresos laborales por debajo del salario mínimo nacional
* CÓDIGO:    322
* GRUPOS:    Región / Ascendencia étnico-racial / Sexo / Tramo de edad / Quintil de ingreso / Pobreza
* FUENTE:    ECH (INE)
* PERÍODO:   2001-2020 País urbano / 2006-2020 Total país 


**
**MODIFICACIÓN MANUAL: CÓDIGOS Y LOCAL J PARA VARIABLES AUXILIARES**

    *TOTAL PAÍS
	local i      = 1
	local codind = 322                  // Código de indicador
	local grupo  = 18                   // Cantidad de grupos
	local canio  = 15                   // Cantidad de años de la serie

	local filas = (1 + `grupo') * `canio' // Cantidad de grupos más el total por cantidad de años de la serie
	matrix def       MATR= J(`filas',3,.)
	matrix colnames  MATR= VALOR AUXILIAR ANIO

	foreach anio of numlist $seieTOT  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	g smn_h = smn / 200	
	g horamen=bc_horas_1 * 4.3
	g yhora=bc_pt2/horamen 
	
	g salario_insuf = 0
	replace salario_insuf=1 if yhora<smn_h

	
	g tramo_edad=.
	replace tramo_edad=1 if bc_pe3>=14 & bc_pe3<=18
	replace tramo_edad=2 if bc_pe3>=19 & bc_pe3<=24
	replace tramo_edad=3 if bc_pe3>=25 & bc_pe3<=29
	replace tramo_edad=4 if bc_pe3>=30 & bc_pe3<=64
	replace tramo_edad=5 if bc_pe3>=65 
	
	cap drop bd_region
	g bd_region=.
	replace bd_region=1 if region_4== 1 | region_4== 2
	replace bd_region=2 if region_4== 3
	replace bd_region=3 if region_4== 4

    mean salario_insuf [aw=bc_pesoan] if bc_pobp==2
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  0
    matrix MATR  [`i',3]=  `anio'
	
	
	/*NO se procesa país urbano para no duplicar valores*/
	
	local i  = `i' + 1
	local j  = 	1
	foreach val of numlist 2/3  {
	mean salario_insuf [aw=bc_pesoan] if bc_pobp==2 & bd_region==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
	matrix MATR  [`i',3]=  `anio'
	
	local i  = `i' + 1
	local j  = `j' + 1
    }
    *
	
	local j  = 	3
	foreach val of numlist 1/2  {
	mean salario_insuf [aw=bc_pesoan] if bc_pobp==2 & bc_pe2==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *
	
	local j  = 	5
	foreach val of numlist 1/5  {
	mean salario_insuf [aw=bc_pesoan] if bc_pobp==2 & tramo_edad==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *
	
	local j  = 	10
	foreach val of numlist 1/2  {
	mean salario_insuf [aw=bc_pesoan] if bc_pobp==2 & bd_e29_1==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *
	
	local j  = 	12
	foreach val of numlist 1/5  {
	mean salario_insuf [aw=bc_pesoan] if bc_pobp==2 & bd_quintilesy==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *
	
	local j  = 	17
	foreach val of numlist 0/1  {
	mean salario_insuf [aw=bc_pesoan] if bc_pobp==2 & pobre06==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *

	}
    *

**
use "$bases\fusionada_personasyhogares_20.dta", clear
global anio 20
	mat def aux_2020 = J(12,1,.)
	
	g tramo_edad=.
	replace tramo_edad=1 if e27>=14 & e27<=18
	replace tramo_edad=2 if e27>=19 & e27<=24
	replace tramo_edad=3 if e27>=25 & e27<=29
	replace tramo_edad=4 if e27>=30 & e27<=64
	replace tramo_edad=5 if e27>=65 
	

g uno=1  						//Variabe auxiliar para construir cantidad de personas
replace uno=0 if e30==14 		//Excluyo de la suma al servicio doméstico

egen bd_ht19 = sum (uno), by (numero) 
label var bd_ht19 "Cantidad de pseronas (sin servicio doméstico)"

rename HT11 ht11

g ypc1=ht11/bd_ht19 if e30==1
g quintilesy_prev=.
foreach i of numlist 1/12 {
xtile quintilesy_prev`i'= ypc1 [aw=pesomen] if mes==`i', n(5)
replace quintilesy_prev=quintilesy_prev`i' if mes==`i'
drop quintilesy_prev`i'
}

egen bd_quintilesy= max (quintilesy_prev), by (numero)
label var bd_quintilesy "Quintil de ingreso per cápita del hogar"

drop uno ypc1 quintilesy_prev

rename pobre_06 pobre06

	rename pobpcoac bc_pobp

	rename pt2 bc_pt2	
	
	g smn_h = smn / 200	
	
	g bc_horas_1=f85 if bc_pobp==2
	replace bc_horas_1=-9 if bc_pobp!=2
	
	g horamen=bc_horas_1 * 4.3
	g yhora=bc_pt2/horamen 
	
	
	g salario_insuf = 0
	replace salario_insuf=1 if yhora<smn_h
	
	cap drop bd_region
	g bd_region=.
	replace bd_region=1 if region_4== 1 | region_4== 2
	replace bd_region=2 if region_4== 3
	replace bd_region=3 if region_4== 4

foreach mes of numlist 1/12 {
	mean salario_insuf if mes==`mes'&bc_pobp==2 [aw=pesomen]
	matrix aux_2020 [`mes',1]=e(b)
}

	local i  =  267 // (1 + `grupo') * (`canio'-1) 
	
	
	matrix MATR  [`i',1]=  (aux_2020[1,1]+aux_2020[2,1]+aux_2020[3,1]+aux_2020[4,1]+ /*
						*/ aux_2020[5,1]+aux_2020[6,1]+aux_2020[7,1]+aux_2020[8,1]+ /*
						*/ aux_2020[9,1]+aux_2020[10,1]+aux_2020[11,1]+aux_2020[12,1])/12
	matrix MATR  [`i',2]=  0
	matrix MATR  [`i',3]=  $anio
   	
	local i  = `i' + 1 /*NO se procesa país urbano para no duplicar valores*/
	
	local j  = 	1
	foreach val of numlist 2/3  {
foreach mes of numlist 1/12 {
	mean salario_insuf [aw=pesomen] if mes==`mes' & bd_region==`val'&bc_pobp==2 
	matrix aux_2020 [`mes',1]=e(b)
}
	matrix MATR  [`i',1]=  (aux_2020[1,1]+aux_2020[2,1]+aux_2020[3,1]+aux_2020[4,1]+ /*
						*/ aux_2020[5,1]+aux_2020[6,1]+aux_2020[7,1]+aux_2020[8,1]+ /*
						*/ aux_2020[9,1]+aux_2020[10,1]+aux_2020[11,1]+aux_2020[12,1])/12
	matrix MATR  [`i',2]=  `j'
	matrix MATR  [`i',3]=  $anio
	
	local i  = `i' + 1
	local j  = `j' + 1
    }
    *

	
	local j  = 	3
	foreach val of numlist 1/2  {
foreach mes of numlist 1/12 {
	mean salario_insuf [aw=pesomen] if mes==`mes' & e26==`val'&bc_pobp==2 
	matrix aux_2020 [`mes',1]=e(b)
}
	matrix MATR  [`i',1]=  (aux_2020[1,1]+aux_2020[2,1]+aux_2020[3,1]+aux_2020[4,1]+ /*
						*/ aux_2020[5,1]+aux_2020[6,1]+aux_2020[7,1]+aux_2020[8,1]+ /*
						*/ aux_2020[9,1]+aux_2020[10,1]+aux_2020[11,1]+aux_2020[12,1])/12
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  $anio
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *
	
	local j  = 	5
	foreach val of numlist 1/5  {
foreach mes of numlist 1/12 {
	mean salario_insuf [aw=pesomen] if mes==`mes' & tramo_edad==`val'&bc_pobp==2 
	matrix aux_2020 [`mes',1]=e(b)
}
	matrix MATR  [`i',1]=  (aux_2020[1,1]+aux_2020[2,1]+aux_2020[3,1]+aux_2020[4,1]+ /*
						*/ aux_2020[5,1]+aux_2020[6,1]+aux_2020[7,1]+aux_2020[8,1]+ /*
						*/ aux_2020[9,1]+aux_2020[10,1]+aux_2020[11,1]+aux_2020[12,1])/12
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  $anio
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *
	
	local j  = 	10
	foreach val of numlist 1/2  {

foreach mes of numlist 1/12 {
	mean salario_insuf [aw=pesomen] if mes==`mes' & e29_1==`val'&bc_pobp==2 
	matrix aux_2020 [`mes',1]=e(b)
}
	matrix MATR  [`i',1]=  (aux_2020[1,1]+aux_2020[2,1]+aux_2020[3,1]+aux_2020[4,1]+ /*
						*/ aux_2020[5,1]+aux_2020[6,1]+aux_2020[7,1]+aux_2020[8,1]+ /*
						*/ aux_2020[9,1]+aux_2020[10,1]+aux_2020[11,1]+aux_2020[12,1])/12

	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  $anio
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *
	
	local j  = 	12
	foreach val of numlist 1/5  {
foreach mes of numlist 1/12 {
	mean salario_insuf [aw=pesomen] if mes==`mes' & bd_quintilesy==`val'&bc_pobp==2 
	matrix aux_2020 [`mes',1]=e(b)
}
	matrix MATR  [`i',1]=  (aux_2020[1,1]+aux_2020[2,1]+aux_2020[3,1]+aux_2020[4,1]+ /*
						*/ aux_2020[5,1]+aux_2020[6,1]+aux_2020[7,1]+aux_2020[8,1]+ /*
						*/ aux_2020[9,1]+aux_2020[10,1]+aux_2020[11,1]+aux_2020[12,1])/12
	
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  $anio
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *
	
	local j  = 	17
	foreach val of numlist 0/1  {
foreach mes of numlist 1/12 {
	mean salario_insuf [aw=pesomen] if mes==`mes' & pobre06==`val'&bc_pobp==2 
	matrix aux_2020 [`mes',1]=e(b)
}
	matrix MATR  [`i',1]=  (aux_2020[1,1]+aux_2020[2,1]+aux_2020[3,1]+aux_2020[4,1]+ /*
						*/ aux_2020[5,1]+aux_2020[6,1]+aux_2020[7,1]+aux_2020[8,1]+ /*
						*/ aux_2020[9,1]+aux_2020[10,1]+aux_2020[11,1]+aux_2020[12,1])/12
	
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  $anio
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *
	
	
	
	
**

xml_tab MATR, save("$tabulados\Auxiliares\AUX`codind'_TP.xls") replace

**
/*PROBLEMA: GUARDAR COMO EXCEL 97, STATA NO ABRE CON ESTE COMANDO ARCHIVOS EXCEL 2003*/
**MODIFICACIÓN MANUAL: GUARDAR COMO LA BASE DE DATOS Y EDITAR NOMBRES DE INDICADORES E INFORMACIÓN DE METADATO**
**MODIFICACIÓN MANUAL: ACTUALIZAR LOCALS**

	local i      = 1
	local codind = 322                  // Código de indicador
	local grupo  = 18                   // Cantidad de grupos
	local canio  = 15                   // Cantidad de años de la serie


import  excel "$tabulados\Auxiliares\AUX`codind'_TP.xls", firstrow clear

rename ANIO ANIO_ANT
g ANIO=2000+ANIO_ANT if ANIO_ANT<=20
replace ANIO=1900+ANIO_ANT if ANIO_ANT>80
sort AUXILIAR ANIO
drop if ANIO==.
drop ANIO_ANT

g CODIND                 = 322
g NOMINDICADOR           = "PORCENTAJE DE OCUPADOS QUE PERCIBEN UN INGRESO POR DEBAJO DEL SMN"
g SEXO                   = "TODOS"
g ASCENDENCIA    		 = "TODOS"
g QUINTIL       		 = "TODOS"
g DEPARTAMENTOUY		 = ""
g URBANORURALUY 		 = "TODOS"
g PAÍS			 		 = "URUGUAY"
g RESPONSABLE			 = "JIMENA PANDOLFI"

**

**
**MODIFICACIÓN MANUAL: CÓDIGOS Y TÍTULOS DE LA VARIABLES AUXILIARES CORRESPONDIENTES**

replace URBANORURALUY    = "URBANO (MENOS DE 5.000 HABITANTES)"   if AUXILIAR==1
replace URBANORURALUY    = "RURAL DISPERSO"                       if AUXILIAR==2

replace SEXO    = "VARONES"            if AUXILIAR==3
replace SEXO    = "MUJERES"            if AUXILIAR==4

replace NOMINDICADOR    = "PORCENTAJE DE OCUPADOS DE 14 A 18 AÑOS QUE PERCIBEN UN INGRESO POR DEBAJO DEL SMN"  if AUXILIAR==5
replace NOMINDICADOR    = "PORCENTAJE DE OCUPADOS DE 19 A 24 AÑOS QUE PERCIBEN UN INGRESO POR DEBAJO DEL SMN"  if AUXILIAR==6
replace NOMINDICADOR    = "PORCENTAJE DE OCUPADOS DE 25 A 29 AÑOS QUE PERCIBEN UN INGRESO POR DEBAJO DEL SMN"  if AUXILIAR==7
replace NOMINDICADOR    = "PORCENTAJE DE OCUPADOS DE 30 A 64 AÑOS QUE PERCIBEN UN INGRESO POR DEBAJO DEL SMN"  if AUXILIAR==8
replace NOMINDICADOR    = "PORCENTAJE DE OCUPADOS DE 65 AÑOS Y MÁS QUE PERCIBEN UN INGRESO POR DEBAJO DEL SMN" if AUXILIAR==9

replace ASCENDENCIA    = "AFRO"        if AUXILIAR==10
replace ASCENDENCIA    = "NO AFRO"     if AUXILIAR==11

replace QUINTIL			 = "QUINTIL 1"							  if AUXILIAR==12
replace QUINTIL			 = "QUINTIL 2"							  if AUXILIAR==13
replace QUINTIL			 = "QUINTIL 3"							  if AUXILIAR==14
replace QUINTIL			 = "QUINTIL 4"							  if AUXILIAR==15
replace QUINTIL			 = "QUINTIL 5"							  if AUXILIAR==16

replace NOMINDICADOR    = "PORCENTAJE DE OCUPADOS EN HOGARES NO EN SITUACIÓN DE POBREZA QUE PERCIBEN UN INGRESO POR DEBAJO DEL SMN"    if AUXILIAR==17
replace NOMINDICADOR    = "PORCENTAJE DE OCUPADOS EN HOGARES EN SITUACIÓN DE POBREZA QUE PERCIBEN UN INGRESO POR DEBAJO DEL SMN"       if AUXILIAR==18
 

** 

** 
save "$tabulados\Auxiliares\\`codind'_TP.dta", replace




************************************************************************************************
************************************************************************************************

    *PAÍS URBANO
	local i      = 1
	local codind = 322                  // Código de indicador
	local grupo  = 13                   // Cantidad de grupos (se agrega un grupo para el total)
	local canio  = 20                   // Cantidad de años de la serie

	local filas = `grupo' * `canio' // Cantidad de grupos más el total por cantidad de años de la serie
	matrix def       MATR= J(`filas',3,.)
	matrix colnames  MATR= VALOR AUXILIAR ANIO

	foreach anio of numlist $seieURB  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	g tramo_edad=.
	replace tramo_edad=1 if bc_pe3>=14 & bc_pe3<=18
	replace tramo_edad=2 if bc_pe3>=19 & bc_pe3<=24
	replace tramo_edad=3 if bc_pe3>=25 & bc_pe3<=29
	replace tramo_edad=4 if bc_pe3>=30 & bc_pe3<=64
	replace tramo_edad=5 if bc_pe3>=65 

	
	g smn_h = smn / 200	
	g horamen=bc_horas_1 * 4.3
	g yhora=bc_pt2/horamen 
	
	g salario_insuf = 0
	replace salario_insuf=1 if yhora<smn_h

	

	mean salario_insuf [aw=bc_pesoan] if bc_pobp==2 & bc_filtloc==1
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  0
	matrix MATR  [`i',3]=  `anio'
   	
	local i  = `i' + 1

	local j  = 	1
	foreach val of numlist 1/2  { 
	mean salario_insuf [aw=bc_pesoan] if bc_pobp==2 & bc_pe2==`val' & bc_filtloc==1
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *
	
	local j  = 	3
	foreach val of numlist 1/5  {
	mean salario_insuf [aw=bc_pesoan] if bc_pobp==2 & tramo_edad==`val' & bc_filtloc==1
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *
	
	local j  = 	8
	foreach val of numlist 1/5  {
	mean salario_insuf [aw=bc_pesoan] if bc_pobp==2 & bd_quintilesy==`val' & bc_filtloc==1
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *
	
	}
    *
**

use "$bases\fusionada_personasyhogares_20.dta", clear
global anio 20
	mat def aux_2020 = J(12,1,.)
	
	g tramo_edad=.
	replace tramo_edad=1 if e27>=14 & e27<=18
	replace tramo_edad=2 if e27>=19 & e27<=24
	replace tramo_edad=3 if e27>=25 & e27<=29
	replace tramo_edad=4 if e27>=30 & e27<=64
	replace tramo_edad=5 if e27>=65 
	

g uno=1  						//Variabe auxiliar para construir cantidad de personas
replace uno=0 if e30==14 		//Excluyo de la suma al servicio doméstico

rename HT11 ht11
egen bd_ht19 = sum (uno), by (numero) 
label var bd_ht19 "Cantidad de pseronas (sin servicio doméstico)"

g ypc1=ht11/bd_ht19 if e30==1
g quintilesy_prev=.
foreach i of numlist 1/12 {
xtile quintilesy_prev`i'= ypc1 [aw=pesomen] if mes==`i', n(5)
replace quintilesy_prev=quintilesy_prev`i' if mes==`i'
drop quintilesy_prev`i'
}

egen bd_quintilesy= max (quintilesy_prev), by (numero)
label var bd_quintilesy "Quintil de ingreso per cápita del hogar"

drop uno ypc1 quintilesy_prev

rename pobre_06 pobre06

	rename pobpcoac bc_pobp

	rename pt2 bc_pt2	
	
	g smn_h = smn / 200	
	
	g bc_horas_1=f85 if bc_pobp==2
	replace bc_horas_1=-9 if bc_pobp!=2
	
	g horamen=bc_horas_1 * 4.3
	g yhora=bc_pt2/horamen 
	
	
	g salario_insuf = 0
	replace salario_insuf=1 if yhora<smn_h
	
	cap drop bc_filtloc
	g bc_filtloc=region_4<3
	
	g bc_pe2=e26
	
foreach mes of numlist 1/12 {
	mean salario_insuf if mes==`mes'& bc_filtloc==1&bc_pobp==2 [aw=pesomen]
	matrix aux_2020 [`mes',1]=e(b)
}

	local i  =  /*`grupo'*(`canio'-1)+ 1*/ 248
	
	
	matrix MATR  [`i',1]=  (aux_2020[1,1]+aux_2020[2,1]+aux_2020[3,1]+aux_2020[4,1]+ /*
						*/ aux_2020[5,1]+aux_2020[6,1]+aux_2020[7,1]+aux_2020[8,1]+ /*
						*/ aux_2020[9,1]+aux_2020[10,1]+aux_2020[11,1]+aux_2020[12,1])/12
	matrix MATR  [`i',2]=  0
	matrix MATR  [`i',3]=  $anio
   	
	local i  = `i' + 1 
	
	
	local j  = 	1
	foreach val of numlist 1/2  {
foreach mes of numlist 1/12 {
	mean salario_insuf [aw=pesomen] if mes==`mes' & bc_pe2==`val'& bc_filtloc==1 &bc_pobp==2
	matrix aux_2020 [`mes',1]=e(b)
}
	matrix MATR  [`i',1]=  (aux_2020[1,1]+aux_2020[2,1]+aux_2020[3,1]+aux_2020[4,1]+ /*
						*/ aux_2020[5,1]+aux_2020[6,1]+aux_2020[7,1]+aux_2020[8,1]+ /*
						*/ aux_2020[9,1]+aux_2020[10,1]+aux_2020[11,1]+aux_2020[12,1])/12
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  $anio
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *
	
	local j  = 	3
	foreach val of numlist 1/5  {
foreach mes of numlist 1/12 {
	mean salario_insuf [aw=pesomen] if mes==`mes' & tramo_edad==`val'& bc_filtloc==1&bc_pobp==2
	matrix aux_2020 [`mes',1]=e(b)
}
	matrix MATR  [`i',1]=  (aux_2020[1,1]+aux_2020[2,1]+aux_2020[3,1]+aux_2020[4,1]+ /*
						*/ aux_2020[5,1]+aux_2020[6,1]+aux_2020[7,1]+aux_2020[8,1]+ /*
						*/ aux_2020[9,1]+aux_2020[10,1]+aux_2020[11,1]+aux_2020[12,1])/12
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  $anio
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *
	
	local j  = 	8
	foreach val of numlist 1/5  {
foreach mes of numlist 1/12 {
	mean salario_insuf [aw=pesomen] if mes==`mes' & bd_quintilesy==`val'& bc_filtloc==1&bc_pobp==2
	matrix aux_2020 [`mes',1]=e(b)
}
	matrix MATR  [`i',1]=  (aux_2020[1,1]+aux_2020[2,1]+aux_2020[3,1]+aux_2020[4,1]+ /*
						*/ aux_2020[5,1]+aux_2020[6,1]+aux_2020[7,1]+aux_2020[8,1]+ /*
						*/ aux_2020[9,1]+aux_2020[10,1]+aux_2020[11,1]+aux_2020[12,1])/12
	
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  $anio
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *
		
	

**

xml_tab MATR, save("$tabulados\Auxiliares\AUX`codind'_PU.xls") replace

**
/*PROBLEMA: GUARDAR COMO EXCEL 97, STATA NO ABRE CON ESTE COMANDO ARCHIVOS EXCEL 2003*/
**MODIFICACIÓN MANUAL: GUARDAR COMO LA BASE DE DATOS Y EDITAR NOMBRES DE INDICADORES E INFORMACIÓN DE METADATO**
**MODIFICACIÓN MANUAL: ACTUALIZAR LOCALS**

	local i      = 1
	local codind = 322                // Código de indicador
	local grupo  = 13                  // Cantidad de grupos
	local canio  = 20                 // Cantidad de años de la serie

import  excel "$tabulados\Auxiliares\AUX`codind'_PU.xls", firstrow clear
rename ANIO ANIO_ANT
g ANIO=2000+ANIO_ANT if ANIO_ANT<=20
replace ANIO=1900+ANIO_ANT if ANIO_ANT>80
sort AUXILIAR ANIO
drop if ANIO==.
drop ANIO_ANT
tostring VALOR, replace

g CODIND                 = 322
g NOMINDICADOR           = "PORCENTAJE DE OCUPADOS QUE PERCIBEN UN INGRESO POR DEBAJO DEL SMN"
g SEXO                   = "TODOS"
g ASCENDENCIA    		 = "TODOS"
g QUINTIL       		 = "TODOS"
g DEPARTAMENTOUY		 = ""
g URBANORURALUY 		 = "URBANO (MÁS DE 5.000 HABITANTES)"
g PAÍS			 		 = "URUGUAY"
g RESPONSABLE			 = "JIMENA PANDOLFI"

**

**
**MODIFICACIÓN MANUAL: CÓDIGOS Y TÍTULOS DE LA VARIABLES AUXILIARES CORRESPONDIENTES**

replace URBANORURALUY    = "URBANO (MÁS DE 5.000 HABITANTES)"     

replace SEXO    = "VARONES"            if AUXILIAR==1
replace SEXO    = "MUJERES"            if AUXILIAR==2

replace NOMINDICADOR    = "PORCENTAJE DE OCUPADOS DE 14 A 18 AÑOS QUE PERCIBEN UN INGRESO POR DEBAJO DEL SMN"  if AUXILIAR==3
replace NOMINDICADOR    = "PORCENTAJE DE OCUPADOS DE 19 A 24 AÑOS QUE PERCIBEN UN INGRESO POR DEBAJO DEL SMN"  if AUXILIAR==4
replace NOMINDICADOR    = "PORCENTAJE DE OCUPADOS DE 25 A 29 AÑOS QUE PERCIBEN UN INGRESO POR DEBAJO DEL SMN"  if AUXILIAR==5
replace NOMINDICADOR    = "PORCENTAJE DE OCUPADOS DE 30 A 64 AÑOS QUE PERCIBEN UN INGRESO POR DEBAJO DEL SMN"  if AUXILIAR==6
replace NOMINDICADOR    = "PORCENTAJE DE OCUPADOS DE 65 AÑOS Y MÁS QUE PERCIBEN UN INGRESO POR DEBAJO DEL SMN" if AUXILIAR==7

replace QUINTIL			 = "QUINTIL 1"							  if AUXILIAR==8
replace QUINTIL			 = "QUINTIL 2"							  if AUXILIAR==9
replace QUINTIL			 = "QUINTIL 3"							  if AUXILIAR==10
replace QUINTIL			 = "QUINTIL 4"							  if AUXILIAR==11
replace QUINTIL			 = "QUINTIL 5"							  if AUXILIAR==12
** 

export excel  "$tabulados\Auxiliares\\`codind'_PU.dta", cell(A1) firstrow(varlabels) replace



************************************************************************************************
************************************************************************************************
*FUSIÓN PAÍS URBA


append using "$tabulados\Auxiliares\\`codind'_TP.dta", force
export excel  "$tabulados\\`codind'.xls", cell(A1) firstrow(varlabels) replace
