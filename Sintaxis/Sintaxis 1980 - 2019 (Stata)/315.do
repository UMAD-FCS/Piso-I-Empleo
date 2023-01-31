
*======================================================================================*
*                     **   EMPLEO, SALARIOS Y TRANSFERENCIAS   **
*                            MATRIZ PARA BASE WEB
*
*
* Creación:      15/10/2020
* Institución:   UMAD, FCS
* Responsable:   Jimena Pandolfi
* Descripción:   Construcción de matriz para base web (Piso II - Motores)para 
*				 indicadores de dimiensión temática "Empleo, salarios y transferencias"
*
*
*======================================================================================*


*======================================================================================*
*                           UBICACIÓN DE ARCHIVOS      
*======================================================================================*

	cd "C:\Users\Usuario\Dropbox\1. Unidad de Métodos y Acceso a Datos\1. Observatorio UMAD"
	global bases "C:\Users\Usuario\Dropbox\1. Unidad de Métodos y Acceso a Datos\1. Observatorio UMAD\PISO I_MICRODATOS\ECH\Microdatos\Compatibilizadas Iecon\En uso"
	global tabulados C:\Users\Usuario\Dropbox\1. Unidad de Métodos y Acceso a Datos\1. Observatorio UMAD\PISO II_MOTORES\Base web\3. Empleo, salarios y transferencias\Extracciones parciales


*======================================================================================*
*              CARACTERÍSTICAS GENERALES DE LA MATRIZ DE EXPORTACAIÓN      
*======================================================================================*

    global seieURB 1/19   		// Años de serie para país urbano
	global seieTOT 6/19         // Años de serie para total país

	  

*======================================================================================*
*                 3.1. EMPLEO     
*======================================================================================*

* INDICADOR: Porcentaje de ocupados que no están registrados en la seguridad social en ocupación principal
* CÓDIGO:    315
* GRUPOS:    Región / Ascendencia étnico-racial / Sexo / Tramo de edad / Quintil de ingreso / Pobreza
* FUENTE:    ECH (INE)
* PERÍODO:   1990-2019 País urbano / 2006-2019 Total país 


**
**MODIFICACIÓN MANUAL: CÓDIGOS Y LOCAL J PARA VARIABLES AUXILIARES**

    *TOTAL PAÍS
	local i      = 1
	local codind = 315                  // Código de indicador
	local grupo  = 18                   // Cantidad de grupos
	local canio  = 14                   // Cantidad de años de la serie

	local filas = (1 + `grupo') * `canio' // Cantidad de grupos más el total por cantidad de años de la serie
	matrix def       MATR= J(`filas',3,.)
	matrix colnames  MATR= VALOR AUXILIAR ANIO

	foreach anio of numlist $seieTOT  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	g tramo_edad=.
	replace tramo_edad=1 if bc_pe3>=14 & bc_pe3<=18
	replace tramo_edad=2 if bc_pe3>=19 & bc_pe3<=24
	replace tramo_edad=3 if bc_pe3>=25 & bc_pe3<=29
	replace tramo_edad=4 if bc_pe3>=30 & bc_pe3<=64
	replace tramo_edad=5 if bc_pe3>=65 
	
	recode bc_register (2=1) (1=0)
	replace bc_register=. if bc_register!=1 & bc_register!=0
	
	mean bc_register [aw=bc_pesoan] if bc_pobp==2
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  0
	matrix MATR  [`i',3]=  `anio'
   	
	local i  = `i' + 1 /*NO se procesa país urbano para no duplicar valores*/
	
	local j  = 	1
	foreach val of numlist 2/3  {
	mean bc_register [aw=bc_pesoan] if bc_pobp==2 & region_3==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
	matrix MATR  [`i',3]=  `anio'
	
	local i  = `i' + 1
	local j  = `j' + 1
    }
    *
	
	local j  = 	3
	foreach val of numlist 1/2  {
	mean bc_register [aw=bc_pesoan] if bc_pobp==2 & bc_pe2==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *
	
	local j  = 	5
	foreach val of numlist 1/5  {
	mean bc_register [aw=bc_pesoan] if bc_pobp==2 & tramo_edad==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *
	
	local j  = 	10
	foreach val of numlist 1/2  {
	mean bc_register [aw=bc_pesoan] if bc_pobp==2 & bd_e29_1==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *
	
	local j  = 	12
	foreach val of numlist 1/5  {
	mean bc_register [aw=bc_pesoan] if bc_pobp==2 & bd_quintilesy==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *
	
	local j  = 	17
	foreach val of numlist 0/1  {
	mean bc_register [aw=bc_pesoan] if bc_pobp==2 & pobre06==`val'
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

xml_tab MATR, save("$tabulados\Auxiliares\AUX`codind'_TP.xls") replace

**
/*PROBLEMA: GUARDAR COMO EXCEL 97, STATA NO ABRE CON ESTE COMANDO ARCHIVOS EXCEL 2003*/
**MODIFICACIÓN MANUAL: GUARDAR COMO LA BASE DE DATOS Y EDITAR NOMBRES DE INDICADORES E INFORMACIÓN DE METADATO**
**MODIFICACIÓN MANUAL: ACTUALIZAR LOCALS**

	local i      = 1
	local codind = 315                  // Código de indicador
	local grupo  = 18                   // Cantidad de grupos
	local canio  = 14                   // Cantidad de años de la serie


import  excel "$tabulados\Auxiliares\AUX`codind'_TP.xls", firstrow clear

sort AUXILIAR ANIO
drop if ANIO==.

g CODIND                 = 315
g NOMINDICADOR           = "PORCENTAJE DE OCUPADOS NO REGISTRADOS EN SEGURIDAD SOCIAL EN OCUPACIÓN PRINCIPAL"
g SEXO                   = "NA"
g ASCENDENCIA    		 = "NA"
g QUINTIL       		 = "NA"
g DEPARTAMENTOUY		 = "NA"
g URBANORURALUY 		 = "NA"
g PAÍS			 		 = "URUGUAY"
g RESPONSABLE			 = "JIMENA PANDOLFI"

**

**
**MODIFICACIÓN MANUAL: CÓDIGOS Y TÍTULOS DE LA VARIABLES AUXILIARES CORRESPONDIENTES**

replace URBANORURALUY    = "URBANO (MENOS DE 5.000 HABITANTES)"   if AUXILIAR==1
replace URBANORURALUY    = "RURAL DISPERSO"                       if AUXILIAR==2

replace SEXO    = "VARONES"            if AUXILIAR==3
replace SEXO    = "MUJERES"            if AUXILIAR==4

replace NOMINDICADOR    = "PORCENTAJE DE OCUPADOS DE PERSONAS DE 14 A 18 AÑOS NO REGISTRADOS EN SEGURIDAD SOCIAL EN OCUPACIÓN PRINCIPAL"  if AUXILIAR==5
replace NOMINDICADOR    = "PORCENTAJE DE OCUPADOS DE PERSONAS DE 19 A 24 AÑOS NO REGISTRADOS EN SEGURIDAD SOCIAL EN OCUPACIÓN PRINCIPAL"  if AUXILIAR==6
replace NOMINDICADOR    = "PORCENTAJE DE OCUPADOS DE PERSONAS DE 25 A 29 AÑOS NO REGISTRADOS EN SEGURIDAD SOCIAL EN OCUPACIÓN PRINCIPAL"  if AUXILIAR==7
replace NOMINDICADOR    = "PORCENTAJE DE OCUPADOS DE PERSONAS DE 30 A 64 AÑOS NO REGISTRADOS EN SEGURIDAD SOCIAL EN OCUPACIÓN PRINCIPAL"  if AUXILIAR==8
replace NOMINDICADOR    = "PORCENTAJE DE OCUPADOS DE PERSONAS DE 65 AÑOS Y MÁS NO REGISTRADOS EN SEGURIDAD SOCIAL EN OCUPACIÓN PRINCIPAL" if AUXILIAR==9

replace ASCENDENCIA    = "AFRO"        if AUXILIAR==10
replace ASCENDENCIA    = "NO AFRO"     if AUXILIAR==11

replace QUINTIL			 = "QUINTIL 1"							  if AUXILIAR==12
replace QUINTIL			 = "QUINTIL 2"							  if AUXILIAR==13
replace QUINTIL			 = "QUINTIL 3"							  if AUXILIAR==14
replace QUINTIL			 = "QUINTIL 4"							  if AUXILIAR==15
replace QUINTIL			 = "QUINTIL 5"							  if AUXILIAR==16

replace NOMINDICADOR    = "PORCENTAJE DE OCUPADOS NO REGISTRADOS EN SEGURIDAD SOCIAL EN OCUPACIÓN PRINCIPAL EN HOGARES NO EN SITUACIÓN DE POBREZA"    if AUXILIAR==17
replace NOMINDICADOR    = "PORCENTAJE DE OCUPADOS NO REGISTRADOS EN SEGURIDAD SOCIAL EN OCUPACIÓN PRINCIPAL EN HOGARES EN SITUACIÓN DE POBREZA"       if AUXILIAR==18
 

** 

** 
save "$tabulados\Auxiliares\\`codind'_TP.dta", replace




************************************************************************************************
************************************************************************************************

    *PAÍS URBANO
	local i      = 1
	local codind = 315                  // Código de indicador
	local grupo  = 13                   // Cantidad de grupos (se agrega un grupo para el total)
	local canio  = 19                   // Cantidad de años de la serie

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
	
	recode bc_register (2=1) (1=0)
	replace bc_register=. if bc_register!=1 & bc_register!=0
	
	mean bc_register [aw=bc_pesoan] if bc_pobp==2 & bc_filtloc==1
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  0
	matrix MATR  [`i',3]=  `anio'
   	
	local i  = `i' + 1

	local j  = 	1
	foreach val of numlist 1/2  { 
	mean bc_register [aw=bc_pesoan] if bc_pobp==2 & bc_pe2==`val' & bc_filtloc==1
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *
	
	local j  = 	3
	foreach val of numlist 1/5  {
	mean bc_register [aw=bc_pesoan] if bc_pobp==2 & tramo_edad==`val' & bc_filtloc==1
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *
	
	local j  = 	8
	foreach val of numlist 1/5  {
	mean bc_register [aw=bc_pesoan] if bc_pobp==2 & bd_quintilesy==`val' & bc_filtloc==1
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

xml_tab MATR, save("$tabulados\Auxiliares\AUX`codind'_PU.xls") replace

**
/*PROBLEMA: GUARDAR COMO EXCEL 97, STATA NO ABRE CON ESTE COMANDO ARCHIVOS EXCEL 2003*/
**MODIFICACIÓN MANUAL: GUARDAR COMO LA BASE DE DATOS Y EDITAR NOMBRES DE INDICADORES E INFORMACIÓN DE METADATO**
**MODIFICACIÓN MANUAL: ACTUALIZAR LOCALS**

	local i      = 1
	local codind = 315                // Código de indicador
	local grupo  = 13                  // Cantidad de grupos
	local canio  = 19                 // Cantidad de años de la serie

import  excel "$tabulados\Auxiliares\AUX`codind'_PU.xls", firstrow clear
sort AUXILIAR ANIO
drop if ANIO==.

g CODIND                 = 315
g NOMINDICADOR           = "PORCENTAJE DE OCUPADOS NO REGISTRADOS EN SEGURIDAD SOCIAL EN OCUPACIÓN PRINCIPAL"
g SEXO                   = "NA"
g ASCENDENCIA    		 = "NA"
g QUINTIL       		 = "NA"
g DEPARTAMENTOUY		 = "NA"
g URBANORURALUY 		 = "NA"
g PAÍS			 		 = "URUGUAY"
g RESPONSABLE			 = "JIMENA PANDOLFI"

**

**
**MODIFICACIÓN MANUAL: CÓDIGOS Y TÍTULOS DE LA VARIABLES AUXILIARES CORRESPONDIENTES**

replace URBANORURALUY    = "URBANO (MÁS DE 5.000 HABITANTES)"     

replace SEXO    = "VARONES"            if AUXILIAR==1
replace SEXO    = "MUJERES"            if AUXILIAR==2

replace NOMINDICADOR    = "PORCENTAJE DE OCUPADOS DE PERSONAS DE 14 A 18 AÑOS NO REGISTRADOS EN SEGURIDAD SOCIAL EN OCUPACIÓN PRINCIPAL"  if AUXILIAR==3
replace NOMINDICADOR    = "PORCENTAJE DE OCUPADOS DE PERSONAS DE 19 A 24 AÑOS NO REGISTRADOS EN SEGURIDAD SOCIAL EN OCUPACIÓN PRINCIPAL"  if AUXILIAR==4
replace NOMINDICADOR    = "PORCENTAJE DE OCUPADOS DE PERSONAS DE 25 A 29 AÑOS NO REGISTRADOS EN SEGURIDAD SOCIAL EN OCUPACIÓN PRINCIPAL"  if AUXILIAR==5
replace NOMINDICADOR    = "PORCENTAJE DE OCUPADOS DE PERSONAS DE 30 A 64 AÑOS NO REGISTRADOS EN SEGURIDAD SOCIAL EN OCUPACIÓN PRINCIPAL"  if AUXILIAR==6
replace NOMINDICADOR    = "PORCENTAJE DE OCUPADOS DE PERSONAS DE 65 AÑOS Y MÁS NO REGISTRADOS EN SEGURIDAD SOCIAL EN OCUPACIÓN PRINCIPAL" if AUXILIAR==7

replace QUINTIL			 = "QUINTIL 1"							  if AUXILIAR==8
replace QUINTIL			 = "QUINTIL 2"							  if AUXILIAR==9
replace QUINTIL			 = "QUINTIL 3"							  if AUXILIAR==10
replace QUINTIL			 = "QUINTIL 4"							  if AUXILIAR==11
replace QUINTIL			 = "QUINTIL 5"							  if AUXILIAR==12
** 

export excel  "$tabulados\Auxiliares\\`codind'_PU.dta", cell(A1) firstrow(varlabels) replace



************************************************************************************************
************************************************************************************************
*FUSIÓN PAÍS URBANO Y TOTAL PAÍS

append using "$tabulados\Auxiliares\\`codind'_TP.dta", force
export excel  "$tabulados\\`codind'.xls", cell(A1) firstrow(varlabels) replace
