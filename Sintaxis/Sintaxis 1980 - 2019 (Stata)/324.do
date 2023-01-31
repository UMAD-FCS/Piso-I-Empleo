
*======================================================================================*
*                     **   EMPLEO, SALARIOS Y TRANSFERENCIAS   **
*                            MATRIZ PARA BASE WEB
*
*
* Creación:      28/10/2020
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
*                 3.2. SALARIO     
*======================================================================================*

* INDICADOR: Brecha de ingresos  por sexo por hora de trabajo en ocupación principal
* CÓDIGO:    324
* GRUPOS:    Región / Ascendencia étnico-racial / Tramo de edad / Quintil de ingreso / Pobreza
* FUENTE:    ECH (INE)
* PERÍODO:   2001-2019 País urbano / 2006-2019 Total país 


**
**MODIFICACIÓN MANUAL: CÓDIGOS Y LOCAL J PARA VARIABLES AUXILIARES**

    *TOTAL PAÍS
	local i      = 1
	local codind = 324                  // Código de indicador
	local grupo  = 16                   // Cantidad de grupos
	local canio  = 14                   // Cantidad de años de la serie

	local filas = (1 + `grupo') * `canio' // Cantidad de grupos más el total por cantidad de años de la serie
	matrix def       MATR= J(`filas',4,.)
	matrix colnames  MATR= VALOR1 AUXILIAR ANIO VALOR2

	foreach anio of numlist $seieTOT  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	
	g tramo_edad=.
	replace tramo_edad=1 if bc_pe3>=14 & bc_pe3<=18
	replace tramo_edad=2 if bc_pe3>=19 & bc_pe3<=24
	replace tramo_edad=3 if bc_pe3>=25 & bc_pe3<=29
	replace tramo_edad=4 if bc_pe3>=30 & bc_pe3<=64
	replace tramo_edad=5 if bc_pe3>=65 
	

	g horamen=bc_horas_1 * 4.3
	g yhora=bc_pt2/horamen 
	g yhoradef = yhora/bc_ipc_tot
	
	*varones
	mean yhoradef [aw=bc_pesoan] if bc_pobp==2 & bc_pe2==1
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  0
    matrix MATR  [`i',3]=  `anio'
	
	*mujeres
	mean yhoradef [aw=bc_pesoan] if bc_pobp==2 & bc_pe2==2
	matrix MATR  [`i',4]=  e(b)
	
	
	/*NO se procesa país urbano para no duplicar valores*/

	local i  = `i' + 1
	local j  = 	1
	foreach val of numlist 2/3  {

	mean yhoradef [aw=bc_pesoan] if bc_pobp==2 & region_3==`val' & bc_pe2==1
	matrix MATR  [`i',1]=  e(b)
	mean yhoradef [aw=bc_pesoan] if bc_pobp==2 & region_3==`val' & bc_pe2==2
	matrix MATR  [`i',4]=  e(b)

	matrix MATR  [`i',2]=  `j'
	matrix MATR  [`i',3]=  `anio'
	
	local i  = `i' + 1
	local j  = `j' + 1
    }
    *
	
	local j  = 	3
	foreach val of numlist 1/5  {
	mean yhoradef [aw=bc_pesoan] if bc_pobp==2 & tramo_edad==`val' & bc_pe2==1
	matrix MATR  [`i',1]=  e(b)
	mean yhoradef [aw=bc_pesoan] if bc_pobp==2 & tramo_edad==`val' & bc_pe2==2
	matrix MATR  [`i',4]=  e(b)

	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *
	
	local j  = 	8
	foreach val of numlist 1/2  {
	mean yhoradef [aw=bc_pesoan] if bc_pobp==2 & bd_e29_1==`val' & bc_pe2==1
	matrix MATR  [`i',1]=  e(b)
	mean yhoradef [aw=bc_pesoan] if bc_pobp==2 & bd_e29_1==`val' & bc_pe2==2
	matrix MATR  [`i',4]=  e(b)
	
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *
	
	local j  = 	10
	foreach val of numlist 1/5  {
	mean yhoradef [aw=bc_pesoan] if bc_pobp==2 & bd_quintilesy==`val' & bc_pe2==1
	matrix MATR  [`i',1]=  e(b)
	mean yhoradef [aw=bc_pesoan] if bc_pobp==2 & bd_quintilesy==`val' & bc_pe2==2
	matrix MATR  [`i',4]=  e(b)

	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *
	
	local j  = 	15
	foreach val of numlist 0/1  {
	mean yhoradef [aw=bc_pesoan] if bc_pobp==2 & pobre06==`val' & bc_pe2==1
	matrix MATR  [`i',1]=  e(b)
	mean yhoradef [aw=bc_pesoan] if bc_pobp==2 & pobre06==`val' & bc_pe2==2
	matrix MATR  [`i',4]=  e(b)

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
	local codind = 324                  // Código de indicador
	local grupo  = 16                   // Cantidad de grupos
	local canio  = 14                   // Cantidad de años de la serie


import  excel "$tabulados\Auxiliares\AUX`codind'_TP.xls", firstrow clear

destring AUXILIAR, replace
destring ANIO, replace
destring VALOR1, replace
destring VALOR2, replace

sort AUXILIAR ANIO
drop if ANIO==.

g CODIND                 = 324
g NOMINDICADOR           = "BRECHA DE INGRESOS POR TRABAJO POR SEXO EN OCUPACIÓN PRINCIPAL"
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

replace NOMINDICADOR    = "BRECHA DE INGRESOS POR TRABAJO POR SEXO EN OCUPACIÓN PRINCIPAL ENTRE OCUPADOS DE 14 A 18 AÑOS"  if AUXILIAR==3
replace NOMINDICADOR    = "BRECHA DE INGRESOS POR TRABAJO POR SEXO EN OCUPACIÓN PRINCIPAL ENTRE OCUPADOS DE 19 A 24 AÑOS"  if AUXILIAR==4
replace NOMINDICADOR    = "BRECHA DE INGRESOS POR TRABAJO POR SEXO EN OCUPACIÓN PRINCIPAL ENTRE OCUPADOS DE 25 A 29 AÑOS"  if AUXILIAR==5
replace NOMINDICADOR    = "BRECHA DE INGRESOS POR TRABAJO POR SEXO EN OCUPACIÓN PRINCIPAL ENTRE OCUPADOS DE 30 A 64 AÑOS"  if AUXILIAR==6
replace NOMINDICADOR    = "BRECHA DE INGRESOS POR TRABAJO POR SEXO EN OCUPACIÓN PRINCIPAL ENTRE OCUPADOS DE 65 AÑOS Y MÁS" if AUXILIAR==7

replace ASCENDENCIA    = "AFRO"        if AUXILIAR==8
replace ASCENDENCIA    = "NO AFRO"     if AUXILIAR==9

replace QUINTIL			 = "QUINTIL 1"							  if AUXILIAR==10
replace QUINTIL			 = "QUINTIL 2"							  if AUXILIAR==11
replace QUINTIL			 = "QUINTIL 3"							  if AUXILIAR==12
replace QUINTIL			 = "QUINTIL 4"							  if AUXILIAR==13
replace QUINTIL			 = "QUINTIL 5"							  if AUXILIAR==14

replace NOMINDICADOR    = "BRECHA DE INGRESOS POR TRABAJO POR SEXO EN OCUPACIÓN PRINCIPAL ENTRE OCUPADOS EN HOGARES NO EN SITUACIÓN DE POBREZA"    if AUXILIAR==15
replace NOMINDICADOR    = "BRECHA DE INGRESOS POR TRABAJO POR SEXO EN OCUPACIÓN PRINCIPAL ENTRE OCUPADOS EN HOGARES EN SITUACIÓN DE POBREZA"       if AUXILIAR==16
 

** 

**
g VALOR = VALOR2 / VALOR1
drop VALOR2 VALOR1
save "$tabulados\Auxiliares\\`codind'_TP.dta", replace




************************************************************************************************
************************************************************************************************

    *PAÍS URBANO
	local i      = 1
	local codind = 324                  // Código de indicador
	local grupo  = 11                   // Cantidad de grupos (se agrega un grupo para el total)
	local canio  = 19                   // Cantidad de años de la serie

	local filas = `grupo' * `canio' // Cantidad de grupos más el total por cantidad de años de la serie
	matrix def       MATR= J(`filas',4,.)
	matrix colnames  MATR= VALOR1 AUXILIAR ANIO VALOR2

	
	
	foreach anio of numlist $seieURB  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	g tramo_edad=.
	replace tramo_edad=1 if bc_pe3>=14 & bc_pe3<=18
	replace tramo_edad=2 if bc_pe3>=19 & bc_pe3<=24
	replace tramo_edad=3 if bc_pe3>=25 & bc_pe3<=29
	replace tramo_edad=4 if bc_pe3>=30 & bc_pe3<=64
	replace tramo_edad=5 if bc_pe3>=65 
	
	g horamen=bc_horas_1 * 4.3
	g yhora=bc_pt2/horamen 
	g yhoradef = yhora/bc_ipc_tot
	
	*varones
	mean yhoradef [aw=bc_pesoan] if bc_pobp==2 & bc_pe2==1 & bc_filtloc==1
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  0
    matrix MATR  [`i',3]=  `anio'
	
	*mujeres
	mean yhoradef [aw=bc_pesoan] if bc_pobp==2 & bc_pe2==2 & bc_filtloc==1
	matrix MATR  [`i',4]=  e(b)

	matrix MATR  [`i',2]=  0
	matrix MATR  [`i',3]=  `anio'
   	
	local i  = `i' + 1

	
	local j  = 	1
	foreach val of numlist 1/5  {
	mean yhoradef [aw=bc_pesoan] if bc_pobp==2 & bc_pe2==1 & bc_filtloc==1 & tramo_edad==`val'
	matrix MATR  [`i',1]=  e(b)

	mean yhoradef [aw=bc_pesoan] if bc_pobp==2 & bc_pe2==2 & bc_filtloc==1 & tramo_edad==`val'
	matrix MATR  [`i',4]=  e(b)
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *
	
	local j  = 	6
	foreach val of numlist 1/5  {
	mean yhoradef [aw=bc_pesoan] if bc_pobp==2 & bc_pe2==1 & bc_filtloc==1 & bd_quintilesy==`val'
	matrix MATR  [`i',1]=  e(b)

	mean yhoradef [aw=bc_pesoan] if bc_pobp==2 & bc_pe2==2 & bc_filtloc==1 & bd_quintilesy==`val'
	matrix MATR  [`i',4]=  e(b)
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
	local codind = 324                // Código de indicador
	local grupo  = 11                  // Cantidad de grupos
	local canio  = 19                 // Cantidad de años de la serie

import  excel "$tabulados\Auxiliares\AUX`codind'_PU.xls", firstrow clear
sort AUXILIAR ANIO
drop if ANIO==.
tostring VALOR, replace

g CODIND                 = 324
g NOMINDICADOR           = "BRECHA DE INGRESOS POR TRABAJO POR SEXO EN OCUPACIÓN PRINCIPAL"
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

replace NOMINDICADOR    = "BRECHA DE INGRESOS POR TRABAJO POR SEXO EN OCUPACIÓN PRINCIPAL ENTRE OCUPADOS DE 14 A 18 AÑOS"  if AUXILIAR==1
replace NOMINDICADOR    = "BRECHA DE INGRESOS POR TRABAJO POR SEXO EN OCUPACIÓN PRINCIPAL ENTRE OCUPADOS DE 19 A 24 AÑOS"  if AUXILIAR==2
replace NOMINDICADOR    = "BRECHA DE INGRESOS POR TRABAJO POR SEXO EN OCUPACIÓN PRINCIPAL ENTRE OCUPADOS DE 25 A 29 AÑOS"  if AUXILIAR==3
replace NOMINDICADOR    = "BRECHA DE INGRESOS POR TRABAJO POR SEXO EN OCUPACIÓN PRINCIPAL ENTRE OCUPADOS DE 30 A 64 AÑOS"  if AUXILIAR==4
replace NOMINDICADOR    = "BRECHA DE INGRESOS POR TRABAJO POR SEXO EN OCUPACIÓN PRINCIPAL ENTRE OCUPADOS DE 65 AÑOS Y MÁS" if AUXILIAR==5

replace QUINTIL			 = "QUINTIL 1"							  if AUXILIAR==6
replace QUINTIL			 = "QUINTIL 2"							  if AUXILIAR==7
replace QUINTIL			 = "QUINTIL 3"							  if AUXILIAR==8
replace QUINTIL			 = "QUINTIL 4"							  if AUXILIAR==9
replace QUINTIL			 = "QUINTIL 5"							  if AUXILIAR==10
** 

g VALOR = VALOR2 / VALOR1
drop VALOR2 VALOR1


export excel  "$tabulados\Auxiliares\\`codind'_PU.dta", cell(A1) firstrow(varlabels) replace



************************************************************************************************
************************************************************************************************
*FUSIÓN PAÍS URBA


append using "$tabulados\Auxiliares\\`codind'_TP.dta", force
export excel  "$tabulados\\`codind'.xls", cell(A1) firstrow(varlabels) replace
