
*======================================================================================*
*                     **   EMPLEO, SALARIOS Y TRANSFERENCIAS   **
*                            MATRIZ PARA BASE WEB
*
*
* Creación:      03/11/2020
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

	global seieTOT 9/19         // Años de serie para total país

	  

*======================================================================================*
*                 3.3. TRANSFERENCIAS     
*======================================================================================*

* INDICADOR: Porcentaje de hogares que perciben asignaciones familiares (solo Plan de Equidad)
* CÓDIGO:    338
* GRUPOS:    Región / Sexo del jefe / Quintil de ingreso / Pobreza
* FUENTE:    ECH (INE)
* PERÍODO:   2006-2019 Total país 


**
**MODIFICACIÓN MANUAL: CÓDIGOS Y LOCAL J PARA VARIABLES AUXILIARES**

    *TOTAL PAÍS
	local i      = 1
	local codind = 340                 // Código de indicador
	local grupo  = 13                   // Cantidad de grupos
	local canio  = 11                   // Cantidad de años de la serie

	local filas = (1 + `grupo') * `canio' // Cantidad de grupos más el total por cantidad de años de la serie
	matrix def       MATR= J(`filas',3,.)
	matrix colnames  MATR= VALOR AUXILIAR ANIO

	foreach anio of numlist $seieTOT  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	egen h_afam_pe = sum (afam_pe), by (bc_correlat)
	replace h_afam_pe=1 if h_afam_pe>0
	
	g       jefe_mas   = . 
	replace jefe_mas   = 1 if bc_pe4==1 & bc_pe2==1
	replace jefe_mas   = 2 if bc_pe4==1 & bc_pe2==2
	replace jefe_mas   = 0 if bc_pe4>1  & bc_pe4<=7
	egen    sexojefe   = max (jefe_mas), by (bc_correlat)
	
    mean h_afam_pe [aw=bc_pesoan] if bc_pe4==1
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  0
    matrix MATR  [`i',3]=  `anio'
	
	
	/*NO se procesa país urbano para no duplicar valores*/
	
	local i  = `i' + 1 
	local j  = 	1
	foreach val of numlist 1/3  {
	mean h_afam_pe [aw=bc_pesoan] if bc_pe4==1 & region_3==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
	matrix MATR  [`i',3]=  `anio'
	
	local i  = `i' + 1
	local j  = `j' + 1
    }
    *
	
	local j  = 	4
	foreach val of numlist 1/2  {
	mean h_afam_pe [aw=bc_pesoan] if bc_pe4==1 & sexojefe==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *
	
	local j  = 	6
	foreach val of numlist 1/5  {
	mean h_afam_pe [aw=bc_pesoan] if bc_pe4==1 & bd_quintilesy==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *
	
	local j  = 	11
	foreach val of numlist 0/1  {
	mean h_afam_pe [aw=bc_pesoan] if bc_pe4==1 & pobre06==`val'
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
	local codind = 340                  // Código de indicador
	local grupo  = 13                   // Cantidad de grupos
	local canio  = 11                   // Cantidad de años de la serie


import  excel "$tabulados\Auxiliares\AUX`codind'_TP.xls", firstrow clear

destring AUXILIAR, replace
destring ANIO, replace
destring VALOR, replace

sort AUXILIAR ANIO
drop if ANIO==.

g CODIND                 = 340
g NOMINDICADOR           = "PORCENTAJE DE HOGARES QUE PERCIBEN ASIGNACIONES FAMILIARES (SOLO PLAN DE EQUIDAD)"
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

replace URBANORURALUY    = "URBANO (MÁS DE 5.000 HABITANTES)"     if AUXILIAR==1
replace URBANORURALUY    = "URBANO (MENOS DE 5.000 HABITANTES)"   if AUXILIAR==2
replace URBANORURALUY    = "RURAL DISPERSO"                       if AUXILIAR==3

replace NOMINDICADOR    = "PORCENTAJE DE HOGARES CON JEFATURA MASCULINA QUE PERCIBEN ASIGNACIONES FAMILIARES (SOLO PLAN DE EQUIDAD)" if AUXILIAR==4
replace NOMINDICADOR    = "PORCENTAJE DE HOGARES CON JEFATURA FEMENINA QUE PERCIBEN ASIGNACIONES FAMILIARES (SOLO PLAN DE EQUIDAD)"  if AUXILIAR==5

replace QUINTIL			 = "QUINTIL 1"							  if AUXILIAR==6
replace QUINTIL			 = "QUINTIL 2"							  if AUXILIAR==7
replace QUINTIL			 = "QUINTIL 3"							  if AUXILIAR==8
replace QUINTIL			 = "QUINTIL 4"							  if AUXILIAR==9
replace QUINTIL			 = "QUINTIL 5"							  if AUXILIAR==10

replace NOMINDICADOR    = "PORCENTAJE DE HOGARES NO EN SITUACIÓN DE POBREZA QUE PERCIBEN ASIGNACIONES FAMILIARES (SOLO PLAN DE EQUIDAD)"    if AUXILIAR==11
replace NOMINDICADOR    = "PORCENTAJE DE HOGARES EN SITUACIÓN DE POBREZA QUE PERCIBEN ASIGNACIONES FAMILIARES (SOLO PLAN DE EQUIDAD)"       if AUXILIAR==12
 

** 

** 
export excel  "$tabulados\\`codind'.xls", cell(A1) firstrow(varlabels) replace
