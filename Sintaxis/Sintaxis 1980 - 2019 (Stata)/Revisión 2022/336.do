
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

	global seieTOT 6/19         // Años de serie para total país

	  

*======================================================================================*
*                 3.2. SALARIO     
*======================================================================================*

* INDICADOR: Porcentaje de personas que perciben jubilaciones por debajo del SMN
* CÓDIGO:    336
* GRUPOS:    Región / Ascendencia étnico-racial / Sexo / Quintil de ingreso / Pobreza
* FUENTE:    ECH (INE)
* PERÍODO:   2006-2020 Total país 


**
**MODIFICACIÓN MANUAL: CÓDIGOS Y LOCAL J PARA VARIABLES AUXILIARES**

    *TOTAL PAÍS
	local i      = 1
	local codind = 336                  // Código de indicador
	local grupo  = 14                   // Cantidad de grupos
	local canio  = 15                   // Cantidad de años de la serie

	local filas = (1 + `grupo') * `canio' // Cantidad de grupos más el total por cantidad de años de la serie
	matrix def       MATR= J(`filas',3,.)
	matrix colnames  MATR= VALOR AUXILIAR ANIO

	foreach anio of numlist $seieTOT  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
		
	g jubilaciones = bc_pg911 + bc_pg921	
	g jubil_insuf_smn = 0
	replace jubil_insuf_smn=1 if jubilaciones<smn

	cap drop bd_region
	g bd_region=.
	replace bd_region=1 if region_4== 1 | region_4== 2
	replace bd_region=2 if region_4== 3
	replace bd_region=3 if region_4== 4
	
    mean jubil_insuf_smn [aw=bc_pesoan] if jubilaciones>0
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  0
    matrix MATR  [`i',3]=  `anio'
	
	
	/*NO se procesa país urbano para no duplicar valores*/
	
	local i  = `i' + 1 
	local j  = 	1
	foreach val of numlist 1/3  {
	mean jubil_insuf_smn [aw=bc_pesoan] if jubilaciones>0 & bd_region==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
	matrix MATR  [`i',3]=  `anio'
	
	local i  = `i' + 1
	local j  = `j' + 1
    }
    *
	
	local j  = 	4
	foreach val of numlist 1/2  {
	mean jubil_insuf_smn [aw=bc_pesoan] if jubilaciones>0 & bc_pe2==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *
	
	local j  = 	6
	foreach val of numlist 1/2  {
	mean jubil_insuf_smn [aw=bc_pesoan] if jubilaciones>0 & bd_e29_1==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *
	
	local j  = 	8
	foreach val of numlist 1/5  {
	mean jubil_insuf_smn [aw=bc_pesoan] if jubilaciones>0 & bd_quintilesy==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *
	
	local j  = 	13
	foreach val of numlist 0/1  {
	mean jubil_insuf_smn [aw=bc_pesoan] if jubilaciones>0 & pobre06==`val'
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
	

g uno=1  						//Variabe auxiliar para construir cantidad de personas
replace uno=0 if e30==14 		//Excluyo de la suma al servicio doméstico

rename HT11  ht11
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

g bc_pe3=e27
rename pobre_06 pobre06

	g jubilaciones = g148_1_1 + g148_1_2 + g148_1_3 + g148_1_4 + g148_1_5 /*
	*/ + g148_1_6 + g148_1_7 + g148_1_8 + g148_1_9 + g148_1_10 + g148_1_11
	g jubil_insuf_lp = 0
	replace jubil_insuf_lp=1 if jubilaciones<smn
	
	cap drop bd_region
	g bd_region=.
	replace bd_region=1 if region_4== 1 | region_4== 2
	replace bd_region=2 if region_4== 3
	replace bd_region=3 if region_4== 4

foreach mes of numlist 1/12 {
	mean jubil_insuf_lp if mes==`mes'&jubilaciones>0 [aw=pesomen]
	matrix aux_2020 [`mes',1]=e(b)
}

	local i  =  211 // (1 + `grupo') * (`canio'-1) 
	
	
	matrix MATR  [`i',1]=  (aux_2020[1,1]+aux_2020[2,1]+aux_2020[3,1]+aux_2020[4,1]+ /*
						*/ aux_2020[5,1]+aux_2020[6,1]+aux_2020[7,1]+aux_2020[8,1]+ /*
						*/ aux_2020[9,1]+aux_2020[10,1]+aux_2020[11,1]+aux_2020[12,1])/12
	matrix MATR  [`i',2]=  0
	matrix MATR  [`i',3]=  $anio
   	
	local i  = `i' + 1 /*NO se procesa país urbano para no duplicar valores*/
	
	local j  = 	1
	foreach val of numlist 1/3  {
foreach mes of numlist 1/12 {
	mean jubil_insuf_lp [aw=pesomen] if mes==`mes' & bd_region==`val'&jubilaciones>0
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

	
	local j  = 	4
	foreach val of numlist 1/2  {
foreach mes of numlist 1/12 {
	mean jubil_insuf_lp [aw=pesomen] if mes==`mes' & e26==`val'&jubilaciones>0
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
	
	local j  = 	6
	foreach val of numlist 1/2  {

foreach mes of numlist 1/12 {
	mean jubil_insuf_lp [aw=pesomen] if mes==`mes' & e29_1==`val'&jubilaciones>0
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
	mean jubil_insuf_lp [aw=pesomen] if mes==`mes' & bd_quintilesy==`val' &jubilaciones>0
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
	
	local j  = 	13
	foreach val of numlist 0/1  {
foreach mes of numlist 1/12 {
	mean jubil_insuf_lp [aw=pesomen] if mes==`mes' & pobre06==`val'&jubilaciones>0
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

**

xml_tab MATR, save("$tabulados\Auxiliares\AUX`codind'_TP.xls") replace

**
/*PROBLEMA: GUARDAR COMO EXCEL 97, STATA NO ABRE CON ESTE COMANDO ARCHIVOS EXCEL 2003*/
**MODIFICACIÓN MANUAL: GUARDAR COMO LA BASE DE DATOS Y EDITAR NOMBRES DE INDICADORES E INFORMACIÓN DE METADATO**
**MODIFICACIÓN MANUAL: ACTUALIZAR LOCALS**

	local i      = 1
	local codind = 336                  // Código de indicador
	local grupo  = 14                   // Cantidad de grupos
	local canio  = 15                   // Cantidad de años de la serie


import  excel "$tabulados\Auxiliares\AUX`codind'_TP.xls", firstrow clear

destring AUXILIAR, replace
destring ANIO, replace
destring VALOR, replace

rename ANIO ANIO_ANT
g ANIO=2000+ANIO_ANT if ANIO_ANT<=20
replace ANIO=1900+ANIO_ANT if ANIO_ANT>80
sort AUXILIAR ANIO
drop if ANIO==.
drop ANIO_ANT

g CODIND                 = 336
g NOMINDICADOR           = "PORCENTAJE PERSONAS QUE PERCIBEN JUBILACIONES POR DEBAJO DEL SALARIO MÍNIMO NACIONAL"
g SEXO                   = "TODOS"
g ASCENDENCIA    		 = "TODOS"
g QUINTIL       		 = "TODOS"
g DEPARTAMENTOUY		 = ""
g URBANORURALUY 		 = "TOTAL PAÍS"
g PAÍS			 		 = "URUGUAY"
g RESPONSABLE			 = "JIMENA PANDOLFI"

**

**
**MODIFICACIÓN MANUAL: CÓDIGOS Y TÍTULOS DE LA VARIABLES AUXILIARES CORRESPONDIENTES**

replace URBANORURALUY    = "URBANO (MÁS DE 5.000 HABITANTES)"     if AUXILIAR==1
replace URBANORURALUY    = "URBANO (MENOS DE 5.000 HABITANTES)"   if AUXILIAR==2
replace URBANORURALUY    = "RURAL DISPERSO"                       if AUXILIAR==3

replace SEXO    = "VARONES"            if AUXILIAR==4
replace SEXO    = "MUJERES"            if AUXILIAR==5

replace ASCENDENCIA    = "AFRO"        if AUXILIAR==6
replace ASCENDENCIA    = "NO AFRO"     if AUXILIAR==7

replace QUINTIL			 = "QUINTIL 1"							  if AUXILIAR==8
replace QUINTIL			 = "QUINTIL 2"							  if AUXILIAR==9
replace QUINTIL			 = "QUINTIL 3"							  if AUXILIAR==10
replace QUINTIL			 = "QUINTIL 4"							  if AUXILIAR==11
replace QUINTIL			 = "QUINTIL 5"							  if AUXILIAR==12

replace NOMINDICADOR    = "PORCENTAJE DE PERSONAS EN HOGARES NO EN SITUACIÓN DE POBREZA QUE PERCIBEN JUBILACIONES POR DEBAJO DEL SALARIO MÍNIMO NACIONAL"    if AUXILIAR==13
replace NOMINDICADOR    = "PORCENTAJE DE PERSONAS EN HOGARES EN SITUACIÓN DE POBREZA QUE PERCIBEN JUBILACIONES POR DEBAJO DEL SALARIO MÍNIMO NACIONAL"       if AUXILIAR==14
 

** 

** 
export excel  "$tabulados\\`codind'.xls", cell(A1) firstrow(varlabels) replace
