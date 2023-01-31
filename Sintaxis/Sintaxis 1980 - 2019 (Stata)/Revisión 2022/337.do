
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

/*	cd "C:\Users\Shari\Dropbox\UMAD"
	global bases "C:\Users\Shari\Dropbox\UMAD\Bases de datos\Encuesta Continua de Hogares (ECH)\Compatibilizadas IECON\Bases DESCA"
	global tabulados "C:\Users\Shari\Dropbox\UMAD\Sociodemografico\Indicadores empleo\Tabulados"
*/
	cd "C:\Users\Usuario\Dropbox\1. Unidad de Métodos y Acceso a Datos\1. Observatorio UMAD"
	global bases "C:\Users\Usuario\Dropbox\1. Unidad de Métodos y Acceso a Datos\1. Observatorio UMAD\PISO I_MICRODATOS\ECH\Microdatos\Compatibilizadas Iecon\En uso"
	global tabulados C:\Users\Usuario\Dropbox\1. Unidad de Métodos y Acceso a Datos\1. Observatorio UMAD\PISO II_MOTORES\Base web\3. Empleo, salarios y transferencias\Extracciones parciales



*======================================================================================*
*              CARACTERÍSTICAS GENERALES DE LA MATRIZ DE EXPORTACAIÓN      
*======================================================================================*

	global seieTOT 6/19         // Años de serie para total país

	  

*======================================================================================*
*                 3.3. TRANSFERENCIAS     
*======================================================================================*

* INDICADOR: Porcentaje de personas que perciben pensiones por debajo del salario mínimo nacional
* CÓDIGO:    337
* GRUPOS:    Región / Ascendencia étnico-racial / Sexo / Quintil de ingreso / Pobreza
* FUENTE:    ECH (INE)
* PERÍODO:   2001-2020 País urbano / 2006-2020 Total país 


**
**MODIFICACIÓN MANUAL: CÓDIGOS Y LOCAL J PARA VARIABLES AUXILIARES**

    *TOTAL PAÍS
	local i      = 1
	local codind = 337                  // Código de indicador
	local grupo  = 14                   // Cantidad de grupos
	local canio  = 15                   // Cantidad de años de la serie

	local filas = (1 + `grupo') * `canio' // Cantidad de grupos más el total por cantidad de años de la serie
	matrix def       MATR= J(`filas',3,.)
	matrix colnames  MATR= VALOR AUXILIAR ANIO

	foreach anio of numlist $seieTOT  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	
	g pensiones = bc_pg912 + bc_pg922	
	g pensiones_insuf_smn = 0
	replace pensiones_insuf_smn=1 if pensiones<smn

	cap drop bd_region
	g bd_region=.
	replace bd_region=1 if region_4== 1 | region_4== 2
	replace bd_region=2 if region_4== 3
	replace bd_region=3 if region_4== 4
	
    mean pensiones_insuf_smn [aw=bc_pesoan] if pensiones>0
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  0
    matrix MATR  [`i',3]=  `anio'
	
	
	/*NO se procesa país urbano para no duplicar valores*/
	
	local i  = `i' + 1 
	local j  = 	1
	foreach val of numlist 1/3  {
	mean pensiones_insuf_smn [aw=bc_pesoan] if pensiones>0 & bd_region==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
	matrix MATR  [`i',3]=  `anio'
	
	local i  = `i' + 1
	local j  = `j' + 1
    }
    *
	
	local j  = 	4
	foreach val of numlist 1/2  {
	mean pensiones_insuf_smn [aw=bc_pesoan] if pensiones>0 & bc_pe2==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *
	
	local j  = 	6
	foreach val of numlist 1/2  {
	mean pensiones_insuf_smn [aw=bc_pesoan] if pensiones>0 & bd_e29_1==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *
	
	local j  = 	8
	foreach val of numlist 1/5  {
	mean pensiones_insuf_smn [aw=bc_pesoan] if pensiones>0 & bd_quintilesy==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *
	
	local j  = 	13
	foreach val of numlist 0/1  {
	mean pensiones_insuf_smn [aw=bc_pesoan] if pensiones>0 & pobre06==`val'
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

g bc_pe3=e27
rename pobre_06 pobre06

	g pensiones = g148_2_1 + g148_2_2 + g148_2_3 + g148_2_4 + g148_2_5 /*
			  */+ g148_2_6 + g148_2_7 + g148_2_8 + g148_2_9 + g148_2_10 /*
			  */+ g148_2_11 + g148_2_12 
	g pensiones_insuf_lp = 0
	replace pensiones_insuf_lp=1 if pensiones<smn
	
	cap drop bd_region
	g bd_region=.
	replace bd_region=1 if region_4== 1 | region_4== 2
	replace bd_region=2 if region_4== 3
	replace bd_region=3 if region_4== 4

foreach mes of numlist 1/12 {
	mean pensiones_insuf_lp if mes==`mes'&pensiones>0 [aw=pesomen]
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
	mean pensiones_insuf_lp [aw=pesomen] if mes==`mes' & bd_region==`val'&pensiones>0
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
	mean pensiones_insuf_lp [aw=pesomen] if mes==`mes' & e26==`val'&pensiones>0
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
	mean pensiones_insuf_lp [aw=pesomen] if mes==`mes' & e29_1==`val'&pensiones>0
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
	mean pensiones_insuf_lp [aw=pesomen] if mes==`mes' & bd_quintilesy==`val' &pensiones>0
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
	mean pensiones_insuf_lp [aw=pesomen] if mes==`mes' & pobre06==`val'&pensiones>0
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
	local codind = 337                  // Código de indicador
	local grupo  = 14                   // Cantidad de grupos
	local canio  = 15                   // Cantidad de años de la serie


import  excel "$tabulados\Auxiliares\AUX`codind'_TP.xls", firstrow clear

destring AUXILIAR, replace
destring ANIO, replace
destring VALOR, replace

sort AUXILIAR ANIO
drop if ANIO==.

g CODIND                 = 337
g NOMINDICADOR           = "PORCENTAJE PERSONAS QUE PERCIBEN PENSIONES POR DEBAJO DEL SALARIO MÍNIMO NACIONAL"
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

replace NOMINDICADOR    = "PORCENTAJE DE PERSONAS EN HOGARES NO EN SITUACIÓN DE POBREZA QUE PERCIBEN PENSIONES POR DEBAJO DEL SALARIO MÍNIMO NACIONAL"    if AUXILIAR==13
replace NOMINDICADOR    = "PORCENTAJE DE PERSONAS EN HOGARES EN SITUACIÓN DE POBREZA QUE PERCIBEN PENSIONES POR DEBAJO DEL SALARIO MÍNIMO NACIONAL"       if AUXILIAR==14
 

** 

** 
export excel  "$tabulados\\`codind'.xls", cell(A1) firstrow(varlabels) replace
