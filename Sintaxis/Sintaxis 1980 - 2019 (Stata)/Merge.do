*======================================================================================*
*                       **   POBREZA Y DESIGUALDAD   **
*                            MATRIZ PARA BASE WEB
*
*
* Creación:      01/11/2020
* Institución:   UMAD, FCS-UdelaR
* Responsable:   Jimena Pandolfi
* Descripción:   Construcción de matriz para base web (Piso II - Motores)para 
*				 indicadores de dimiensión temática "empleo, salarios y transferencias". 
*			     Fusión de bases parciales
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
*                           DOCUMENTOS A FUSIONAR      
*======================================================================================*

	global parciales  311/324 331/341  //Para la transformación de bases
	global parciales2 312/324 331/341  //Para la función merge (se quita la primera)

*======================================================================================*
*                           TRANSFORMACIÓN DE BASES    
*======================================================================================*

	foreach parcial of numlist  $parciales  {
    import excel "$tabulados\\`parcial'.xls", firstrow clear
	keep VALOR ANIO CODIND NOMINDICADOR SEXO ASCENDENCIA QUINTIL DEPARTAMENTOUY URBANORURALUY PAÍS RESPONSABLE
	drop if VALOR==.
	save "$tabulados\Stata\\`parcial'.dta", replace
	}
    *
	
*======================================================================================*
*                           FUSIÓN DE BASES    
*======================================================================================*

use "$tabulados\\Stata\\311.dta", clear
save "C:\Users\Usuario\Dropbox\1. Unidad de Métodos y Acceso a Datos\1. Observatorio UMAD\PISO II_MOTORES\Base web\Base Motor_Pobreza.dta", replace

	foreach parcial of numlist $parciales2  {
	append using "$tabulados\\Stata\\`parcial'.dta", force
	}
    *

order CODIND NOMINDICADOR SEXO ASCENDENCIA QUINTIL DEPARTAMENTOUY URBANORURALUY PAÍS ANIO VALOR RESPONSABLE


recode ANIO /*
*/(0=2000)  /*
*/(1=2001)  /*
*/(2=2002)  /*
*/(3=2003)  /*
*/(4=2004)  /*
*/(5=2005)  /*
*/(6=2006)  /*
*/(7=2007)  /*
*/(8=2008)  /*
*/(9=2009)  /*
*/(10=2010) /*
*/(11=2011)  /*
*/(12=2012)  /*
*/(13=2013)  /*
*/(14=2014)  /*
*/(15=2015)  /*
*/(16=2016)  /*
*/(17=2017)  /*
*/(18=2018)  /*
*/(19=2019)  /*
*/(81=1981)  /*
*/(82=1982)  /*
*/(83=1983)  /*
*/(84=1984)  /*
*/(85=1985)  /*
*/(86=1986)  /*
*/(87=1987)  /*
*/(88=1988)  /*
*/(89=1989)  /*
*/(90=1990)  /*
*/(91=1991)  /*
*/(92=1992)  /*
*/(93=1993)  /*
*/(94=1994)  /*
*/(95=1995)  /*
*/(96=1996)  /*
*/(97=1997)  /*
*/(98=1998)  /*
*/(99=1999)  

ta ANIO, m
ta CODIND,m
recode CODIND (.=318) //Error en código
ta SEXO, m
ta ASCENDENCIA, m
ta QUINTIL, m
ta DEPARTAMENTOUY, m
ta URBANORURALUY, m
ta PAÍS, m
ta VALOR, m
ta RESPONSABLE, m


save "C:\Users\Usuario\Dropbox\1. Unidad de Métodos y Acceso a Datos\1. Observatorio UMAD\PISO II_MOTORES\Base web\Base Motor_Empleo_prev.dta", replace

	
*======================================================================================*
*                           CORRECCIONES DE CÓDIGOS (14/07)
*
*  	1) Crear una variable de jerarquía: Se incorpora un variable dicotómica que indica 
* 	los valores del indicador que se abren por defecto en la apertura de la página. La 
*	variable asume el valor 1 cuando el valor corresponde al dato por defecto y 0 cuando 
*	no (esto busca resolver el problemas de visualización de aperturas de indicadores 
*	construidos a partir de la ECH). 
*
*	2) Variable URBANORURALUY:  por defecto se introduce "TOTAL PAÍS,"  exceptuando 
*	cuando corresponde otra categoría. Por tanto, esta variable no debería tener valores vacíos. 
* 
*	3) Aperturas (SEXO, ASCENDENCIA, DEPARTAMENTO, QUINTIL, u otras que se agreguen a la base): 
*	Si la apertura existe para el indicador seleccionado pero no corresponde poner el valor de 
*	la categoría se pone el texto "TODOS". No corresponde poner valores vacíos en este caso.  
*   
*======================================================================================*


	/*Si se quiere correr esta sintaxis de nuevo actualizar la fecha del archivo*/
	/*import excel  "C:\Users\Usuario\Dropbox\1. Unidad de Métodos y Acceso a Datos\1. Observatorio UMAD\PISO II_MOTORES\Base web\Base Motor_Pobreza_11062020.xls", firstrow clear*/


	*0. Genero un código único por indicador (Mantengo el original de control)
	
	rename CODIND COD_JIMENA
	encode NOMINDICADOR, g (CODIND)
	lab drop CODIND


	
	*i. Genero variables auxilares dicotómicas que indican si existe la apertura para el indicador

	foreach var of varlist SEXO ASCENDENCIA QUINTIL {
	ta `var', g(aux_`var')
	}
	*
	
	egen dic_SEXO =           max (aux_SEXO1)          , by (CODIND)
	egen dic_ASCENDENCIA =    max (aux_ASCENDENCIA1)   , by (CODIND)
	egen dic_QUINTIL =        max (aux_QUINTIL2)       , by (CODIND) 

	ta COD_JIMENA if dic_SEXO==1
	ta COD_JIMENA if dic_ASCENDENCIA==1	
	ta COD_JIMENA if dic_QUINTIL==1	
	drop aux_*	
	

	*ii. Corrijo categoría "TODOS" (pto. 3) de las variables de apertura
	
	replace SEXO= "TODOS" if SEXO=="NA" & dic_SEXO==1
	replace SEXO= "."     if SEXO=="NA" & dic_SEXO==0
	
	replace ASCENDENCIA= "TODOS" if ASCENDENCIA=="NA" & dic_ASCENDENCIA==1
	replace ASCENDENCIA= "."     if ASCENDENCIA=="NA" & dic_ASCENDENCIA==0
	
	replace QUINTIL= "TODOS" if QUINTIL=="NA" & dic_QUINTIL==1
	replace QUINTIL= "."     if QUINTIL=="NA" & dic_QUINTIL==0

	replace DEPARTAMENTOUY= "." if DEPARTAMENTOUY=="NA"
	
	
	*iii. Corrijo variable URBANORURALUY (pto. 2)
	
	replace URBANORURALUY= "TOTAL PAÍS" if URBANORURALUY=="NA"

	
	*iv. Genero la variable JERARQUÍA (pto. 1)
	
	ta URBANORURALUY, g(aux_URBANORURALUY)
	egen dic_URBANORURALUY =   max (aux_URBANORURALUY4) , by (CODIND)
	
	egen aux_ANIO= min (ANIO), by (CODIND)
	replace aux_ANIO = 9999 if aux_ANIO<2006
	
	g JERARQUIA=0

	
	// Indicadores sin dato en país urbano
	replace JERARQUIA=1 if dic_URBANORURALUY==0 & SEXO=="." & ASCENDENCIA=="." & QUINTIL=="."
	replace JERARQUIA=1 if dic_URBANORURALUY==0 & COD_JIMENA==112 & QUINTIL=="TODOS" //Unico indicador en que hay aperturas
	

	// Indicadores con dato en país urbano

	    //Sin información antes de 2006
	    replace JERARQUIA=1 if dic_URBANORURALUY==1 & aux_ANIO==2006 & URBANORURALUY=="TOTAL PAÍS" & SEXO=="."     & ASCENDENCIA=="."     & QUINTIL=="." 
	    replace JERARQUIA=1 if dic_URBANORURALUY==1 & aux_ANIO==2006 & URBANORURALUY=="TOTAL PAÍS" & SEXO=="TODOS" & ASCENDENCIA=="TODOS" & QUINTIL=="TODOS" 
	    replace JERARQUIA=1 if dic_URBANORURALUY==1 & aux_ANIO==2006 & URBANORURALUY=="TOTAL PAÍS" & SEXO=="TODOS" & ASCENDENCIA=="TODOS" & QUINTIL=="." 
	    replace JERARQUIA=1 if dic_URBANORURALUY==1 & aux_ANIO==2006 & URBANORURALUY=="TOTAL PAÍS" & SEXO=="."     & ASCENDENCIA=="."     & QUINTIL=="TODOS" 
	    replace JERARQUIA=1 if dic_URBANORURALUY==1 & aux_ANIO==2006 & URBANORURALUY=="TOTAL PAÍS" & SEXO=="TODOS" & ASCENDENCIA=="."     & QUINTIL=="." 
		
		
        //Con información antes de 2006
	    replace JERARQUIA=1 if dic_URBANORURALUY==1 & aux_ANIO==9999 & URBANORURALUY=="URBANO (MÁS DE 5.000 HABITANTES)" & SEXO=="."     & ASCENDENCIA=="."     & QUINTIL=="." 
	    replace JERARQUIA=1 if dic_URBANORURALUY==1 & aux_ANIO==9999 & URBANORURALUY=="URBANO (MÁS DE 5.000 HABITANTES)" & SEXO=="TODOS" & ASCENDENCIA=="TODOS" & QUINTIL=="TODOS" 
	    replace JERARQUIA=1 if dic_URBANORURALUY==1 & aux_ANIO==9999 & URBANORURALUY=="URBANO (MÁS DE 5.000 HABITANTES)" & SEXO=="TODOS" & ASCENDENCIA=="TODOS" & QUINTIL=="." 
	    replace JERARQUIA=1 if dic_URBANORURALUY==1 & aux_ANIO==9999 & URBANORURALUY=="URBANO (MÁS DE 5.000 HABITANTES)" & SEXO=="."     & ASCENDENCIA=="."     & QUINTIL=="TODOS" 
	    replace JERARQUIA=1 if dic_URBANORURALUY==1 & aux_ANIO==9999 & URBANORURALUY=="URBANO (MÁS DE 5.000 HABITANTES)" & SEXO=="TODOS" & ASCENDENCIA=="."     & QUINTIL=="." 
		
			
	*v. Elimino variables auxiliares
	
    drop aux_* dic_* COD_JIMENA
	order CODIND
	lab var CODIND "CODIND"
	lab var JERARQUIA "JERARQUIA"

	export excel  "C:\Users\Usuario\Dropbox\1. Unidad de Métodos y Acceso a Datos\1. Observatorio UMAD\PISO II_MOTORES\Base web\Base Motor_Empleo_07122020.xls", cell(A1) firstrow(varlabels) replace

	























































