	global bases "C:\Users\Usuario\Dropbox\1. Unidad de Métodos y Acceso a Datos\1. Observatorio UMAD\PISO I_MICRODATOS\ECH\Microdatos\Compatibilizadas Iecon\En uso"

	
	foreach anio of numlist 1/20  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear

	cap drop smn
g smn = .
replace smn = 1092		if bc_anio ==	2001		
replace smn = 1110		if bc_anio ==	2002		
replace smn = 1145		if bc_anio ==	2003		& bc_mes >= 1 & bc_mes<= 4
replace smn = 1170		if bc_anio ==	2003		& bc_mes >= 5 & bc_mes<=8
replace smn = 1194		if bc_anio ==	2003		& bc_mes >= 9 & bc_mes<= 12
replace smn = 1242		if bc_anio ==	2004		& bc_mes >= 1 & bc_mes<= 6
replace smn = 1310		if bc_anio ==	2004		& bc_mes >= 7 & bc_mes<= 12
replace smn = 2050		if bc_anio ==	2005		& bc_mes >= 1 & bc_mes<= 6
replace smn = 2500		if bc_anio ==	2005		& bc_mes >= 7 & bc_mes<= 12
replace smn = 2618		if bc_anio ==	2006		& bc_mes >= 1 & bc_mes<= 6
replace smn = 3000		if bc_anio ==	2006		& bc_mes >= 7 & bc_mes<= 12
replace smn = 3075		if bc_anio ==	2007		& bc_mes >= 1 & bc_mes<= 6
replace smn = 3244		if bc_anio ==	2007		& bc_mes >= 7 & bc_mes<= 12
replace smn = 3416		if bc_anio ==	2008		& bc_mes >= 1 & bc_mes<= 6
replace smn = 4150		if bc_anio ==	2008		& bc_mes >= 7 & bc_mes<= 12
replace smn = 4441		if bc_anio ==	2009		
replace smn = 4799		if bc_anio ==	2010		
replace smn = 6000		if bc_anio ==	2011		
replace smn = 7200		if bc_anio ==	2012		
replace smn = 7920		if bc_anio ==	2013		
replace smn = 8960		if bc_anio ==	2014		
replace smn = 10000		if bc_anio ==	2015		
replace smn = 11150		if bc_anio ==	2016		
replace smn = 12265		if bc_anio ==	2017		
replace smn = 13430		if bc_anio ==	2018		
replace smn = 15000		if bc_anio ==	2019		& bc_mes >= 1 & bc_mes<= 6
replace smn = 15650		if bc_anio ==	2019		& bc_mes >= 7 & bc_mes<= 12
replace smn = 16300		if bc_anio ==	2020		

save "$bases\Fusionada_personasyhogares_`anio'.dta", replace
	}
    *
