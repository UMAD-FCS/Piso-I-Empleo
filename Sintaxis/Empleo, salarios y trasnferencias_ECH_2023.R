
### Indicadores empleo, salarios y transferencias
### Unidad de Métodos y Acceso a datos
### Observatorio Uruguay

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

library(rio)
library(survey)
library(srvyr)
library(tidyverse)
library(laeken)
library(statar)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Referencias generales base motor PISO I ###

PAIS        <- "URUGUAY"	
FECHA       <- 2023
FECHA2      <- "01/01/2023"
RESPONSABLE	<- "JIMENA PANDOLFI"
JERARQUIA	<- ""
SECTORPRODUCTIVO	<- ""	
SECTORINSTITUCIONAL	<- ""
ACTIVIDADESECONOMICAS	<- ""
DIVISIÓNECONOMICA	<- ""	
CONCEPTO	<- ""	
RELACIONFAMILIAR	<- ""

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  
### Carga de bases ###

base            <- rio::import("Bases/ECH_implantacion_2023.Rdata")
bases_men       <- rio::import("Bases/ECH_panel_2023.Rdata")

varsimplant <- select(base, ID, HT11, HT19, HT13, e30, pobre06)
varsimplant <- varsimplant %>% distinct(ID, .keep_all = TRUE)
bases_men       <- base::merge(bases_men, varsimplant, by = "ID", all.x = TRUE, all.y = FALSE)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Generación de nuevas variables ###

smn <- 21107
smn_h = smn/200


#IPC

base <- base %>% dplyr::mutate(bc_ipc_tot = case_when(
  mes == 1  ~ 0.286995721,
  mes == 2  ~ 0.282605309,
  mes == 3  ~ 0.279798078,
  mes == 4  ~ 0.277298982,
  mes == 5  ~ 0.275240151,
  mes == 6  ~ 0.275267734,
  mes == 7  ~ 0.276549203,
  mes == 8  ~ 0.277553197,
  mes == 9  ~ 0.277076807,
  mes == 10 ~ 0.275395538,
  mes == 11 ~ 0.273682943,
  mes == 12 ~ 0.272746995))


bases_men <- bases_men %>% dplyr::mutate(bc_ipc_tot = case_when(
  mes == 1  ~ 0.286995721,
  mes == 2  ~ 0.282605309,
  mes == 3  ~ 0.279798078,
  mes == 4  ~ 0.277298982,
  mes == 5  ~ 0.275240151,
  mes == 6  ~ 0.275267734,
  mes == 7  ~ 0.276549203,
  mes == 8  ~ 0.277553197,
  mes == 9  ~ 0.277076807,
  mes == 10 ~ 0.275395538,
  mes == 11 ~ 0.273682943,
  mes == 12 ~ 0.272746995))

# Ingresos

bases_men <- bases_men %>% dplyr::mutate(y_pc       =  HT11 / HT19 )                      #Ingreso per-cápita
bases_men <- bases_men %>% dplyr::mutate(y_pc_svl   =  (HT11 - HT13) / HT19)              #Ingreso per-cápita sin valor locativo
bases_men <- bases_men %>% dplyr::mutate(y_pc_d     =  HT11 / HT19 / bc_ipc_tot)          #Ingreso per-cápita deflactado
bases_men <- bases_men %>% dplyr::mutate(y_pc_svl_d =  (HT11 - HT13) / HT19 / bc_ipc_tot) #Ingreso per-cápita sin valor locativo deflactado

base <- base %>% dplyr::mutate(y_pc       =  HT11 / HT19 )                      #Ingreso per-cápita
base <- base %>% dplyr::mutate(y_pc_svl   =  (HT11 - HT13) / HT19)              #Ingreso per-cápita sin valor locativo
base <- base %>% dplyr::mutate(y_pc_d     =  HT11 / HT19 / bc_ipc_tot)          #Ingreso per-cápita deflactado
base <- base %>% dplyr::mutate(y_pc_svl_d =  (HT11 - HT13) / HT19 / bc_ipc_tot) #Ingreso per-cápita sin valor locativo deflactado


# Quintil de ingresos

base_h <- bases_men %>% distinct(ID, .keep_all = TRUE)
base_h <- base_h %>% dplyr::mutate(quintilesy = statar::xtile(y_pc, n=5, wt = W))
base_h <- base_h[,c("ID","quintilesy")]
bases_men <- merge(bases_men, base_h, by = "ID")

base_h_a <- base %>% distinct(ID, .keep_all = TRUE)
base_h_a <- base_h_a %>% dplyr::mutate(quintilesy = statar::xtile(y_pc, n=5, wt = W_ANO))
base_h_a <- base_h_a[,c("ID","quintilesy")]
base <- merge(base, base_h_a, by = "ID")


# Sexo

bases_men <- bases_men %>% dplyr::mutate(bc_pe2 = e26)
base <- base %>% dplyr::mutate(bc_pe2 = e26)


# Ascendencia afro

bases_men <- bases_men %>% dplyr::mutate(bd_e29_1 = e29_1)
base <- base %>% dplyr::mutate(bd_e29_1 = e29_1)


# Sexo del jefe/a de hogar

bases_men <- bases_men[order(bases_men$ID, bases_men$e30, decreasing = FALSE), ]
bases_men_h <- bases_men %>% distinct(ID, .keep_all = TRUE)
bases_men_h <- bases_men_h %>% dplyr::mutate(sexojefe = case_when(e30 == 1 & e26 == 1 ~ 1,
                                                        e30 == 1 & e26 == 2 ~ 2,
                                                        e30 != 1 ~ 99))
bases_men_h <- bases_men_h[,c("ID","sexojefe")]
bases_men <- merge(bases_men, bases_men_h, by = "ID")


base <- base[order(base$ID, base$e30, decreasing = FALSE), ]
base_h <- base %>% distinct(ID, .keep_all = TRUE)
base_h <- base_h %>% dplyr::mutate(sexojefe = case_when(e30 == 1 & e26 == 1 ~ 1,
                                                                  e30 == 1 & e26 == 2 ~ 2,
                                                                  e30 != 1 ~ 99))
base_h <- base_h[,c("ID","sexojefe")]
base <- merge(base, base_h, by = "ID")


# Pobreza


bases_men <- bases_men %>% dplyr::mutate(pobre_06 = case_when(pobre06 == 1 ~ 1,
                                                              pobre06 == 0 ~ 2))

base <- base %>% dplyr::mutate(pobre_06 = case_when(pobre06 == 1 ~ 1,
                                                    pobre06 == 0 ~ 2))

# Tramo de edad


bases_men <- bases_men %>% dplyr::mutate(tramo_edad = case_when(e27 >= 14 & e27 <= 18 ~  1,
                                                      e27 >= 19 & e27 <= 24 ~  2,
                                                      e27 >= 25 & e27 <= 29 ~  3,
                                                      e27 >= 30 & e27 <= 64 ~  4,
                                                      e27 >= 65             ~  5))
base <- base %>% dplyr::mutate(tramo_edad = case_when(e27 >= 14 & e27 <= 18 ~  1,
                                                                e27 >= 19 & e27 <= 24 ~  2,
                                                                e27 >= 25 & e27 <= 29 ~  3,
                                                                e27 >= 30 & e27 <= 64 ~  4,
                                                                e27 >= 65             ~  5))



# Población económicamente activa

bases_men <- bases_men %>% dplyr::mutate(pea = ifelse(POBPCOAC==2 | POBPCOAC==3 | POBPCOAC==4 | POBPCOAC==5, 1, 0))
base <- base %>% dplyr::mutate(pea = ifelse(POBPCOAC==2 | POBPCOAC==3 | POBPCOAC==4 | POBPCOAC==5, 1, 0))


# Ocupados

bases_men <- bases_men %>% dplyr::mutate(ocup = ifelse(POBPCOAC==2, 1, 0))
base <- base %>% dplyr::mutate(ocup = ifelse(POBPCOAC==2, 1, 0))


# Desempleados

bases_men <- bases_men %>% dplyr::mutate(desocup = ifelse(POBPCOAC==3 | POBPCOAC==4 | POBPCOAC==5, 1, 0))
base <- base %>% dplyr::mutate(desocup = ifelse(POBPCOAC==3 | POBPCOAC==4 | POBPCOAC==5, 1, 0))


# Horas remuneradas

bases_men <- bases_men  %>% dplyr::mutate(bc_horas = f85+f98)
base <- base  %>% dplyr::mutate(bc_horas = f85+f98)


# Subempleo

bases_men <- bases_men %>% dplyr::mutate(bc_subocupado = ifelse(f102 == 1 & f104 == 5 & bc_horas > 0 & bc_horas < 40, 1, 0))
base <- base %>% dplyr::mutate(bc_subocupado = ifelse(f102 == 1 & f104 == 5 & bc_horas > 0 & bc_horas < 40, 1, 0))



# No aporte a SS en trabajo principal y secundario

base <- base %>% dplyr::mutate(bc_register2 = ifelse(f96 == 1 | f82 == 1, 0, 1)) #no se pregunta por aprote en ocupacion secundaria en panel


# Aporte a SS en trabajo principal

bases_men <- bases_men %>% dplyr::mutate(bc_register = ifelse(f82 == 1, 1, 0))
base <- base %>% dplyr::mutate(bc_register = ifelse(f82 == 1, 1, 0))


# No aporte por totalidad del salario

base <- base %>% dplyr::mutate(noaportatot = ifelse(f84 == 2, 1, 0))


# Salario en ocupación principal

base <- base %>% dplyr::mutate(horamen = f85 * 4.3)
base <- base %>% dplyr::mutate(yhora = PT2/horamen)
base <- base %>% dplyr::mutate(yhoradef = case_when(f85>0 ~ yhora*bc_ipc_tot,
                                                    f85==0 ~ 0))
                               

# Línea de pobreza individual

base <- base %>% dplyr::mutate(regionlp = case_when(REGION_4 == 1 ~ 1,
                                                    REGION_4 == 2 | REGION_4 == 3 ~ 2,
                                                    REGION_4 == 4 ~ 3))


base <- base %>% dplyr::mutate(grupolp = case_when(regionlp == 1 & mes == 1  ~ 1,
                                                   regionlp == 1 & mes == 2  ~ 2,
                                                   regionlp == 1 & mes == 3  ~ 3,
                                                   regionlp == 1 & mes == 4  ~ 4,
                                                   regionlp == 1 & mes == 5  ~ 5,
                                                   regionlp == 1 & mes == 6  ~ 6,
                                                   regionlp == 1 & mes == 7  ~ 7,
                                                   regionlp == 1 & mes == 8  ~ 8,
                                                   regionlp == 1 & mes == 9  ~ 9,
                                                   regionlp == 1 & mes == 10 ~ 10,
                                                   regionlp == 1 & mes == 11 ~ 11,
                                                   regionlp == 1 & mes == 12 ~ 12,
                                                   regionlp == 2 & mes == 1  ~ 13,
                                                   regionlp == 2 & mes == 2  ~ 14,
                                                   regionlp == 2 & mes == 3  ~ 15,
                                                   regionlp == 2 & mes == 4  ~ 16,
                                                   regionlp == 2 & mes == 5  ~ 17,
                                                   regionlp == 2 & mes == 6  ~ 18,
                                                   regionlp == 2 & mes == 7  ~ 19,
                                                   regionlp == 2 & mes == 8  ~ 20,
                                                   regionlp == 2 & mes == 9  ~ 21,
                                                   regionlp == 2 & mes == 10 ~ 22,
                                                   regionlp == 2 & mes == 11 ~ 23,
                                                   regionlp == 2 & mes == 12 ~ 24, 
                                                   regionlp == 3 & mes == 1  ~ 25,
                                                   regionlp == 3 & mes == 2  ~ 26,
                                                   regionlp == 3 & mes == 3  ~ 27,
                                                   regionlp == 3 & mes == 4  ~ 28,
                                                   regionlp == 3 & mes == 5  ~ 29,
                                                   regionlp == 3 & mes == 6  ~ 30,
                                                   regionlp == 3 & mes == 7  ~ 31,
                                                   regionlp == 3 & mes == 8  ~ 32,
                                                   regionlp == 3 & mes == 9  ~ 33,
                                                   regionlp == 3 & mes == 10 ~ 34,
                                                   regionlp == 3 & mes == 11 ~ 35,
                                                   regionlp == 3 & mes == 12 ~ 36))                                                    




base_unipersonales <- base %>%  filter(HT19==1)
base_unipersonales <- base_unipersonales[,c("grupolp", "lp_06")]
base_unipersonales <- base_unipersonales %>% dplyr::mutate(lp_unipersonales = lp_06)
base_unipersonales <- base_unipersonales[order(base_unipersonales$grupolp, 
                                               base_unipersonales$lp_unipersonales, 
                                               decreasing = TRUE), ]
base_unipersonales <- base_unipersonales %>% distinct(grupolp, .keep_all = TRUE)
base_unipersonales <- base_unipersonales[,c("grupolp","lp_unipersonales")]

base <- merge(base, base_unipersonales, by = "grupolp")





# Salario por debajo de línea de pobreza

base <- base %>% dplyr::mutate(salario_insuf   = ifelse(PT2 < lp_unipersonales, 1, 0))


# Salario por debajo del salario mínimo nacional

base <- base %>% dplyr::mutate(salario_insuf_smn = ifelse(yhora<smn_h, 1, 0))


# Percepción de jubilaciones

base <- base %>% dplyr::mutate(jub = ifelse(g148_1_1 > 0 |
                                                              g148_1_2 > 0 |
                                                              g148_1_3 > 0 |
                                                              g148_1_5 > 0 |
                                                              g148_1_6 > 0 |
                                                              g148_1_7 > 0 |
                                                              g148_1_8 > 0 |
                                                              g148_1_9 > 0 |
                                                              g148_1_10 > 0 |
                                                              g148_1_11 > 0, 1, 0)) # No está la variable g148_1_4 "UNIÓN POSTAL"


# Percepción de pensiones



base <- base %>% dplyr::mutate(pens = ifelse(g148_2_1 > 0 |
                                                               g148_2_2 > 0 |
                                                               g148_2_3 > 0 |
                                                               g148_2_5 > 0 |
                                                               g148_2_6 > 0 |
                                                               g148_2_7 > 0 |
                                                               g148_2_8 > 0 |
                                                               g148_2_9 > 0 |
                                                               g148_2_10 > 0 |
                                                               g148_2_11 > 0, 1, 0)) # No está la variable g148_2_4 "UNIÓN POSTAL"

# No percepción de jubilaciones o pensiones

base <- base %>% dplyr::mutate(nijubpens = ifelse(jub == 1 | pens == 1, 0, 1))


# Ingresos por jubilaciones

base <- base %>% dplyr::mutate(y_jub = g148_1_1 + g148_1_2 + g148_1_3 + g148_1_5 + g148_1_6 + g148_1_7 + g148_1_8 + g148_1_9 + g148_1_12 + g148_1_10 + g148_1_11)


# Ingresos por jubilaciones por debajo de la línea de pobreza

base <- base %>% dplyr::mutate(jub_insuf = ifelse(y_jub < lp_unipersonales, 1, 0))



# Ingresos por jubilaciones por debajo del salario mínimo nacional


base <- base %>% dplyr::mutate(jub_insuf_smn = ifelse(y_jub < smn, 1, 0))


# Ingresos por pensiones


base <- base %>% dplyr::mutate(y_pens = g148_2_1 + g148_2_2 + g148_2_3 + g148_2_5 + g148_2_6 + g148_2_7 + g148_2_8 + g148_2_9 + g148_2_12 + g148_2_10 + g148_2_11)


# Ingresos por pensiones debajo de la línea de pobreza

base <- base %>% dplyr::mutate(pens_insuf = ifelse(y_pens < lp_unipersonales, 1, 0))



# Ingresos por pensiones debajo del SMN


base <- base %>% dplyr::mutate(pens_insuf_smn = ifelse(y_pens < smn, 1, 0))


# Desempleados que perciben seguro de desempleo

base <- base %>% dplyr::mutate(seguroparo = ifelse(POBPCOAC == 5, 1, 0))
bases_men <- bases_men %>% dplyr::mutate(seguroparo = ifelse(POBPCOAC == 5, 1, 0))



# Captación AFAM


base <- base %>% dplyr::mutate(afam_pe = ifelse((g152 == 1 & f73 != 2 & f92 !=2 ) | (g150 == 1 & (POBPCOAC %in% c(1, 3, 4, 6, 7, 8, 11) | (POBPCOAC == 2 & f82 != 1 & f96 != 1))), 1, 0),   
                               afam_cont = ifelse(g150 == 1 & afam_pe == 0, 1, 0),
                               afam_total = ifelse(afam_pe == 1 | afam_cont == 1, 1, 0)) 

base_afam <- base[,c("ID","afam_pe", "afam_cont", "afam_total")]
base_afam <- base_afam %>% dplyr::mutate(h_afam_pe = afam_pe,
                                         h_afam_cont = afam_cont,
                                         h_afam_total = afam_total)

base_afam_1 <- base_afam[order(base_afam$ID, base_afam$h_afam_pe, decreasing = TRUE), ]
base_afam_2 <- base_afam[order(base_afam$ID, base_afam$h_afam_cont, decreasing = TRUE), ]
base_afam_3 <- base_afam[order(base_afam$ID, base_afam$h_afam_total, decreasing = TRUE), ]

base_afam_1 <- base_afam_1 %>% distinct(ID, .keep_all = TRUE)
base_afam_2 <- base_afam_2 %>% distinct(ID, .keep_all = TRUE)
base_afam_3 <- base_afam_3 %>% distinct(ID, .keep_all = TRUE)

base_afam_1 <- base_afam_1[,c("ID","h_afam_pe")]
base_afam_2 <- base_afam_2[,c("ID","h_afam_cont")]
base_afam_3 <- base_afam_3[,c("ID","h_afam_total")]

base <- merge(base, base_afam_1, by = "ID")
base <- merge(base, base_afam_2, by = "ID")
base <- merge(base, base_afam_3, by = "ID")



#  Captación TUS


base <- base %>% dplyr::mutate(tus = ifelse(e560==1, 1, 0))

base_tus <- base[,c("ID","tus")]
base_tus <- base_tus %>% dplyr::mutate(h_tus = tus)
base_tus <- base_tus[order(base_tus$ID, base_tus$h_tus, decreasing = TRUE), ]
base_tus <- base_tus %>% distinct(ID, .keep_all = TRUE)
base_tus <- base_tus[,c("ID","h_tus")]
base <- merge(base, base_tus, by = "ID")


# Categoría de la ocupación

bases_men <- bases_men %>% dplyr::mutate(apr = ifelse(f73 == 1 , 1, 0))
bases_men <- bases_men %>% dplyr::mutate(apu = ifelse(f73 == 2 , 1, 0))
bases_men <- bases_men %>% dplyr::mutate(coo = ifelse(f73 == 3 , 1, 0))
bases_men <- bases_men %>% dplyr::mutate(pat = ifelse(f73 == 4 , 1, 0))
bases_men <- bases_men %>% dplyr::mutate(cps = ifelse(f73 == 5 , 1, 0))
bases_men <- bases_men %>% dplyr::mutate(cpc = ifelse(f73 == 6 , 1, 0))
bases_men <- bases_men %>% dplyr::mutate(mnr = ifelse(f73 == 7 , 1, 0))
bases_men <- bases_men %>% dplyr::mutate(pse = ifelse(f73 == 8 , 1, 0))


categoria <- c("apr", "apu", "coo", "pat", "cps", "cpc", "mnr", "pse")




# Tipo de ocupación


bases_men <- bases_men %>% dplyr::mutate(mil = ifelse(f71_2 >  0000 & f71_2 < 1000, 1, 0))
bases_men <- bases_men %>% dplyr::mutate(dir = ifelse(f71_2 >= 1000 & f71_2 < 2000, 1, 0))
bases_men <- bases_men %>% dplyr::mutate(pro = ifelse(f71_2 >= 2000 & f71_2 < 3000, 1, 0))
bases_men <- bases_men %>% dplyr::mutate(tec = ifelse(f71_2 >= 3000 & f71_2 < 4000, 1, 0))
bases_men <- bases_men %>% dplyr::mutate(adm = ifelse(f71_2 >= 4000 & f71_2 < 5000, 1, 0))
bases_men <- bases_men %>% dplyr::mutate(ser = ifelse(f71_2 >= 5000 & f71_2 < 6000, 1, 0))
bases_men <- bases_men %>% dplyr::mutate(agr = ifelse(f71_2 >= 6000 & f71_2 < 7000, 1, 0))
bases_men <- bases_men %>% dplyr::mutate(ofi = ifelse(f71_2 >= 7000 & f71_2 < 8000, 1, 0))
bases_men <- bases_men %>% dplyr::mutate(ope = ifelse(f71_2 >= 8000 & f71_2 < 9000, 1, 0))
bases_men <- bases_men %>% dplyr::mutate(ele = ifelse(f71_2 >= 9000 , 1, 0))


ocupacion <- c("mil", "dir", "pro", "tec", "adm", "ser", "agr", "ofi", "ope", "ele")


# Rama de actividad

bases_men <- bases_men %>% dplyr::mutate(r1  = ifelse(f72_2 >  0000 & f72_2 < 1000, 1, 0))
bases_men <- bases_men %>% dplyr::mutate(r2  = ifelse(f72_2 >  1000 & f72_2 < 4000, 1, 0))
bases_men <- bases_men %>% dplyr::mutate(r3  = ifelse(f72_2 >  4000 & f72_2 < 4500, 1, 0))
bases_men <- bases_men %>% dplyr::mutate(r4  = ifelse(f72_2 >= 4500 & f72_2 < 4900, 1, 0))
bases_men <- bases_men %>% dplyr::mutate(r5  = ifelse(f72_2 >= 4900 & f72_2 < 5500, 1, 0))
bases_men <- bases_men %>% dplyr::mutate(r6  = ifelse((f72_2 >= 5500 & f72_2 < 5800) | (f72_2 >= 9000 & f72_2 < 9400  | (f72_2 >= 9400 & f72_2 < 9700)), 1, 0))
bases_men <- bases_men %>% dplyr::mutate(r7  = ifelse(f72_2 >= 5800 & f72_2 < 6400, 1, 0))
bases_men <- bases_men %>% dplyr::mutate(r8  = ifelse(f72_2 >= 6400 & f72_2 < 6800, 1, 0))
bases_men <- bases_men %>% dplyr::mutate(r9  = ifelse(f72_2 >= 6800 & f72_2 < 6900, 1, 0))
bases_men <- bases_men %>% dplyr::mutate(r10 = ifelse(f72_2 >= 6900 & f72_2 < 7500, 1, 0))
bases_men <- bases_men %>% dplyr::mutate(r11 = ifelse(f72_2 >= 7500 & f72_2 < 8400, 1, 0))
bases_men <- bases_men %>% dplyr::mutate(r12 = ifelse((f72_2 >= 8400 & f72_2 < 8500) | (f72_2 == 9900), 1, 0))
bases_men <- bases_men %>% dplyr::mutate(r13 = ifelse(f72_2 >= 8500 & f72_2 < 8600, 1, 0))
bases_men <- bases_men %>% dplyr::mutate(r14 = ifelse(f72_2 >= 8600 & f72_2 < 9000, 1, 0))
bases_men <- bases_men %>% dplyr::mutate(r17 = ifelse(f72_2 >= 9700 & f72_2 < 9900, 1, 0))

rama <- c("r1", "r2", "r3", "r4", "r5", "r6", "r7", "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r17")





# Restricciones al empleo

base <- base %>% dplyr::mutate(nre = ifelse(bc_register2 == 1 & bc_subocupado == 0 , 1, 0))
base <- base %>% dplyr::mutate(sub = ifelse(bc_register2 == 0 & bc_subocupado == 1 , 1, 0))
base <- base %>% dplyr::mutate(amb = ifelse(bc_register2 == 1 & bc_subocupado == 1 , 1, 0))
base <- base %>% dplyr::mutate(sin = ifelse(bc_register2 == 0 & bc_subocupado == 0 , 1, 0))

restricciones <- c("nre", "sub", "amb", "sin")



rm(base_h, bases_men_h, base_unipersonales, base_tus, base_afam, 
   base_afam_1, base_afam_2, base_afam_3, varsimplant, base_h_a)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Bases a nivel hogar ###                               

base_h <- base %>% distinct(ID, .keep_all = TRUE)
bases_men_h <- bases_men %>% distinct(ID, .keep_all = TRUE)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Información de muestreo ###


base_men_svy <- srvyr::as_survey_design(bases_men, ids = ID, weights = W)
base_svy <- srvyr::as_survey_design(base, ids = ID, weights = W_ANO)

base_men_h_svy <- srvyr::as_survey_design(bases_men_h, ids = ID, weights = W)
base_h_svy <- srvyr::as_survey_design(base_h, ids = ID, weights = W_ANO)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Estimación de indicadores ###

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
### 311 Tasa de actividad
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

## Total país


# Total

a_mes <- base_men_svy %>%
  srvyr::filter(e27>=14) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(pea, na.rm=T))

c_ano <- mean(as.numeric(a_mes$colname))
c_ano <- as.data.frame(c_ano)
c_ano <- c_ano %>% dplyr::mutate(aux = 1)
c_ano <- c_ano %>% dplyr::rename(VALOR = c_ano)

# Ascendencia étnico racial

a_afro <- function(x) {
  x <- base_men_svy %>%
    filter(bd_e29_1 == x & e27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pea, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_afro(x = i)
}     


c_afro <- as.data.frame(VALOR)
c_afro <- c_afro %>% dplyr::mutate(aux = c(2,3))
c_afro <- c_afro[,c("VALOR", "aux")]



# Quintil de ingresos

a_quintil <- function(x) {
  x <- base_men_svy %>%
    filter(quintilesy == x & e27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pea, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:5){
  VALOR[i] <- a_quintil(x = i)
}     

 

c_quintil <- as.data.frame(VALOR)
c_quintil <- c_quintil %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil <- c_quintil[,c("VALOR", "aux")]




# Sexo

a_sexo <- function(x) {
  x <- base_men_svy %>%
    filter(bc_pe2 == x & e27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pea, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_sexo(x = i)
}     



c_sexo <- as.data.frame(VALOR)
c_sexo <- c_sexo %>% dplyr::mutate(aux = c(9, 10))
c_sexo <- c_sexo[,c("VALOR", "aux")]


# Tramo de edad

a_edad <- function(x) {
  x <- base_men_svy %>%
    filter(tramo_edad == x & e27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pea, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:5){
  VALOR[i] <- a_edad(x = i)
}     


c_edad <- as.data.frame(VALOR)
c_edad <- c_edad %>% dplyr::mutate(aux = c(11, 12, 13, 14, 15))
c_edad <- c_edad[,c("VALOR", "aux")]



# Región

a_region <- function(x) {
  x <- base_men_svy %>%
    filter(REGION_4 == x & e27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pea, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 3:4){
  VALOR[i] <- a_region(x = i)
}     


c_region <- as.data.frame(VALOR)
c_region <- as.data.frame(c_region[c(3,4),])
c_region <- c_region %>% dplyr::mutate(aux = c(16, 17))
c_region <- c_region %>% rename("VALOR" = "c_region[c(3, 4), ]")
c_region <- c_region[,c("VALOR", "aux")]


# Condición de pobreza del hogar

a_pobre <- function(x) {
  x <- base_men_svy %>%
    filter(pobre_06 == x & e27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pea, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_pobre(x = i)
}     


c_pobre <- as.data.frame(VALOR)
c_pobre <- c_pobre %>% dplyr::mutate(aux = c(18, 19))
c_pobre <- c_pobre[,c("VALOR", "aux")]


## País Urbano

# Total

a_mes <- base_men_svy %>%
  srvyr::filter(e27>=14 & (REGION_4 == 1 | REGION_4 == 2)) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(pea, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

c_ano_pu <- as.data.frame(a_sem)
c_ano_pu <- c_ano_pu %>% dplyr::mutate(aux = 1)
c_ano_pu <- c_ano_pu %>% dplyr::rename(VALOR = a_sem)

# Quintil de ingresos

a_quintil <- function(x) {
  x <- base_men_svy %>%
    filter(quintilesy == x & e27>=14 & (REGION_4 == 1 | REGION_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pea, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:5){
  VALOR[i] <- a_quintil(x = i)
}     


c_quintil_pu <- as.data.frame(VALOR)
c_quintil_pu <- c_quintil_pu %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil_pu <- c_quintil_pu[,c("VALOR", "aux")]


# Sexo

a_sexo <- function(x) {
  x <- base_men_svy %>%
    filter(bc_pe2 == x & e27>=14 & (REGION_4 == 1 | REGION_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pea, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_sexo(x = i)
}     


c_sexo_pu <- as.data.frame(VALOR)
c_sexo_pu <- c_sexo_pu %>% dplyr::mutate(aux = c(9, 10))
c_sexo_pu <- c_sexo_pu[,c("VALOR", "aux")]


# Tramo de edad

a_edad <- function(x) {
  x <- base_men_svy %>%
    filter(tramo_edad == x & e27>=14 & (REGION_4 == 1 | REGION_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pea, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:5){
  VALOR[i] <- a_edad(x = i)
}     


c_edad_pu <- as.data.frame(VALOR)
c_edad_pu <- c_edad_pu %>% dplyr::mutate(aux = c(11, 12, 13, 14, 15))
c_edad_pu <- c_edad_pu[,c("VALOR", "aux")]



CODIND       <- 311
NOMINDICADOR <- "TASA DE ACTIVIDAD"

VALOR        <- rbind(c_ano, c_afro, c_quintil, c_sexo, c_edad, c_region, c_pobre)
VALOR        <- VALOR  %>% dplyr::mutate(SEXO = case_when(aux == 9  ~ "VARONES", 
                                                          aux == 10 ~ "MUJERES", 
                                                          TRUE     ~ "TODOS"),
                                         ASCENDENCIA  = case_when(aux == 2 ~ "AFRO", 
                                                                  aux == 3 ~ "NO AFRO",
                                                                  TRUE     ~ "TODOS"), 
                                          QUINTIL =      case_when(aux == 4 ~ "QUINTIL 1",
                                                                  aux == 5 ~ "QUINTIL 2",
                                                                  aux == 6 ~ "QUINTIL 3",
                                                                  aux == 7 ~ "QUINTIL 4",
                                                                  aux == 8 ~ "QUINTIL 5",
                                                                  TRUE     ~ "TODOS"),
                                         DEPARTAMENTO = "", 
                                         URBANORURALUY = case_when(aux == 16 ~ "URBANO (MENOS DE 5.000 HABITANTES)",
                                                                   aux == 17 ~ "RURAL DISPERSO",
                                                                   TRUE     ~ "TOTAL PAÍS"),
                                         EDAD =          case_when(aux == 11 ~ "14 a 18",
                                                                   aux == 12 ~ "19 a 24",
                                                                   aux == 13 ~ "25 a 29",
                                                                   aux == 14 ~ "30 a 64",
                                                                   aux == 15 ~ "65 y más",
                                                                   TRUE     ~ ""),
                                         SITUACION_HOGAR = case_when(aux == 18 ~ "No en situación de pobreza",
                                                                     aux == 19  ~"En situación de pobreza",
                                                                     TRUE     ~ ""),
                                         RAMA_DE_ACTIVIDAD = "",
                                         TIPO_DE_OCUPACION = "",
                                         CATEGORÍOCUP = "",
                                         RESTRICCIONES_AL_EMPLEO = "",
                                         SEXO_JEFE       = "")
                                           
                                           
VALOR_pu        <- rbind(c_ano_pu, c_quintil_pu, c_sexo_pu, c_edad_pu)
VALOR_pu        <- VALOR_pu  %>% dplyr::mutate(SEXO = case_when(aux == 9  ~ "VARONES", 
                                                          aux == 10 ~ "MUJERES", 
                                                          TRUE     ~ "TODOS"),
                                         ASCENDENCIA  =  "TODOS", 
                                         QUINTIL =      case_when(aux == 4 ~ "QUINTIL 1",
                                                                  aux == 5 ~ "QUINTIL 2",
                                                                  aux == 6 ~ "QUINTIL 3",
                                                                  aux == 7 ~ "QUINTIL 4",
                                                                  aux == 8 ~ "QUINTIL 5",
                                                                  TRUE     ~ "TODOS"),
                                         DEPARTAMENTO = "", 
                                         URBANORURALUY = "URBANO (MÁS DE 5.000 HABITANTES)",
                                         EDAD =          case_when(aux == 11 ~ "14 a 18",
                                                                   aux == 12 ~ "19 a 24",
                                                                   aux == 13 ~ "25 a 29",
                                                                   aux == 14 ~ "30 a 64",
                                                                   aux == 15 ~ "65 y más",
                                                                   TRUE     ~ ""),
                                         SITUACION_HOGAR = "",
                                         RAMA_DE_ACTIVIDAD = "",
                                         TIPO_DE_OCUPACION = "",
                                         CATEGORÍOCUP = "",
                                         RESTRICCIONES_AL_EMPLEO = "",
                                         SEXO_JEFE       = "")

VALOR <- rbind (VALOR, VALOR_pu)
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Este indicador se calcula a partir de la encuesta telefónica del primer semestre de 2021 y el formulario telefónico de modalidad panel del segundo semestre de 2021. En el segundo semestre, la condición de pobreza y el quintil de ingresos del hogar corresponden a los ingresos declarados durante la implantación del panel en la encuesta presencial."


t_01 <- cbind(CODIND,	NOMINDICADOR,	PAIS,	FECHA,	FECHA2,	VALOR,	RESPONSABLE,	JERARQUIA,	SECTORPRODUCTIVO,	SECTORINSTITUCIONAL,	ACTIVIDADESECONOMICAS,	DIVISIÓNECONOMICA,	CONCEPTO,	RELACIONFAMILIAR, NOTA_INDICADOR)
t_01 <- subset(t_01, select=c(CODIND,	NOMINDICADOR,	SEXO,	ASCENDENCIA,	QUINTIL, DEPARTAMENTO, URBANORURALUY,	PAIS,	FECHA,	FECHA2,	VALOR,	RESPONSABLE,	JERARQUIA,	SECTORPRODUCTIVO,	SECTORINSTITUCIONAL,	ACTIVIDADESECONOMICAS,	DIVISIÓNECONOMICA,	CONCEPTO,	CATEGORÍOCUP,	RELACIONFAMILIAR,	EDAD, SITUACION_HOGAR, RAMA_DE_ACTIVIDAD, TIPO_DE_OCUPACION, RESTRICCIONES_AL_EMPLEO, SEXO_JEFE, NOTA_INDICADOR))





# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
### 312 Tasa de empleo
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

## Total país


# Total

a_mes <- base_men_svy %>%
  srvyr::filter(e27>=14) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(ocup, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))


c_ano <- as.data.frame(a_sem)
c_ano <- c_ano %>% dplyr::mutate(aux = 1)
c_ano <- c_ano %>% dplyr::rename(VALOR = a_sem)

# Ascendencia étnico racial

a_afro <- function(x) {
  x <- base_men_svy %>%
    filter(bd_e29_1 == x & e27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(ocup, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_afro(x = i)
}     



c_afro <- as.data.frame(VALOR)
c_afro <- c_afro %>% dplyr::mutate(aux = c(2,3))
c_afro <- c_afro[,c("VALOR", "aux")]



# Quintil de ingresos

a_quintil <- function(x) {
  x <- base_men_svy %>%
    filter(quintilesy == x & e27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(ocup, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:5){
  VALOR[i] <- a_quintil(x = i)
}     



c_quintil <- as.data.frame(VALOR)
c_quintil <- c_quintil %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil <- c_quintil[,c("VALOR", "aux")]

# Sexo

a_sexo <- function(x) {
  x <- base_men_svy %>%
    filter(bc_pe2 == x & e27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(ocup, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_sexo(x = i)
}     


c_sexo <- as.data.frame(VALOR)
c_sexo <- c_sexo %>% dplyr::mutate(aux = c(9, 10))
c_sexo <- c_sexo[,c("VALOR", "aux")]

# Tramo de edad

a_edad <- function(x) {
  x <- base_men_svy %>%
    filter(tramo_edad == x & e27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(ocup, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:5){
  VALOR[i] <- a_edad(x = i)
}     

c_edad <- as.data.frame(VALOR)
c_edad <- c_edad %>% dplyr::mutate(aux = c(11, 12, 13, 14, 15))
c_edad <- c_edad[,c("VALOR", "aux")]



# Región

a_region <- function(x) {
  x <- base_men_svy %>%
    filter(REGION_4 == x & e27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(ocup, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 3:4){
  VALOR[i] <- a_region(x = i)
}     



c_region <- as.data.frame(VALOR)
c_region <- as.data.frame(c_region[c(3,4),])
c_region <- c_region %>% dplyr::mutate(aux = c(16, 17))
c_region <- c_region %>% rename("VALOR" = "c_region[c(3, 4), ]")
c_region <- c_region[,c("VALOR", "aux")]




# Condición de pobreza del hogar

a_pobre <- function(x) {
  x <- base_men_svy %>%
    filter(pobre_06 == x & e27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(ocup, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_pobre(x = i)
}     



c_pobre <- as.data.frame(VALOR)
c_pobre <- c_pobre %>% dplyr::mutate(aux = c(18, 19))
c_pobre <- c_pobre[,c("VALOR", "aux")]


## País Urbano

# Total

a_mes <- base_men_svy %>%
  srvyr::filter(e27>=14 & (REGION_4 == 1 | REGION_4 == 2)) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(ocup, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))


c_ano_pu <- as.data.frame(a_sem)
c_ano_pu <- c_ano_pu %>% dplyr::mutate(aux = 1)
c_ano_pu <- c_ano_pu %>% dplyr::rename(VALOR = a_sem)

# Quintil de ingresos

a_quintil <- function(x) {
  x <- base_men_svy %>%
    filter(quintilesy == x & e27>=14 & (REGION_4 == 1 | REGION_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(ocup, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:5){
  VALOR[i] <- a_quintil(x = i)
}     


c_quintil_pu <- as.data.frame(VALOR)
c_quintil_pu <- c_quintil_pu %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil_pu <- c_quintil_pu[,c("VALOR", "aux")]


# Sexo

a_sexo <- function(x) {
  x <- base_men_svy %>%
    filter(bc_pe2 == x & e27>=14 & (REGION_4 == 1 | REGION_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(ocup, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_sexo(x = i)
}     



c_sexo_pu <- as.data.frame(VALOR)
c_sexo_pu <- c_sexo_pu %>% dplyr::mutate(aux = c(9, 10))
c_sexo_pu <- c_sexo_pu[,c("VALOR", "aux")]


# Tramo de edad

a_edad <- function(x) {
  x <- base_men_svy %>%
    filter(tramo_edad == x & e27>=14 & (REGION_4 == 1 | REGION_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(ocup, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:5){
  VALOR[i] <- a_edad(x = i)
}     



c_edad_pu <- as.data.frame(VALOR)
c_edad_pu <- c_edad_pu %>% dplyr::mutate(aux = c(11, 12, 13, 14, 15))
c_edad_pu <- c_edad_pu[,c("VALOR", "aux")]



CODIND       <- 312
NOMINDICADOR <- "TASA DE EMPLEO"

VALOR        <- rbind(c_ano, c_afro, c_quintil, c_sexo, c_edad, c_region, c_pobre)
VALOR        <- VALOR  %>% dplyr::mutate(SEXO = case_when(aux == 9  ~ "VARONES", 
                                                          aux == 10 ~ "MUJERES", 
                                                          TRUE     ~ "TODOS"),
                                         ASCENDENCIA  = case_when(aux == 2 ~ "AFRO", 
                                                                  aux == 3 ~ "NO AFRO",
                                                                  TRUE     ~ "TODOS"), 
                                         QUINTIL =      case_when(aux == 4 ~ "QUINTIL 1",
                                                                  aux == 5 ~ "QUINTIL 2",
                                                                  aux == 6 ~ "QUINTIL 3",
                                                                  aux == 7 ~ "QUINTIL 4",
                                                                  aux == 8 ~ "QUINTIL 5",
                                                                  TRUE     ~ "TODOS"),
                                         DEPARTAMENTO = "", 
                                         URBANORURALUY = case_when(aux == 16 ~ "URBANO (MENOS DE 5.000 HABITANTES)",
                                                                   aux == 17 ~ "RURAL DISPERSO",
                                                                   TRUE     ~ "TOTAL PAÍS"),
                                         EDAD =          case_when(aux == 11 ~ "14 a 18",
                                                                   aux == 12 ~ "19 a 24",
                                                                   aux == 13 ~ "25 a 29",
                                                                   aux == 14 ~ "30 a 64",
                                                                   aux == 15 ~ "65 y más",
                                                                   TRUE     ~ ""),
                                         SITUACION_HOGAR = case_when(aux == 18 ~ "No en situación de pobreza",
                                                                     aux == 19  ~"En situación de pobreza",
                                                                     TRUE     ~ ""),
                                         RAMA_DE_ACTIVIDAD = "",
                                         TIPO_DE_OCUPACION = "",
                                         CATEGORÍOCUP = "",
                                         RESTRICCIONES_AL_EMPLEO = "",
                                         SEXO_JEFE       = "")


VALOR_pu        <- rbind(c_ano_pu, c_quintil_pu, c_sexo_pu, c_edad_pu)
VALOR_pu        <- VALOR_pu  %>% dplyr::mutate(SEXO = case_when(aux == 9  ~ "VARONES", 
                                                                aux == 10 ~ "MUJERES", 
                                                                TRUE     ~ "TODOS"),
                                               ASCENDENCIA  =  "TODOS", 
                                               QUINTIL =      case_when(aux == 4 ~ "QUINTIL 1",
                                                                        aux == 5 ~ "QUINTIL 2",
                                                                        aux == 6 ~ "QUINTIL 3",
                                                                        aux == 7 ~ "QUINTIL 4",
                                                                        aux == 8 ~ "QUINTIL 5",
                                                                        TRUE     ~ "TODOS"),
                                               DEPARTAMENTO = "", 
                                               URBANORURALUY = "URBANO (MÁS DE 5.000 HABITANTES)",
                                               EDAD =          case_when(aux == 11 ~ "14 a 18",
                                                                         aux == 12 ~ "19 a 24",
                                                                         aux == 13 ~ "25 a 29",
                                                                         aux == 14 ~ "30 a 64",
                                                                         aux == 15 ~ "65 y más",
                                                                         TRUE     ~ ""),
                                               SITUACION_HOGAR = "",
                                               RAMA_DE_ACTIVIDAD = "",
                                               TIPO_DE_OCUPACION = "",
                                               CATEGORÍOCUP = "",
                                               RESTRICCIONES_AL_EMPLEO = "",
                                               SEXO_JEFE       = "")

VALOR <- rbind (VALOR, VALOR_pu)
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Este indicador se calcula a partir de la encuesta telefónica del primer semestre de 2021 y el formulario telefónico de modalidad panel del segundo semestre de 2021. En el segundo semestre, la condición de pobreza y el quintil de ingresos del hogar corresponden a los ingresos declarados durante la implantación del panel en la encuesta presencial."


t_02 <- cbind(CODIND,	NOMINDICADOR,	PAIS,	FECHA,	FECHA2,	VALOR,	RESPONSABLE,	JERARQUIA,	SECTORPRODUCTIVO,	SECTORINSTITUCIONAL,	ACTIVIDADESECONOMICAS,	DIVISIÓNECONOMICA,	CONCEPTO,	RELACIONFAMILIAR, NOTA_INDICADOR)
t_02 <- subset(t_02, select=c(CODIND,	NOMINDICADOR,	SEXO,	ASCENDENCIA,	QUINTIL, DEPARTAMENTO, URBANORURALUY,	PAIS,	FECHA,	FECHA2,	VALOR,	RESPONSABLE,	JERARQUIA,	SECTORPRODUCTIVO,	SECTORINSTITUCIONAL,	ACTIVIDADESECONOMICAS,	DIVISIÓNECONOMICA,	CONCEPTO,	CATEGORÍOCUP,	RELACIONFAMILIAR,	EDAD, SITUACION_HOGAR, RAMA_DE_ACTIVIDAD, TIPO_DE_OCUPACION, RESTRICCIONES_AL_EMPLEO, SEXO_JEFE, NOTA_INDICADOR))



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
### 313 Tasa de desempleo
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

## Total país


# Total

a_mes <- base_men_svy %>%
  srvyr::filter(pea == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(desocup, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

c_ano <- as.data.frame(a_sem)
c_ano <- c_ano %>% dplyr::mutate(aux = 1)
c_ano <- c_ano %>% dplyr::rename(VALOR = a_sem)

# Ascendencia étnico racial

a_afro <- function(x) {
  x <- base_men_svy %>%
    filter(bd_e29_1 == x & pea == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(desocup, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_afro(x = i)
}     


c_afro <- as.data.frame(VALOR)
c_afro <- c_afro %>% dplyr::mutate(aux = c(2,3))
c_afro <- c_afro[,c("VALOR", "aux")]



# Quintil de ingresos

a_quintil <- function(x) {
  x <- base_men_svy %>%
    filter(quintilesy == x & pea == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(desocup, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:5){
  VALOR[i] <- a_quintil(x = i)
}     
     

c_quintil <- as.data.frame(VALOR)
c_quintil <- c_quintil %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil <- c_quintil[,c("VALOR", "aux")]

# Sexo

a_sexo <- function(x) {
  x <- base_men_svy %>%
    filter(bc_pe2 == x & pea == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(desocup, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_sexo(x = i)
}     


c_sexo <- as.data.frame(VALOR)
c_sexo <- c_sexo %>% dplyr::mutate(aux = c(9, 10))
c_sexo <- c_sexo[,c("VALOR", "aux")]

# Tramo de edad

a_edad <- function(x) {
  x <- base_men_svy %>%
    filter(tramo_edad == x & pea == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(desocup, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:5){
  VALOR[i] <- a_edad(x = i)
}     


c_edad <- as.data.frame(VALOR)
c_edad <- c_edad %>% dplyr::mutate(aux = c(11, 12, 13, 14, 15))
c_edad <- c_edad[,c("VALOR", "aux")]



# Región

a_region <- function(x) {
  x <- base_men_svy %>%
    filter(REGION_4 == x & pea == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(desocup, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 3:4){
  VALOR[i] <- a_region(x = i)
}     

     
c_region <- as.data.frame(VALOR)
c_region <- as.data.frame(c_region[c(3,4),])
c_region <- c_region %>% dplyr::mutate(aux = c(16, 17))
c_region <- c_region %>% rename("VALOR" = "c_region[c(3, 4), ]")
c_region <- c_region[,c("VALOR", "aux")]

# Condición de pobreza del hogar

a_pobre <- function(x) {
  x <- base_men_svy %>%
    filter(pobre_06 == x & pea == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(desocup, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_pobre(x = i)
}     


c_pobre <- as.data.frame(VALOR)
c_pobre <- c_pobre %>% dplyr::mutate(aux = c(18, 19))
c_pobre <- c_pobre[,c("VALOR", "aux")]


## País Urbano

# Total

a_mes <- base_men_svy %>%
  srvyr::filter(pea == 1 & (REGION_4 == 1 | REGION_4 == 2)) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(desocup, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))


c_ano_pu <- as.data.frame(a_sem)
c_ano_pu <- c_ano_pu %>% dplyr::mutate(aux = 1)
c_ano_pu <- c_ano_pu %>% dplyr::rename(VALOR = a_sem)

# Quintil de ingresos

a_quintil <- function(x) {
  x <- base_men_svy %>%
    filter(quintilesy == x & pea == 1 & (REGION_4 == 1 | REGION_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(desocup, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:5){
  VALOR[i] <- a_quintil(x = i)
}     

    

c_quintil_pu <- as.data.frame(VALOR)
c_quintil_pu <- c_quintil_pu %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil_pu <- c_quintil_pu[,c("VALOR", "aux")]


# Sexo

a_sexo <- function(x) {
  x <- base_men_svy %>%
    filter(bc_pe2 == x & pea == 1 & (REGION_4 == 1 | REGION_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(desocup, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_sexo(x = i)
}     


c_sexo_pu <- as.data.frame(VALOR)
c_sexo_pu <- c_sexo_pu %>% dplyr::mutate(aux = c(9, 10))
c_sexo_pu <- c_sexo_pu[,c("VALOR", "aux")]


# Tramo de edad

a_edad <- function(x) {
  x <- base_men_svy %>%
    filter(tramo_edad == x & pea == 1 & (REGION_4 == 1 | REGION_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(desocup, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:5){
  VALOR[i] <- a_edad(x = i)
}     



c_edad_pu <- as.data.frame(VALOR)
c_edad_pu <- c_edad_pu %>% dplyr::mutate(aux = c(11, 12, 13, 14, 15))
c_edad_pu <- c_edad_pu[,c("VALOR", "aux")]



CODIND       <- 313
NOMINDICADOR <- "TASA DE DESEMPLEO"

VALOR        <- rbind(c_ano, c_afro, c_quintil, c_sexo, c_edad, c_region, c_pobre)
VALOR        <- VALOR  %>% dplyr::mutate(SEXO = case_when(aux == 9  ~ "VARONES", 
                                                          aux == 10 ~ "MUJERES", 
                                                          TRUE     ~ "TODOS"),
                                         ASCENDENCIA  = case_when(aux == 2 ~ "AFRO", 
                                                                  aux == 3 ~ "NO AFRO",
                                                                  TRUE     ~ "TODOS"), 
                                         QUINTIL =      case_when(aux == 4 ~ "QUINTIL 1",
                                                                  aux == 5 ~ "QUINTIL 2",
                                                                  aux == 6 ~ "QUINTIL 3",
                                                                  aux == 7 ~ "QUINTIL 4",
                                                                  aux == 8 ~ "QUINTIL 5",
                                                                  TRUE     ~ "TODOS"),
                                         DEPARTAMENTO = "", 
                                         URBANORURALUY = case_when(aux == 16 ~ "URBANO (MENOS DE 5.000 HABITANTES)",
                                                                   aux == 17 ~ "RURAL DISPERSO",
                                                                   TRUE     ~ "TOTAL PAÍS"),
                                         EDAD =          case_when(aux == 11 ~ "14 a 18",
                                                                   aux == 12 ~ "19 a 24",
                                                                   aux == 13 ~ "25 a 29",
                                                                   aux == 14 ~ "30 a 64",
                                                                   aux == 15 ~ "65 y más",
                                                                   TRUE     ~ ""),
                                         SITUACION_HOGAR = case_when(aux == 18 ~ "No en situación de pobreza",
                                                                     aux == 19  ~"En situación de pobreza",
                                                                     TRUE     ~ ""),
                                         RAMA_DE_ACTIVIDAD = "",
                                         TIPO_DE_OCUPACION = "",
                                         CATEGORÍOCUP = "",
                                         RESTRICCIONES_AL_EMPLEO = "",
                                         SEXO_JEFE       = "")


VALOR_pu        <- rbind(c_ano_pu, c_quintil_pu, c_sexo_pu, c_edad_pu)
VALOR_pu        <- VALOR_pu  %>% dplyr::mutate(SEXO = case_when(aux == 9  ~ "VARONES", 
                                                                aux == 10 ~ "MUJERES", 
                                                                TRUE     ~ "TODOS"),
                                               ASCENDENCIA  =  "TODOS", 
                                               QUINTIL =      case_when(aux == 4 ~ "QUINTIL 1",
                                                                        aux == 5 ~ "QUINTIL 2",
                                                                        aux == 6 ~ "QUINTIL 3",
                                                                        aux == 7 ~ "QUINTIL 4",
                                                                        aux == 8 ~ "QUINTIL 5",
                                                                        TRUE     ~ "TODOS"),
                                               DEPARTAMENTO = "", 
                                               URBANORURALUY = "URBANO (MÁS DE 5.000 HABITANTES)",
                                               EDAD =          case_when(aux == 11 ~ "14 a 18",
                                                                         aux == 12 ~ "19 a 24",
                                                                         aux == 13 ~ "25 a 29",
                                                                         aux == 14 ~ "30 a 64",
                                                                         aux == 15 ~ "65 y más",
                                                                         TRUE     ~ ""),
                                               SITUACION_HOGAR = "",
                                               RAMA_DE_ACTIVIDAD = "",
                                               TIPO_DE_OCUPACION = "",
                                               CATEGORÍOCUP = "",
                                               RESTRICCIONES_AL_EMPLEO = "",
                                               SEXO_JEFE       = "")

VALOR <- rbind (VALOR, VALOR_pu)
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Este indicador se calcula a partir de la encuesta telefónica del primer semestre de 2021 y el formulario telefónico de modalidad panel del segundo semestre de 2021. En el segundo semestre, la condición de pobreza y el quintil de ingresos del hogar corresponden a los ingresos declarados durante la implantación del panel en la encuesta presencial."


t_03 <- cbind(CODIND,	NOMINDICADOR,	PAIS,	FECHA,	FECHA2,	VALOR,	RESPONSABLE,	JERARQUIA,	SECTORPRODUCTIVO,	SECTORINSTITUCIONAL,	ACTIVIDADESECONOMICAS,	DIVISIÓNECONOMICA,	CONCEPTO,	RELACIONFAMILIAR, NOTA_INDICADOR)
t_03 <- subset(t_03, select=c(CODIND,	NOMINDICADOR,	SEXO,	ASCENDENCIA,	QUINTIL, DEPARTAMENTO, URBANORURALUY,	PAIS,	FECHA,	FECHA2,	VALOR,	RESPONSABLE,	JERARQUIA,	SECTORPRODUCTIVO,	SECTORINSTITUCIONAL,	ACTIVIDADESECONOMICAS,	DIVISIÓNECONOMICA,	CONCEPTO,	CATEGORÍOCUP,	RELACIONFAMILIAR,	EDAD, SITUACION_HOGAR, RAMA_DE_ACTIVIDAD, TIPO_DE_OCUPACION, RESTRICCIONES_AL_EMPLEO, SEXO_JEFE, NOTA_INDICADOR))



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# 314 - Tasa de subempleo
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


## Total país


# Total

a_mes <- base_men_svy %>%
  srvyr::filter(ocup == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(bc_subocupado, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))


c_ano <- as.data.frame(a_sem)
c_ano <- c_ano %>% dplyr::mutate(aux = 1)
c_ano <- c_ano %>% dplyr::rename(VALOR = a_sem)

# Ascendencia étnico racial

a_afro <- function(x) {
  x <- base_men_svy %>%
    filter(bd_e29_1 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_subocupado, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_afro(x = i)
}     


c_afro <- as.data.frame(VALOR)
c_afro <- c_afro %>% dplyr::mutate(aux = c(2,3))
c_afro <- c_afro[,c("VALOR", "aux")]



# Quintil de ingresos

a_quintil <- function(x) {
  x <- base_men_svy %>%
    filter(quintilesy == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_subocupado, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:5){
  VALOR[i] <- a_quintil(x = i)
}     



c_quintil <- as.data.frame(VALOR)
c_quintil <- c_quintil %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil <- c_quintil[,c("VALOR", "aux")]

# Sexo

a_sexo <- function(x) {
  x <- base_men_svy %>%
    filter(bc_pe2 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_subocupado, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_sexo(x = i)
}     

  

c_sexo <- as.data.frame(VALOR)
c_sexo <- c_sexo %>% dplyr::mutate(aux = c(9, 10))
c_sexo <- c_sexo[,c("VALOR", "aux")]

# Tramo de edad

a_edad <- function(x) {
  x <- base_men_svy %>%
    filter(tramo_edad == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_subocupado, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:5){
  VALOR[i] <- a_edad(x = i)
}     


c_edad <- as.data.frame(VALOR)
c_edad <- c_edad %>% dplyr::mutate(aux = c(11, 12, 13, 14, 15))
c_edad <- c_edad[,c("VALOR", "aux")]



# Región

a_region <- function(x) {
  x <- base_men_svy %>%
    filter(REGION_4 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_subocupado, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 3:4){
  VALOR[i] <- a_region(x = i)
}     


c_region <- as.data.frame(VALOR)
c_region <- as.data.frame(c_region[c(3,4),])
c_region <- c_region %>% dplyr::mutate(aux = c(16, 17))
c_region <- c_region %>% rename("VALOR" = "c_region[c(3, 4), ]")
c_region <- c_region[,c("VALOR", "aux")]

# Condición de pobreza del hogar

a_pobre <- function(x) {
  x <- base_men_svy %>%
    filter(pobre_06 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_subocupado, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_pobre(x = i)
}     



c_pobre <- as.data.frame(VALOR)
c_pobre <- c_pobre %>% dplyr::mutate(aux = c(18, 19))
c_pobre <- c_pobre[,c("VALOR", "aux")]

## País Urbano

# Total

a_mes <- base_men_svy %>%
  srvyr::filter(ocup == 1 & (REGION_4 == 1 | REGION_4 == 2)) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(bc_subocupado, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))


c_ano_pu <- as.data.frame(a_sem)
c_ano_pu <- c_ano_pu %>% dplyr::mutate(aux = 1)
c_ano_pu <- c_ano_pu %>% dplyr::rename(VALOR = a_sem)

# Quintil de ingresos

a_quintil <- function(x) {
  x <- base_men_svy %>%
    filter(quintilesy == x & ocup == 1 & (REGION_4 == 1 | REGION_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_subocupado, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:5){
  VALOR[i] <- a_quintil(x = i)
}     

     

c_quintil_pu <- as.data.frame(VALOR)
c_quintil_pu <- c_quintil_pu %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil_pu <- c_quintil_pu[,c("VALOR", "aux")]


# Sexo

a_sexo <- function(x) {
  x <- base_men_svy %>%
    filter(bc_pe2 == x & ocup == 1 & (REGION_4 == 1 | REGION_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_subocupado, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_sexo(x = i)
}     


c_sexo_pu <- as.data.frame(VALOR)
c_sexo_pu <- c_sexo_pu %>% dplyr::mutate(aux = c(9, 10))
c_sexo_pu <- c_sexo_pu[,c("VALOR", "aux")]


# Tramo de edad

a_edad <- function(x) {
  x <- base_men_svy %>%
    filter(tramo_edad == x & ocup == 1 & (REGION_4 == 1 | REGION_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_subocupado, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:5){
  VALOR[i] <- a_edad(x = i)
}     



c_edad_pu <- as.data.frame(VALOR)
c_edad_pu <- c_edad_pu %>% dplyr::mutate(aux = c(11, 12, 13, 14, 15))
c_edad_pu <- c_edad_pu[,c("VALOR", "aux")]



CODIND       <- 314
NOMINDICADOR <- "TASA DE SUBEMPLEO"

VALOR        <- rbind(c_ano, c_afro, c_quintil, c_sexo, c_edad, c_region, c_pobre)
VALOR        <- VALOR  %>% dplyr::mutate(SEXO = case_when(aux == 9  ~ "VARONES", 
                                                          aux == 10 ~ "MUJERES", 
                                                          TRUE     ~ "TODOS"),
                                         ASCENDENCIA  = case_when(aux == 2 ~ "AFRO", 
                                                                  aux == 3 ~ "NO AFRO",
                                                                  TRUE     ~ "TODOS"), 
                                         QUINTIL =      case_when(aux == 4 ~ "QUINTIL 1",
                                                                  aux == 5 ~ "QUINTIL 2",
                                                                  aux == 6 ~ "QUINTIL 3",
                                                                  aux == 7 ~ "QUINTIL 4",
                                                                  aux == 8 ~ "QUINTIL 5",
                                                                  TRUE     ~ "TODOS"),
                                         DEPARTAMENTO = "", 
                                         URBANORURALUY = case_when(aux == 16 ~ "URBANO (MENOS DE 5.000 HABITANTES)",
                                                                   aux == 17 ~ "RURAL DISPERSO",
                                                                   TRUE     ~ "TOTAL PAÍS"),
                                         EDAD =          case_when(aux == 11 ~ "14 a 18",
                                                                   aux == 12 ~ "19 a 24",
                                                                   aux == 13 ~ "25 a 29",
                                                                   aux == 14 ~ "30 a 64",
                                                                   aux == 15 ~ "65 y más",
                                                                   TRUE     ~ ""),
                                         SITUACION_HOGAR = case_when(aux == 18 ~ "No en situación de pobreza",
                                                                     aux == 19  ~"En situación de pobreza",
                                                                     TRUE     ~ ""),
                                         RAMA_DE_ACTIVIDAD = "",
                                         TIPO_DE_OCUPACION = "",
                                         CATEGORÍOCUP = "",
                                         RESTRICCIONES_AL_EMPLEO = "",
                                         SEXO_JEFE       = "")


VALOR_pu        <- rbind(c_ano_pu, c_quintil_pu, c_sexo_pu, c_edad_pu)
VALOR_pu        <- VALOR_pu  %>% dplyr::mutate(SEXO = case_when(aux == 9  ~ "VARONES", 
                                                                aux == 10 ~ "MUJERES", 
                                                                TRUE     ~ "TODOS"),
                                               ASCENDENCIA  =  "TODOS", 
                                               QUINTIL =      case_when(aux == 4 ~ "QUINTIL 1",
                                                                        aux == 5 ~ "QUINTIL 2",
                                                                        aux == 6 ~ "QUINTIL 3",
                                                                        aux == 7 ~ "QUINTIL 4",
                                                                        aux == 8 ~ "QUINTIL 5",
                                                                        TRUE     ~ "TODOS"),
                                               DEPARTAMENTO = "", 
                                               URBANORURALUY = "URBANO (MÁS DE 5.000 HABITANTES)",
                                               EDAD =          case_when(aux == 11 ~ "14 a 18",
                                                                         aux == 12 ~ "19 a 24",
                                                                         aux == 13 ~ "25 a 29",
                                                                         aux == 14 ~ "30 a 64",
                                                                         aux == 15 ~ "65 y más",
                                                                         TRUE     ~ ""),
                                               SITUACION_HOGAR = "",
                                               RAMA_DE_ACTIVIDAD = "",
                                               TIPO_DE_OCUPACION = "",
                                               CATEGORÍOCUP = "" ,
                                               RESTRICCIONES_AL_EMPLEO = "",
                                               SEXO_JEFE       = "")

VALOR <- rbind (VALOR, VALOR_pu)
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Este indicador se calcula a partir de la encuesta telefónica del primer semestre de 2021 y el formulario telefónico de modalidad panel del segundo semestre de 2021. En el segundo semestre, la condición de pobreza y el quintil de ingresos del hogar corresponden a los ingresos declarados durante la implantación del panel en la encuesta presencial."


t_04 <- cbind(CODIND,	NOMINDICADOR,	PAIS,	FECHA,	FECHA2,	VALOR,	RESPONSABLE,	JERARQUIA,	SECTORPRODUCTIVO,	SECTORINSTITUCIONAL,	ACTIVIDADESECONOMICAS,	DIVISIÓNECONOMICA,	CONCEPTO,	RELACIONFAMILIAR, NOTA_INDICADOR)
t_04 <- subset(t_04, select=c(CODIND,	NOMINDICADOR,	SEXO,	ASCENDENCIA,	QUINTIL, DEPARTAMENTO, URBANORURALUY,	PAIS,	FECHA,	FECHA2,	VALOR,	RESPONSABLE,	JERARQUIA,	SECTORPRODUCTIVO,	SECTORINSTITUCIONAL,	ACTIVIDADESECONOMICAS,	DIVISIÓNECONOMICA,	CONCEPTO,	CATEGORÍOCUP,	RELACIONFAMILIAR,	EDAD, SITUACION_HOGAR, RAMA_DE_ACTIVIDAD, TIPO_DE_OCUPACION, RESTRICCIONES_AL_EMPLEO, SEXO_JEFE, NOTA_INDICADOR))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# 315 - Porcentaje de ocupados no registrados en seguridad social en ocupación principal
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


## Total país


# Total

a_mes <- base_men_svy %>%
  srvyr::filter(ocup == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(bc_register, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))


c_ano <- as.data.frame(a_sem)
c_ano <- c_ano %>% dplyr::mutate(aux = 1)
c_ano <- c_ano %>% dplyr::rename(VALOR = a_sem)

# Ascendencia étnico racial

a_afro <- function(x) {
  x <- base_men_svy %>%
    filter(bd_e29_1 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_afro(x = i)
}     


c_afro <- as.data.frame(VALOR)
c_afro <- c_afro %>% dplyr::mutate(aux = c(2,3))
c_afro <- c_afro[,c("VALOR", "aux")]



# Quintil de ingresos

a_quintil <- function(x) {
  x <- base_men_svy %>%
    filter(quintilesy == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:5){
  VALOR[i] <- a_quintil(x = i)
}     



c_quintil <- as.data.frame(VALOR)
c_quintil <- c_quintil %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil <- c_quintil[,c("VALOR", "aux")]

# Sexo

a_sexo <- function(x) {
  x <- base_men_svy %>%
    filter(bc_pe2 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_sexo(x = i)
}     


c_sexo <- as.data.frame(VALOR)
c_sexo <- c_sexo %>% dplyr::mutate(aux = c(9, 10))
c_sexo <- c_sexo[,c("VALOR", "aux")]

# Tramo de edad

a_edad <- function(x) {
  x <- base_men_svy %>%
    filter(tramo_edad == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:5){
  VALOR[i] <- a_edad(x = i)
}     


c_edad <- as.data.frame(VALOR)
c_edad <- c_edad %>% dplyr::mutate(aux = c(11, 12, 13, 14, 15))
c_edad <- c_edad[,c("VALOR", "aux")]



# Región

a_region <- function(x) {
  x <- base_men_svy %>%
    filter(REGION_4 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 3:4){
  VALOR[i] <- a_region(x = i)
}     

c_region <- as.data.frame(VALOR)
c_region <- as.data.frame(c_region[c(3,4),])
c_region <- c_region %>% dplyr::mutate(aux = c(16, 17))
c_region <- c_region %>% rename("VALOR" = "c_region[c(3, 4), ]")
c_region <- c_region[,c("VALOR", "aux")]


# Condición de pobreza del hogar

a_pobre <- function(x) {
  x <- base_men_svy %>%
    filter(pobre_06 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_pobre(x = i)
}     


c_pobre <- as.data.frame(VALOR)
c_pobre <- c_pobre %>% dplyr::mutate(aux = c(18, 19))
c_pobre <- c_pobre[,c("VALOR", "aux")]

## País Urbano

# Total

a_mes <- base_men_svy %>%
  srvyr::filter(ocup == 1 & (REGION_4 == 1 | REGION_4 == 2)) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(bc_register, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))


c_ano_pu <- as.data.frame(a_sem)
c_ano_pu <- c_ano_pu %>% dplyr::mutate(aux = 1)
c_ano_pu <- c_ano_pu %>% dplyr::rename(VALOR = a_sem)

# Quintil de ingresos

a_quintil <- function(x) {
  x <- base_men_svy %>%
    filter(quintilesy == x & ocup == 1 & (REGION_4 == 1 | REGION_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:5){
  VALOR[i] <- a_quintil(x = i)
}     
    

c_quintil_pu <- as.data.frame(VALOR)
c_quintil_pu <- c_quintil_pu %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil_pu <- c_quintil_pu[,c("VALOR", "aux")]


# Sexo

a_sexo <- function(x) {
  x <- base_men_svy %>%
    filter(bc_pe2 == x & ocup == 1 & (REGION_4 == 1 | REGION_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_sexo(x = i)
}     


c_sexo_pu <- as.data.frame(VALOR)
c_sexo_pu <- c_sexo_pu %>% dplyr::mutate(aux = c(9, 10))
c_sexo_pu <- c_sexo_pu[,c("VALOR", "aux")]


# Tramo de edad

a_edad <- function(x) {
  x <- base_men_svy %>%
    filter(tramo_edad == x & ocup == 1 & (REGION_4 == 1 | REGION_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:5){
  VALOR[i] <- a_edad(x = i)
}     


c_edad_pu <- as.data.frame(VALOR)
c_edad_pu <- c_edad_pu %>% dplyr::mutate(aux = c(11, 12, 13, 14, 15))
c_edad_pu <- c_edad_pu[,c("VALOR", "aux")]



CODIND       <- 315
NOMINDICADOR <- "PORCENTAJE DE OCUPADOS NO REGISTRADOS EN SEGURIDAD SOCIAL EN OCUPACIÓN PRINCIPAL"

VALOR        <- rbind(c_ano, c_afro, c_quintil, c_sexo, c_edad, c_region, c_pobre)
VALOR        <- VALOR  %>% dplyr::mutate(SEXO = case_when(aux == 9  ~ "VARONES", 
                                                          aux == 10 ~ "MUJERES", 
                                                          TRUE     ~ "TODOS"),
                                         ASCENDENCIA  = case_when(aux == 2 ~ "AFRO", 
                                                                  aux == 3 ~ "NO AFRO",
                                                                  TRUE     ~ "TODOS"), 
                                         QUINTIL =      case_when(aux == 4 ~ "QUINTIL 1",
                                                                  aux == 5 ~ "QUINTIL 2",
                                                                  aux == 6 ~ "QUINTIL 3",
                                                                  aux == 7 ~ "QUINTIL 4",
                                                                  aux == 8 ~ "QUINTIL 5",
                                                                  TRUE     ~ "TODOS"),
                                         DEPARTAMENTO = "", 
                                         URBANORURALUY = case_when(aux == 16 ~ "URBANO (MENOS DE 5.000 HABITANTES)",
                                                                   aux == 17 ~ "RURAL DISPERSO",
                                                                   TRUE     ~ "TOTAL PAÍS"),
                                         EDAD =          case_when(aux == 11 ~ "14 a 18",
                                                                   aux == 12 ~ "19 a 24",
                                                                   aux == 13 ~ "25 a 29",
                                                                   aux == 14 ~ "30 a 64",
                                                                   aux == 15 ~ "65 y más",
                                                                   TRUE     ~ ""),
                                         SITUACION_HOGAR = case_when(aux == 18 ~ "No en situación de pobreza",
                                                                     aux == 19  ~"En situación de pobreza",
                                                                     TRUE     ~ ""),
                                         RAMA_DE_ACTIVIDAD = "",
                                         TIPO_DE_OCUPACION = "",
                                         CATEGORÍOCUP = "",
                                         RESTRICCIONES_AL_EMPLEO = "",
                                         SEXO_JEFE       = "")


VALOR_pu        <- rbind(c_ano_pu, c_quintil_pu, c_sexo_pu, c_edad_pu)
VALOR_pu        <- VALOR_pu  %>% dplyr::mutate(SEXO = case_when(aux == 9  ~ "VARONES", 
                                                                aux == 10 ~ "MUJERES", 
                                                                TRUE     ~ "TODOS"),
                                               ASCENDENCIA  =  "TODOS", 
                                               QUINTIL =      case_when(aux == 4 ~ "QUINTIL 1",
                                                                        aux == 5 ~ "QUINTIL 2",
                                                                        aux == 6 ~ "QUINTIL 3",
                                                                        aux == 7 ~ "QUINTIL 4",
                                                                        aux == 8 ~ "QUINTIL 5",
                                                                        TRUE     ~ "TODOS"),
                                               DEPARTAMENTO = "", 
                                               URBANORURALUY = "URBANO (MÁS DE 5.000 HABITANTES)",
                                               EDAD =          case_when(aux == 11 ~ "14 a 18",
                                                                         aux == 12 ~ "19 a 24",
                                                                         aux == 13 ~ "25 a 29",
                                                                         aux == 14 ~ "30 a 64",
                                                                         aux == 15 ~ "65 y más",
                                                                         TRUE     ~ ""),
                                               SITUACION_HOGAR = "",
                                               RAMA_DE_ACTIVIDAD = "",
                                               TIPO_DE_OCUPACION = "",
                                               CATEGORÍOCUP = "",
                                               RESTRICCIONES_AL_EMPLEO = "",
                                               SEXO_JEFE       = "")

VALOR <- rbind (VALOR, VALOR_pu)
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Este indicador se calcula a partir de la encuesta telefónica del primer semestre de 2021 y el formulario telefónico de modalidad panel del segundo semestre de 2021. En el segundo semestre, la condición de pobreza y el quintil de ingresos del hogar corresponden a los ingresos declarados durante la implantación del panel en la encuesta presencial."


t_05 <- cbind(CODIND,	NOMINDICADOR,	PAIS,	FECHA,	FECHA2,	VALOR,	RESPONSABLE,	JERARQUIA,	SECTORPRODUCTIVO,	SECTORINSTITUCIONAL,	ACTIVIDADESECONOMICAS,	DIVISIÓNECONOMICA,	CONCEPTO,	RELACIONFAMILIAR, NOTA_INDICADOR)
t_05 <- subset(t_05, select=c(CODIND,	NOMINDICADOR,	SEXO,	ASCENDENCIA,	QUINTIL, DEPARTAMENTO, URBANORURALUY,	PAIS,	FECHA,	FECHA2,	VALOR,	RESPONSABLE,	JERARQUIA,	SECTORPRODUCTIVO,	SECTORINSTITUCIONAL,	ACTIVIDADESECONOMICAS,	DIVISIÓNECONOMICA,	CONCEPTO,	CATEGORÍOCUP,	RELACIONFAMILIAR,	EDAD, SITUACION_HOGAR, RAMA_DE_ACTIVIDAD, TIPO_DE_OCUPACION, RESTRICCIONES_AL_EMPLEO, SEXO_JEFE, NOTA_INDICADOR))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# 316 - Promedio de horas de trabajo remuneradas en ocupación principal y secundario
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


## Total país


# Total

a_mes <- base_men_svy %>%
  srvyr::filter(ocup == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(bc_horas, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))


c_ano <- as.data.frame(a_sem)
c_ano <- c_ano %>% dplyr::mutate(aux = 1)
c_ano <- c_ano %>% dplyr::rename(VALOR = a_sem)

# Ascendencia étnico racial

a_afro <- function(x) {
  x <- base_men_svy %>%
    filter(bd_e29_1 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_horas, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_afro(x = i)
}     


c_afro <- as.data.frame(VALOR)
c_afro <- c_afro %>% dplyr::mutate(aux = c(2,3))
c_afro <- c_afro[,c("VALOR", "aux")]



# Quintil de ingresos

a_quintil <- function(x) {
  x <- base_men_svy %>%
    filter(quintilesy == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_horas, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:5){
  VALOR[i] <- a_quintil(x = i)
}     

   

c_quintil <- as.data.frame(VALOR)
c_quintil <- c_quintil %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil <- c_quintil[,c("VALOR", "aux")]

# Sexo

a_sexo <- function(x) {
  x <- base_men_svy %>%
    filter(bc_pe2 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_horas, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_sexo(x = i)
}     

c_sexo <- as.data.frame(VALOR)
c_sexo <- c_sexo %>% dplyr::mutate(aux = c(9, 10))
c_sexo <- c_sexo[,c("VALOR", "aux")]

# Tramo de edad

a_edad <- function(x) {
  x <- base_men_svy %>%
    filter(tramo_edad == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_horas, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:5){
  VALOR[i] <- a_edad(x = i)
}     


c_edad <- as.data.frame(VALOR)
c_edad <- c_edad %>% dplyr::mutate(aux = c(11, 12, 13, 14, 15))
c_edad <- c_edad[,c("VALOR", "aux")]



# Región

a_region <- function(x) {
  x <- base_men_svy %>%
    filter(REGION_4 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_horas, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 3:4){
  VALOR[i] <- a_region(x = i)
}     

c_region <- as.data.frame(VALOR)
c_region <- as.data.frame(c_region[c(3,4),])
c_region <- c_region %>% dplyr::mutate(aux = c(16, 17))
c_region <- c_region %>% rename("VALOR" = "c_region[c(3, 4), ]")
c_region <- c_region[,c("VALOR", "aux")]


# Condición de pobreza del hogar

a_pobre <- function(x) {
  x <- base_men_svy %>%
    filter(pobre_06 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_horas, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_pobre(x = i)
}     


c_pobre <- as.data.frame(VALOR)
c_pobre <- c_pobre %>% dplyr::mutate(aux = c(18, 19))
c_pobre <- c_pobre[,c("VALOR", "aux")]

## País Urbano

# Total

a_mes <- base_men_svy %>%
  srvyr::filter(ocup == 1 & (REGION_4 == 1 | REGION_4 == 2)) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(bc_horas, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))


c_ano_pu <- as.data.frame(a_sem)
c_ano_pu <- c_ano_pu %>% dplyr::mutate(aux = 1)
c_ano_pu <- c_ano_pu %>% dplyr::rename(VALOR = a_sem)

# Quintil de ingresos

a_quintil <- function(x) {
  x <- base_men_svy %>%
    filter(quintilesy == x & ocup == 1 & (REGION_4 == 1 | REGION_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_horas, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:5){
  VALOR[i] <- a_quintil(x = i)
}     


c_quintil_pu <- as.data.frame(VALOR)
c_quintil_pu <- c_quintil_pu %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil_pu <- c_quintil_pu[,c("VALOR", "aux")]


# Sexo

a_sexo <- function(x) {
  x <- base_men_svy %>%
    filter(bc_pe2 == x & ocup == 1 & (REGION_4 == 1 | REGION_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_horas, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_sexo(x = i)
}     

c_sexo_pu <- as.data.frame(VALOR)
c_sexo_pu <- c_sexo_pu %>% dplyr::mutate(aux = c(9, 10))
c_sexo_pu <- c_sexo_pu[,c("VALOR", "aux")]


# Tramo de edad

a_edad <- function(x) {
  x <- base_men_svy %>%
    filter(tramo_edad == x & ocup == 1 & (REGION_4 == 1 | REGION_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_horas, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:5){
  VALOR[i] <- a_edad(x = i)
}     


c_edad_pu <- as.data.frame(VALOR)
c_edad_pu <- c_edad_pu %>% dplyr::mutate(aux = c(11, 12, 13, 14, 15))
c_edad_pu <- c_edad_pu[,c("VALOR", "aux")]



CODIND       <- 316
NOMINDICADOR <- "PROMEDIO DE HORAS SEMANALES DE TRABAJO REMUNERADAS"

VALOR        <- rbind(c_ano, c_afro, c_quintil, c_sexo, c_edad, c_region, c_pobre)
VALOR        <- VALOR  %>% dplyr::mutate(SEXO = case_when(aux == 9  ~ "VARONES", 
                                                          aux == 10 ~ "MUJERES", 
                                                          TRUE     ~ "TODOS"),
                                         ASCENDENCIA  = case_when(aux == 2 ~ "AFRO", 
                                                                  aux == 3 ~ "NO AFRO",
                                                                  TRUE     ~ "TODOS"), 
                                         QUINTIL =      case_when(aux == 4 ~ "QUINTIL 1",
                                                                  aux == 5 ~ "QUINTIL 2",
                                                                  aux == 6 ~ "QUINTIL 3",
                                                                  aux == 7 ~ "QUINTIL 4",
                                                                  aux == 8 ~ "QUINTIL 5",
                                                                  TRUE     ~ "TODOS"),
                                         DEPARTAMENTO = "", 
                                         URBANORURALUY = case_when(aux == 16 ~ "URBANO (MENOS DE 5.000 HABITANTES)",
                                                                   aux == 17 ~ "RURAL DISPERSO",
                                                                   TRUE     ~ "TOTAL PAÍS"),
                                         EDAD =          case_when(aux == 11 ~ "14 a 18",
                                                                   aux == 12 ~ "19 a 24",
                                                                   aux == 13 ~ "25 a 29",
                                                                   aux == 14 ~ "30 a 64",
                                                                   aux == 15 ~ "65 y más",
                                                                   TRUE     ~ ""),
                                         SITUACION_HOGAR = case_when(aux == 18 ~ "No en situación de pobreza",
                                                                     aux == 19  ~"En situación de pobreza",
                                                                     TRUE     ~ ""),
                                         RAMA_DE_ACTIVIDAD = "",
                                         TIPO_DE_OCUPACION = "",
                                         CATEGORÍOCUP = "",
                                         RESTRICCIONES_AL_EMPLEO = "",
                                         SEXO_JEFE       = "")


VALOR_pu        <- rbind(c_ano_pu, c_quintil_pu, c_sexo_pu, c_edad_pu)
VALOR_pu        <- VALOR_pu  %>% dplyr::mutate(SEXO = case_when(aux == 9  ~ "VARONES", 
                                                                aux == 10 ~ "MUJERES", 
                                                                TRUE     ~ "TODOS"),
                                               ASCENDENCIA  =  "TODOS", 
                                               QUINTIL =      case_when(aux == 4 ~ "QUINTIL 1",
                                                                        aux == 5 ~ "QUINTIL 2",
                                                                        aux == 6 ~ "QUINTIL 3",
                                                                        aux == 7 ~ "QUINTIL 4",
                                                                        aux == 8 ~ "QUINTIL 5",
                                                                        TRUE     ~ "TODOS"),
                                               DEPARTAMENTO = "", 
                                               URBANORURALUY = "URBANO (MÁS DE 5.000 HABITANTES)",
                                               EDAD =          case_when(aux == 11 ~ "14 a 18",
                                                                         aux == 12 ~ "19 a 24",
                                                                         aux == 13 ~ "25 a 29",
                                                                         aux == 14 ~ "30 a 64",
                                                                         aux == 15 ~ "65 y más",
                                                                         TRUE     ~ ""),
                                               SITUACION_HOGAR = "",
                                               RAMA_DE_ACTIVIDAD = "",
                                               TIPO_DE_OCUPACION = "",
                                               CATEGORÍOCUP = "",
                                               RESTRICCIONES_AL_EMPLEO = "",
                                               SEXO_JEFE       = "")

VALOR <- rbind (VALOR, VALOR_pu)
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Este indicador se calcula a partir de la encuesta telefónica del primer semestre de 2021 y el formulario telefónico de modalidad panel del segundo semestre de 2021. En el segundo semestre, la condición de pobreza y el quintil de ingresos del hogar corresponden a los ingresos declarados durante la implantación del panel en la encuesta presencial."


t_06 <- cbind(CODIND,	NOMINDICADOR,	PAIS,	FECHA,	FECHA2,	VALOR,	RESPONSABLE,	JERARQUIA,	SECTORPRODUCTIVO,	SECTORINSTITUCIONAL,	ACTIVIDADESECONOMICAS,	DIVISIÓNECONOMICA,	CONCEPTO,	RELACIONFAMILIAR, NOTA_INDICADOR)
t_06 <- subset(t_06, select=c(CODIND,	NOMINDICADOR,	SEXO,	ASCENDENCIA,	QUINTIL, DEPARTAMENTO, URBANORURALUY,	PAIS,	FECHA,	FECHA2,	VALOR,	RESPONSABLE,	JERARQUIA,	SECTORPRODUCTIVO,	SECTORINSTITUCIONAL,	ACTIVIDADESECONOMICAS,	DIVISIÓNECONOMICA,	CONCEPTO,	CATEGORÍOCUP,	RELACIONFAMILIAR,	EDAD, SITUACION_HOGAR, RAMA_DE_ACTIVIDAD, TIPO_DE_OCUPACION, RESTRICCIONES_AL_EMPLEO, SEXO_JEFE, NOTA_INDICADOR))



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# 321 - Ingreso por hora promedio en ocupación principal
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


## Total país


# Total

a_mes <- base_svy %>%
  srvyr::filter(ocup == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

c_ano <- as.data.frame(a_sem)
c_ano <- c_ano %>% dplyr::mutate(aux = 1)
c_ano <- c_ano %>% dplyr::rename(VALOR = a_sem)

# Ascendencia étnico racial

a_afro <- function(x) {
  x <- base_svy %>%
    filter(bd_e29_1 == x & ocup == 1 & f85>0) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_afro(x = i)
}     


c_afro <- as.data.frame(VALOR)
c_afro <- c_afro %>% dplyr::mutate(aux = c(2,3))
c_afro <- c_afro[,c("VALOR", "aux")]



# Quintil de ingresos

a_quintil <- function(x) {
  x <- base_svy %>%
    filter(quintilesy == x & ocup == 1 & f85>0) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:5){
  VALOR[i] <- a_quintil(x = i)
}     

c_quintil <- as.data.frame(VALOR)
c_quintil <- c_quintil %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil <- c_quintil[,c("VALOR", "aux")]

# Sexo

a_sexo <- function(x) {
  x <- base_svy %>%
    filter(bc_pe2 == x & ocup == 1 & f85>0) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_sexo(x = i)
}     

c_sexo <- as.data.frame(VALOR)
c_sexo <- c_sexo %>% dplyr::mutate(aux = c(9, 10))
c_sexo <- c_sexo[,c("VALOR", "aux")]

# Tramo de edad

a_edad <- function(x) {
  x <- base_svy %>%
    filter(tramo_edad == x & ocup == 1 & f85>0) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:5){
  VALOR[i] <- a_edad(x = i)
}     


c_edad <- as.data.frame(VALOR)
c_edad <- c_edad %>% dplyr::mutate(aux = c(11, 12, 13, 14, 15))
c_edad <- c_edad[,c("VALOR", "aux")]



# Región

a_region <- function(x) {
  x <- base_svy %>%
    filter(REGION_4 == x & ocup == 1 & f85>0) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 3:4){
  VALOR[i] <- a_region(x = i)
}     

c_region <- as.data.frame(VALOR)
c_region <- as.data.frame(c_region[c(3,4),])
c_region <- c_region %>% dplyr::mutate(aux = c(16, 17))
c_region <- c_region %>% rename("VALOR" = "c_region[c(3, 4), ]")
c_region <- c_region[,c("VALOR", "aux")]


# Condición de pobreza del hogar

a_pobre <- function(x) {
  x <- base_svy %>%
    filter(pobre_06 == x & ocup == 1 & f85>0) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_pobre(x = i)
}     

c_pobre <- as.data.frame(VALOR)
c_pobre <- c_pobre %>% dplyr::mutate(aux = c(18, 19))
c_pobre <- c_pobre[,c("VALOR", "aux")]

## País Urbano

# Total

a_mes <- base_svy %>%
  srvyr::filter(ocup == 1 & (REGION_4 == 1 | REGION_4 == 2) & f85>0) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))


c_ano_pu <- as.data.frame(a_sem)
c_ano_pu <- c_ano_pu %>% dplyr::mutate(aux = 1)
c_ano_pu <- c_ano_pu %>% dplyr::rename(VALOR = a_sem)

# Quintil de ingresos

a_quintil <- function(x) {
  x <- base_svy %>%
    filter(quintilesy == x & ocup == 1 & (REGION_4 == 1 | REGION_4 == 2) & f85>0) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:5){
  VALOR[i] <- a_quintil(x = i)
}     

c_quintil_pu <- as.data.frame(VALOR)
c_quintil_pu <- c_quintil_pu %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil_pu <- c_quintil_pu[,c("VALOR", "aux")]


# Sexo

a_sexo <- function(x) {
  x <- base_svy %>%
    filter(bc_pe2 == x & ocup == 1 & (REGION_4 == 1 | REGION_4 == 2) & f85>0) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_sexo(x = i)
}     


c_sexo_pu <- as.data.frame(VALOR)
c_sexo_pu <- c_sexo_pu %>% dplyr::mutate(aux = c(9, 10))
c_sexo_pu <- c_sexo_pu[,c("VALOR", "aux")]


# Tramo de edad

a_edad <- function(x) {
  x <- base_svy %>%
    filter(tramo_edad == x & ocup == 1 & (REGION_4 == 1 | REGION_4 == 2) & f85>0) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:5){
  VALOR[i] <- a_edad(x = i)
}     


c_edad_pu <- as.data.frame(VALOR)
c_edad_pu <- c_edad_pu %>% dplyr::mutate(aux = c(11, 12, 13, 14, 15))
c_edad_pu <- c_edad_pu[,c("VALOR", "aux")]



CODIND       <- 321
NOMINDICADOR <- "INGRESO POR HORA PROMEDIO EN OCUPACIÓN PRINCIPAL (DEF. BASE 2010)"

VALOR        <- rbind(c_ano, c_afro, c_quintil, c_sexo, c_edad, c_region, c_pobre)
VALOR        <- VALOR  %>% dplyr::mutate(SEXO = case_when(aux == 9  ~ "VARONES", 
                                                          aux == 10 ~ "MUJERES", 
                                                          TRUE     ~ "TODOS"),
                                         ASCENDENCIA  = case_when(aux == 2 ~ "AFRO", 
                                                                  aux == 3 ~ "NO AFRO",
                                                                  TRUE     ~ "TODOS"), 
                                         QUINTIL =      case_when(aux == 4 ~ "QUINTIL 1",
                                                                  aux == 5 ~ "QUINTIL 2",
                                                                  aux == 6 ~ "QUINTIL 3",
                                                                  aux == 7 ~ "QUINTIL 4",
                                                                  aux == 8 ~ "QUINTIL 5",
                                                                  TRUE     ~ "TODOS"),
                                         DEPARTAMENTO = "", 
                                         URBANORURALUY = case_when(aux == 16 ~ "URBANO (MENOS DE 5.000 HABITANTES)",
                                                                   aux == 17 ~ "RURAL DISPERSO",
                                                                   TRUE     ~ "TOTAL PAÍS"),
                                         EDAD =          case_when(aux == 11 ~ "14 a 18",
                                                                   aux == 12 ~ "19 a 24",
                                                                   aux == 13 ~ "25 a 29",
                                                                   aux == 14 ~ "30 a 64",
                                                                   aux == 15 ~ "65 y más",
                                                                   TRUE     ~ ""),
                                         SITUACION_HOGAR = case_when(aux == 18 ~ "No en situación de pobreza",
                                                                     aux == 19  ~"En situación de pobreza",
                                                                     TRUE     ~ ""),
                                         RAMA_DE_ACTIVIDAD = "",
                                         TIPO_DE_OCUPACION = "",
                                         CATEGORÍOCUP = "",
                                         RESTRICCIONES_AL_EMPLEO = "",
                                         SEXO_JEFE       = "")


VALOR_pu        <- rbind(c_ano_pu, c_quintil_pu, c_sexo_pu, c_edad_pu)
VALOR_pu        <- VALOR_pu  %>% dplyr::mutate(SEXO = case_when(aux == 9  ~ "VARONES", 
                                                                aux == 10 ~ "MUJERES", 
                                                                TRUE     ~ "TODOS"),
                                               ASCENDENCIA  =  "TODOS", 
                                               QUINTIL =      case_when(aux == 4 ~ "QUINTIL 1",
                                                                        aux == 5 ~ "QUINTIL 2",
                                                                        aux == 6 ~ "QUINTIL 3",
                                                                        aux == 7 ~ "QUINTIL 4",
                                                                        aux == 8 ~ "QUINTIL 5",
                                                                        TRUE     ~ "TODOS"),
                                               DEPARTAMENTO = "", 
                                               URBANORURALUY = "URBANO (MÁS DE 5.000 HABITANTES)",
                                               EDAD =          case_when(aux == 11 ~ "14 a 18",
                                                                         aux == 12 ~ "19 a 24",
                                                                         aux == 13 ~ "25 a 29",
                                                                         aux == 14 ~ "30 a 64",
                                                                         aux == 15 ~ "65 y más",
                                                                         TRUE     ~ ""),
                                               SITUACION_HOGAR = "",
                                               RAMA_DE_ACTIVIDAD = "",
                                               TIPO_DE_OCUPACION = "",
                                               CATEGORÍOCUP = "",
                                               RESTRICCIONES_AL_EMPLEO = "",
                                               SEXO_JEFE       = "")

VALOR <- rbind (VALOR, VALOR_pu)
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Este indicador se calcula a partir de la encuesta telefónica del primer semestre de 2021 y el formulario de implantación de modalidad panel del segundo semestre de 2021. En el segundo semestre, la condición de pobreza y el quintil de ingresos del hogar corresponden a los ingresos declarados durante la implantación del panel en la encuesta presencial."


t_07 <- cbind(CODIND,	NOMINDICADOR,	PAIS,	FECHA,	FECHA2,	VALOR,	RESPONSABLE,	JERARQUIA,	SECTORPRODUCTIVO,	SECTORINSTITUCIONAL,	ACTIVIDADESECONOMICAS,	DIVISIÓNECONOMICA,	CONCEPTO,	RELACIONFAMILIAR, NOTA_INDICADOR)
t_07 <- subset(t_07, select=c(CODIND,	NOMINDICADOR,	SEXO,	ASCENDENCIA,	QUINTIL, DEPARTAMENTO, URBANORURALUY,	PAIS,	FECHA,	FECHA2,	VALOR,	RESPONSABLE,	JERARQUIA,	SECTORPRODUCTIVO,	SECTORINSTITUCIONAL,	ACTIVIDADESECONOMICAS,	DIVISIÓNECONOMICA,	CONCEPTO,	CATEGORÍOCUP,	RELACIONFAMILIAR,	EDAD, SITUACION_HOGAR, RAMA_DE_ACTIVIDAD, TIPO_DE_OCUPACION, RESTRICCIONES_AL_EMPLEO, SEXO_JEFE, NOTA_INDICADOR))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# 322 - Porcentaje de ocupados que perciben un ingreso por debajo del SMN
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


## Total país


# Total

a_mes <- base_svy %>%
  srvyr::filter(ocup == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(salario_insuf_smn, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

c_ano <- as.data.frame(a_sem)
c_ano <- c_ano %>% dplyr::mutate(aux = 1)
c_ano <- c_ano %>% dplyr::rename(VALOR = a_sem)

# Ascendencia étnico racial

a_afro <- function(x) {
  x <- base_svy %>%
    filter(bd_e29_1 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_afro(x = i)
}     


c_afro <- as.data.frame(VALOR)
c_afro <- c_afro %>% dplyr::mutate(aux = c(2,3))
c_afro <- c_afro[,c("VALOR", "aux")]



# Quintil de ingresos

a_quintil <- function(x) {
  x <- base_svy %>%
    filter(quintilesy == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:5){
  VALOR[i] <- a_quintil(x = i)
}     

c_quintil <- as.data.frame(VALOR)
c_quintil <- c_quintil %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil <- c_quintil[,c("VALOR", "aux")]

# Sexo

a_sexo <- function(x) {
  x <- base_svy %>%
    filter(bc_pe2 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_sexo(x = i)
}     


c_sexo <- as.data.frame(VALOR)
c_sexo <- c_sexo %>% dplyr::mutate(aux = c(9, 10))
c_sexo <- c_sexo[,c("VALOR", "aux")]

# Tramo de edad

a_edad <- function(x) {
  x <- base_svy %>%
    filter(tramo_edad == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:5){
  VALOR[i] <- a_edad(x = i)
}     


c_edad <- as.data.frame(VALOR)
c_edad <- c_edad %>% dplyr::mutate(aux = c(11, 12, 13, 14, 15))
c_edad <- c_edad[,c("VALOR", "aux")]



# Región

a_region <- function(x) {
  x <- base_svy %>%
    filter(REGION_4 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 3:4){
  VALOR[i] <- a_region(x = i)
}     

c_region <- as.data.frame(VALOR)
c_region <- as.data.frame(c_region[c(3,4),])
c_region <- c_region %>% dplyr::mutate(aux = c(16, 17))
c_region <- c_region %>% rename("VALOR" = "c_region[c(3, 4), ]")
c_region <- c_region[,c("VALOR", "aux")]


# Condición de pobreza del hogar

a_pobre <- function(x) {
  x <- base_svy %>%
    filter(pobre_06 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_pobre(x = i)
}     


c_pobre <- as.data.frame(VALOR)
c_pobre <- c_pobre %>% dplyr::mutate(aux = c(18, 19))
c_pobre <- c_pobre[,c("VALOR", "aux")]

## País Urbano

# Total

a_mes <- base_svy %>%
  srvyr::filter(ocup == 1 & (REGION_4 == 1 | REGION_4 == 2)) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(salario_insuf_smn, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))


c_ano_pu <- as.data.frame(a_sem)
c_ano_pu <- c_ano_pu %>% dplyr::mutate(aux = 1)
c_ano_pu <- c_ano_pu %>% dplyr::rename(VALOR = a_sem)

# Quintil de ingresos

a_quintil <- function(x) {
  x <- base_svy %>%
    filter(quintilesy == x & ocup == 1 & (REGION_4 == 1 | REGION_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:5){
  VALOR[i] <- a_quintil(x = i)
}     

c_quintil_pu <- as.data.frame(VALOR)
c_quintil_pu <- c_quintil_pu %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil_pu <- c_quintil_pu[,c("VALOR", "aux")]


# Sexo

a_sexo <- function(x) {
  x <- base_svy %>%
    filter(bc_pe2 == x & ocup == 1 & (REGION_4 == 1 | REGION_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_sexo(x = i)
}     

c_sexo_pu <- as.data.frame(VALOR)
c_sexo_pu <- c_sexo_pu %>% dplyr::mutate(aux = c(9, 10))
c_sexo_pu <- c_sexo_pu[,c("VALOR", "aux")]


# Tramo de edad

a_edad <- function(x) {
  x <- base_svy %>%
    filter(tramo_edad == x & ocup == 1 & (REGION_4 == 1 | REGION_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:5){
  VALOR[i] <- a_edad(x = i)
}     

c_edad_pu <- as.data.frame(VALOR)
c_edad_pu <- c_edad_pu %>% dplyr::mutate(aux = c(11, 12, 13, 14, 15))
c_edad_pu <- c_edad_pu[,c("VALOR", "aux")]



CODIND       <- 322
NOMINDICADOR <- "PORCENTAJE DE OCUPADOS QUE PERCIBEN UN INGRESO POR DEBAJO DEL SMN"

VALOR        <- rbind(c_ano, c_afro, c_quintil, c_sexo, c_edad, c_region, c_pobre)
VALOR        <- VALOR  %>% dplyr::mutate(SEXO = case_when(aux == 9  ~ "VARONES", 
                                                          aux == 10 ~ "MUJERES", 
                                                          TRUE     ~ "TODOS"),
                                         ASCENDENCIA  = case_when(aux == 2 ~ "AFRO", 
                                                                  aux == 3 ~ "NO AFRO",
                                                                  TRUE     ~ "TODOS"), 
                                         QUINTIL =      case_when(aux == 4 ~ "QUINTIL 1",
                                                                  aux == 5 ~ "QUINTIL 2",
                                                                  aux == 6 ~ "QUINTIL 3",
                                                                  aux == 7 ~ "QUINTIL 4",
                                                                  aux == 8 ~ "QUINTIL 5",
                                                                  TRUE     ~ "TODOS"),
                                         DEPARTAMENTO = "", 
                                         URBANORURALUY = case_when(aux == 16 ~ "URBANO (MENOS DE 5.000 HABITANTES)",
                                                                   aux == 17 ~ "RURAL DISPERSO",
                                                                   TRUE     ~ "TOTAL PAÍS"),
                                         EDAD =          case_when(aux == 11 ~ "14 a 18",
                                                                   aux == 12 ~ "19 a 24",
                                                                   aux == 13 ~ "25 a 29",
                                                                   aux == 14 ~ "30 a 64",
                                                                   aux == 15 ~ "65 y más",
                                                                   TRUE     ~ ""),
                                         SITUACION_HOGAR = case_when(aux == 18 ~ "No en situación de pobreza",
                                                                     aux == 19  ~"En situación de pobreza",
                                                                     TRUE     ~ ""),
                                         RAMA_DE_ACTIVIDAD = "",
                                         TIPO_DE_OCUPACION = "",
                                         CATEGORÍOCUP = "",
                                         RESTRICCIONES_AL_EMPLEO = "",
                                         SEXO_JEFE       = "")


VALOR_pu        <- rbind(c_ano_pu, c_quintil_pu, c_sexo_pu, c_edad_pu)
VALOR_pu        <- VALOR_pu  %>% dplyr::mutate(SEXO = case_when(aux == 9  ~ "VARONES", 
                                                                aux == 10 ~ "MUJERES", 
                                                                TRUE     ~ "TODOS"),
                                               ASCENDENCIA  =  "TODOS", 
                                               QUINTIL =      case_when(aux == 4 ~ "QUINTIL 1",
                                                                        aux == 5 ~ "QUINTIL 2",
                                                                        aux == 6 ~ "QUINTIL 3",
                                                                        aux == 7 ~ "QUINTIL 4",
                                                                        aux == 8 ~ "QUINTIL 5",
                                                                        TRUE     ~ "TODOS"),
                                               DEPARTAMENTO = "", 
                                               URBANORURALUY = "URBANO (MÁS DE 5.000 HABITANTES)",
                                               EDAD =          case_when(aux == 11 ~ "14 a 18",
                                                                         aux == 12 ~ "19 a 24",
                                                                         aux == 13 ~ "25 a 29",
                                                                         aux == 14 ~ "30 a 64",
                                                                         aux == 15 ~ "65 y más",
                                                                         TRUE     ~ ""),
                                               SITUACION_HOGAR = "",
                                               RAMA_DE_ACTIVIDAD = "",
                                               TIPO_DE_OCUPACION = "",
                                               CATEGORÍOCUP = "",
                                               RESTRICCIONES_AL_EMPLEO = "",
                                               SEXO_JEFE       = "")

VALOR <- rbind (VALOR, VALOR_pu)
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Este indicador se calcula a partir de la encuesta telefónica del primer semestre de 2021 y el formulario telefónico de modalidad panel del segundo semestre de 2021. En el segundo semestre, la condición de pobreza y el quintil de ingresos del hogar corresponden a los ingresos declarados durante la implantación del panel en la encuesta presencial."


t_08 <- cbind(CODIND,	NOMINDICADOR,	PAIS,	FECHA,	FECHA2,	VALOR,	RESPONSABLE,	JERARQUIA,	SECTORPRODUCTIVO,	SECTORINSTITUCIONAL,	ACTIVIDADESECONOMICAS,	DIVISIÓNECONOMICA,	CONCEPTO,	RELACIONFAMILIAR, NOTA_INDICADOR)
t_08 <- subset(t_08, select=c(CODIND,	NOMINDICADOR,	SEXO,	ASCENDENCIA,	QUINTIL, DEPARTAMENTO, URBANORURALUY,	PAIS,	FECHA,	FECHA2,	VALOR,	RESPONSABLE,	JERARQUIA,	SECTORPRODUCTIVO,	SECTORINSTITUCIONAL,	ACTIVIDADESECONOMICAS,	DIVISIÓNECONOMICA,	CONCEPTO,	CATEGORÍOCUP,	RELACIONFAMILIAR,	EDAD, SITUACION_HOGAR, RAMA_DE_ACTIVIDAD, TIPO_DE_OCUPACION, RESTRICCIONES_AL_EMPLEO, SEXO_JEFE, NOTA_INDICADOR))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# 323 - Porcentaje de ocupados que perciben un ingreso por debajo de la línea de pobreza
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


## Total país

# Total

a_mes <- base_svy %>%
  srvyr::filter(ocup == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(salario_insuf, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

c_ano <- as.data.frame(a_sem)
c_ano <- c_ano %>% dplyr::mutate(aux = 1)
c_ano <- c_ano %>% dplyr::rename(VALOR = a_sem)

# Ascendencia étnico racial

a_afro <- function(x) {
  x <- base_svy %>%
    filter(bd_e29_1 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_afro(x = i)
}     


c_afro <- as.data.frame(VALOR)
c_afro <- c_afro %>% dplyr::mutate(aux = c(2,3))
c_afro <- c_afro[,c("VALOR", "aux")]



# Quintil de ingresos

a_quintil <- function(x) {
  x <- base_svy %>%
    filter(quintilesy == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:5){
  VALOR[i] <- a_quintil(x = i)
}     



c_quintil <- as.data.frame(VALOR)
c_quintil <- c_quintil %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil <- c_quintil[,c("VALOR", "aux")]

# Sexo

a_sexo <- function(x) {
  x <- base_svy %>%
    filter(bc_pe2 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_sexo(x = i)
}     


c_sexo <- as.data.frame(VALOR)
c_sexo <- c_sexo %>% dplyr::mutate(aux = c(9, 10))
c_sexo <- c_sexo[,c("VALOR", "aux")]

# Tramo de edad

a_edad <- function(x) {
  x <- base_svy %>%
    filter(tramo_edad == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:5){
  VALOR[i] <- a_edad(x = i)
}     


c_edad <- as.data.frame(VALOR)
c_edad <- c_edad %>% dplyr::mutate(aux = c(11, 12, 13, 14, 15))
c_edad <- c_edad[,c("VALOR", "aux")]



# Región

a_region <- function(x) {
  x <- base_svy %>%
    filter(REGION_4 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 3:4){
  VALOR[i] <- a_region(x = i)
}     


c_region <- as.data.frame(VALOR)
c_region <- as.data.frame(c_region[c(3,4),])
c_region <- c_region %>% dplyr::mutate(aux = c(16, 17))
c_region <- c_region %>% rename("VALOR" = "c_region[c(3, 4), ]")
c_region <- c_region[,c("VALOR", "aux")]

# Condición de pobreza del hogar

a_pobre <- function(x) {
  x <- base_svy %>%
    filter(pobre_06 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_pobre(x = i)
}     


c_pobre <- as.data.frame(VALOR)
c_pobre <- c_pobre %>% dplyr::mutate(aux = c(18, 19))
c_pobre <- c_pobre[,c("VALOR", "aux")]

## País Urbano

# Total

a_mes <- base_svy %>%
  srvyr::filter(ocup == 1 & (REGION_4 == 1 | REGION_4 == 2)) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(salario_insuf, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

c_ano_pu <- as.data.frame(a_sem)
c_ano_pu <- c_ano_pu %>% dplyr::mutate(aux = 1)
c_ano_pu <- c_ano_pu %>% dplyr::rename(VALOR = a_sem)

# Quintil de ingresos

a_quintil <- function(x) {
  x <- base_svy %>%
    filter(quintilesy == x & ocup == 1 & (REGION_4 == 1 | REGION_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:5){
  VALOR[i] <- a_quintil(x = i)
}     


c_quintil_pu <- as.data.frame(VALOR)
c_quintil_pu <- c_quintil_pu %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil_pu <- c_quintil_pu[,c("VALOR", "aux")]


# Sexo

a_sexo <- function(x) {
  x <- base_svy %>%
    filter(bc_pe2 == x & ocup == 1 & (REGION_4 == 1 | REGION_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_sexo(x = i)
}     


c_sexo_pu <- as.data.frame(VALOR)
c_sexo_pu <- c_sexo_pu %>% dplyr::mutate(aux = c(9, 10))
c_sexo_pu <- c_sexo_pu[,c("VALOR", "aux")]


# Tramo de edad

a_edad <- function(x) {
  x <- base_svy %>%
    filter(tramo_edad == x & ocup == 1 & (REGION_4 == 1 | REGION_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:5){
  VALOR[i] <- a_edad(x = i)
}     


c_edad_pu <- as.data.frame(VALOR)
c_edad_pu <- c_edad_pu %>% dplyr::mutate(aux = c(11, 12, 13, 14, 15))
c_edad_pu <- c_edad_pu[,c("VALOR", "aux")]



CODIND       <- 323
NOMINDICADOR <- "PORCENTAJE DE OCUPADOS QUE PERCIBEN UN INGRESO POR DEBAJO DE LA LÍNEA DE POBREZA"

VALOR        <- rbind(c_ano, c_afro, c_quintil, c_sexo, c_edad, c_region, c_pobre)
VALOR        <- VALOR  %>% dplyr::mutate(SEXO = case_when(aux == 9  ~ "VARONES", 
                                                          aux == 10 ~ "MUJERES", 
                                                          TRUE     ~ "TODOS"),
                                         ASCENDENCIA  = case_when(aux == 2 ~ "AFRO", 
                                                        aux == 3 ~ "NO AFRO",
                                                        TRUE     ~ "TODOS"), 
                                         QUINTIL =      case_when(aux == 4 ~ "QUINTIL 1",
                                                                  aux == 5 ~ "QUINTIL 2",
                                                                  aux == 6 ~ "QUINTIL 3",
                                                                  aux == 7 ~ "QUINTIL 4",
                                                                  aux == 8 ~ "QUINTIL 5",
                                                                  TRUE     ~ "TODOS"),
                                         DEPARTAMENTO = "", 
                                         URBANORURALUY = case_when(aux == 16 ~ "URBANO (MENOS DE 5.000 HABITANTES)",
                                                                   aux == 17 ~ "RURAL DISPERSO",
                                                                   TRUE     ~ "TOTAL PAÍS"),
                                         EDAD =          case_when(aux == 11 ~ "14 a 18",
                                                                   aux == 12 ~ "19 a 24",
                                                                   aux == 13 ~ "25 a 29",
                                                                   aux == 14 ~ "30 a 64",
                                                                   aux == 15 ~ "65 y más",
                                                                   TRUE     ~ ""),
                                         SITUACION_HOGAR = case_when(aux == 18 ~ "No en situación de pobreza",
                                                                     aux == 19 ~ "En situación de pobreza",
                                                                     TRUE     ~ ""),
                                         RAMA_DE_ACTIVIDAD = "",
                                         TIPO_DE_OCUPACION = "",
                                         CATEGORÍOCUP = "",
                                         RESTRICCIONES_AL_EMPLEO = "",
                                         SEXO_JEFE       = "")


VALOR_pu        <- rbind(c_ano_pu, c_quintil_pu, c_sexo_pu, c_edad_pu)
VALOR_pu        <- VALOR_pu  %>% dplyr::mutate(SEXO = case_when(aux == 9  ~ "VARONES", 
                                                                aux == 10 ~ "MUJERES", 
                                                                TRUE     ~ "TODOS"),
                                               ASCENDENCIA  =  "TODOS", 
                                               QUINTIL =      case_when(aux == 4 ~ "QUINTIL 1",
                                                                        aux == 5 ~ "QUINTIL 2",
                                                                        aux == 6 ~ "QUINTIL 3",
                                                                        aux == 7 ~ "QUINTIL 4",
                                                                        aux == 8 ~ "QUINTIL 5",
                                                                        TRUE     ~ "TODOS"),
                                               DEPARTAMENTO = "", 
                                               URBANORURALUY = "URBANO (MÁS DE 5.000 HABITANTES)",
                                               EDAD =          case_when(aux == 11 ~ "14 a 18",
                                                                         aux == 12 ~ "19 a 24",
                                                                         aux == 13 ~ "25 a 29",
                                                                         aux == 14 ~ "30 a 64",
                                                                         aux == 15 ~ "65 y más",
                                                                         TRUE     ~ ""),
                                               SITUACION_HOGAR = "",
                                               RAMA_DE_ACTIVIDAD = "",
                                               TIPO_DE_OCUPACION = "",
                                               CATEGORÍOCUP = "",
                                               RESTRICCIONES_AL_EMPLEO = "",
                                               SEXO_JEFE       = "")

VALOR <- rbind (VALOR, VALOR_pu)
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Este indicador se calcula a partir de la encuesta telefónica del primer semestre de 2021 y el formulario telefónico de modalidad panel del segundo semestre de 2021. En el segundo semestre, la condición de pobreza y el quintil de ingresos del hogar corresponden a los ingresos declarados durante la implantación del panel en la encuesta presencial."


t_09 <- cbind(CODIND,	NOMINDICADOR,	PAIS,	FECHA,	FECHA2,	VALOR,	RESPONSABLE,	JERARQUIA,	SECTORPRODUCTIVO,	SECTORINSTITUCIONAL,	ACTIVIDADESECONOMICAS,	DIVISIÓNECONOMICA,	CONCEPTO,	RELACIONFAMILIAR, NOTA_INDICADOR)
t_09 <- subset(t_09, select=c(CODIND,	NOMINDICADOR,	SEXO,	ASCENDENCIA,	QUINTIL, DEPARTAMENTO, URBANORURALUY,	PAIS,	FECHA,	FECHA2,	VALOR,	RESPONSABLE,	JERARQUIA,	SECTORPRODUCTIVO,	SECTORINSTITUCIONAL,	ACTIVIDADESECONOMICAS,	DIVISIÓNECONOMICA,	CONCEPTO,	CATEGORÍOCUP,	RELACIONFAMILIAR,	EDAD, SITUACION_HOGAR, RAMA_DE_ACTIVIDAD, TIPO_DE_OCUPACION, RESTRICCIONES_AL_EMPLEO, SEXO_JEFE, NOTA_INDICADOR))




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# 324 - Brecha de ingresos por trabajo por sexo en ocupación principal
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


## Total país

# Total

a_mes_varones <- base_svy %>%
  srvyr::filter(ocup == 1 & bc_pe2 == 1 & f85>0) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))

a_sem_varones <- mean(as.numeric(a_mes_varones$colname))

a_mes_mujeres <- base_svy %>%
  srvyr::filter(ocup == 1 & bc_pe2 == 2 & f85>0) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))

a_sem_mujeres <- mean(as.numeric(a_mes_mujeres$colname))


c_ano <- as.data.frame(t(c(a_sem_varones, a_sem_mujeres)))
c_ano <- c_ano %>% dplyr::mutate(VALOR = V2 / V1)
c_ano <- c_ano %>% dplyr::mutate(aux = 1)
c_ano <- c_ano[, c("VALOR", "aux")]




# Ascendencia étnico racial

a_afro_varones <- function(x) {
  x <- base_svy %>%
    filter(bd_e29_1 == x & ocup == 1  & bc_pe2 == 1 & f85>0) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

VALOR_varones <- numeric()

for(i in 1:2){
  VALOR_varones[i] <- a_afro_varones(x = i)
}     

a_afro_mujeres <- function(x) {
  x <- base_svy %>%
    filter(bd_e29_1 == x & ocup == 1  & bc_pe2 == 2 & f85>0) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

VALOR_mujeres <- numeric()

for(i in 1:2){
  VALOR_mujeres[i] <- a_afro_mujeres(x = i)
} 


c_afro <- as.data.frame(cbind(VALOR_varones, VALOR_mujeres))
c_afro <- c_afro %>% dplyr::mutate(VALOR = VALOR_mujeres / VALOR_varones)
c_afro <- c_afro %>% dplyr::mutate(aux = c(2,3))
c_afro <- c_afro[,c("VALOR", "aux")]



# Quintil de ingresos

a_quintil_varones <- function(x) {
  x <- base_svy %>%
    filter(quintilesy == x & ocup == 1  & bc_pe2 == 1 & f85>0) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

VALOR_varones <- numeric()

for(i in 1:5){
  VALOR_varones[i] <- a_quintil_varones(x = i)
}     

a_quintil_mujeres <- function(x) {
  x <- base_svy %>%
    filter(quintilesy == x & ocup == 1  & bc_pe2 == 2 & f85>0) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

VALOR_mujeres <- numeric()

for(i in 1:5){
  VALOR_mujeres[i] <- a_quintil_mujeres(x = i)
}  


c_quintil <- as.data.frame(cbind(VALOR_varones, VALOR_mujeres))
c_quintil <- c_quintil %>% dplyr::mutate(VALOR = VALOR_mujeres / VALOR_varones)
c_quintil <- c_quintil %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil <- c_quintil[,c("VALOR", "aux")]


# Tramo de edad

a_edad_varones <- function(x) {
  x <- base_svy %>%
    filter(tramo_edad == x & ocup == 1 & bc_pe2 == 1 & f85>0) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

VALOR_varones <- numeric()

for(i in 1:5){
  VALOR_varones[i] <- a_edad_varones(x = i)
}  

a_edad_mujeres <- function(x) {
  x <- base_svy %>%
    filter(tramo_edad == x & ocup == 1 & bc_pe2 == 2 & f85>0) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

VALOR_mujeres <- numeric()

for(i in 1:5){
  VALOR_mujeres[i] <- a_edad_mujeres(x = i)
} 



c_edad <- as.data.frame(cbind(VALOR_varones, VALOR_mujeres))
c_edad <- c_edad %>% dplyr::mutate(VALOR = VALOR_mujeres / VALOR_varones)
c_edad <- c_edad %>% dplyr::mutate(aux = c(11, 12, 13, 14, 15))
c_edad <- c_edad[,c("VALOR", "aux")]



# Región

a_region_varones <- function(x) {
  x <- base_svy %>%
    filter(REGION_4 == x & ocup == 1 & bc_pe2 == 1 & f85>0) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

VALOR_varones <- numeric()

for(i in 3:4){
  VALOR_varones[i] <- a_region_varones(x = i)
}     

a_region_mujeres <- function(x) {
  x <- base_svy %>%
    filter(REGION_4 == x & ocup == 1 & bc_pe2 == 2 & f85>0) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

VALOR_mujeres <- numeric()

for(i in 3:4){
  VALOR_mujeres[i] <- a_region_mujeres(x = i)
}     



c_region <- as.data.frame(cbind(VALOR_varones, VALOR_mujeres))
c_region <- c_region %>% dplyr::mutate(VALOR = VALOR_mujeres / VALOR_varones)
c_region <- as.data.frame(c_region[c(3,4),])
c_region <- c_region %>% dplyr::mutate(aux = c(16, 17))
c_region <- c_region[,c("VALOR", "aux")]


# Condición de pobreza del hogar

a_pobre_varones <- function(x) {
  x <- base_svy %>%
    filter(pobre_06 == x & ocup == 1 & bc_pe2 == 1 & f85>0) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

VALOR_varones <- numeric()

for(i in 1:2){
  VALOR_varones[i] <- a_pobre_varones(x = i)
}     

a_pobre_mujeres <- function(x) {
  x <- base_svy %>%
    filter(pobre_06 == x & ocup == 1 & bc_pe2 == 1 & f85>0) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

VALOR_mujeres <- numeric()

for(i in 1:2){
  VALOR_mujeres[i] <- a_pobre_mujeres(x = i)
} 



c_pobre <- as.data.frame(cbind(VALOR_varones, VALOR_mujeres))
c_pobre <- c_pobre %>% dplyr::mutate(VALOR = VALOR_mujeres / VALOR_varones)
c_pobre <- c_pobre %>% dplyr::mutate(aux = c(18, 19))
c_pobre <- c_pobre[,c("VALOR", "aux")]

## País Urbano

# Total

a_mes_varones <- base_svy %>%
  srvyr::filter(ocup == 1 & (REGION_4 == 1 | REGION_4 == 2) & bc_pe2 == 1 & f85>0) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))

a_sem_varones <- mean(as.numeric(a_mes_varones$colname))

a_mes_mujeres <- base_svy %>%
  srvyr::filter(ocup == 1 & (REGION_4 == 1 | REGION_4 == 2) & bc_pe2 == 2 & f85>0) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))

a_sem_mujeres <- mean(as.numeric(a_mes_mujeres$colname))


c_ano_pu <- as.data.frame(t(c(a_sem_varones, a_sem_mujeres)))

c_ano_pu <- c_ano_pu %>% dplyr::mutate(VALOR = a_sem_varones / a_sem_mujeres)
c_ano_pu <- c_ano_pu %>% dplyr::mutate(aux = 1)
c_ano_pu <- c_ano_pu[, c("VALOR", "aux")]


# Quintil de ingresos

a_quintil_varones <- function(x) {
  x <- base_svy %>%
    filter(quintilesy == x & ocup == 1 & (REGION_4 == 1 | REGION_4 == 2) & bc_pe2 == 1 & f85>0) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

VALOR_varones <- numeric()

for(i in 1:5){
  VALOR_varones[i] <- a_quintil_varones(x = i)
}  

a_quintil_mujeres <- function(x) {
  x <- base_svy %>%
    filter(quintilesy == x & ocup == 1 & (REGION_4 == 1 | REGION_4 == 2) & bc_pe2 == 2 & f85>0) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

VALOR_mujeres <- numeric()

for(i in 1:5){
  VALOR_mujeres[i] <- a_quintil_mujeres(x = i)
}     



c_quintil_pu <- as.data.frame(cbind(VALOR_varones, VALOR_mujeres))
c_quintil_pu <- c_quintil_pu %>% dplyr::mutate(VALOR = VALOR_mujeres / VALOR_varones)
c_quintil_pu <- c_quintil_pu %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil_pu <- c_quintil_pu[,c("VALOR", "aux")]



# Tramo de edad

a_edad_varones <- function(x) {
  x <- base_svy %>%
    filter(tramo_edad == x & ocup == 1 & (REGION_4 == 1 | REGION_4 == 2) & bc_pe2 == 1 & f85>0) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

VALOR_varones <- numeric()

for(i in 1:5){
  VALOR_varones[i] <- a_edad_varones(x = i)
}     

a_edad_mujeres <- function(x) {
  x <- base_svy %>%
    filter(tramo_edad == x & ocup == 1 & (REGION_4 == 1 | REGION_4 == 2) & bc_pe2 == 2 & f85>0) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

VALOR_mujeres <- numeric()

for(i in 1:5){
  VALOR_mujeres[i] <- a_edad_mujeres(x = i)
}     



c_edad_pu <- as.data.frame(cbind(VALOR_varones, VALOR_mujeres))
c_edad_pu <- c_edad_pu %>% dplyr::mutate(VALOR = VALOR_mujeres / VALOR_varones)
c_edad_pu <- c_edad_pu %>% dplyr::mutate(aux = c(11, 12, 13, 14, 15))
c_edad_pu <- c_edad_pu[,c("VALOR", "aux")]



CODIND       <- 324
NOMINDICADOR <- "BRECHA DE INGRESOS POR TRABAJO POR SEXO EN OCUPACIÓN PRINCIPAL"

VALOR        <- rbind(c_ano, c_afro, c_quintil, c_edad, c_region, c_pobre)
VALOR        <- VALOR  %>% dplyr::mutate(SEXO = "TODOS",
                                         ASCENDENCIA  = case_when(aux == 2 ~ "AFRO", 
                                                                  aux == 3 ~ "NO AFRO",
                                                                  TRUE     ~ "TODOS"), 
                                         QUINTIL =      case_when(aux == 4 ~ "QUINTIL 1",
                                                                  aux == 5 ~ "QUINTIL 2",
                                                                  aux == 6 ~ "QUINTIL 3",
                                                                  aux == 7 ~ "QUINTIL 4",
                                                                  aux == 8 ~ "QUINTIL 5",
                                                                  TRUE     ~ "TODOS"),
                                         DEPARTAMENTO = "", 
                                         URBANORURALUY = case_when(aux == 16 ~ "URBANO (MENOS DE 5.000 HABITANTES)",
                                                                   aux == 17 ~ "RURAL DISPERSO",
                                                                   TRUE     ~ "TOTAL PAÍS"),
                                         EDAD =          case_when(aux == 11 ~ "14 a 18",
                                                                   aux == 12 ~ "19 a 24",
                                                                   aux == 13 ~ "25 a 29",
                                                                   aux == 14 ~ "30 a 64",
                                                                   aux == 15 ~ "65 y más",
                                                                   TRUE     ~ ""),
                                         SITUACION_HOGAR = case_when(aux == 18 ~ "No en situación de pobreza",
                                                                     aux == 19  ~"En situación de pobreza",
                                                                     TRUE     ~ ""),
                                         RAMA_DE_ACTIVIDAD = "",
                                         TIPO_DE_OCUPACION = "",
                                         CATEGORÍOCUP = "",
                                         RESTRICCIONES_AL_EMPLEO = "",
                                         SEXO_JEFE       = "")


VALOR_pu        <- rbind(c_ano_pu, c_quintil_pu, c_edad_pu)
VALOR_pu        <- VALOR_pu  %>% dplyr::mutate(SEXO = "TODOS",
                                               ASCENDENCIA  =  "TODOS", 
                                               QUINTIL =      case_when(aux == 4 ~ "QUINTIL 1",
                                                                        aux == 5 ~ "QUINTIL 2",
                                                                        aux == 6 ~ "QUINTIL 3",
                                                                        aux == 7 ~ "QUINTIL 4",
                                                                        aux == 8 ~ "QUINTIL 5",
                                                                        TRUE     ~ "TODOS"),
                                               DEPARTAMENTO = "", 
                                               URBANORURALUY = "URBANO (MÁS DE 5.000 HABITANTES)",
                                               EDAD =          case_when(aux == 11 ~ "14 a 18",
                                                                         aux == 12 ~ "19 a 24",
                                                                         aux == 13 ~ "25 a 29",
                                                                         aux == 14 ~ "30 a 64",
                                                                         aux == 15 ~ "65 y más",
                                                                         TRUE     ~ ""),
                                               SITUACION_HOGAR = "",
                                               RAMA_DE_ACTIVIDAD = "",
                                               TIPO_DE_OCUPACION = "",
                                               CATEGORÍOCUP = "",
                                               RESTRICCIONES_AL_EMPLEO = "",
                                               SEXO_JEFE       = "")

VALOR <- rbind (VALOR, VALOR_pu)
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Este indicador se calcula a partir de la encuesta telefónica del primer semestre de 2021 y el formulario telefónico de modalidad panel del segundo semestre de 2021. En el segundo semestre, la condición de pobreza y el quintil de ingresos del hogar corresponden a los ingresos declarados durante la implantación del panel en la encuesta presencial."


t_10 <- cbind(CODIND,	NOMINDICADOR,	PAIS,	FECHA,	FECHA2,	VALOR,	RESPONSABLE,	JERARQUIA,	SECTORPRODUCTIVO,	SECTORINSTITUCIONAL,	ACTIVIDADESECONOMICAS,	DIVISIÓNECONOMICA,	CONCEPTO,	RELACIONFAMILIAR, NOTA_INDICADOR)
t_10 <- subset(t_10, select=c(CODIND,	NOMINDICADOR,	SEXO,	ASCENDENCIA,	QUINTIL, DEPARTAMENTO, URBANORURALUY,	PAIS,	FECHA,	FECHA2,	VALOR,	RESPONSABLE,	JERARQUIA,	SECTORPRODUCTIVO,	SECTORINSTITUCIONAL,	ACTIVIDADESECONOMICAS,	DIVISIÓNECONOMICA,	CONCEPTO,	CATEGORÍOCUP,	RELACIONFAMILIAR,	EDAD, SITUACION_HOGAR, RAMA_DE_ACTIVIDAD, TIPO_DE_OCUPACION, RESTRICCIONES_AL_EMPLEO, SEXO_JEFE, NOTA_INDICADOR))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# 331 - Porcentaje de mayores de 65 años que perciben jubilación
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


## Total país

# Total

a_mes <- base_svy %>%
  srvyr::filter(tramo_edad == 5) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(jub, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

c_ano <- as.data.frame(a_sem)
c_ano <- c_ano %>% dplyr::mutate(aux = 1)
c_ano <- c_ano %>% dplyr::rename(VALOR = a_sem)

# Ascendencia étnico racial

a_afro <- function(x) {
  x <- base_svy %>%
    filter(bd_e29_1 == x & tramo_edad == 5) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_afro(x = i)
}     


c_afro <- as.data.frame(VALOR)
c_afro <- c_afro %>% dplyr::mutate(aux = c(2,3))
c_afro <- c_afro[,c("VALOR", "aux")]



# Quintil de ingresos

a_quintil <- function(x) {
  x <- base_svy %>%
    filter(quintilesy == x & tramo_edad == 5) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:5){
  VALOR[i] <- a_quintil(x = i)
}     


c_quintil <- as.data.frame(VALOR)
c_quintil <- c_quintil %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil <- c_quintil[,c("VALOR", "aux")]

# Sexo

a_sexo <- function(x) {
  x <- base_svy %>%
    filter(bc_pe2 == x & tramo_edad == 5) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_sexo(x = i)
}     


c_sexo <- as.data.frame(VALOR)
c_sexo <- c_sexo %>% dplyr::mutate(aux = c(9, 10))
c_sexo <- c_sexo[,c("VALOR", "aux")]



# Región

a_region <- function(x) {
  x <- base_svy %>%
    filter(REGION_4 == x & tramo_edad == 5) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 3:4){
  VALOR[i] <- a_region(x = i)
}     


c_region <- as.data.frame(VALOR)
c_region <- as.data.frame(c_region[c(3,4),])
c_region <- c_region %>% dplyr::mutate(aux = c(16, 17))
c_region <- c_region %>% rename("VALOR" = "c_region[c(3, 4), ]")
c_region <- c_region[,c("VALOR", "aux")]

# Condición de pobreza del hogar

a_pobre <- function(x) {
  x <- base_svy %>%
    filter(pobre_06 == x & tramo_edad == 5) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_pobre(x = i)
}     



c_pobre <- as.data.frame(VALOR)
c_pobre <- c_pobre %>% dplyr::mutate(aux = c(18, 19))
c_pobre <- c_pobre[,c("VALOR", "aux")]

## País Urbano

# Total

a_mes <- base_svy %>%
  srvyr::filter(tramo_edad == 5 & (REGION_4 == 1 | REGION_4 == 2)) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(jub, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

c_ano_pu <- as.data.frame(a_sem)
c_ano_pu <- c_ano_pu %>% dplyr::mutate(aux = 1)
c_ano_pu <- c_ano_pu %>% dplyr::rename(VALOR = a_sem)

# Quintil de ingresos

a_quintil <- function(x) {
  x <- base_svy %>%
    filter(quintilesy == x & tramo_edad == 5 & (REGION_4 == 1 | REGION_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:5){
  VALOR[i] <- a_quintil(x = i)
}     



c_quintil_pu <- as.data.frame(VALOR)
c_quintil_pu <- c_quintil_pu %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil_pu <- c_quintil_pu[,c("VALOR", "aux")]


# Sexo

a_sexo <- function(x) {
  x <- base_svy %>%
    filter(bc_pe2 == x & tramo_edad == 5 & (REGION_4 == 1 | REGION_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_sexo(x = i)
}     


c_sexo_pu <- as.data.frame(VALOR)
c_sexo_pu <- c_sexo_pu %>% dplyr::mutate(aux = c(9, 10))
c_sexo_pu <- c_sexo_pu[,c("VALOR", "aux")]





CODIND       <- 331
NOMINDICADOR <- "PORCENTAJE DE MAYORES DE 65 AÑOS QUE PERCIBEN JUBILACIÓN"

VALOR        <- rbind(c_ano, c_afro, c_quintil, c_sexo, c_region, c_pobre)
VALOR        <- VALOR  %>% dplyr::mutate(SEXO = case_when(aux == 9  ~ "VARONES", 
                                                          aux == 10 ~ "MUJERES", 
                                                          TRUE     ~ "TODOS"),
                                         ASCENDENCIA  = case_when(aux == 2 ~ "AFRO", 
                                                                  aux == 3 ~ "NO AFRO",
                                                                  TRUE     ~ "TODOS"), 
                                         QUINTIL =      case_when(aux == 4 ~ "QUINTIL 1",
                                                                  aux == 5 ~ "QUINTIL 2",
                                                                  aux == 6 ~ "QUINTIL 3",
                                                                  aux == 7 ~ "QUINTIL 4",
                                                                  aux == 8 ~ "QUINTIL 5",
                                                                  TRUE     ~ "TODOS"),
                                         DEPARTAMENTO = "", 
                                         URBANORURALUY = case_when(aux == 16 ~ "URBANO (MENOS DE 5.000 HABITANTES)",
                                                                   aux == 17 ~ "RURAL DISPERSO",
                                                                   TRUE     ~ "TOTAL PAÍS"),
                                         EDAD =          "",
                                         SITUACION_HOGAR = case_when(aux == 18 ~ "No en situación de pobreza",
                                                                     aux == 19  ~"En situación de pobreza",
                                                                     TRUE     ~ ""),
                                         RAMA_DE_ACTIVIDAD = "",
                                         TIPO_DE_OCUPACION = "",
                                         CATEGORÍOCUP = "",
                                         RESTRICCIONES_AL_EMPLEO = "",
                                         SEXO_JEFE       = "")


VALOR_pu        <- rbind(c_ano_pu, c_quintil_pu, c_sexo_pu)
VALOR_pu        <- VALOR_pu  %>% dplyr::mutate(SEXO = case_when(aux == 9  ~ "VARONES", 
                                                                aux == 10 ~ "MUJERES", 
                                                                TRUE     ~ "TODOS"),
                                               ASCENDENCIA  =  "TODOS", 
                                               QUINTIL =      case_when(aux == 4 ~ "QUINTIL 1",
                                                                        aux == 5 ~ "QUINTIL 2",
                                                                        aux == 6 ~ "QUINTIL 3",
                                                                        aux == 7 ~ "QUINTIL 4",
                                                                        aux == 8 ~ "QUINTIL 5",
                                                                        TRUE     ~ "TODOS"),
                                               DEPARTAMENTO = "", 
                                               URBANORURALUY = "URBANO (MÁS DE 5.000 HABITANTES)",
                                               EDAD =          "",
                                               SITUACION_HOGAR = "",
                                               RAMA_DE_ACTIVIDAD = "",
                                               TIPO_DE_OCUPACION = "",
                                               CATEGORÍOCUP = "",
                                               RESTRICCIONES_AL_EMPLEO = "",
                                               SEXO_JEFE       = "")

VALOR <- rbind (VALOR, VALOR_pu)
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Este indicador se calcula a partir de la encuesta telefónica del primer semestre de 2021 y el formulario telefónico de modalidad panel del segundo semestre de 2021. En el segundo semestre, la condición de pobreza y el quintil de ingresos del hogar corresponden a los ingresos declarados durante la implantación del panel en la encuesta presencial."


t_11 <- cbind(CODIND,	NOMINDICADOR,	PAIS,	FECHA,	FECHA2,	VALOR,	RESPONSABLE,	JERARQUIA,	SECTORPRODUCTIVO,	SECTORINSTITUCIONAL,	ACTIVIDADESECONOMICAS,	DIVISIÓNECONOMICA,	CONCEPTO,	RELACIONFAMILIAR, NOTA_INDICADOR)
t_11 <- subset(t_11, select=c(CODIND,	NOMINDICADOR,	SEXO,	ASCENDENCIA,	QUINTIL, DEPARTAMENTO, URBANORURALUY,	PAIS,	FECHA,	FECHA2,	VALOR,	RESPONSABLE,	JERARQUIA,	SECTORPRODUCTIVO,	SECTORINSTITUCIONAL,	ACTIVIDADESECONOMICAS,	DIVISIÓNECONOMICA,	CONCEPTO,	CATEGORÍOCUP,	RELACIONFAMILIAR,	EDAD, SITUACION_HOGAR, RAMA_DE_ACTIVIDAD, TIPO_DE_OCUPACION, RESTRICCIONES_AL_EMPLEO, SEXO_JEFE, NOTA_INDICADOR))




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# 332 - Porcentaje de mayores de 65 años que perciben pensión
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


## Total país

# Total

a_mes <- base_svy %>%
  srvyr::filter(tramo_edad == 5) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(pens, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))


c_ano <- as.data.frame(a_sem)
c_ano <- c_ano %>% dplyr::mutate(aux = 1)
c_ano <- c_ano %>% dplyr::rename(VALOR = a_sem)

# Ascendencia étnico racial

a_afro <- function(x) {
  x <- base_svy %>%
    filter(bd_e29_1 == x & tramo_edad == 5) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_afro(x = i)
}     


c_afro <- as.data.frame(VALOR)
c_afro <- c_afro %>% dplyr::mutate(aux = c(2,3))
c_afro <- c_afro[,c("VALOR", "aux")]



# Quintil de ingresos

a_quintil <- function(x) {
  x <- base_svy %>%
    filter(quintilesy == x & tramo_edad == 5) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:5){
  VALOR[i] <- a_quintil(x = i)
}     

c_quintil <- as.data.frame(VALOR)
c_quintil <- c_quintil %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil <- c_quintil[,c("VALOR", "aux")]

# Sexo

a_sexo <- function(x) {
  x <- base_svy %>%
    filter(bc_pe2 == x & tramo_edad == 5) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_sexo(x = i)
}     



c_sexo <- as.data.frame(VALOR)
c_sexo <- c_sexo %>% dplyr::mutate(aux = c(9, 10))
c_sexo <- c_sexo[,c("VALOR", "aux")]



# Región

a_region <- function(x) {
  x <- base_svy %>%
    filter(REGION_4 == x & tramo_edad == 5) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 3:4){
  VALOR[i] <- a_region(x = i)
}     


c_region <- as.data.frame(VALOR)
c_region <- as.data.frame(c_region[c(3,4),])
c_region <- c_region %>% dplyr::mutate(aux = c(16, 17))
c_region <- c_region %>% rename("VALOR" = "c_region[c(3, 4), ]")
c_region <- c_region[,c("VALOR", "aux")]

# Condición de pobreza del hogar

a_pobre <- function(x) {
  x <- base_svy %>%
    filter(pobre_06 == x & tramo_edad == 5) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_pobre(x = i)
}     


c_pobre <- as.data.frame(VALOR)
c_pobre <- c_pobre %>% dplyr::mutate(aux = c(18, 19))
c_pobre <- c_pobre[,c("VALOR", "aux")]

## País Urbano

# Total

a_mes <- base_svy %>%
  srvyr::filter(tramo_edad == 5 & (REGION_4 == 1 | REGION_4 == 2)) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(pens, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

c_ano_pu <- as.data.frame(a_sem)
c_ano_pu <- c_ano_pu %>% dplyr::mutate(aux = 1)
c_ano_pu <- c_ano_pu %>% dplyr::rename(VALOR = a_sem)

# Quintil de ingresos

a_quintil <- function(x) {
  x <- base_svy %>%
    filter(quintilesy == x & tramo_edad == 5 & (REGION_4 == 1 | REGION_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:5){
  VALOR[i] <- a_quintil(x = i)
}     

c_quintil_pu <- as.data.frame(VALOR)
c_quintil_pu <- c_quintil_pu %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil_pu <- c_quintil_pu[,c("VALOR", "aux")]


# Sexo

a_sexo <- function(x) {
  x <- base_svy %>%
    filter(bc_pe2 == x & tramo_edad == 5 & (REGION_4 == 1 | REGION_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_sexo(x = i)
}     

c_sexo_pu <- as.data.frame(VALOR)
c_sexo_pu <- c_sexo_pu %>% dplyr::mutate(aux = c(9, 10))
c_sexo_pu <- c_sexo_pu[,c("VALOR", "aux")]





CODIND       <- 332
NOMINDICADOR <- "PORCENTAJE DE MAYORES DE 65 AÑOS QUE PERCIBEN PENSIÓN"

VALOR        <- rbind(c_ano, c_afro, c_quintil, c_sexo, c_region, c_pobre)
VALOR        <- VALOR  %>% dplyr::mutate(SEXO = case_when(aux == 9  ~ "VARONES", 
                                                          aux == 10 ~ "MUJERES", 
                                                          TRUE     ~ "TODOS"),
                                         ASCENDENCIA  = case_when(aux == 2 ~ "AFRO", 
                                                                  aux == 3 ~ "NO AFRO",
                                                                  TRUE     ~ "TODOS"), 
                                         QUINTIL =      case_when(aux == 4 ~ "QUINTIL 1",
                                                                  aux == 5 ~ "QUINTIL 2",
                                                                  aux == 6 ~ "QUINTIL 3",
                                                                  aux == 7 ~ "QUINTIL 4",
                                                                  aux == 8 ~ "QUINTIL 5",
                                                                  TRUE     ~ "TODOS"),
                                         DEPARTAMENTO = "", 
                                         URBANORURALUY = case_when(aux == 16 ~ "URBANO (MENOS DE 5.000 HABITANTES)",
                                                                   aux == 17 ~ "RURAL DISPERSO",
                                                                   TRUE     ~ "TOTAL PAÍS"),
                                         EDAD =          "",
                                         SITUACION_HOGAR = case_when(aux == 18 ~ "No en situación de pobreza",
                                                                     aux == 19  ~"En situación de pobreza",
                                                                     TRUE     ~ ""),
                                         RAMA_DE_ACTIVIDAD = "",
                                         TIPO_DE_OCUPACION = "",
                                         CATEGORÍOCUP = "",
                                         RESTRICCIONES_AL_EMPLEO = "",
                                         SEXO_JEFE       = "")


VALOR_pu        <- rbind(c_ano_pu, c_quintil_pu, c_sexo_pu)
VALOR_pu        <- VALOR_pu  %>% dplyr::mutate(SEXO = case_when(aux == 9  ~ "VARONES", 
                                                                aux == 10 ~ "MUJERES", 
                                                                TRUE     ~ "TODOS"),
                                               ASCENDENCIA  =  "TODOS", 
                                               QUINTIL =      case_when(aux == 4 ~ "QUINTIL 1",
                                                                        aux == 5 ~ "QUINTIL 2",
                                                                        aux == 6 ~ "QUINTIL 3",
                                                                        aux == 7 ~ "QUINTIL 4",
                                                                        aux == 8 ~ "QUINTIL 5",
                                                                        TRUE     ~ "TODOS"),
                                               DEPARTAMENTO = "", 
                                               URBANORURALUY = "URBANO (MÁS DE 5.000 HABITANTES)",
                                               EDAD =          "",
                                               SITUACION_HOGAR = "",
                                               RAMA_DE_ACTIVIDAD = "",
                                               TIPO_DE_OCUPACION = "",
                                               CATEGORÍOCUP = "",
                                               RESTRICCIONES_AL_EMPLEO = "",
                                               SEXO_JEFE       = "")

VALOR <- rbind (VALOR, VALOR_pu)
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Este indicador se calcula a partir de la encuesta telefónica del primer semestre de 2021 y el formulario telefónico de modalidad panel del segundo semestre de 2021. En el segundo semestre, la condición de pobreza y el quintil de ingresos del hogar corresponden a los ingresos declarados durante la implantación del panel en la encuesta presencial."


t_12 <- cbind(CODIND,	NOMINDICADOR,	PAIS,	FECHA,	FECHA2,	VALOR,	RESPONSABLE,	JERARQUIA,	SECTORPRODUCTIVO,	SECTORINSTITUCIONAL,	ACTIVIDADESECONOMICAS,	DIVISIÓNECONOMICA,	CONCEPTO,	RELACIONFAMILIAR, NOTA_INDICADOR)
t_12 <- subset(t_12, select=c(CODIND,	NOMINDICADOR,	SEXO,	ASCENDENCIA,	QUINTIL, DEPARTAMENTO, URBANORURALUY,	PAIS,	FECHA,	FECHA2,	VALOR,	RESPONSABLE,	JERARQUIA,	SECTORPRODUCTIVO,	SECTORINSTITUCIONAL,	ACTIVIDADESECONOMICAS,	DIVISIÓNECONOMICA,	CONCEPTO,	CATEGORÍOCUP,	RELACIONFAMILIAR,	EDAD, SITUACION_HOGAR, RAMA_DE_ACTIVIDAD, TIPO_DE_OCUPACION, RESTRICCIONES_AL_EMPLEO, SEXO_JEFE, NOTA_INDICADOR))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# 333 - Porcentaje de mayores de 65 años que no perciben ni jubilación ni pensión
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


## Total país

# Total

a_mes <- base_svy %>%
  srvyr::filter(tramo_edad == 5) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(nijubpens, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

c_ano <- as.data.frame(a_sem)
c_ano <- c_ano %>% dplyr::mutate(aux = 1)
c_ano <- c_ano %>% dplyr::rename(VALOR = a_sem)

# Ascendencia étnico racial

a_afro <- function(x) {
  x <- base_svy %>%
    filter(bd_e29_1 == x & tramo_edad == 5) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nijubpens, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_afro(x = i)
}     

c_afro <- as.data.frame(VALOR)
c_afro <- c_afro %>% dplyr::mutate(aux = c(2,3))
c_afro <- c_afro[,c("VALOR", "aux")]



# Quintil de ingresos

a_quintil <- function(x) {
  x <- base_svy %>%
    filter(quintilesy == x & tramo_edad == 5) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nijubpens, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:5){
  VALOR[i] <- a_quintil(x = i)
}     

c_quintil <- as.data.frame(VALOR)
c_quintil <- c_quintil %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil <- c_quintil[,c("VALOR", "aux")]

# Sexo

a_sexo <- function(x) {
  x <- base_svy %>%
    filter(bc_pe2 == x & tramo_edad == 5) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nijubpens, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_sexo(x = i)
}     

c_sexo <- as.data.frame(VALOR)
c_sexo <- c_sexo %>% dplyr::mutate(aux = c(9, 10))
c_sexo <- c_sexo[,c("VALOR", "aux")]



# Región

a_region <- function(x) {
  x <- base_svy %>%
    filter(REGION_4 == x & tramo_edad == 5) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nijubpens, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 3:4){
  VALOR[i] <- a_region(x = i)
}     


c_region <- as.data.frame(VALOR)
c_region <- as.data.frame(c_region[c(3,4),])
c_region <- c_region %>% dplyr::mutate(aux = c(16, 17))
c_region <- c_region %>% rename("VALOR" = "c_region[c(3, 4), ]")
c_region <- c_region[,c("VALOR", "aux")]


# Condición de pobreza del hogar

a_pobre <- function(x) {
  x <- base_svy %>%
    filter(pobre_06 == x & tramo_edad == 5) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nijubpens, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_pobre(x = i)
}     

c_pobre <- as.data.frame(VALOR)
c_pobre <- c_pobre %>% dplyr::mutate(aux = c(18, 19))
c_pobre <- c_pobre[,c("VALOR", "aux")]

## País Urbano

# Total

a_mes <- base_svy %>%
  srvyr::filter(tramo_edad == 5 & (REGION_4 == 1 | REGION_4 == 2)) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(nijubpens, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

c_ano_pu <- as.data.frame(a_sem)
c_ano_pu <- c_ano_pu %>% dplyr::mutate(aux = 1)
c_ano_pu <- c_ano_pu %>% dplyr::rename(VALOR = a_sem)

# Quintil de ingresos

a_quintil <- function(x) {
  x <- base_svy %>%
    filter(quintilesy == x & tramo_edad == 5 & (REGION_4 == 1 | REGION_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nijubpens, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:5){
  VALOR[i] <- a_quintil(x = i)
}     

c_quintil_pu <- as.data.frame(VALOR)
c_quintil_pu <- c_quintil_pu %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil_pu <- c_quintil_pu[,c("VALOR", "aux")]


# Sexo

a_sexo <- function(x) {
  x <- base_svy %>%
    filter(bc_pe2 == x & tramo_edad == 5 & (REGION_4 == 1 | REGION_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nijubpens, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_sexo(x = i)
}     


c_sexo_pu <- as.data.frame(VALOR)
c_sexo_pu <- c_sexo_pu %>% dplyr::mutate(aux = c(9, 10))
c_sexo_pu <- c_sexo_pu[,c("VALOR", "aux")]





CODIND       <- 333
NOMINDICADOR <- "PORCENTAJE DE MAYORES DE 65 AÑOS QUE NO PERCIBEN JUBILACIONES NI PENSIONES"

VALOR        <- rbind(c_ano, c_afro, c_quintil, c_sexo, c_region, c_pobre)
VALOR        <- VALOR  %>% dplyr::mutate(SEXO = case_when(aux == 9  ~ "VARONES", 
                                                          aux == 10 ~ "MUJERES", 
                                                          TRUE     ~ "TODOS"),
                                         ASCENDENCIA  = case_when(aux == 2 ~ "AFRO", 
                                                                  aux == 3 ~ "NO AFRO",
                                                                  TRUE     ~ "TODOS"), 
                                         QUINTIL =      case_when(aux == 4 ~ "QUINTIL 1",
                                                                  aux == 5 ~ "QUINTIL 2",
                                                                  aux == 6 ~ "QUINTIL 3",
                                                                  aux == 7 ~ "QUINTIL 4",
                                                                  aux == 8 ~ "QUINTIL 5",
                                                                  TRUE     ~ "TODOS"),
                                         DEPARTAMENTO = "", 
                                         URBANORURALUY = case_when(aux == 16 ~ "URBANO (MENOS DE 5.000 HABITANTES)",
                                                                   aux == 17 ~ "RURAL DISPERSO",
                                                                   TRUE     ~ "TOTAL PAÍS"),
                                         EDAD =          "",
                                         SITUACION_HOGAR = case_when(aux == 18 ~ "No en situación de pobreza",
                                                                     aux == 19  ~"En situación de pobreza",
                                                                     TRUE     ~ ""),
                                         RAMA_DE_ACTIVIDAD = "",
                                         TIPO_DE_OCUPACION = "",
                                         CATEGORÍOCUP = "",
                                         RESTRICCIONES_AL_EMPLEO = "",
                                         SEXO_JEFE       = "")



VALOR_pu        <- rbind(c_ano_pu, c_quintil_pu, c_sexo_pu)
VALOR_pu        <- VALOR_pu  %>% dplyr::mutate(SEXO = case_when(aux == 9  ~ "VARONES", 
                                                                aux == 10 ~ "MUJERES", 
                                                                TRUE     ~ "TODOS"),
                                               ASCENDENCIA  =  "TODOS", 
                                               QUINTIL =      case_when(aux == 4 ~ "QUINTIL 1",
                                                                        aux == 5 ~ "QUINTIL 2",
                                                                        aux == 6 ~ "QUINTIL 3",
                                                                        aux == 7 ~ "QUINTIL 4",
                                                                        aux == 8 ~ "QUINTIL 5",
                                                                        TRUE     ~ "TODOS"),
                                               DEPARTAMENTO = "", 
                                               URBANORURALUY = "URBANO (MÁS DE 5.000 HABITANTES)",
                                               EDAD =          "",
                                               SITUACION_HOGAR = "",
                                               RAMA_DE_ACTIVIDAD = "",
                                               TIPO_DE_OCUPACION = "",
                                               CATEGORÍOCUP = "",
                                               RESTRICCIONES_AL_EMPLEO = "",
                                               SEXO_JEFE       = "")

VALOR <- rbind (VALOR, VALOR_pu)
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Este indicador se calcula a partir de la encuesta telefónica del primer semestre de 2021 y el formulario telefónico de modalidad panel del segundo semestre de 2021. En el segundo semestre, la condición de pobreza y el quintil de ingresos del hogar corresponden a los ingresos declarados durante la implantación del panel en la encuesta presencial."


t_13 <- cbind(CODIND,	NOMINDICADOR,	PAIS,	FECHA,	FECHA2,	VALOR,	RESPONSABLE,	JERARQUIA,	SECTORPRODUCTIVO,	SECTORINSTITUCIONAL,	ACTIVIDADESECONOMICAS,	DIVISIÓNECONOMICA,	CONCEPTO,	RELACIONFAMILIAR, NOTA_INDICADOR)
t_13 <- subset(t_13, select=c(CODIND,	NOMINDICADOR,	SEXO,	ASCENDENCIA,	QUINTIL, DEPARTAMENTO, URBANORURALUY,	PAIS,	FECHA,	FECHA2,	VALOR,	RESPONSABLE,	JERARQUIA,	SECTORPRODUCTIVO,	SECTORINSTITUCIONAL,	ACTIVIDADESECONOMICAS,	DIVISIÓNECONOMICA,	CONCEPTO,	CATEGORÍOCUP,	RELACIONFAMILIAR,	EDAD, SITUACION_HOGAR, RAMA_DE_ACTIVIDAD, TIPO_DE_OCUPACION, RESTRICCIONES_AL_EMPLEO, SEXO_JEFE, NOTA_INDICADOR))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# 334 - Porcentaje de personas que perciben jubilaciones por debajo de la línea de pobreza
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


## Total país

# Total

a_mes <- base_svy %>%
  srvyr::filter(jub == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(jub_insuf, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

c_ano <- as.data.frame(a_sem)
c_ano <- c_ano %>% dplyr::mutate(aux = 1)
c_ano <- c_ano %>% dplyr::rename(VALOR = a_sem)

# Ascendencia étnico racial

a_afro <- function(x) {
  x <- base_svy %>%
    filter(bd_e29_1 == x & jub == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub_insuf, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_afro(x = i)
}     


c_afro <- as.data.frame(VALOR)
c_afro <- c_afro %>% dplyr::mutate(aux = c(2,3))
c_afro <- c_afro[,c("VALOR", "aux")]



# Quintil de ingresos

a_quintil <- function(x) {
  x <- base_svy %>%
    filter(quintilesy == x & jub == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub_insuf, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:5){
  VALOR[i] <- a_quintil(x = i)
}     

c_quintil <- as.data.frame(VALOR)
c_quintil <- c_quintil %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil <- c_quintil[,c("VALOR", "aux")]

# Sexo

a_sexo <- function(x) {
  x <- base_svy %>%
    filter(bc_pe2 == x & jub == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub_insuf, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_sexo(x = i)
}     

c_sexo <- as.data.frame(VALOR)
c_sexo <- c_sexo %>% dplyr::mutate(aux = c(9, 10))
c_sexo <- c_sexo[,c("VALOR", "aux")]



# Región

a_region <- function(x) {
  x <- base_svy %>%
    filter(REGION_4 == x & jub == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub_insuf, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 3:4){
  VALOR[i] <- a_region(x = i)
}     

c_region <- as.data.frame(VALOR)
c_region <- as.data.frame(c_region[c(3,4),])
c_region <- c_region %>% dplyr::mutate(aux = c(16, 17))
c_region <- c_region %>% rename("VALOR" = "c_region[c(3, 4), ]")
c_region <- c_region[,c("VALOR", "aux")]


# Condición de pobreza del hogar

a_pobre <- function(x) {
  x <- base_svy %>%
    filter(pobre_06 == x & jub == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub_insuf, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_pobre(x = i)
}     

c_pobre <- as.data.frame(VALOR)
c_pobre <- c_pobre %>% dplyr::mutate(aux = c(18, 19))
c_pobre <- c_pobre[,c("VALOR", "aux")]

## País Urbano

# Total

a_mes <- base_svy %>%
  srvyr::filter(jub == 1 & (REGION_4 == 1 | REGION_4 == 2)) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(jub_insuf, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

c_ano_pu <- as.data.frame(a_sem)
c_ano_pu <- c_ano_pu %>% dplyr::mutate(aux = 1)
c_ano_pu <- c_ano_pu %>% dplyr::rename(VALOR = a_sem)

# Quintil de ingresos

a_quintil <- function(x) {
  x <- base_svy %>%
    filter(quintilesy == x & jub == 1 & (REGION_4 == 1 | REGION_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub_insuf, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:5){
  VALOR[i] <- a_quintil(x = i)
}     


c_quintil_pu <- as.data.frame(VALOR)
c_quintil_pu <- c_quintil_pu %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil_pu <- c_quintil_pu[,c("VALOR", "aux")]


# Sexo

a_sexo <- function(x) {
  x <- base_svy %>%
    filter(bc_pe2 == x & jub == 1 & (REGION_4 == 1 | REGION_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub_insuf, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_sexo(x = i)
}     

c_sexo_pu <- as.data.frame(VALOR)
c_sexo_pu <- c_sexo_pu %>% dplyr::mutate(aux = c(9, 10))
c_sexo_pu <- c_sexo_pu[,c("VALOR", "aux")]





CODIND       <- 334
NOMINDICADOR <- "PORCENTAJE PERSONAS QUE PERCIBEN JUBILACIONES POR DEBAJO DE LA LÍNEA DE POBREZA"

VALOR        <- rbind(c_ano, c_afro, c_quintil, c_sexo, c_region, c_pobre)
VALOR        <- VALOR  %>% dplyr::mutate(SEXO = case_when(aux == 9  ~ "VARONES", 
                                                          aux == 10 ~ "MUJERES", 
                                                          TRUE     ~ "TODOS"),
                                          ASCENDENCIA  = case_when(aux == 2 ~ "AFRO", 
                                                                   aux == 3 ~ "NO AFRO",
                                                                   TRUE     ~ "TODOS"), 
                                         QUINTIL =      case_when(aux == 4 ~ "QUINTIL 1",
                                                                  aux == 5 ~ "QUINTIL 2",
                                                                  aux == 6 ~ "QUINTIL 3",
                                                                  aux == 7 ~ "QUINTIL 4",
                                                                  aux == 8 ~ "QUINTIL 5",
                                                                  TRUE     ~ "TODOS"),
                                         DEPARTAMENTO = "", 
                                         URBANORURALUY = case_when(aux == 16 ~ "URBANO (MENOS DE 5.000 HABITANTES)",
                                                                   aux == 17 ~ "RURAL DISPERSO",
                                                                   TRUE     ~ "TOTAL PAÍS"),
                                         EDAD =          "",
                                         SITUACION_HOGAR = case_when(aux == 18 ~ "No en situación de pobreza",
                                                                     aux == 19  ~"En situación de pobreza",
                                                                     TRUE     ~ ""),
                                         RAMA_DE_ACTIVIDAD = "",
                                         TIPO_DE_OCUPACION = "",
                                         CATEGORÍOCUP = "",
                                         RESTRICCIONES_AL_EMPLEO = "",
                                         SEXO_JEFE       = "")



VALOR_pu        <- rbind(c_ano_pu, c_quintil_pu, c_sexo_pu)
VALOR_pu        <- VALOR_pu  %>% dplyr::mutate(SEXO = case_when(aux == 9  ~ "VARONES", 
                                                                aux == 10 ~ "MUJERES", 
                                                                TRUE     ~ "TODOS"),
                                               ASCENDENCIA  =  "TODOS", 
                                               QUINTIL =      case_when(aux == 4 ~ "QUINTIL 1",
                                                                        aux == 5 ~ "QUINTIL 2",
                                                                        aux == 6 ~ "QUINTIL 3",
                                                                        aux == 7 ~ "QUINTIL 4",
                                                                        aux == 8 ~ "QUINTIL 5",
                                                                        TRUE     ~ "TODOS"),
                                               DEPARTAMENTO = "", 
                                               URBANORURALUY = "URBANO (MÁS DE 5.000 HABITANTES)",
                                               EDAD =          "",
                                               SITUACION_HOGAR = "",
                                               RAMA_DE_ACTIVIDAD = "",
                                               TIPO_DE_OCUPACION = "",
                                               CATEGORÍOCUP = "",
                                               RESTRICCIONES_AL_EMPLEO = "",
                                               SEXO_JEFE       = "")

VALOR <- rbind (VALOR, VALOR_pu)
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Este indicador se calcula a partir de la encuesta telefónica del primer semestre de 2021 y el formulario telefónico de modalidad panel del segundo semestre de 2021. En el segundo semestre, la condición de pobreza y el quintil de ingresos del hogar corresponden a los ingresos declarados durante la implantación del panel en la encuesta presencial."

t_14 <- cbind(CODIND,	NOMINDICADOR,	PAIS,	FECHA,	FECHA2,	VALOR,	RESPONSABLE,	JERARQUIA,	SECTORPRODUCTIVO,	SECTORINSTITUCIONAL,	ACTIVIDADESECONOMICAS,	DIVISIÓNECONOMICA,	CONCEPTO,	RELACIONFAMILIAR, NOTA_INDICADOR)
t_14 <- subset(t_14, select=c(CODIND,	NOMINDICADOR,	SEXO,	ASCENDENCIA,	QUINTIL, DEPARTAMENTO, URBANORURALUY,	PAIS,	FECHA,	FECHA2,	VALOR,	RESPONSABLE,	JERARQUIA,	SECTORPRODUCTIVO,	SECTORINSTITUCIONAL,	ACTIVIDADESECONOMICAS,	DIVISIÓNECONOMICA,	CONCEPTO,	CATEGORÍOCUP,	RELACIONFAMILIAR,	EDAD, SITUACION_HOGAR, RAMA_DE_ACTIVIDAD, TIPO_DE_OCUPACION, RESTRICCIONES_AL_EMPLEO, SEXO_JEFE, NOTA_INDICADOR))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# 335 - Porcentaje de personas que perciben pensiones por debajo de la línea de pobreza
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


## Total país

# Total

a_mes <- base_svy %>%
  srvyr::filter(pens == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(pens_insuf, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

c_ano <- as.data.frame(a_sem)
c_ano <- c_ano %>% dplyr::mutate(aux = 1)
c_ano <- c_ano %>% dplyr::rename(VALOR = a_sem)

# Ascendencia étnico racial

a_afro <- function(x) {
  x <- base_svy %>%
    filter(bd_e29_1 == x & pens == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens_insuf, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_afro(x = i)
}     

c_afro <- as.data.frame(VALOR)
c_afro <- c_afro %>% dplyr::mutate(aux = c(2,3))
c_afro <- c_afro[,c("VALOR", "aux")]



# Quintil de ingresos

a_quintil <- function(x) {
  x <- base_svy %>%
    filter(quintilesy == x & pens == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens_insuf, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:5){
  VALOR[i] <- a_quintil(x = i)
}     


c_quintil <- as.data.frame(VALOR)
c_quintil <- c_quintil %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil <- c_quintil[,c("VALOR", "aux")]

# Sexo

a_sexo <- function(x) {
  x <- base_svy %>%
    filter(bc_pe2 == x & pens == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens_insuf, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_sexo(x = i)
}     

c_sexo <- as.data.frame(VALOR)
c_sexo <- c_sexo %>% dplyr::mutate(aux = c(9, 10))
c_sexo <- c_sexo[,c("VALOR", "aux")]



# Región

a_region <- function(x) {
  x <- base_svy %>%
    filter(REGION_4 == x & pens == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens_insuf, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 3:4){
  VALOR[i] <- a_region(x = i)
}     

c_region <- as.data.frame(VALOR)
c_region <- as.data.frame(c_region[c(3,4),])
c_region <- c_region %>% dplyr::mutate(aux = c(16, 17))
c_region <- c_region %>% rename("VALOR" = "c_region[c(3, 4), ]")
c_region <- c_region[,c("VALOR", "aux")]


# Condición de pobreza del hogar

a_pobre <- function(x) {
  x <- base_svy %>%
    filter(pobre_06 == x & pens == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens_insuf, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_pobre(x = i)
}     

c_pobre <- as.data.frame(VALOR)
c_pobre <- c_pobre %>% dplyr::mutate(aux = c(18, 19))
c_pobre <- c_pobre[,c("VALOR", "aux")]

## País Urbano

# Total

a_mes <- base_svy %>%
  srvyr::filter(pens == 1 & (REGION_4 == 1 | REGION_4 == 2)) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(pens_insuf, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

c_ano_pu <- as.data.frame(a_sem)
c_ano_pu <- c_ano_pu %>% dplyr::mutate(aux = 1)
c_ano_pu <- c_ano_pu %>% dplyr::rename(VALOR = a_sem)

# Quintil de ingresos

a_quintil <- function(x) {
  x <- base_svy %>%
    filter(quintilesy == x & pens == 1 & (REGION_4 == 1 | REGION_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens_insuf, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:5){
  VALOR[i] <- a_quintil(x = i)
}     

c_quintil_pu <- as.data.frame(VALOR)
c_quintil_pu <- c_quintil_pu %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil_pu <- c_quintil_pu[,c("VALOR", "aux")]


# Sexo

a_sexo <- function(x) {
  x <- base_svy %>%
    filter(bc_pe2 == x & pens == 1 & (REGION_4 == 1 | REGION_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens_insuf, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_sexo(x = i)
}     

c_sexo_pu <- as.data.frame(VALOR)
c_sexo_pu <- c_sexo_pu %>% dplyr::mutate(aux = c(9, 10))
c_sexo_pu <- c_sexo_pu[,c("VALOR", "aux")]


CODIND       <- 335
NOMINDICADOR <- "PORCENTAJE DE PERSONAS QUE PERCIBEN PENSIONES POR DEBAJO DE LA LÍNEA DE POBREZA"

VALOR        <- rbind(c_ano, c_afro, c_quintil, c_sexo, c_region, c_pobre)
VALOR        <- VALOR  %>% dplyr::mutate(SEXO = case_when(aux == 9  ~ "VARONES", 
                                                          aux == 10 ~ "MUJERES", 
                                                          TRUE     ~ "TODOS"),
                                         ASCENDENCIA  = case_when(aux == 2 ~ "AFRO", 
                                                                  aux == 3 ~ "NO AFRO",
                                                                  TRUE     ~ "TODOS"), 
                                         QUINTIL =      case_when(aux == 4 ~ "QUINTIL 1",
                                                                  aux == 5 ~ "QUINTIL 2",
                                                                  aux == 6 ~ "QUINTIL 3",
                                                                  aux == 7 ~ "QUINTIL 4",
                                                                  aux == 8 ~ "QUINTIL 5",
                                                                  TRUE     ~ "TODOS"),
                                         DEPARTAMENTO = "", 
                                         URBANORURALUY = case_when(aux == 16 ~ "URBANO (MENOS DE 5.000 HABITANTES)",
                                                                   aux == 17 ~ "RURAL DISPERSO",
                                                                   TRUE     ~ "TOTAL PAÍS"),
                                         EDAD =          "",
                                         SITUACION_HOGAR = case_when(aux == 18 ~ "No en situación de pobreza",
                                                                     aux == 19  ~"En situación de pobreza",
                                                                     TRUE     ~ ""),
                                         RAMA_DE_ACTIVIDAD = "",
                                         TIPO_DE_OCUPACION = "",
                                         CATEGORÍOCUP = "",
                                         RESTRICCIONES_AL_EMPLEO = "",
                                         SEXO_JEFE       = "")


VALOR_pu        <- rbind(c_ano_pu, c_quintil_pu, c_sexo_pu)
VALOR_pu        <- VALOR_pu  %>% dplyr::mutate(SEXO = case_when(aux == 9  ~ "VARONES", 
                                                                aux == 10 ~ "MUJERES", 
                                                                TRUE     ~ "TODOS"),
                                               ASCENDENCIA  =  "TODOS", 
                                               QUINTIL =      case_when(aux == 4 ~ "QUINTIL 1",
                                                                        aux == 5 ~ "QUINTIL 2",
                                                                        aux == 6 ~ "QUINTIL 3",
                                                                        aux == 7 ~ "QUINTIL 4",
                                                                        aux == 8 ~ "QUINTIL 5",
                                                                        TRUE     ~ "TODOS"),
                                               DEPARTAMENTO = "", 
                                               URBANORURALUY = "URBANO (MÁS DE 5.000 HABITANTES)",
                                               EDAD =          "",
                                               SITUACION_HOGAR = "",
                                               RAMA_DE_ACTIVIDAD = "",
                                               TIPO_DE_OCUPACION = "",
                                               CATEGORÍOCUP = "",
                                               RESTRICCIONES_AL_EMPLEO = "",
                                               SEXO_JEFE       = "")

VALOR <- rbind (VALOR, VALOR_pu)
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Este indicador se calcula a partir de la encuesta telefónica del primer semestre de 2021 y el formulario telefónico de modalidad panel del segundo semestre de 2021. En el segundo semestre, la condición de pobreza y el quintil de ingresos del hogar corresponden a los ingresos declarados durante la implantación del panel en la encuesta presencial."

t_15 <- cbind(CODIND,	NOMINDICADOR,	PAIS,	FECHA,	FECHA2,	VALOR,	RESPONSABLE,	JERARQUIA,	SECTORPRODUCTIVO,	SECTORINSTITUCIONAL,	ACTIVIDADESECONOMICAS,	DIVISIÓNECONOMICA,	CONCEPTO,	RELACIONFAMILIAR, NOTA_INDICADOR)
t_15 <- subset(t_15, select=c(CODIND,	NOMINDICADOR,	SEXO,	ASCENDENCIA,	QUINTIL, DEPARTAMENTO, URBANORURALUY,	PAIS,	FECHA,	FECHA2,	VALOR,	RESPONSABLE,	JERARQUIA,	SECTORPRODUCTIVO,	SECTORINSTITUCIONAL,	ACTIVIDADESECONOMICAS,	DIVISIÓNECONOMICA,	CONCEPTO,	CATEGORÍOCUP,	RELACIONFAMILIAR,	EDAD, SITUACION_HOGAR, RAMA_DE_ACTIVIDAD, TIPO_DE_OCUPACION, RESTRICCIONES_AL_EMPLEO, SEXO_JEFE, NOTA_INDICADOR))



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# 336 - Porcentaje de personas que perciben jubilaciones por debajo del SMN
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


## Total país

# Total

a_mes <- base_svy %>%
  srvyr::filter(jub == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(jub_insuf_smn, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

c_ano <- as.data.frame(a_sem)
c_ano <- c_ano %>% dplyr::mutate(aux = 1)
c_ano <- c_ano %>% dplyr::rename(VALOR = a_sem)

# Ascendencia étnico racial

a_afro <- function(x) {
  x <- base_svy %>%
    filter(bd_e29_1 == x & jub == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_afro(x = i)
}     

c_afro <- as.data.frame(VALOR)
c_afro <- c_afro %>% dplyr::mutate(aux = c(2,3))
c_afro <- c_afro[,c("VALOR", "aux")]



# Quintil de ingresos

a_quintil <- function(x) {
  x <- base_svy %>%
    filter(quintilesy == x & jub == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:5){
  VALOR[i] <- a_quintil(x = i)
}     

c_quintil <- as.data.frame(VALOR)
c_quintil <- c_quintil %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil <- c_quintil[,c("VALOR", "aux")]

# Sexo

a_sexo <- function(x) {
  x <- base_svy %>%
    filter(bc_pe2 == x & jub == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_sexo(x = i)
}     

c_sexo <- as.data.frame(VALOR)
c_sexo <- c_sexo %>% dplyr::mutate(aux = c(9, 10))
c_sexo <- c_sexo[,c("VALOR", "aux")]



# Región

a_region <- function(x) {
  x <- base_svy %>%
    filter(REGION_4 == x & jub == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 3:4){
  VALOR[i] <- a_region(x = i)
}     

c_region <- as.data.frame(VALOR)
c_region <- as.data.frame(c_region[c(3,4),])
c_region <- c_region %>% dplyr::mutate(aux = c(16, 17))
c_region <- c_region %>% rename("VALOR" = "c_region[c(3, 4), ]")
c_region <- c_region[,c("VALOR", "aux")]

# Condición de pobreza del hogar

a_pobre <- function(x) {
  x <- base_svy %>%
    filter(pobre_06 == x & jub == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_pobre(x = i)
}     


c_pobre <- as.data.frame(VALOR)
c_pobre <- c_pobre %>% dplyr::mutate(aux = c(18, 19))
c_pobre <- c_pobre[,c("VALOR", "aux")]

## País Urbano

# Total

a_mes <- base_svy %>%
  srvyr::filter(jub == 1 & (REGION_4 == 1 | REGION_4 == 2)) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(jub_insuf_smn, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

c_ano_pu <- as.data.frame(a_sem)
c_ano_pu <- c_ano_pu %>% dplyr::mutate(aux = 1)
c_ano_pu <- c_ano_pu %>% dplyr::rename(VALOR = a_sem)

# Quintil de ingresos

a_quintil <- function(x) {
  x <- base_svy %>%
    filter(quintilesy == x & jub == 1 & (REGION_4 == 1 | REGION_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:5){
  VALOR[i] <- a_quintil(x = i)
}     

c_quintil_pu <- as.data.frame(VALOR)
c_quintil_pu <- c_quintil_pu %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil_pu <- c_quintil_pu[,c("VALOR", "aux")]


# Sexo

a_sexo <- function(x) {
  x <- base_svy %>%
    filter(bc_pe2 == x & jub == 1 & (REGION_4 == 1 | REGION_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_sexo(x = i)
}     

c_sexo_pu <- as.data.frame(VALOR)
c_sexo_pu <- c_sexo_pu %>% dplyr::mutate(aux = c(9, 10))
c_sexo_pu <- c_sexo_pu[,c("VALOR", "aux")]


CODIND       <- 336
NOMINDICADOR <- "PORCENTAJE PERSONAS QUE PERCIBEN JUBILACIONES POR DEBAJO DEL SALARIO MÍNIMO NACIONAL"

VALOR        <- rbind(c_ano, c_afro, c_quintil, c_sexo, c_region, c_pobre)
VALOR        <- VALOR  %>% dplyr::mutate(SEXO = case_when(aux == 9  ~ "VARONES", 
                                                          aux == 10 ~ "MUJERES", 
                                                          TRUE     ~ "TODOS"),
                                         ASCENDENCIA  = case_when(aux == 2 ~ "AFRO", 
                                                                  aux == 3 ~ "NO AFRO",
                                                                  TRUE     ~ "TODOS"), 
                                         QUINTIL =      case_when(aux == 4 ~ "QUINTIL 1",
                                                                  aux == 5 ~ "QUINTIL 2",
                                                                  aux == 6 ~ "QUINTIL 3",
                                                                  aux == 7 ~ "QUINTIL 4",
                                                                  aux == 8 ~ "QUINTIL 5",
                                                                  TRUE     ~ "TODOS"),
                                         DEPARTAMENTO = "", 
                                         URBANORURALUY = case_when(aux == 16 ~ "URBANO (MENOS DE 5.000 HABITANTES)",
                                                                   aux == 17 ~ "RURAL DISPERSO",
                                                                   TRUE     ~ "TOTAL PAÍS"),
                                         EDAD =          "",
                                         SITUACION_HOGAR = case_when(aux == 18 ~ "No en situación de pobreza",
                                                                     aux == 19  ~"En situación de pobreza",
                                                                     TRUE     ~ ""),
                                         RAMA_DE_ACTIVIDAD = "",
                                         TIPO_DE_OCUPACION = "",
                                         CATEGORÍOCUP = "",
                                         RESTRICCIONES_AL_EMPLEO = "",
                                         SEXO_JEFE       = "")


VALOR_pu        <- rbind(c_ano_pu, c_quintil_pu, c_sexo_pu)
VALOR_pu        <- VALOR_pu  %>% dplyr::mutate(SEXO = case_when(aux == 9  ~ "VARONES", 
                                                                aux == 10 ~ "MUJERES", 
                                                                TRUE     ~ "TODOS"),
                                               ASCENDENCIA  =  "TODOS", 
                                               QUINTIL =      case_when(aux == 4 ~ "QUINTIL 1",
                                                                        aux == 5 ~ "QUINTIL 2",
                                                                        aux == 6 ~ "QUINTIL 3",
                                                                        aux == 7 ~ "QUINTIL 4",
                                                                        aux == 8 ~ "QUINTIL 5",
                                                                        TRUE     ~ "TODOS"),
                                               DEPARTAMENTO = "", 
                                               URBANORURALUY = "URBANO (MÁS DE 5.000 HABITANTES)",
                                               EDAD =          "",
                                               SITUACION_HOGAR = "" ,
                                               RAMA_DE_ACTIVIDAD = "",
                                               TIPO_DE_OCUPACION = "",
                                               CATEGORÍOCUP = "",
                                               RESTRICCIONES_AL_EMPLEO = "",
                                               SEXO_JEFE       = "")

VALOR <- rbind (VALOR, VALOR_pu)
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Este indicador se calcula a partir de la encuesta telefónica del primer semestre de 2021 y el formulario telefónico de modalidad panel del segundo semestre de 2021. En el segundo semestre, la condición de pobreza y el quintil de ingresos del hogar corresponden a los ingresos declarados durante la implantación del panel en la encuesta presencial."

t_16 <- cbind(CODIND,	NOMINDICADOR,	PAIS,	FECHA,	FECHA2,	VALOR,	RESPONSABLE,	JERARQUIA,	SECTORPRODUCTIVO,	SECTORINSTITUCIONAL,	ACTIVIDADESECONOMICAS,	DIVISIÓNECONOMICA,	CONCEPTO,	RELACIONFAMILIAR, NOTA_INDICADOR)
t_16 <- subset(t_16, select=c(CODIND,	NOMINDICADOR,	SEXO,	ASCENDENCIA,	QUINTIL, DEPARTAMENTO, URBANORURALUY,	PAIS,	FECHA,	FECHA2,	VALOR,	RESPONSABLE,	JERARQUIA,	SECTORPRODUCTIVO,	SECTORINSTITUCIONAL,	ACTIVIDADESECONOMICAS,	DIVISIÓNECONOMICA,	CONCEPTO,	CATEGORÍOCUP,	RELACIONFAMILIAR,	EDAD, SITUACION_HOGAR, RAMA_DE_ACTIVIDAD, TIPO_DE_OCUPACION, RESTRICCIONES_AL_EMPLEO, SEXO_JEFE, NOTA_INDICADOR))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# 337 - Porcentaje de personas que perciben pensiones por debajo del SMN
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


## Total país

# Total

a_mes <- base_svy %>%
  srvyr::filter(pens == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(pens_insuf_smn, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

c_ano <- as.data.frame(a_sem)
c_ano <- c_ano %>% dplyr::mutate(aux = 1)
c_ano <- c_ano %>% dplyr::rename(VALOR = a_sem)

# Ascendencia étnico racial

a_afro <- function(x) {
  x <- base_svy %>%
    filter(bd_e29_1 == x & pens == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_afro(x = i)
}     

c_afro <- as.data.frame(VALOR)
c_afro <- c_afro %>% dplyr::mutate(aux = c(2,3))
c_afro <- c_afro[,c("VALOR", "aux")]



# Quintil de ingresos

a_quintil <- function(x) {
  x <- base_svy %>%
    filter(quintilesy == x & pens == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:5){
  VALOR[i] <- a_quintil(x = i)
}     

c_quintil <- as.data.frame(VALOR)
c_quintil <- c_quintil %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil <- c_quintil[,c("VALOR", "aux")]

# Sexo

a_sexo <- function(x) {
  x <- base_svy %>%
    filter(bc_pe2 == x & pens == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_sexo(x = i)
}     


c_sexo <- as.data.frame(VALOR)
c_sexo <- c_sexo %>% dplyr::mutate(aux = c(9, 10))
c_sexo <- c_sexo[,c("VALOR", "aux")]



# Región

a_region <- function(x) {
  x <- base_svy %>%
    filter(REGION_4 == x & pens == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 3:4){
  VALOR[i] <- a_region(x = i)
}     

c_region <- as.data.frame(VALOR)
c_region <- as.data.frame(c_region[c(3,4),])
c_region <- c_region %>% dplyr::mutate(aux = c(16, 17))
c_region <- c_region %>% rename("VALOR" = "c_region[c(3, 4), ]")
c_region <- c_region[,c("VALOR", "aux")]


# Condición de pobreza del hogar

a_pobre <- function(x) {
  x <- base_svy %>%
    filter(pobre_06 == x & pens == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_pobre(x = i)
}     

c_pobre <- as.data.frame(VALOR)
c_pobre <- c_pobre %>% dplyr::mutate(aux = c(18, 19))
c_pobre <- c_pobre[,c("VALOR", "aux")]

## País Urbano

# Total

a_mes <- base_svy %>%
  srvyr::filter(pens == 1 & (REGION_4 == 1 | REGION_4 == 2)) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(pens_insuf_smn, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

c_ano_pu <- as.data.frame(a_sem)
c_ano_pu <- c_ano_pu %>% dplyr::mutate(aux = 1)
c_ano_pu <- c_ano_pu %>% dplyr::rename(VALOR = a_sem)

# Quintil de ingresos

a_quintil <- function(x) {
  x <- base_svy %>%
    filter(quintilesy == x & pens == 1 & (REGION_4 == 1 | REGION_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:5){
  VALOR[i] <- a_quintil(x = i)
}     


c_quintil_pu <- as.data.frame(VALOR)
c_quintil_pu <- c_quintil_pu %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil_pu <- c_quintil_pu[,c("VALOR", "aux")]


# Sexo

a_sexo <- function(x) {
  x <- base_svy %>%
    filter(bc_pe2 == x & pens == 1 & (REGION_4 == 1 | REGION_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_sexo(x = i)
}     

c_sexo_pu <- as.data.frame(VALOR)
c_sexo_pu <- c_sexo_pu %>% dplyr::mutate(aux = c(9, 10))
c_sexo_pu <- c_sexo_pu[,c("VALOR", "aux")]



CODIND       <- 337
NOMINDICADOR <- "PORCENTAJE PERSONAS QUE PERCIBEN PENSIONES POR DEBAJO DEL SALARIO MÍNIMO NACIONAL"

VALOR        <- rbind(c_ano, c_afro, c_quintil, c_sexo, c_region, c_pobre)
VALOR        <- VALOR  %>% dplyr::mutate(SEXO = case_when(aux == 9  ~ "VARONES", 
                                                          aux == 10 ~ "MUJERES", 
                                                          TRUE     ~ "TODOS"),
                                        ASCENDENCIA  = case_when(aux == 2 ~ "AFRO", 
                                                                 aux == 3 ~ "NO AFRO",
                                                                 TRUE     ~ "TODOS"), 
                                         QUINTIL =      case_when(aux == 4 ~ "QUINTIL 1",
                                                                  aux == 5 ~ "QUINTIL 2",
                                                                  aux == 6 ~ "QUINTIL 3",
                                                                  aux == 7 ~ "QUINTIL 4",
                                                                  aux == 8 ~ "QUINTIL 5",
                                                                  TRUE     ~ "TODOS"),
                                         DEPARTAMENTO = "", 
                                         URBANORURALUY = case_when(aux == 16 ~ "URBANO (MENOS DE 5.000 HABITANTES)",
                                                                   aux == 17 ~ "RURAL DISPERSO",
                                                                   TRUE     ~ "TOTAL PAÍS"),
                                         EDAD =          "",
                                         SITUACION_HOGAR = case_when(aux == 18 ~ "No en situación de pobreza",
                                                                     aux == 19  ~"En situación de pobreza",
                                                                     TRUE     ~ ""),
                                        RAMA_DE_ACTIVIDAD = "",
                                        TIPO_DE_OCUPACION = "",
                                        CATEGORÍOCUP = "",
                                        RESTRICCIONES_AL_EMPLEO = "",
                                        SEXO_JEFE       = "")


VALOR_pu        <- rbind(c_ano_pu, c_quintil_pu, c_sexo_pu)
VALOR_pu        <- VALOR_pu  %>% dplyr::mutate(SEXO = case_when(aux == 9  ~ "VARONES", 
                                                                aux == 10 ~ "MUJERES", 
                                                                TRUE     ~ "TODOS"),
                                               ASCENDENCIA  =  "TODOS", 
                                               QUINTIL =      case_when(aux == 4 ~ "QUINTIL 1",
                                                                        aux == 5 ~ "QUINTIL 2",
                                                                        aux == 6 ~ "QUINTIL 3",
                                                                        aux == 7 ~ "QUINTIL 4",
                                                                        aux == 8 ~ "QUINTIL 5",
                                                                        TRUE     ~ "TODOS"),
                                               DEPARTAMENTO = "", 
                                               URBANORURALUY = "URBANO (MÁS DE 5.000 HABITANTES)",
                                               EDAD =          "",
                                               SITUACION_HOGAR = "",
                                               RAMA_DE_ACTIVIDAD = "",
                                               TIPO_DE_OCUPACION = "",
                                               CATEGORÍOCUP = "",
                                               RESTRICCIONES_AL_EMPLEO = "",
                                               SEXO_JEFE       = "")

VALOR <- rbind (VALOR, VALOR_pu)
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Este indicador se calcula a partir de la encuesta telefónica del primer semestre de 2021 y el formulario telefónico de modalidad panel del segundo semestre de 2021. En el segundo semestre, la condición de pobreza y el quintil de ingresos del hogar corresponden a los ingresos declarados durante la implantación del panel en la encuesta presencial."


t_17 <- cbind(CODIND,	NOMINDICADOR,	PAIS,	FECHA,	FECHA2,	VALOR,	RESPONSABLE,	JERARQUIA,	SECTORPRODUCTIVO,	SECTORINSTITUCIONAL,	ACTIVIDADESECONOMICAS,	DIVISIÓNECONOMICA,	CONCEPTO,	RELACIONFAMILIAR, NOTA_INDICADOR)
t_17 <- subset(t_17, select=c(CODIND,	NOMINDICADOR,	SEXO,	ASCENDENCIA,	QUINTIL, DEPARTAMENTO, URBANORURALUY,	PAIS,	FECHA,	FECHA2,	VALOR,	RESPONSABLE,	JERARQUIA,	SECTORPRODUCTIVO,	SECTORINSTITUCIONAL,	ACTIVIDADESECONOMICAS,	DIVISIÓNECONOMICA,	CONCEPTO,	CATEGORÍOCUP,	RELACIONFAMILIAR,	EDAD, SITUACION_HOGAR, RAMA_DE_ACTIVIDAD, TIPO_DE_OCUPACION, RESTRICCIONES_AL_EMPLEO, SEXO_JEFE, NOTA_INDICADOR))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# 338 - Porcentaje de desecupados que perciben seguro de desempleo
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #



## Total país


# Total

a_mes <- base_men_svy %>%
  srvyr::filter(desocup == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(seguroparo, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

c_ano <- as.data.frame(a_sem)
c_ano <- c_ano %>% dplyr::mutate(aux = 1)
c_ano <- c_ano %>% dplyr::rename(VALOR = a_sem)

# Ascendencia étnico racial

a_afro <- function(x) {
  x <- base_men_svy %>%
    filter(bd_e29_1 == x & desocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(seguroparo, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_afro(x = i)
}     

c_afro <- as.data.frame(VALOR)
c_afro <- c_afro %>% dplyr::mutate(aux = c(2,3))
c_afro <- c_afro[,c("VALOR", "aux")]



# Quintil de ingresos

a_quintil <- function(x) {
  x <- base_men_svy %>%
    filter(quintilesy == x & desocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(seguroparo, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:5){
  VALOR[i] <- a_quintil(x = i)
}     


c_quintil <- as.data.frame(VALOR)
c_quintil <- c_quintil %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil <- c_quintil[,c("VALOR", "aux")]

# Sexo

a_sexo <- function(x) {
  x <- base_men_svy %>%
    filter(bc_pe2 == x & desocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(seguroparo, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_sexo(x = i)
}     


c_sexo <- as.data.frame(VALOR)
c_sexo <- c_sexo %>% dplyr::mutate(aux = c(9, 10))
c_sexo <- c_sexo[,c("VALOR", "aux")]

# Tramo de edad

a_edad <- function(x) {
  x <- base_men_svy %>%
    filter(tramo_edad == x & desocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(seguroparo, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:5){
  VALOR[i] <- a_edad(x = i)
}     

c_edad <- as.data.frame(VALOR)
c_edad <- c_edad %>% dplyr::mutate(aux = c(11, 12, 13, 14, 15))
c_edad <- c_edad[,c("VALOR", "aux")]



# Región

a_region <- function(x) {
  x <- base_men_svy %>%
    filter(REGION_4 == x & desocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(seguroparo, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 3:4){
  VALOR[i] <- a_region(x = i)
}     

c_region <- as.data.frame(VALOR)
c_region <- as.data.frame(c_region[c(3,4),])
c_region <- c_region %>% dplyr::mutate(aux = c(16, 17))
c_region <- c_region %>% rename("VALOR" = "c_region[c(3, 4), ]")
c_region <- c_region[,c("VALOR", "aux")]


# Condición de pobreza del hogar

a_pobre <- function(x) {
  x <- base_men_svy %>%
    filter(pobre_06 == x & desocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(seguroparo, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_pobre(x = i)
}     

c_pobre <- as.data.frame(VALOR)
c_pobre <- c_pobre %>% dplyr::mutate(aux = c(18, 19))
c_pobre <- c_pobre[,c("VALOR", "aux")]


## País Urbano

# Total

a_mes <- base_men_svy %>%
  srvyr::filter(desocup == 1 & (REGION_4 == 1 | REGION_4 == 2)) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(seguroparo, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

c_ano_pu <- as.data.frame(a_sem)
c_ano_pu <- c_ano_pu %>% dplyr::mutate(aux = 1)
c_ano_pu <- c_ano_pu %>% dplyr::rename(VALOR = a_sem)

# Quintil de ingresos

a_quintil <- function(x) {
  x <- base_men_svy %>%
    filter(quintilesy == x & desocup == 1 & (REGION_4 == 1 | REGION_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(seguroparo, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:5){
  VALOR[i] <- a_quintil(x = i)
}     



c_quintil_pu <- as.data.frame(VALOR)
c_quintil_pu <- c_quintil_pu %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil_pu <- c_quintil_pu[,c("VALOR", "aux")]


# Sexo

a_sexo <- function(x) {
  x <- base_men_svy %>%
    filter(bc_pe2 == x & desocup == 1 & (REGION_4 == 1 | REGION_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(seguroparo, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_sexo(x = i)
}     

c_sexo_pu <- as.data.frame(VALOR)
c_sexo_pu <- c_sexo_pu %>% dplyr::mutate(aux = c(9, 10))
c_sexo_pu <- c_sexo_pu[,c("VALOR", "aux")]


# Tramo de edad

a_edad <- function(x) {
  x <- base_men_svy %>%
    filter(tramo_edad == x & desocup == 1 & (REGION_4 == 1 | REGION_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(seguroparo, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:5){
  VALOR[i] <- a_edad(x = i)
}     


c_edad_pu <- as.data.frame(VALOR)
c_edad_pu <- c_edad_pu %>% dplyr::mutate(aux = c(11, 12, 13, 14, 15))
c_edad_pu <- c_edad_pu[,c("VALOR", "aux")]



CODIND       <- 338
NOMINDICADOR <- "PORCENTAJE DE DESOCUPADOS QUE PERCIBEN SEGURO DE DESEMPLEO"

VALOR        <- rbind(c_ano, c_afro, c_quintil, c_sexo, c_edad, c_region, c_pobre)
VALOR        <- VALOR  %>% dplyr::mutate(SEXO = case_when(aux == 9  ~ "VARONES", 
                                                          aux == 10 ~ "MUJERES", 
                                                          TRUE     ~ "TODOS"),
                                         ASCENDENCIA  = case_when(aux == 2 ~ "AFRO", 
                                                                  aux == 3 ~ "NO AFRO",
                                                                  TRUE     ~ "TODOS"), 
                                         QUINTIL =      case_when(aux == 4 ~ "QUINTIL 1",
                                                                  aux == 5 ~ "QUINTIL 2",
                                                                  aux == 6 ~ "QUINTIL 3",
                                                                  aux == 7 ~ "QUINTIL 4",
                                                                  aux == 8 ~ "QUINTIL 5",
                                                                  TRUE     ~ "TODOS"),
                                         DEPARTAMENTO = "", 
                                         URBANORURALUY = case_when(aux == 16 ~ "URBANO (MENOS DE 5.000 HABITANTES)",
                                                                   aux == 17 ~ "RURAL DISPERSO",
                                                                   TRUE     ~ "TOTAL PAÍS"),
                                         EDAD =          case_when(aux == 11 ~ "14 a 18",
                                                                   aux == 12 ~ "19 a 24",
                                                                   aux == 13 ~ "25 a 29",
                                                                   aux == 14 ~ "30 a 64",
                                                                   aux == 15 ~ "65 y más",
                                                                   TRUE     ~ ""),
                                         SITUACION_HOGAR = case_when(aux == 18 ~ "No en situación de pobreza",
                                                                     aux == 19  ~"En situación de pobreza",
                                                                     TRUE     ~ ""),
                                         RAMA_DE_ACTIVIDAD = "",
                                         TIPO_DE_OCUPACION = "",
                                         CATEGORÍOCUP = "",
                                         RESTRICCIONES_AL_EMPLEO = "",
                                         SEXO_JEFE       = "")


VALOR_pu        <- rbind(c_ano_pu, c_quintil_pu, c_sexo_pu, c_edad_pu)
VALOR_pu        <- VALOR_pu  %>% dplyr::mutate(SEXO = case_when(aux == 9  ~ "VARONES", 
                                                                aux == 10 ~ "MUJERES", 
                                                                TRUE     ~ "TODOS"),
                                               ASCENDENCIA  =  "TODOS", 
                                               QUINTIL =      case_when(aux == 4 ~ "QUINTIL 1",
                                                                        aux == 5 ~ "QUINTIL 2",
                                                                        aux == 6 ~ "QUINTIL 3",
                                                                        aux == 7 ~ "QUINTIL 4",
                                                                        aux == 8 ~ "QUINTIL 5",
                                                                        TRUE     ~ "TODOS"),
                                               DEPARTAMENTO = "", 
                                               URBANORURALUY = "URBANO (MÁS DE 5.000 HABITANTES)",
                                               EDAD =          case_when(aux == 11 ~ "14 a 18",
                                                                         aux == 12 ~ "19 a 24",
                                                                         aux == 13 ~ "25 a 29",
                                                                         aux == 14 ~ "30 a 64",
                                                                         aux == 15 ~ "65 y más",
                                                                         TRUE     ~ ""),
                                               SITUACION_HOGAR = "",
                                               RAMA_DE_ACTIVIDAD = "",
                                               TIPO_DE_OCUPACION = "",
                                               CATEGORÍOCUP = "",
                                               RESTRICCIONES_AL_EMPLEO = "",
                                               SEXO_JEFE       = "")

VALOR <- rbind (VALOR, VALOR_pu)
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Este indicador se calcula a partir de la encuesta telefónica del primer semestre de 2021 y el formulario telefónico de modalidad panel del segundo semestre de 2021. En el segundo semestre, la condición de pobreza y el quintil de ingresos del hogar corresponden a los ingresos declarados durante la implantación del panel en la encuesta presencial."


t_18 <- cbind(CODIND,	NOMINDICADOR,	PAIS,	FECHA,	FECHA2,	VALOR,	RESPONSABLE,	JERARQUIA,	SECTORPRODUCTIVO,	SECTORINSTITUCIONAL,	ACTIVIDADESECONOMICAS,	DIVISIÓNECONOMICA,	CONCEPTO,	RELACIONFAMILIAR, NOTA_INDICADOR)
t_18 <- subset(t_18, select=c(CODIND,	NOMINDICADOR,	SEXO,	ASCENDENCIA,	QUINTIL, DEPARTAMENTO, URBANORURALUY,	PAIS,	FECHA,	FECHA2,	VALOR,	RESPONSABLE,	JERARQUIA,	SECTORPRODUCTIVO,	SECTORINSTITUCIONAL,	ACTIVIDADESECONOMICAS,	DIVISIÓNECONOMICA,	CONCEPTO,	CATEGORÍOCUP,	RELACIONFAMILIAR,	EDAD, SITUACION_HOGAR, RAMA_DE_ACTIVIDAD, TIPO_DE_OCUPACION, RESTRICCIONES_AL_EMPLEO, SEXO_JEFE, NOTA_INDICADOR))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# 339 - Porcentaje de hogares que perciben AFAM
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #



## Total país


# Total

a_mes <- base_h_svy %>%
  srvyr::summarise(colname = srvyr::survey_mean(h_afam_total, na.rm=T))

VALOR <- mean(as.numeric(a_mes$colname))

c_ano <- as.data.frame(VALOR)
c_ano <- c_ano %>% dplyr::mutate(aux = 1)
c_ano<- select(c_ano, c(VALOR, aux))


# Quintil de ingresos

a_quintil <- function(x) {
  x <- base_h_svy %>%
    filter(quintilesy == x) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(h_afam_total, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:5){
  VALOR[i] <- a_quintil(x = i)
}     


c_quintil <- as.data.frame(VALOR)
c_quintil <- c_quintil %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil <- c_quintil[,c("VALOR", "aux")]


# Región

a_region <- function(x) {
  x <- base_h_svy %>%
    filter(REGION_4 == x) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(h_afam_total, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 3:4){
  VALOR[i] <- a_region(x = i)
}     

c_region <- as.data.frame(VALOR)
c_region <- as.data.frame(c_region[c(3,4),])
c_region <- c_region %>% dplyr::mutate(aux = c(16, 17))
c_region <- c_region %>% rename("VALOR" = "c_region[c(3, 4), ]")
c_region <- c_region[,c("VALOR", "aux")]

# Sexo de jefe/a hogar

a_sexojefe <- function(x) {
  x <- base_h_svy %>%
    filter(sexojefe == x) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(h_afam_total, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_sexojefe(x = i)
}     

c_sexojefe <- as.data.frame(VALOR)
c_sexojefe <- c_sexojefe %>% dplyr::mutate(aux = c(20, 21))
c_sexojefe <- c_sexojefe[,c("VALOR", "aux")]

# Condición de pobreza del hogar

a_pobre <- function(x) {
  x <- base_h_svy %>%
    filter(pobre_06 == x) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(h_afam_total, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_pobre(x = i)
}     

c_pobre <- as.data.frame(VALOR)
c_pobre <- c_pobre %>% dplyr::mutate(aux = c(18, 19))
c_pobre <- c_pobre[,c("VALOR", "aux")]

# País Urbano

a_mes <- base_h_svy %>%
  filter(REGION_4 == 1 | REGION_4 == 2) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(h_afam_total, na.rm=T))

VALOR <- mean(as.numeric(a_mes$colname))
c_ano_pu <- as.data.frame(VALOR)


c_ano_pu <- c_ano_pu %>% dplyr::mutate(aux = 1)
c_ano_pu<- select(c_ano_pu, c(VALOR, aux))




CODIND       <- 339
NOMINDICADOR <- "PORCENTAJE DE HOGARES QUE PERCIBEN ASIGNACIONES FAMILIARES"

VALOR        <- rbind(c_ano, c_quintil, c_region, c_pobre, c_sexojefe)
VALOR        <- VALOR  %>% dplyr::mutate(SEXO = "",
                                         ASCENDENCIA  = "", 
                                         QUINTIL =      case_when(aux == 4 ~ "QUINTIL 1",
                                                                  aux == 5 ~ "QUINTIL 2",
                                                                  aux == 6 ~ "QUINTIL 3",
                                                                  aux == 7 ~ "QUINTIL 4",
                                                                  aux == 8 ~ "QUINTIL 5",
                                                                  TRUE     ~ "TODOS"),
                                         DEPARTAMENTO = "", 
                                         URBANORURALUY = case_when(aux == 16 ~ "URBANO (MENOS DE 5.000 HABITANTES)",
                                                                   aux == 17 ~ "RURAL DISPERSO",
                                                                   TRUE     ~ "TOTAL PAÍS"),
                                         EDAD =         "", 
                                         SITUACION_HOGAR = case_when(aux == 18 ~ "No en situación de pobreza",
                                                                     aux == 19  ~"En situación de pobreza",
                                                                     TRUE     ~ ""), 
                                         SEXO_JEFE       = case_when(aux == 20 ~ "Jefe varón",
                                                                     aux == 21  ~"Jefa mujer",
                                                                     TRUE     ~ ""),
                                         RAMA_DE_ACTIVIDAD = "",
                                         TIPO_DE_OCUPACION = "",
                                         CATEGORÍOCUP = "",
                                         RESTRICCIONES_AL_EMPLEO = "")

VALOR_pu        <- rbind(c_ano_pu)
VALOR_pu        <- VALOR_pu  %>% dplyr::mutate(SEXO = "",
                                               ASCENDENCIA  =  "", 
                                               QUINTIL =      "TODOS",
                                               DEPARTAMENTO = "", 
                                               URBANORURALUY = "URBANO (MÁS DE 5.000 HABITANTES)",
                                               EDAD =          "",
                                               SITUACION_HOGAR = "",
                                               RAMA_DE_ACTIVIDAD = "",
                                               TIPO_DE_OCUPACION = "",
                                               CATEGORÍOCUP = "",
                                               RESTRICCIONES_AL_EMPLEO = "",
                                               SEXO_JEFE       = "")

VALOR <- rbind (VALOR, VALOR_pu)

NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Este indicador se calcula a partir de la encuesta telefónica del primer semestre de 2021 y el formulario telefónico de modalidad panel del segundo semestre de 2021. En el segundo semestre, la condición de pobreza y el quintil de ingresos del hogar corresponden a los ingresos declarados durante la implantación del panel en la encuesta presencial."

t_19 <- cbind(CODIND,	NOMINDICADOR,	PAIS,	FECHA,	FECHA2,	VALOR,	RESPONSABLE,	JERARQUIA,	SECTORPRODUCTIVO,	SECTORINSTITUCIONAL,	ACTIVIDADESECONOMICAS,	DIVISIÓNECONOMICA,	CONCEPTO,	RELACIONFAMILIAR, NOTA_INDICADOR)
t_19 <- subset(t_19, select=c(CODIND,	NOMINDICADOR,	SEXO,	ASCENDENCIA,	QUINTIL, DEPARTAMENTO, URBANORURALUY,	PAIS,	FECHA,	FECHA2,	VALOR,	RESPONSABLE,	JERARQUIA,	SECTORPRODUCTIVO,	SECTORINSTITUCIONAL,	ACTIVIDADESECONOMICAS,	DIVISIÓNECONOMICA,	CONCEPTO,	CATEGORÍOCUP,	RELACIONFAMILIAR,	EDAD, SITUACION_HOGAR, RAMA_DE_ACTIVIDAD, TIPO_DE_OCUPACION, RESTRICCIONES_AL_EMPLEO, SEXO_JEFE, NOTA_INDICADOR))



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# 340 - Porcentaje de hogares que perciben AFAM (Solo Plan de Equidad)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #



## Total país


# Total


a_mes <- base_h_svy %>%
  srvyr::summarise(colname = srvyr::survey_mean(h_afam_pe, na.rm=T))

VALOR <- mean(as.numeric(a_mes$colname))

c_ano <- as.data.frame(VALOR)
c_ano <- c_ano %>% dplyr::mutate(aux = 1)
c_ano<- select(c_ano, c(VALOR, aux))



# Quintil de ingresos

a_quintil <- function(x) {
  x <- base_h_svy %>%
    filter(quintilesy == x) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(h_afam_pe, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:5){
  VALOR[i] <- a_quintil(x = i)
}     

c_quintil <- as.data.frame(VALOR)
c_quintil <- c_quintil %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil <- c_quintil[,c("VALOR", "aux")]


# Región

a_region <- function(x) {
  x <- base_h_svy %>%
    filter(REGION_4 == x) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(h_afam_pe, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 3:4){
  VALOR[i] <- a_region(x = i)
}     

c_region <- as.data.frame(VALOR)
c_region <- as.data.frame(c_region[c(3,4),])
c_region <- c_region %>% dplyr::mutate(aux = c(16, 17))
c_region <- c_region %>% rename("VALOR" = "c_region[c(3, 4), ]")
c_region <- c_region[,c("VALOR", "aux")]

# Sexo de jefe/a hogar

a_sexojefe <- function(x) {
  x <- base_h_svy %>%
    filter(sexojefe == x) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(h_afam_pe, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_sexojefe(x = i)
}     

c_sexojefe <- as.data.frame(VALOR)
c_sexojefe <- c_sexojefe %>% dplyr::mutate(aux = c(20, 21))
c_sexojefe <- c_sexojefe[,c("VALOR", "aux")]

# Condición de pobreza del hogar

a_pobre <- function(x) {
  x <- base_h_svy %>%
    filter(pobre_06 == x) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(h_afam_pe, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_pobre(x = i)
}     

c_pobre <- as.data.frame(VALOR)
c_pobre <- c_pobre %>% dplyr::mutate(aux = c(18, 19))
c_pobre <- c_pobre[,c("VALOR", "aux")]


# País Urbano

a_mes <- base_h_svy %>%
  filter(REGION_4 == 1 | REGION_4 == 2) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(h_afam_pe, na.rm=T))

VALOR <- mean(as.numeric(a_mes$colname))
c_ano_pu <- as.data.frame(VALOR)

c_ano_pu <- c_ano_pu %>% dplyr::mutate(aux = 1)
c_ano_pu<- select(c_ano_pu, c(VALOR, aux))

CODIND       <- 340
NOMINDICADOR <- "PORCENTAJE DE HOGARES QUE PERCIBEN ASIGNACIONES FAMILIARES (SOLO PLAN DE EQUIDAD)"

VALOR        <- rbind(c_ano,c_quintil, c_region, c_pobre, c_sexojefe)
VALOR        <- VALOR  %>% dplyr::mutate(SEXO = "",
                                         ASCENDENCIA  = "", 
                                         QUINTIL =      case_when(aux == 4 ~ "QUINTIL 1",
                                                                  aux == 5 ~ "QUINTIL 2",
                                                                  aux == 6 ~ "QUINTIL 3",
                                                                  aux == 7 ~ "QUINTIL 4",
                                                                  aux == 8 ~ "QUINTIL 5",
                                                                  TRUE     ~ "TODOS"),
                                         DEPARTAMENTO = "", 
                                         URBANORURALUY = case_when(aux == 16 ~ "URBANO (MENOS DE 5.000 HABITANTES)",
                                                                   aux == 17 ~ "RURAL DISPERSO",
                                                                   TRUE     ~ "TOTAL PAÍS"),
                                         EDAD =         "", 
                                         SITUACION_HOGAR = case_when(aux == 18 ~ "No en situación de pobreza",
                                                                     aux == 19  ~"En situación de pobreza",
                                                                     TRUE     ~ ""), 
                                         SEXO_JEFE       = case_when(aux == 20 ~ "Jefe varón",
                                                                     aux == 21  ~"Jefa mujer",
                                                                     TRUE     ~ ""),
                                         RAMA_DE_ACTIVIDAD = "",
                                         TIPO_DE_OCUPACION = "",
                                         CATEGORÍOCUP = "",
                                         RESTRICCIONES_AL_EMPLEO = "")

VALOR_pu        <- rbind(c_ano_pu)
VALOR_pu        <- VALOR_pu  %>% dplyr::mutate(SEXO = "",
                                               ASCENDENCIA  =  "", 
                                               QUINTIL =      "TODOS",
                                               DEPARTAMENTO = "", 
                                               URBANORURALUY = "URBANO (MÁS DE 5.000 HABITANTES)",
                                               EDAD =          "",
                                               SITUACION_HOGAR = "",
                                               RAMA_DE_ACTIVIDAD = "",
                                               TIPO_DE_OCUPACION = "",
                                               CATEGORÍOCUP = "",
                                               RESTRICCIONES_AL_EMPLEO = "",
                                               SEXO_JEFE       = "")

VALOR <- rbind (VALOR, VALOR_pu)
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Este indicador se calcula a partir de la encuesta telefónica del primer semestre de 2021 y el formulario telefónico de modalidad panel del segundo semestre de 2021. En el segundo semestre, la condición de pobreza y el quintil de ingresos del hogar corresponden a los ingresos declarados durante la implantación del panel en la encuesta presencial."

t_20 <- cbind(CODIND,	NOMINDICADOR,	PAIS,	FECHA,	FECHA2,	VALOR,	RESPONSABLE,	JERARQUIA,	SECTORPRODUCTIVO,	SECTORINSTITUCIONAL,	ACTIVIDADESECONOMICAS,	DIVISIÓNECONOMICA,	CONCEPTO,	RELACIONFAMILIAR, NOTA_INDICADOR)
t_20 <- subset(t_20, select=c(CODIND,	NOMINDICADOR,	SEXO,	ASCENDENCIA,	QUINTIL, DEPARTAMENTO, URBANORURALUY,	PAIS,	FECHA,	FECHA2,	VALOR,	RESPONSABLE,	JERARQUIA,	SECTORPRODUCTIVO,	SECTORINSTITUCIONAL,	ACTIVIDADESECONOMICAS,	DIVISIÓNECONOMICA,	CONCEPTO,	CATEGORÍOCUP,	RELACIONFAMILIAR,	EDAD, SITUACION_HOGAR, RAMA_DE_ACTIVIDAD, TIPO_DE_OCUPACION, RESTRICCIONES_AL_EMPLEO, SEXO_JEFE, NOTA_INDICADOR))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# 341 - Porcentaje de hogares que perciben TUS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #



## Total país


# Total

a_mes <- base_h_svy %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(h_tus, na.rm=T))

VALOR <- mean(as.numeric(a_mes$colname))
c_ano <- as.data.frame(VALOR)


c_ano <- c_ano %>% dplyr::mutate(aux = 1)
c_ano<- select(c_ano, c(VALOR, aux))

# Quintil de ingresos

a_quintil <- function(x) {
  x <- base_h_svy %>%
    filter(quintilesy == x) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(h_tus, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:5){
  VALOR[i] <- a_quintil(x = i)
}     

c_quintil <- as.data.frame(VALOR)
c_quintil <- c_quintil %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil <- c_quintil[,c("VALOR", "aux")]


# Región

a_region <- function(x) {
  x <- base_h_svy %>%
    filter(REGION_4 == x) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(h_tus, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 3:4){
  VALOR[i] <- a_region(x = i)
}     


c_region <- as.data.frame(VALOR)
c_region <- as.data.frame(c_region[c(3,4),])
c_region <- c_region %>% dplyr::mutate(aux = c(16, 17))
c_region <- c_region %>% rename("VALOR" = "c_region[c(3, 4), ]")
c_region <- c_region[,c("VALOR", "aux")]


# Sexo de jefe/a hogar

a_sexojefe <- function(x) {
  x <- base_h_svy %>%
    filter(sexojefe == x) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(h_tus, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_sexojefe(x = i)
}     


c_sexojefe <- as.data.frame(VALOR)
c_sexojefe <- c_sexojefe %>% dplyr::mutate(aux = c(20, 21))
c_sexojefe <- c_sexojefe[,c("VALOR", "aux")]

# Condición de pobreza del hogar

a_pobre <- function(x) {
  x <- base_h_svy %>%
    filter(pobre_06 == x) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(h_tus, na.rm=T))
  x <- mean(x$colname)
}       

VALOR <- numeric()

for(i in 1:2){
  VALOR[i] <- a_pobre(x = i)
}     

c_pobre <- as.data.frame(VALOR)
c_pobre <- c_pobre %>% dplyr::mutate(aux = c(18, 19))
c_pobre <- c_pobre[,c("VALOR", "aux")]

# País urbano

a_mes <- base_h_svy %>%
  filter(REGION_4 == 1 | REGION_4 == 2) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(h_tus, na.rm=T))

VALOR <- mean(as.numeric(a_mes$colname))
c_ano_pu <- as.data.frame(VALOR)

c_ano_pu <- c_ano_pu %>% dplyr::mutate(aux = 1)
c_ano_pu<- select(c_ano_pu, c(VALOR, aux))

CODIND       <- 341
NOMINDICADOR <- "PORCENTAJE DE HOGARES QUE PERCIBEN TARJETA URUGUAY SOCIAL"

VALOR        <- rbind(c_ano, c_quintil, c_region, c_pobre, c_sexojefe)
VALOR        <- VALOR  %>% dplyr::mutate(SEXO = "",
                                         ASCENDENCIA  = "", 
                                         QUINTIL =      case_when(aux == 4 ~ "QUINTIL 1",
                                                                  aux == 5 ~ "QUINTIL 2",
                                                                  aux == 6 ~ "QUINTIL 3",
                                                                  aux == 7 ~ "QUINTIL 4",
                                                                  aux == 8 ~ "QUINTIL 5",
                                                                  TRUE     ~ "TODOS"),
                                         DEPARTAMENTO = "", 
                                         URBANORURALUY = case_when(aux == 16 ~ "URBANO (MENOS DE 5.000 HABITANTES)",
                                                                   aux == 17 ~ "RURAL DISPERSO",
                                                                   TRUE     ~ "TOTAL PAÍS"),
                                         EDAD =         "", 
                                         SITUACION_HOGAR = case_when(aux == 18 ~ "No en situación de pobreza",
                                                                     aux == 19  ~"En situación de pobreza",
                                                                     TRUE     ~ ""), 
                                         SEXO_JEFE       = case_when(aux == 20 ~ "Jefe varón",
                                                                     aux == 21  ~"Jefa mujer",
                                                                     TRUE     ~ ""),
                                         RAMA_DE_ACTIVIDAD = "",
                                         TIPO_DE_OCUPACION = "",
                                         CATEGORÍOCUP = "",
                                         RESTRICCIONES_AL_EMPLEO = "")
VALOR_pu        <- rbind(c_ano_pu)
VALOR_pu        <- VALOR_pu  %>% dplyr::mutate(SEXO = "",
                                               ASCENDENCIA  =  "", 
                                               QUINTIL =      "TODOS",
                                               DEPARTAMENTO = "", 
                                               URBANORURALUY = "URBANO (MÁS DE 5.000 HABITANTES)",
                                               EDAD =          "",
                                               SITUACION_HOGAR = "",
                                               RAMA_DE_ACTIVIDAD = "",
                                               TIPO_DE_OCUPACION = "",
                                               CATEGORÍOCUP = "",
                                               RESTRICCIONES_AL_EMPLEO = "",
                                               SEXO_JEFE       = "")

VALOR <- rbind (VALOR, VALOR_pu)


NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Este indicador se calcula a partir de la encuesta telefónica del primer semestre de 2021 y el formulario telefónico de modalidad panel del segundo semestre de 2021. En el segundo semestre, la condición de pobreza y el quintil de ingresos del hogar corresponden a los ingresos declarados durante la implantación del panel en la encuesta presencial."

t_21 <- cbind(CODIND,	NOMINDICADOR,	PAIS,	FECHA,	FECHA2,	VALOR,	RESPONSABLE,	JERARQUIA,	SECTORPRODUCTIVO,	SECTORINSTITUCIONAL,	ACTIVIDADESECONOMICAS,	DIVISIÓNECONOMICA,	CONCEPTO,	RELACIONFAMILIAR, NOTA_INDICADOR)
t_21 <- subset(t_21, select=c(CODIND,	NOMINDICADOR,	SEXO,	ASCENDENCIA,	QUINTIL, DEPARTAMENTO, URBANORURALUY,	PAIS,	FECHA,	FECHA2,	VALOR,	RESPONSABLE,	JERARQUIA,	SECTORPRODUCTIVO,	SECTORINSTITUCIONAL,	ACTIVIDADESECONOMICAS,	DIVISIÓNECONOMICA,	CONCEPTO,	CATEGORÍOCUP,	RELACIONFAMILIAR,	EDAD, SITUACION_HOGAR, RAMA_DE_ACTIVIDAD, TIPO_DE_OCUPACION, RESTRICCIONES_AL_EMPLEO, SEXO_JEFE, NOTA_INDICADOR))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# 317 - Distribución de personas ocupadas según categoría de la ocupación
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

Categoria <- c("Asalariado/a privado/a", "Asalariado/a público/a", "Miembro de cooperativa de producción", "Patrón/a", "Cuenta propia sin local ni inversión", "Cuenta propia con local o inversión", 
 "Miembro del hogar no remunerado",  "Trabajador/a de un programa social de empleo")




# Total

base_cat <- base_men_svy %>% filter(ocup == 1)

a_sem <- sapply(base_cat$variables %>% select(categoria), function(x){
  base_cat %>%
    filter(ocup == 1) %>%
    srvyr::summarise(VALOR_1 = srvyr::survey_mean(x, na.rm = TRUE)) 
})

c_ano <- data.frame(t(a_sem))

c_ano <- c_ano %>% dplyr::mutate(aux = 1)
c_ano <- c_ano %>% dplyr::mutate(CATEGORÍOCUP = Categoria)
c_ano <- c_ano[,c("VALOR_1", "aux", "CATEGORÍOCUP")]

# Varones

base_cat <- base_men_svy %>% filter(ocup == 1 & bc_pe2 == 1)

a_sem <- sapply(base_cat$variables %>% select(categoria), function(x){
  base_cat %>%
    filter(ocup == 1) %>%
    srvyr::summarise(VALOR_1 = srvyr::survey_mean(x, na.rm = TRUE)) 
})

c_varones <- data.frame(t(a_sem))

c_varones <- c_varones %>% dplyr::mutate(aux = 9)
c_varones <- c_varones %>% dplyr::mutate(CATEGORÍOCUP = Categoria)
c_varones <- c_varones[,c("VALOR_1", "aux", "CATEGORÍOCUP")]

# Mujeres

base_cat <- base_men_svy %>% filter(ocup == 1 & bc_pe2 == 2)

a_sem <- sapply(base_cat$variables %>% select(categoria), function(x){
  base_cat %>%
    filter(ocup == 1) %>%
    srvyr::summarise(VALOR_1 = srvyr::survey_mean(x, na.rm = TRUE)) 
})

c_mujeres <- data.frame(t(a_sem))

c_mujeres <- c_mujeres %>% dplyr::mutate(aux = 10)
c_mujeres <- c_mujeres %>% dplyr::mutate(CATEGORÍOCUP = Categoria)
c_mujeres <- c_mujeres[,c("VALOR_1", "aux", "CATEGORÍOCUP")]



CODIND       <- 317
NOMINDICADOR <- "Distribución de personas ocupadas por categoría de la ocupación"

VALOR        <- rbind(c_ano, c_varones, c_mujeres)
VALOR        <- VALOR  %>% rename(VALOR = VALOR_1)
VALOR        <- VALOR  %>% dplyr::mutate(SEXO = case_when(aux == 9  ~ "VARONES", 
                                                          aux == 10 ~ "MUJERES", 
                                                          TRUE     ~ "TODOS"),
                                         ASCENDENCIA  = "", 
                                         QUINTIL = "",   
                                         DEPARTAMENTO = "", 
                                         URBANORURALUY = "TOTAL PAÍS",
                                         EDAD =         "", 
                                         SITUACION_HOGAR = "", 
                                         SEXO_JEFE       = "",
                                         RAMA_DE_ACTIVIDAD = "",
                                         TIPO_DE_OCUPACION = "",
                                         RESTRICCIONES_AL_EMPLEO = "")


NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Este indicador se calcula a partir de la encuesta telefónica del primer semestre de 2021 y el formulario telefónico de modalidad panel del segundo semestre de 2021. En el segundo semestre, la condición de pobreza y el quintil de ingresos del hogar corresponden a los ingresos declarados durante la implantación del panel en la encuesta presencial."

t_22 <- cbind(CODIND,	NOMINDICADOR,	PAIS,	FECHA,	FECHA2,	VALOR,	RESPONSABLE,	JERARQUIA,	SECTORPRODUCTIVO,	SECTORINSTITUCIONAL,	ACTIVIDADESECONOMICAS,	DIVISIÓNECONOMICA,	CONCEPTO,	RELACIONFAMILIAR, NOTA_INDICADOR)
t_22 <- subset(t_22, select=c(CODIND,	NOMINDICADOR,	SEXO,	ASCENDENCIA,	QUINTIL, DEPARTAMENTO, URBANORURALUY,	PAIS,	FECHA,	FECHA2,	VALOR,	RESPONSABLE,	JERARQUIA,	SECTORPRODUCTIVO,	SECTORINSTITUCIONAL,	ACTIVIDADESECONOMICAS,	DIVISIÓNECONOMICA,	CONCEPTO,	CATEGORÍOCUP,	RELACIONFAMILIAR,	EDAD, SITUACION_HOGAR, RAMA_DE_ACTIVIDAD, TIPO_DE_OCUPACION, RESTRICCIONES_AL_EMPLEO, SEXO_JEFE, NOTA_INDICADOR))

	
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# 318 - Distribución de personas ocupadas según tipo´de ocupación
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

Ocupacion <- c("Ocupaciones militares",
               "Directores y gerentes",
               "Profesionales e intelectuales",
               "Técnico o profesional de nivel medio",
               "Personal de apoyo administrativo",
               "Trabajadores de servicios y vendedores de comercios y mercados",
               "Agricultores y trabajadores calificados agropecuarios forestales y pesqueros",
               "Oficiales, operarios y artesanos de artes mecánicas y otros oficios",
               "Operadores de instalaciones y máquinas y ensambladores",
               "Ocupaciones elementales")

# Total

base_ocup <- base_men_svy %>% filter(ocup == 1)

a_sem <- sapply(base_ocup$variables %>% select(ocupacion), function(x){
  base_ocup %>%
    filter(ocup == 1) %>%
    srvyr::summarise(VALOR_1 = srvyr::survey_mean(x, na.rm = TRUE)) 
})

c_ano <- data.frame(t(a_sem))

c_ano <- c_ano %>% dplyr::mutate(aux = 1)
c_ano <- c_ano %>% dplyr::mutate(TIPO_DE_OCUPACION = Ocupacion)
c_ano <- c_ano[,c("VALOR_1", "aux", "TIPO_DE_OCUPACION")]

# Varones

base_ocup <- base_men_svy %>% filter(ocup == 1 & bc_pe2 == 1)

a_sem <- sapply(base_ocup$variables %>% select(ocupacion), function(x){
  base_ocup %>%
    filter(ocup == 1) %>%
    srvyr::summarise(VALOR_1 = srvyr::survey_mean(x, na.rm = TRUE)) 
})

c_varones <- data.frame(t(a_sem))

c_varones <- c_varones %>% dplyr::mutate(aux = 9)
c_varones <- c_varones %>% dplyr::mutate(TIPO_DE_OCUPACION = Ocupacion)
c_varones <- c_varones[,c("VALOR_1", "aux", "TIPO_DE_OCUPACION")]

# Mujeres

base_ocup <- base_men_svy %>% filter(ocup == 1 & bc_pe2 == 2)

a_sem <- sapply(base_ocup$variables %>% select(ocupacion), function(x){
  base_ocup %>%
    filter(ocup == 1) %>%
    srvyr::summarise(VALOR_1 = srvyr::survey_mean(x, na.rm = TRUE)) 
})

c_mujeres <- data.frame(t(a_sem))

c_mujeres <- c_mujeres %>% dplyr::mutate(aux = 10)
c_mujeres <- c_mujeres %>% dplyr::mutate(TIPO_DE_OCUPACION = Ocupacion)
c_mujeres <- c_mujeres[,c("VALOR_1", "aux", "TIPO_DE_OCUPACION")]



CODIND       <- 318
NOMINDICADOR <- "Distribución de personas ocupadas por tipo de ocupación (CIUO-08)"

VALOR        <- rbind(c_ano, c_varones, c_mujeres)
VALOR        <- VALOR  %>% rename(VALOR = VALOR_1)
VALOR        <- VALOR  %>% dplyr::mutate(SEXO = case_when(aux == 9  ~ "VARONES", 
                                                          aux == 10 ~ "MUJERES", 
                                                          TRUE     ~ "TODOS"),
                                         ASCENDENCIA  = "", 
                                         QUINTIL = "",   
                                         DEPARTAMENTO = "", 
                                         URBANORURALUY = "TOTAL PAÍS",
                                         EDAD =         "", 
                                         SITUACION_HOGAR = "", 
                                         SEXO_JEFE       = "",
                                         RAMA_DE_ACTIVIDAD = "",
                                         CATEGORÍOCUP = "",
                                         RESTRICCIONES_AL_EMPLEO = "")


NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Este indicador se calcula a partir de la encuesta telefónica del primer semestre de 2021 y el formulario telefónico de modalidad panel del segundo semestre de 2021. En el segundo semestre, la condición de pobreza y el quintil de ingresos del hogar corresponden a los ingresos declarados durante la implantación del panel en la encuesta presencial."

t_23 <- cbind(CODIND,	NOMINDICADOR,	PAIS,	FECHA,	FECHA2,	VALOR,	RESPONSABLE,	JERARQUIA,	SECTORPRODUCTIVO,	SECTORINSTITUCIONAL,	ACTIVIDADESECONOMICAS,	DIVISIÓNECONOMICA,	CONCEPTO,	RELACIONFAMILIAR, NOTA_INDICADOR)
t_23 <- subset(t_23, select=c(CODIND,	NOMINDICADOR,	SEXO,	ASCENDENCIA,	QUINTIL, DEPARTAMENTO, URBANORURALUY,	PAIS,	FECHA,	FECHA2,	VALOR,	RESPONSABLE,	JERARQUIA,	SECTORPRODUCTIVO,	SECTORINSTITUCIONAL,	ACTIVIDADESECONOMICAS,	DIVISIÓNECONOMICA,	CONCEPTO,	CATEGORÍOCUP,	RELACIONFAMILIAR,	EDAD, SITUACION_HOGAR, RAMA_DE_ACTIVIDAD, TIPO_DE_OCUPACION, RESTRICCIONES_AL_EMPLEO, SEXO_JEFE, NOTA_INDICADOR))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# 319 - Distribución de personas ocupadas según rama de actividad
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

Rama <- c("Agropecuaria, pesca, caza y explotación de minas o canteras",
          "Industria manufacturera, suministro de electricidad, gas y agua",
          "Construcción",
          "Comercio por menor y por mayor; alojamiento y servicio de comida",
          "Transporte y almacenamiento",
          "Otras actividades de servicio; arte, entretenimiento y recreación",
          "Informática y comunicación",
          "Actividades financieras y de seguros",
          "Actividades inmobiliarias",
          "Actividades profesionales, científicas y técnicas",
          "Actividades administrativas y servicio de apoyo",
          "Administración pública; defensa y actividades de organizaciones y órganos extraterritoriales",
          "Enseñanza",
          "Servicios sociales y salud",
          "Actividades de los hogares como empleadores")



# Total

base_cat <- base_men_svy %>% filter(ocup == 1)

a_sem <- sapply(base_cat$variables %>% select(rama), function(x){
  base_cat %>%
    filter(ocup == 1) %>%
    srvyr::summarise(VALOR_1 = srvyr::survey_mean(x, na.rm = TRUE)) 
})

c_ano <- data.frame(t(a_sem))

c_ano <- c_ano %>% dplyr::mutate(aux = 1)
c_ano <- c_ano %>% dplyr::mutate(RAMA_DE_ACTIVIDAD = Rama)
c_ano <- c_ano[,c("VALOR_1", "aux", "RAMA_DE_ACTIVIDAD")]

# Varones

base_cat <- base_men_svy %>% filter(ocup == 1 & bc_pe2 == 1)

a_sem <- sapply(base_cat$variables %>% select(rama), function(x){
  base_cat %>%
    filter(ocup == 1) %>%
    srvyr::summarise(VALOR_1 = srvyr::survey_mean(x, na.rm = TRUE)) 
})

c_varones <- data.frame(t(a_sem))

c_varones <- c_varones %>% dplyr::mutate(aux = 9)
c_varones <- c_varones %>% dplyr::mutate(RAMA_DE_ACTIVIDAD = Rama)
c_varones <- c_varones[,c("VALOR_1", "aux", "RAMA_DE_ACTIVIDAD")]

# Mujeres

base_cat <- base_men_svy %>% filter(ocup == 1 & bc_pe2 == 2)

a_sem <- sapply(base_cat$variables %>% select(rama), function(x){
  base_cat %>%
    filter(ocup == 1) %>%
    srvyr::summarise(VALOR_1 = srvyr::survey_mean(x, na.rm = TRUE)) 
})

c_mujeres <- data.frame(t(a_sem))

c_mujeres <- c_mujeres %>% dplyr::mutate(aux = 10)
c_mujeres <- c_mujeres %>% dplyr::mutate(RAMA_DE_ACTIVIDAD = Rama)
c_mujeres <- c_mujeres[,c("VALOR_1", "aux", "RAMA_DE_ACTIVIDAD")]



CODIND       <- 319
NOMINDICADOR <- "Distribución de personas ocupadas por rama de actividad (CIIU Rev. 4)"

VALOR        <- rbind(c_ano, c_varones, c_mujeres)
VALOR        <- VALOR  %>% rename(VALOR = VALOR_1)
VALOR        <- VALOR  %>% dplyr::mutate(SEXO = case_when(aux == 9  ~ "VARONES", 
                                                          aux == 10 ~ "MUJERES", 
                                                          TRUE     ~ "TODOS"),
                                         ASCENDENCIA  = "", 
                                         QUINTIL = "",   
                                         DEPARTAMENTO = "", 
                                         URBANORURALUY = "TOTAL PAÍS",
                                         EDAD =         "", 
                                         SITUACION_HOGAR = "", 
                                         SEXO_JEFE       = "",
                                         TIPO_DE_OCUPACION = "",
                                         CATEGORÍOCUP = "",
                                         RESTRICCIONES_AL_EMPLEO = "")
                                         


NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Este indicador se calcula a partir de la encuesta telefónica del primer semestre de 2021 y el formulario telefónico de modalidad panel del segundo semestre de 2021. En el segundo semestre, la condición de pobreza y el quintil de ingresos del hogar corresponden a los ingresos declarados durante la implantación del panel en la encuesta presencial."


t_24 <- cbind(CODIND,	NOMINDICADOR,	PAIS,	FECHA,	FECHA2,	VALOR,	RESPONSABLE,	JERARQUIA,	SECTORPRODUCTIVO,	SECTORINSTITUCIONAL,	ACTIVIDADESECONOMICAS,	DIVISIÓNECONOMICA,	CONCEPTO,	RELACIONFAMILIAR, NOTA_INDICADOR)
t_24 <- subset(t_24, select=c(CODIND,	NOMINDICADOR,	SEXO,	ASCENDENCIA,	QUINTIL, DEPARTAMENTO, URBANORURALUY,	PAIS,	FECHA,	FECHA2,	VALOR,	RESPONSABLE,	JERARQUIA,	SECTORPRODUCTIVO,	SECTORINSTITUCIONAL,	ACTIVIDADESECONOMICAS,	DIVISIÓNECONOMICA,	CONCEPTO,	CATEGORÍOCUP,	RELACIONFAMILIAR,	EDAD, SITUACION_HOGAR, RAMA_DE_ACTIVIDAD, TIPO_DE_OCUPACION, RESTRICCIONES_AL_EMPLEO, SEXO_JEFE, NOTA_INDICADOR))



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# 320 - Distribución de personas ocupadas según restricciones al empleo
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

Restricciones <- c("No registradas",
                   "Subempleadas",
                   "No registradas y subempleadas",
                   "Sin restricciones al empleo")
  
# Total

base_cat <- base_svy %>% filter(ocup == 1)

a_sem <- sapply(base_cat$variables %>% select(restricciones), function(x){
  base_cat %>%
    filter(ocup == 1) %>%
    srvyr::summarise(VALOR_1 = srvyr::survey_mean(x, na.rm = TRUE)) 
})

c_ano <- data.frame(t(a_sem))

c_ano <- c_ano %>% dplyr::mutate(aux = 1)
c_ano <- c_ano %>% dplyr::mutate(RESTRICCIONES_AL_EMPLEO = Restricciones)
c_ano <- c_ano[,c("VALOR_1", "aux", "RESTRICCIONES_AL_EMPLEO")]

# Varones

base_cat <- base_svy %>% filter(ocup == 1 & bc_pe2 == 1)

a_sem <- sapply(base_cat$variables %>% select(restricciones), function(x){
  base_cat %>%
    filter(ocup == 1) %>%
    srvyr::summarise(VALOR_1 = srvyr::survey_mean(x, na.rm = TRUE)) 
})

c_varones <- data.frame(t(a_sem))

c_varones <- c_varones %>% dplyr::mutate(aux = 9)
c_varones <- c_varones %>% dplyr::mutate(RESTRICCIONES_AL_EMPLEO = Restricciones)
c_varones <- c_varones[,c("VALOR_1", "aux", "RESTRICCIONES_AL_EMPLEO")]

# Mujeres

base_cat <- base_svy %>% filter(ocup == 1 & bc_pe2 == 2)

a_sem <- sapply(base_cat$variables %>% select(restricciones), function(x){
  base_cat %>%
    filter(ocup == 1) %>%
    srvyr::summarise(VALOR_1 = srvyr::survey_mean(x, na.rm = TRUE)) 
})

c_mujeres <- data.frame(t(a_sem))

c_mujeres <- c_mujeres %>% dplyr::mutate(aux = 10)
c_mujeres <- c_mujeres %>% dplyr::mutate(RESTRICCIONES_AL_EMPLEO = Restricciones)
c_mujeres <- c_mujeres[,c("VALOR_1", "aux", "RESTRICCIONES_AL_EMPLEO")]



CODIND       <- 320
NOMINDICADOR <- "Distribución de personas ocupadas por restricciones al empleo"

VALOR        <- rbind(c_ano, c_varones, c_mujeres)
VALOR        <- VALOR  %>% rename(VALOR = VALOR_1)
VALOR        <- VALOR  %>% dplyr::mutate(SEXO = case_when(aux == 9  ~ "VARONES", 
                                                          aux == 10 ~ "MUJERES", 
                                                          TRUE     ~ "TODOS"),
                                         ASCENDENCIA  = "TODOS", 
                                         QUINTIL = "TODOS",   
                                         DEPARTAMENTO = "", 
                                         URBANORURALUY = "TOTAL PAÍS",
                                         EDAD =         "", 
                                         SITUACION_HOGAR = "", 
                                         SEXO_JEFE       = "",
                                         RAMA_DE_ACTIVIDAD = "",
                                         TIPO_DE_OCUPACION = "",
                                         CATEGORÍOCUP = "")


NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Este indicador se calcula a partir de la encuesta telefónica del primer semestre de 2021 y el formulario telefónico de modalidad panel del segundo semestre de 2021. En el segundo semestre, la condición de pobreza y el quintil de ingresos del hogar corresponden a los ingresos declarados durante la implantación del panel en la encuesta presencial."


t_25 <- cbind(CODIND,	NOMINDICADOR,	PAIS,	FECHA,	FECHA2,	VALOR,	RESPONSABLE,	JERARQUIA,	SECTORPRODUCTIVO,	SECTORINSTITUCIONAL,	ACTIVIDADESECONOMICAS,	DIVISIÓNECONOMICA,	CONCEPTO,	RELACIONFAMILIAR, NOTA_INDICADOR)
t_25 <- subset(t_25, select=c(CODIND,	NOMINDICADOR,	SEXO,	ASCENDENCIA,	QUINTIL, DEPARTAMENTO, URBANORURALUY,	PAIS,	FECHA,	FECHA2,	VALOR,	RESPONSABLE,	JERARQUIA,	SECTORPRODUCTIVO,	SECTORINSTITUCIONAL,	ACTIVIDADESECONOMICAS,	DIVISIÓNECONOMICA,	CONCEPTO,	CATEGORÍOCUP,	RELACIONFAMILIAR,	EDAD, SITUACION_HOGAR, RAMA_DE_ACTIVIDAD, TIPO_DE_OCUPACION, RESTRICCIONES_AL_EMPLEO, SEXO_JEFE, NOTA_INDICADOR))




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
### Generación de Base motor ###
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


## Fusión de bases parciales

Empleo_2023 <- rbind(t_01, t_02, t_03, t_04, t_05, t_06, t_07, t_08, t_09, t_10, t_11, t_12, t_13, t_14, t_15, t_16, t_17, t_18, t_19, t_20, t_21, t_22, t_23, t_24, t_25)
rio::export(Empleo_2023, "Empleo_2023.xlsx")


## Piso I

Empleo_2023 <- rio::import("Empleo_2023.xlsx")
Empleo_2023 <- Empleo_2023 %>% mutate(pobr = ifelse(is.na(SITUACION_HOGAR) | SITUACION_HOGAR =="", 0, 1),
                                      edad = ifelse(is.na(EDAD) | EDAD == "", 0, 1),
                                      sjef = ifelse(is.na(SEXO_JEFE) | SEXO_JEFE  ==  "", 0, 1))



piso_1  <- rio::import("C:/Users/Usuario/Dropbox/1. Unidad de Métodos y Acceso a Datos/1. Observatorio UMAD/GIT-HUB UMAD/Piso-I-Empleo/Data/Base_motor_empleo_15062023.xlsx")





Empleo_2023 <-  Empleo_2023  %>% mutate(AUX = case_when(NOMINDICADOR == "TASA DE ACTIVIDAD" & EDAD == "14 a 18" ~ "TASA DE ACTIVIDAD DE PERSONAS DE 14 A 18 AÑOS",
                                                        NOMINDICADOR == "TASA DE ACTIVIDAD" & EDAD == "19 a 24" ~ "TASA DE ACTIVIDAD DE PERSONAS DE 19 A 24 AÑOS",
                                                        NOMINDICADOR == "TASA DE ACTIVIDAD" & EDAD == "25 a 29" ~ "TASA DE ACTIVIDAD DE PERSONAS DE 25 A 29 AÑOS",
                                                        NOMINDICADOR == "TASA DE ACTIVIDAD" & EDAD == "30 a 64" ~ "TASA DE ACTIVIDAD DE PERSONAS DE 30 A 64 AÑOS",
                                                        NOMINDICADOR == "TASA DE ACTIVIDAD" & EDAD == "65 y más" ~ "TASA DE ACTIVIDAD DE PERSONAS DE 65 AÑOS Y MÁS",
                                                        NOMINDICADOR == "Distribución de personas ocupadas por restricciones al empleo" & RESTRICCIONES_AL_EMPLEO == "No registradas"  ~ "PORCENTAJE DE PERSONAS OCUPADAS NO REGISTRADAS",
                                                        NOMINDICADOR == "Distribución de personas ocupadas por restricciones al empleo" & RESTRICCIONES_AL_EMPLEO == "No registradas y subempleadas"  ~  "PORCENTAJE DE PERSONAS OCUPADAS NO REGISTRADAS Y SUBEMPLEADAS",
                                                        NOMINDICADOR == "Distribución de personas ocupadas por restricciones al empleo" & RESTRICCIONES_AL_EMPLEO == "Sin restricciones al empleo" ~ "PORCENTAJE DE PERSONAS OCUPADAS SIN RESTRICCIONES AL EMPLEO",
                                                        NOMINDICADOR == "Distribución de personas ocupadas por restricciones al empleo" & RESTRICCIONES_AL_EMPLEO == "Subempleadas" ~ "PORCENTAJE DE PERSONAS OCUPADAS SUBEMPLEADAS",
                                                        TRUE ~ NOMINDICADOR))
Empleo_2023 <- select(Empleo_2023, -NOMINDICADOR)
Empleo_2023 <- Empleo_2023  %>%  rename(NOMINDICADOR = AUX)

Empleo_2023 <- subset(Empleo_2023, select=c(CODIND,	NOMINDICADOR,	SEXO,	ASCENDENCIA,	QUINTIL, DEPARTAMENTO, URBANORURALUY,	PAIS,	FECHA,	FECHA2,	VALOR,	RESPONSABLE,	JERARQUIA,	SECTORPRODUCTIVO,	SECTORINSTITUCIONAL,	ACTIVIDADESECONOMICAS,	DIVISIÓNECONOMICA,	CONCEPTO,	CATEGORÍOCUP,	RELACIONFAMILIAR,	EDAD, SITUACION_HOGAR, RAMA_DE_ACTIVIDAD, TIPO_DE_OCUPACION, RESTRICCIONES_AL_EMPLEO, SEXO_JEFE, NOTA_INDICADOR, pobr, edad, sjef))



Empleo_2023 <-  Empleo_2023  %>% rename("QUINTIL DE INGRESO" = QUINTIL,
                                        "DEPARTAMENTO UY" = DEPARTAMENTO, 
                                        "PAÍS" = PAIS,
                                        "JERARQUÍA" = JERARQUIA, 
                                        "CATEGORÍA DE OCUPACIÓN RURAL" = CATEGORÍOCUP,
                                        "RELACIÓN FAMILIAR" = RELACIONFAMILIAR)                                                        

names <- names(piso_1)
names (Empleo_2023) = names

piso_1 <- rbind(piso_1, Empleo_2023)


piso_1[is.na(piso_1)]<-''
rio::export(piso_1, "C:/Users/Usuario/Dropbox/1. Unidad de Métodos y Acceso a Datos/1. Observatorio UMAD/GIT-HUB UMAD/Piso-I-Empleo/Data/Base_motor_empleo_03052024.xlsx")
rio::export(piso_1, "Base_motor_empleo_03052024.xlsx")




## Piso II

Empleo_2023 <- rio::import("Empleo_2023.xlsx")
baseweb   <- rio::import("Base Motor_Empleo-Salarios-Transferencias_29.04.2024_web.xlsx") # Base descargada de drive compartido con economía


# Cambia nombres de indicadores de mayúscula a minúscula

Empleo_2023$NOMINDICADOR <- casefold(Empleo_2023$NOMINDICADOR, upper = FALSE) 


aux <- as.data.frame(substr(Empleo_2023$NOMINDICADOR, start = 1, stop = 1))
names (aux) = "V1"
aux$V1 <- casefold(aux$V1, upper = TRUE) 

aux2 <- as.data.frame(substr(Empleo_2023$NOMINDICADOR, start = 2, stop = 100000000))
names (aux2) = "V2"

aux3 <- cbind(aux,aux2)
aux3$NOMINDICADOR_2 <- paste0(aux3$V1,aux3$V2)

aux3 <- aux3   %>%  mutate(NOMINDICADOR_2 = case_when (NOMINDICADOR_2 == "Distribución de personas ocupadas por rama de actividad (ciiu rev. 4)" ~ "Distribución de personas ocupadas por rama de actividad (CIIU Rev. 4)",
                                                       NOMINDICADOR_2 == "Distribución de personas ocupadas por tipo de ocupación (ciuo-08)" ~ "Distribución de personas ocupadas por tipo de ocupación (CIUO-08)",
                                                       NOMINDICADOR_2 == "Porcentaje de ocupados que perciben un ingreso por debajo del smn" ~ "Porcentaje de ocupados que perciben un ingreso por debajo del SMN",
                           TRUE ~ NOMINDICADOR_2))

aux3 <- select(aux3, NOMINDICADOR_2)


Empleo_2023 <- cbind(Empleo_2023,aux3)
Empleo_2023 <- select(Empleo_2023, -NOMINDICADOR)
Empleo_2023 <- rename(Empleo_2023, NOMINDICADOR = NOMINDICADOR_2)


# Cambia formato de fechas

Empleo_2023$FECHA_REC <- as.Date(paste0(Empleo_2023$FECHA,"-01-01"),tryFormats = "%Y-%m-%d")


# Cambia criterio de variable URBANORURALUY

Empleo_2023 <- Empleo_2023  %>% mutate(URBANORURALUY = case_when(URBANORURALUY == "TODOS" ~ "TOTAL PAÍS",
                                                           TRUE ~ URBANORURALUY))


Empleo_2023 <- Empleo_2023  %>% mutate(REGIÓN_original = URBANORURALUY)
Empleo_2023 <- Empleo_2023  %>% mutate(URB_TOT = case_when(URBANORURALUY == "URBANO (MÁS DE 5.000 HABITANTES)"  ~ "U",
                                                     TRUE ~ "T"))


# Genera variable para identificar variable de corte

Empleo_2023 <- Empleo_2023  %>% mutate(CORTE = case_when(SEXO == "VARONES" | SEXO == "MUJERES" ~ "SEXO",
                                                         ASCENDENCIA == "AFRO" | ASCENDENCIA == "NO AFRO" ~ " ASCENDENCIA",
                                                         QUINTIL  == "QUINTIL 1" | 
                                                         QUINTIL  == "QUINTIL 2" |
                                                         QUINTIL  == "QUINTIL 3" |
                                                         QUINTIL  == "QUINTIL 4" |
                                                         QUINTIL  == "QUINTIL 5" ~ "QUINTIL_DE_INGRESO",
                                                         EDAD != "" ~ "EDAD",
                                                         SITUACION_HOGAR != "" ~ "SITUACIÓN_HOGAR", 
                                                         SEXO_JEFE == "Jefa mujer" | SEXO_JEFE == "Jefe varón"  ~ "JEFATURA",
                                                         URBANORURALUY == "RURAL DISPERSO" |
                                                         URBANORURALUY == "URBANO (MENOS DE 5.000 HABITANTES)" ~ "REGIÓN",
                                                         TRUE ~ "TOTAL"))



Empleo_2023 <- Empleo_2023  %>% mutate(CORTE = case_when(CORTE == "TOTAL" & URBANORURALUY == "TOTAL PAÍS" ~ "REGIÓN",                                                   
                                                   TRUE ~ CORTE))


# Cambia criterio de variables de corte

Empleo_2023 <- Empleo_2023  %>% mutate(SEXO = case_when(SEXO == "TODOS" ~ "", 
                                                        TRUE ~ SEXO), 
                                       ASCENDENCIA = case_when(ASCENDENCIA == "TODOS" ~ "", 
                                                         TRUE ~ ASCENDENCIA), 
                                       QUINTIL = case_when(QUINTIL == "TODOS" ~ "", 
                                                                  TRUE ~ QUINTIL))

Empleo_2023 <- Empleo_2023  %>% mutate(REGIÓN = case_when(URBANORURALUY == "RURAL DISPERSO"  ~ "RURAL DISPERSO",
                                                          URBANORURALUY == "URBANO (MENOS DE 5.000 HABITANTES)"  ~ "URBANO (MENOS DE 5.000 HABITANTES)",
                                                          CORTE == "REGIÓN" & URBANORURALUY == "TOTAL PAÍS" ~ "TOTAL PAÍS",
                                                          TRUE ~ ""))



# Genera vairable JERARQUIA para visualización de gráficos


Empleo_2023 <- Empleo_2023  %>% mutate(JERARQUÍA = ifelse(SEXO == "" & ASCENDENCIA == "" & QUINTIL == "" & EDAD == "" & SITUACION_HOGAR == "" & 
                                                      (REGIÓN == "" | REGIÓN == "TOTAL PAÍS"), 1, 0),
                                       JERARQUIA_CAT = 1)

Empleo_2023 <- Empleo_2023  %>% mutate(JERARQUÍA = ifelse(is.na(JERARQUÍA), 1, 0))



# Genera vectores vacíos para las variables de economía

Empleo_2023 <- Empleo_2023  %>% mutate(CORTE_2 = "",
                                 CORTE_3 = "",
                                 FRECUENCIA = "", 
                                 SECTOR_PRODUCTIVO = "",
                                 SECTOR_INSTITUCIONAL = "",
                                 ACTIVIDADES_ECONÓMICAS = "",
                                 DIVISIÓN_ECONÓMICA = "",
                                 PÚBLICO = "",
                                 CONCEPTO = "",
                                 RELACIÓN_FAMILIAR = "",
                                 `SECTOR PÚBLICO` = "")


# Duplica totales para que aparezcan en gráficos

duplicacion <- Empleo_2023 %>% filter(CORTE == "REGIÓN" & REGIÓN == "TOTAL PAÍS")
duplicacion <- duplicacion %>% mutate(CORTE =  "TOTAL", 
                                      REGIÓN = "")


Empleo_2023 <- rbind(Empleo_2023, duplicacion)


# Ordena base final

Empleo_2023 <- subset(Empleo_2023, select=c(CODIND,	NOMINDICADOR, PAIS, FECHA, FECHA_REC, VALOR, RESPONSABLE, JERARQUÍA, JERARQUIA_CAT, CORTE, CORTE_2, CORTE_3, FRECUENCIA,
                                      SEXO_JEFE, RESTRICCIONES_AL_EMPLEO, RAMA_DE_ACTIVIDAD, TIPO_DE_OCUPACION,  CATEGORÍOCUP, SITUACION_HOGAR, 
                                      SEXO,	ASCENDENCIA,	QUINTIL, DEPARTAMENTO, URB_TOT, REGIÓN, REGIÓN_original, SECTOR_PRODUCTIVO, SECTOR_INSTITUCIONAL,
                                      ACTIVIDADES_ECONÓMICAS, DIVISIÓN_ECONÓMICA, `SECTOR PÚBLICO`, CONCEPTO, RELACIÓN_FAMILIAR, EDAD))

rio::export(Empleo_2023, "prueba.xlsx")


# Realiza pequeños ajustes en nombres de variables (luego de chequear orden correcto)

names <-names(baseweb)
names (Empleo_2023) = names

baseweb_2023 <- rbind(baseweb, Empleo_2023)



# Corrección de corte urbano-rural para web

urbano  <- Empleo_2023 %>% filter(CORTE == "TOTAL" & REGIÓN_original == "URBANO (MÁS DE 5.000 HABITANTES)")

urbano <- urbano  %>% mutate(CORTE = "REGIÓN",
                             REGIÓN = "URBANO (MÁS DE 5.000 HABITANTES)",
                             JERARQUÍA = 0,
                             URB_TOT = "T")

urbano <- subset(urbano, select=c(CODIND,	NOMINDICADOR, PAÍS, FECHA, FECHA_REC, VALOR, RESPONSABLE, JERARQUÍA, JERARQUIA_CAT, CORTE, CORTE_2, CORTE_3, FRECUENCIA,
                                  JEFATURA, RESTRICCIONES_AL_EMPLEO, RAMA_DE_ACTIVIDAD, TIPO_DE_OCUPACIÓN,  CATEGORÍA_DE_OCUPACIÓN, SITUACIÓN_HOGAR, 
                                  SEXO,	ASCENDENCIA,	QUINTIL_DE_INGRESO, DEPARTAMENTO_UY, URB_TOT, REGIÓN, REGIÓN_original, SECTOR_PRODUCTIVO, SECTOR_INSTITUCIONAL,
                                  ACTIVIDADES_ECONÓMICAS, DIVISIÓN_ECONÓMICA, `SECTOR PÚBLICO`, CONCEPTO, RELACIÓN_FAMILIAR, EDAD))


baseweb_2023 <- rbind(baseweb_2023, urbano)


# Se eliminan algunas filas que no se suben a la web


baseweb_2023 <- baseweb_2023 %>% mutate(filtro = ifelse(FECHA == 2022 & CORTE == "REGIÓN" & (CODIND == 317 | 
                                                                                               CODIND == 319 | 
                                                                                               CODIND == 320 | 
                                                                                               CODIND == 318), 1, 0))

baseweb_2023 <- baseweb_2023 %>%  subset(filtro == 0)
baseweb_2023 <- select(baseweb_2023, -filtro)



# Exporta base final para subir a Drive


rio::export(baseweb_2023, "Empleo_2023_piso II_06052024.xlsx")




