
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
FECHA       <- 2021
FECHA2      <- "01/01/2021"
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

sem1          <- rio::import("Bases/ECH_2021_sem1_terceros.sav")
sem2_implant  <- rio::import("Bases/ECH_implantacion_sem2_2021.csv")
#sem2_panel_07 <- rio::import("Bases/Bases_mensuales_terceros/ECH_07_21.csv")
#sem2_panel_08 <- rio::import("Bases/Bases_mensuales_terceros/ECH_08_21.csv")
#sem2_panel_09 <- rio::import("Bases/Bases_mensuales_terceros/ECH_09_21.csv")
#sem2_panel_10 <- rio::import("Bases/Bases_mensuales_terceros/ECH_10_21.csv")
#sem2_panel_11 <- rio::import("Bases/Bases_mensuales_terceros/ECH_11_21.csv")
#sem2_panel_12 <- rio::import("Bases/Bases_mensuales_terceros/ECH_12_21.csv")

#pesos_boost_07 <- rio::import('Bases/pesos_replicados/pesos_replicados_07-2021.csv')
#pesos_boost_08 <- rio::import('Bases/pesos_replicados/pesos_replicados_08-2021.csv')
#pesos_boost_09 <- rio::import('Bases/pesos_replicados/pesos_replicados_09-2021.csv')
#pesos_boost_10 <- rio::import('Bases/pesos_replicados/pesos_replicados_10-2021.csv')
#pesos_boost_11 <- rio::import('Bases/pesos_replicados/pesos_replicados_11-2021.csv')
#pesos_boost_12 <- rio::import('Bases/pesos_replicados/pesos_replicados_12-2021.csv')


#sem2_panel_07 = sem2_panel_07 %>% dplyr::left_join(pesos_boost_07)
#sem2_panel_08 = sem2_panel_08 %>% dplyr::left_join(pesos_boost_08)
#sem2_panel_09 = sem2_panel_09 %>% dplyr::left_join(pesos_boost_09)
#sem2_panel_10 = sem2_panel_10 %>% dplyr::left_join(pesos_boost_10)
#sem2_panel_11 = sem2_panel_11 %>% dplyr::left_join(pesos_boost_11)
#sem2_panel_12 = sem2_panel_12 %>% dplyr::left_join(pesos_boost_12)

#sem2_panel <- rbind(sem2_panel_07, sem2_panel_08,sem2_panel_09, sem2_panel_10, sem2_panel_11, sem2_panel_12)

#rio::export(sem2_panel, "Bases/sem2_panel.Rdata" )

sem2_panel  <- rio::import("Bases/sem2_panel.Rdata")

sem1          <-  sem1[,c("mes", "HT11", "ht19", "HT13", "numero", "E26", "e29_1", "E27", "pobre06", "POBPCOAC", "F85", "F98", "F102", "F104", "F96", "F82", "F84", "PT2", "region_4", "pesomen", "lp_06",
                          "G148_1_1", "G148_1_2", "G148_1_3", "G148_1_4", "G148_1_5", "G148_1_6", "G148_1_7", "G148_1_8", "G148_1_9", "G148_1_10", "G148_1_11", "G148_1_12", 
                          "G148_2_1", "G148_2_2", "G148_2_3", "G148_2_4", "G148_2_5", "G148_2_6", "G148_2_7", "G148_2_8", "G148_2_9", "G148_2_10", "G148_2_11", "G148_2_12",
                          "G152", "F73", "F92", "G150", "F96", "E560", "e30", "F72_2", "F71_2", "F73")]
sem2_implant  <-  sem2_implant[,c("mes", "ht11", "ht19", "ht13", "ID", "e26", "e29_1", "e27", "pobre", "pobpcoac", "f85", "f98", "f102", "f104", "f96", "f82", "f84", "pt2", "region_4", "w_sem", "lp",
                                  "g148_1_1", "g148_1_2", "g148_1_3", "g148_1_5", "g148_1_6", "g148_1_7", "g148_1_8", "g148_1_9", "g148_1_10", "g148_1_11", "g148_1_12",
                                  "g148_2_1", "g148_2_2", "g148_2_3", "g148_2_5", "g148_2_6", "g148_2_7", "g148_2_8", "g148_2_9", "g148_2_10", "g148_2_11", "g148_2_12",
                                  "g152", "f73", "f92", "g150", "f96", "e560", "e30")]
sem2_panel  <-  sem2_panel[,c("mes", "ID", "e26", "e29_1", "e27", "POBPCOAC", "f85", "f98", "f102", "f104", "f96", "f82", "f72_2", "f71_2", "f73", "region_4", "w")]





# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Generación de nuevas variables ###

smn <- 17930
smn_h = smn/200


#IPC

sem1 <- sem1 %>% dplyr::mutate(bc_ipc_tot = case_when(mes == 1 ~ 0.335542051,
                                                      mes == 2 ~ 0.330249262,
                                                      mes == 3 ~ 0.327549795,
                                                      mes == 4 ~ 0.32554687,
                                                      mes == 5 ~ 0.323919843,
                                                      mes == 6 ~ 0.322448435))

sem2_implant <- sem2_implant %>% dplyr::mutate(bc_ipc_tot = case_when(
  mes == 7  ~ 0.320314392,
  mes == 8  ~ 0.318657357,
  mes == 9  ~ 0.31596912,
  mes == 10 ~ 0.314515807,
  mes == 11 ~ 0.31128448,
  mes == 12 ~ 0.310493463))


sem1 <- sem1 %>% dplyr::mutate(bc_ipc = case_when(    mes == 1 ~ 0.329708731,
                                                      mes == 2 ~ 0.325120854,
                                                      mes == 3 ~ 0.32193312,
                                                      mes == 4 ~ 0.320163041,
                                                      mes == 5 ~ 0.318357918,
                                                      mes == 6 ~ 0.316774856))

sem2_implant <- sem2_implant %>% dplyr::mutate(bc_ipc = case_when(
  mes == 7 ~  0.314794762,
  mes == 8 ~  0.31306265,
  mes == 9 ~  0.310558149,
  mes == 10 ~ 0.309320866,
  mes == 11 ~ 0.306270364,
  mes == 12 ~ 0.305191859))                                                 


sem1 <- sem1 %>% dplyr::mutate(ipc_ene2022 = case_when(    mes == 1 ~ 0.941120053,
                                                           mes == 2 ~ 0.948876194,
                                                           mes == 3 ~ 0.32193312,
                                                           mes == 4 ~ 0.959509612,
                                                           mes == 5 ~ 0.963888078,
                                                           mes == 6 ~ 0.970309829))

sem2_implant <- sem2_implant %>% dplyr::mutate(ipc_ene2022 = case_when(
  mes == 7 ~  0.97535549,
  mes == 8 ~  0.983653726,
  mes == 9 ~  0.988198991,
  mes == 10 ~ 0.998457112,
  mes == 11 ~ 1.001000792,
  mes == 12 ~ 1))                                                 

sem2_panel <- sem2_panel %>% dplyr::mutate(ipc_ene2022 = case_when(
  mes == 7 ~  0.97535549,
  mes == 8 ~  0.983653726,
  mes == 9 ~  0.988198991,
  mes == 10 ~ 0.998457112,
  mes == 11 ~ 1.001000792,
  mes == 12 ~ 1))   


# Ingresos

sem1 <- sem1 %>% dplyr::mutate(y_pc       =  HT11 / ht19 )                      #Ingreso per-cápita
sem1 <- sem1 %>% dplyr::mutate(y_pc_svl   =  (HT11 - HT13) / ht19)              #Ingreso per-cápita sin valor locativo
sem1 <- sem1 %>% dplyr::mutate(y_pc_d     =  (HT11 / ht19) * bc_ipc_tot)          #Ingreso per-cápita deflactado
sem1 <- sem1 %>% dplyr::mutate(y_pc_svl_d =  ((HT11 - HT13) / ht19) * bc_ipc_tot) #Ingreso per-cápita sin valor locativo deflactado

sem2_implant <- sem2_implant %>% dplyr::mutate(y_pc       =  ht11 / ht19 )                      #Ingreso per-cápita
sem2_implant <- sem2_implant %>% dplyr::mutate(y_pc_svl   =  (ht11 - ht13) / ht19)              #Ingreso per-cápita sin valor locativo
sem2_implant <- sem2_implant %>% dplyr::mutate(y_pc_d     =  (ht11 / ht19) * bc_ipc_tot)          #Ingreso per-cápita deflactado
sem2_implant <- sem2_implant %>% dplyr::mutate(y_pc_svl_d =  ((ht11 - ht13) / ht19) * bc_ipc_tot) #Ingreso per-cápita sin valor locativo deflactado


# Quintil de ingresos

sem1_h <- sem1 %>% distinct(numero, .keep_all = TRUE)
sem1_h <- sem1_h %>% dplyr::mutate(quintilesy = statar::xtile(y_pc, n=5, wt = pesomen))
sem1_h <- sem1_h[,c("numero","quintilesy")]
sem1 <- merge(sem1, sem1_h, by = "numero")

sem2_h <- sem2_implant %>% distinct(ID, .keep_all = TRUE)
sem2_h <- sem2_h %>% dplyr::mutate(quintilesy = statar::xtile(y_pc, n=5, wt = w_sem))
sem2_h <- sem2_h[,c("ID","quintilesy")]
sem2_implant <- merge(sem2_implant, sem2_h, by = "ID")

quintil <- sem2_implant[,c("ID","quintilesy")]
sem2_panel  <- merge(sem2_panel, quintil, by = "ID")


# Sexo

sem1 <- sem1 %>% dplyr::mutate(bc_pe2 = E26)
sem2_implant <- sem2_implant %>% dplyr::mutate(bc_pe2 = e26)
sem2_panel <- sem2_panel %>% dplyr::mutate(bc_pe2 = e26)


# Sexo del jefe/a de hogar

sem1 <- sem1[order(sem1$numero, sem1$e30, decreasing = FALSE), ]
sem1_h <- sem1 %>% distinct(numero, .keep_all = TRUE)
sem1_h <- sem1_h %>% dplyr::mutate(sexojefe = case_when(e30 == 1 & E26 == 1 ~ 1,
                                                        e30 == 1 & E26 == 2 ~ 2,
                                                        e30 != 1 ~ 99))
sem1_h <- sem1_h[,c("numero","sexojefe")]
sem1 <- merge(sem1, sem1_h, by = "numero")

sem2_implant <- sem2_implant[order(as.numeric(sem2_implant$ID), sem2_implant$e30, decreasing = FALSE), ]
sem2_h <- sem2_implant %>% distinct(ID, .keep_all = TRUE)
sem2_h <- sem2_h %>% dplyr::mutate(sexojefe = case_when(e30 == 1 & e26 == 1 ~ 1,
                                                        e30 == 1 & e26 == 2 ~ 2,
                                                        e30 != 1 ~ 99))
sem2_h <- sem2_h[,c("ID","sexojefe")]
sem2_implant <- merge(sem2_implant, sem2_h, by = "ID")



# Ascendencia afro

sem1 <- sem1 %>% dplyr::mutate(bd_e29_1 = e29_1)
sem2_implant <- sem2_implant %>% dplyr::mutate(bd_e29_1 = e29_1)
sem2_panel <- sem2_panel %>% dplyr::mutate(bd_e29_1 = e29_1)

# Tramo de edad

sem1 <- sem1 %>% dplyr::mutate(tramo_edad = case_when(E27 >= 14 & E27 <= 18 ~  1,
                                                      E27 >= 19 & E27 <= 24 ~  2,
                                                      E27 >= 25 & E27 <= 29 ~  3,
                                                      E27 >= 30 & E27 <= 64 ~  4,
                                                      E27 >= 65             ~  5))

sem2_implant <- sem2_implant %>% dplyr::mutate(tramo_edad = case_when(e27 >= 14 & e27 <= 18 ~  1,
                                                      e27 >= 19 & e27 <= 24 ~  2,
                                                      e27 >= 25 & e27 <= 29 ~  3,
                                                      e27 >= 30 & e27 <= 64 ~  4,
                                                      e27 >= 65             ~  5))

sem2_panel <- sem2_panel %>% dplyr::mutate(tramo_edad = case_when(e27 >= 14 & e27 <= 18 ~  1,
                                                                      e27 >= 19 & e27 <= 24 ~  2,
                                                                      e27 >= 25 & e27 <= 29 ~  3,
                                                                      e27 >= 30 & e27 <= 64 ~  4,
                                                                      e27 >= 65             ~  5))




# Pobreza

sem1 <- sem1 %>% dplyr::mutate(pobre_06 = case_when(pobre06 == 0 ~  1,
                                                 pobre06 == 1 ~  2))

sem2_implant <- sem2_implant %>% dplyr::mutate(pobre_06 = case_when(pobre == 0 ~  1,
                                                                    pobre == 1 ~  2))

pobres <- sem2_implant[,c("ID","pobre")]
sem2_panel  <- merge(sem2_panel, pobres, by = "ID")

sem2_panel <- sem2_panel %>% dplyr::mutate(pobre_06 = case_when(pobre == 0 ~  1,
                                                                pobre == 1 ~  2))

# Población económicamente activa

sem1 <- sem1 %>% dplyr::mutate(pea = ifelse(POBPCOAC==2 | POBPCOAC==3 | POBPCOAC==4 | POBPCOAC==5, 1, 0))
sem2_panel <- sem2_panel %>% dplyr::mutate(pea = ifelse(POBPCOAC==2 | POBPCOAC==3 | POBPCOAC==4 | POBPCOAC==5, 1, 0))


# Ocupados

sem1 <- sem1 %>% dplyr::mutate(ocup = ifelse(POBPCOAC==2, 1, 0))
sem2_implant <- sem2_implant %>% dplyr::mutate(ocup = ifelse(pobpcoac==2, 1, 0))
sem2_panel <- sem2_panel %>% dplyr::mutate(ocup = ifelse(POBPCOAC==2, 1, 0))


# Desempleados

sem1 <- sem1 %>% dplyr::mutate(desocup = ifelse(POBPCOAC==3 | POBPCOAC==4 | POBPCOAC==5, 1, 0))
sem2_panel <- sem2_panel %>% dplyr::mutate(desocup = ifelse(POBPCOAC==3 | POBPCOAC==4 | POBPCOAC==5, 1, 0))


# Horas remuneradas

sem1 <- sem1 %>% dplyr::mutate(bc_horas = F85+F98)
sem2_implant <- sem2_implant  %>% dplyr::mutate(bc_horas = f85+f98)
sem2_panel <- sem2_panel  %>% dplyr::mutate(bc_horas = f85+f98)



# Subempleo

sem1 <- sem1 %>% dplyr::mutate(bc_subocupado = ifelse(F102 == 1 & F104 == 5 & bc_horas > 0 & bc_horas < 40, 1, 0))
sem2_panel <- sem2_panel %>% dplyr::mutate(bc_subocupado = ifelse(f102 == 1 & f104 == 5 & bc_horas > 0 & bc_horas < 40, 1, 0))


# No aporte a SS en trabajo principal y secundario

sem1 <- sem1 %>% dplyr::mutate(bc_register2 = ifelse(F96 == 1 | F82 == 1, 0, 1))
sem2_implant <- sem2_implant %>% dplyr::mutate(bc_register2 = ifelse(f96 == 1 | f82 == 1, 0, 1))
sem2_panel <- sem2_panel %>% dplyr::mutate(bc_register2 = ifelse(f96 == 1 | f82 == 1, 0, 1))


# No aporte a SS en trabajo principal

sem1 <- sem1 %>% dplyr::mutate(bc_register = ifelse(F82 == 1, 0, 1))
sem2_implant <- sem2_implant %>% dplyr::mutate(bc_register = ifelse(f82 == 1, 0, 1))
sem2_panel <- sem2_panel %>% dplyr::mutate(bc_register = ifelse(f82 == 1, 0, 1))


# No aporte por totalidad del salario

sem1 <- sem1 %>% dplyr::mutate(noaportatot = ifelse(F84 == 2, 1, 0))
sem2_implant <- sem2_implant %>% dplyr::mutate(noaportatot = ifelse(f84 == 2, 1, 0))


# Salario en ocupación principal

sem1 <- sem1 %>% dplyr::mutate(horamen = F85 * 4.3)
sem1 <- sem1 %>% dplyr::mutate(yhora = PT2/horamen)
sem1 <- sem1 %>% dplyr::mutate(yhoradef = case_when(F85>0 ~ yhora*bc_ipc_tot,
                                                    F85==0 ~ 0))
                               
sem2_implant <- sem2_implant %>% dplyr::mutate(horamen = f85 * 4.3)
sem2_implant <- sem2_implant %>% dplyr::mutate(yhora = pt2/horamen)
sem2_implant <- sem2_implant %>% dplyr::mutate(yhoradef = case_when(f85>0 ~ yhora*bc_ipc_tot,
                                                                    f85==0 ~ 0))

# Línea de pobreza individual

sem1 <- sem1 %>% dplyr::mutate(regionlp = case_when(region_4 == 1 ~ 1,
                                                    region_4 == 2 | region_4 == 3 ~ 2,
                                                    region_4 == 4 ~ 3))


sem1 <- sem1 %>% dplyr::mutate(grupolp = case_when(regionlp == 1 & mes == 1 ~ 1,
                                                   regionlp == 1 & mes == 2 ~ 2,
                                                   regionlp == 1 & mes == 3 ~ 3,
                                                   regionlp == 1 & mes == 4 ~ 4,
                                                   regionlp == 1 & mes == 5 ~ 5,
                                                   regionlp == 1 & mes == 6 ~ 6,
                                                   regionlp == 2 & mes == 1 ~ 7,
                                                   regionlp == 2 & mes == 2 ~ 8,
                                                   regionlp == 2 & mes == 3 ~ 9,
                                                   regionlp == 2 & mes == 4 ~ 10,
                                                   regionlp == 2 & mes == 5 ~ 11,
                                                   regionlp == 2 & mes == 6 ~ 12,
                                                   regionlp == 3 & mes == 1 ~ 13,
                                                   regionlp == 3 & mes == 2 ~ 14,
                                                   regionlp == 3 & mes == 3 ~ 15,
                                                   regionlp == 3 & mes == 4 ~ 16,
                                                   regionlp == 3 & mes == 5 ~ 17,
                                                   regionlp == 3 & mes == 6 ~ 18))

sem1_unipersonales <- sem1 %>%  filter(ht19==1)
sem1_unipersonales <- sem1_unipersonales[,c("numero","grupolp", "lp_06")]
sem1_unipersonales <- sem1_unipersonales %>% dplyr::mutate(lp_unipersonales = lp_06)
sem1_unipersonales <- sem1_unipersonales[order(sem1_unipersonales$grupolp, sem1_unipersonales$lp_unipersonales, decreasing = TRUE), ]
sem1_unipersonales <- sem1_unipersonales %>% distinct(grupolp, .keep_all = TRUE)
sem1_unipersonales <- sem1_unipersonales[,c("grupolp","lp_unipersonales")]

sem1 <- merge(sem1, sem1_unipersonales, by = "grupolp")


sem2_implant <- sem2_implant %>% dplyr::mutate(regionlp = case_when(region_4 == 1 ~ 1,
                                                                    region_4 == 2 | region_4 == 3 ~ 2,
                                                                    region_4 == 4 ~ 3))


sem2_implant <- sem2_implant %>% dplyr::mutate(grupolp = case_when(regionlp == 1 & mes == 7 ~ 1,
                                                                   regionlp == 1 & mes == 8 ~ 2,
                                                                   regionlp == 1 & mes == 9 ~ 3,
                                                                   regionlp == 1 & mes == 10 ~ 4,
                                                                   regionlp == 1 & mes == 11 ~ 5,
                                                                   regionlp == 1 & mes == 12 ~ 6,
                                                                   regionlp == 2 & mes == 7 ~ 7,
                                                                   regionlp == 2 & mes == 8 ~ 8,
                                                                   regionlp == 2 & mes == 9 ~ 9,
                                                                   regionlp == 2 & mes == 10 ~ 10,
                                                                   regionlp == 2 & mes == 11 ~ 11,
                                                                   regionlp == 2 & mes == 12 ~ 12,
                                                                   regionlp == 3 & mes == 7 ~ 13,
                                                                   regionlp == 3 & mes == 8 ~ 14,
                                                                   regionlp == 3 & mes == 9 ~ 15,
                                                                   regionlp == 3 & mes == 10 ~ 16,
                                                                   regionlp == 3 & mes == 11 ~ 17,
                                                                   regionlp == 3 & mes == 12 ~ 18))

sem2_implant_unipersonales <- sem2_implant %>%  filter(ht19==1)
sem2_implant_unipersonales <- sem2_implant_unipersonales[,c("grupolp", "lp")]
sem2_implant_unipersonales <- sem2_implant_unipersonales %>% dplyr::mutate(lp_unipersonales = lp)
sem2_implant_unipersonales <- sem2_implant_unipersonales[order(sem2_implant_unipersonales$grupolp, sem2_implant_unipersonales$lp_unipersonales, decreasing = TRUE), ]
sem2_implant_unipersonales <- sem2_implant_unipersonales %>% distinct(grupolp, .keep_all = TRUE)
sem2_implant_unipersonales <- sem2_implant_unipersonales[,c("grupolp","lp_unipersonales")]

sem2_implant <- merge(sem2_implant, sem2_implant_unipersonales, by = "grupolp")



# Salario por debajo de línea de pobreza

sem1 <- sem1 %>% dplyr::mutate(salario_insuf   = ifelse(PT2 < lp_unipersonales, 1, 0))
sem2_implant <- sem2_implant %>% dplyr::mutate(salario_insuf = ifelse(pt2 < lp_unipersonales, 1, 0))


# Salario por debajo del salario mínimo nacional

sem1 <- sem1 %>% dplyr::mutate(salario_insuf_smn = ifelse(yhora<smn_h, 1, 0))
sem2_implant <- sem2_implant %>% dplyr::mutate(salario_insuf_smn = ifelse(yhora<smn_h, 1, 0))


# Percepción de jubilaciones


sem1 <- sem1 %>% dplyr::mutate(jub = ifelse(G148_1_1 > 0 |
                                              G148_1_2 > 0 |
                                              G148_1_3 > 0 |
                                              G148_1_4 > 0 |
                                              G148_1_5 > 0 |
                                              G148_1_6 > 0 |
                                              G148_1_7 > 0 |
                                              G148_1_8 > 0 |
                                              G148_1_9 > 0 |
                                              G148_1_10 > 0 |
                                              G148_1_11 > 0, 1, 0))

sem2_implant <- sem2_implant %>% dplyr::mutate(jub = ifelse(g148_1_1 > 0 |
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

sem1 <- sem1 %>% dplyr::mutate(pens = ifelse(G148_2_1 > 0 |
                                               G148_2_2 > 0 |
                                               G148_2_3 > 0 |
                                               G148_2_4 > 0 |
                                               G148_2_5 > 0 |
                                               G148_2_6 > 0 |
                                               G148_2_7 > 0 |
                                               G148_2_8 > 0 |
                                               G148_2_9 > 0 |
                                               G148_2_10 > 0 |
                                               G148_2_11 > 0, 1, 0))

sem2_implant <- sem2_implant %>% dplyr::mutate(pens = ifelse(g148_2_1 > 0 |
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

sem1 <- sem1 %>% dplyr::mutate(nijubpens = ifelse(jub == 1 | pens == 1, 0, 1))
sem2_implant <- sem2_implant %>% dplyr::mutate(nijubpens = ifelse(jub == 1 | pens == 1, 0, 1))


# Ingresos por jubilaciones

sem1 <- sem1 %>% dplyr::mutate(y_jub = G148_1_1 + G148_1_2 + G148_1_3 + G148_1_4 + G148_1_5 + G148_1_6 + G148_1_7 + G148_1_8 + G148_1_9 + G148_1_12 + G148_1_10 + G148_1_11)
sem2_implant <- sem2_implant %>% dplyr::mutate(y_jub = g148_1_1 + g148_1_2 + g148_1_3 + g148_1_5 + g148_1_6 + g148_1_7 + g148_1_8 + g148_1_9 + g148_1_12 + g148_1_10 + g148_1_11)


# Ingresos por jubilaciones por debajo de la línea de pobreza

sem1 <- sem1 %>% dplyr::mutate(jub_insuf = ifelse(y_jub < lp_unipersonales, 1, 0))
sem2_implant <- sem2_implant %>% dplyr::mutate(jub_insuf = ifelse(y_jub < lp_unipersonales, 1, 0))



# Ingresos por jubilaciones por debajo del salario mínimo nacional


sem1 <- sem1 %>% dplyr::mutate(jub_insuf_smn = ifelse(y_jub < smn, 1, 0))
sem2_implant <- sem2_implant %>% dplyr::mutate(jub_insuf_smn = ifelse(y_jub < smn, 1, 0))


# Ingresos por pensiones


sem1 <- sem1 %>% dplyr::mutate(y_pens = G148_2_1 + G148_2_2 + G148_2_3 + G148_2_4 + G148_2_5 + G148_2_6 + G148_2_7 + G148_2_8 + G148_2_9 + G148_2_12 + G148_2_10 + G148_2_11)
sem2_implant <- sem2_implant %>% dplyr::mutate(y_pens = g148_2_1 + g148_2_2 + g148_2_3 + g148_2_5 + g148_2_6 + g148_2_7 + g148_2_8 + g148_2_9 + g148_2_12 + g148_2_10 + g148_2_11)


# Ingresos por pensiones debajo de la línea de pobreza

sem1 <- sem1 %>% dplyr::mutate(pens_insuf = ifelse(y_pens < lp_unipersonales, 1, 0))
sem2_implant <- sem2_implant %>% dplyr::mutate(pens_insuf = ifelse(y_pens < lp_unipersonales, 1, 0))



# Ingresos por pensiones debajo del SMN


sem1 <- sem1 %>% dplyr::mutate(pens_insuf_smn = ifelse(y_pens < smn, 1, 0))
sem2_implant <- sem2_implant %>% dplyr::mutate(pens_insuf_smn = ifelse(y_pens < smn, 1, 0))


# Desempleados que perciben seguro de desempleo

sem1 <- sem1 %>% dplyr::mutate(seguroparo = ifelse(POBPCOAC == 5, 1, 0))
sem2_panel <- sem2_panel %>% dplyr::mutate(seguroparo = ifelse(POBPCOAC == 5, 1, 0))



# Captación AFAM

sem1 <- sem1 %>% dplyr::mutate(afam_pe = ifelse((G152 == 1 & F73 != 2 & F92 !=2 ) | (G150 == 1 & (POBPCOAC %in% c(1, 3, 4, 6, 7, 8, 11) | (POBPCOAC == 2 & F82 != 1 & F96 != 1))), 1, 0),   
                               afam_cont = ifelse(G150 == 1 & afam_pe == 0, 1, 0),
                               afam_total = ifelse(afam_pe == 1 | afam_cont == 1, 1, 0)) 

sem1_afam <- sem1[,c("numero","afam_pe", "afam_cont", "afam_total")]
sem1_afam <- sem1_afam %>% dplyr::mutate(h_afam_pe = afam_pe,
                                         h_afam_cont = afam_cont,
                                         h_afam_total = afam_total)

sem1_afam_1 <- sem1_afam[order(sem1_afam$numero, sem1_afam$h_afam_pe, decreasing = TRUE), ]
sem1_afam_2 <- sem1_afam[order(sem1_afam$numero, sem1_afam$h_afam_cont, decreasing = TRUE), ]
sem1_afam_3 <- sem1_afam[order(sem1_afam$numero, sem1_afam$h_afam_total, decreasing = TRUE), ]

sem1_afam_1 <- sem1_afam_1 %>% distinct(numero, .keep_all = TRUE)
sem1_afam_2 <- sem1_afam_2 %>% distinct(numero, .keep_all = TRUE)
sem1_afam_3 <- sem1_afam_3 %>% distinct(numero, .keep_all = TRUE)

sem1_afam_1 <- sem1_afam_1[,c("numero","h_afam_pe")]
sem1_afam_2 <- sem1_afam_2[,c("numero","h_afam_cont")]
sem1_afam_3 <- sem1_afam_3[,c("numero","h_afam_total")]

sem1 <- merge(sem1, sem1_afam_1, by = "numero")
sem1 <- merge(sem1, sem1_afam_2, by = "numero")
sem1 <- merge(sem1, sem1_afam_3, by = "numero")


sem2_implant <- sem2_implant %>% dplyr::mutate(afam_pe = ifelse((g152 == 1 & f73 != 2 & f92 !=2 ) | (g150 == 1 & (pobpcoac %in% c(1, 3, 4, 6, 7, 8, 11) | (pobpcoac == 2 & f82 != 1 & f96 != 1))), 1, 0),   
                                               afam_cont = ifelse(g150 == 1 & afam_pe == 0, 1, 0),
                                               afam_total = ifelse(afam_pe == 1 | afam_cont == 1, 1, 0)) 

sem2_afam <- sem2_implant[,c("ID","afam_pe", "afam_cont", "afam_total")]
sem2_afam <- sem2_afam %>% dplyr::mutate(h_afam_pe = afam_pe,
                                         h_afam_cont = afam_cont,
                                         h_afam_total = afam_total)

sem2_afam_1 <- sem2_afam[order(sem2_afam$ID, sem2_afam$h_afam_pe, decreasing = TRUE), ]
sem2_afam_2 <- sem2_afam[order(sem2_afam$ID, sem2_afam$h_afam_cont, decreasing = TRUE), ]
sem2_afam_3 <- sem2_afam[order(sem2_afam$ID, sem2_afam$h_afam_total, decreasing = TRUE), ]

sem2_afam_1 <- sem2_afam_1 %>% distinct(ID, .keep_all = TRUE)
sem2_afam_2 <- sem2_afam_2 %>% distinct(ID, .keep_all = TRUE)
sem2_afam_3 <- sem2_afam_3 %>% distinct(ID, .keep_all = TRUE)

sem2_afam_1 <- sem2_afam_1[,c("ID","h_afam_pe")]
sem2_afam_2 <- sem2_afam_2[,c("ID","h_afam_cont")]
sem2_afam_3 <- sem2_afam_3[,c("ID","h_afam_total")]

sem2_implant <- merge(sem2_implant, sem2_afam_1, by = "ID")
sem2_implant <- merge(sem2_implant, sem2_afam_2, by = "ID")
sem2_implant <- merge(sem2_implant, sem2_afam_3, by = "ID")



#  Captación TUS

sem1 <- sem1 %>% dplyr::mutate(tus = ifelse(E560==1, 1, 0))

sem1_tus <- sem1[,c("numero","tus")]
sem1_tus <- sem1_tus %>% dplyr::mutate(h_tus = tus)
sem1_tus <- sem1_tus[order(sem1_tus$numero, sem1_tus$h_tus, decreasing = TRUE), ]
sem1_tus <- sem1_tus %>% distinct(numero, .keep_all = TRUE)
sem1_tus <- sem1_tus[,c("numero","h_tus")]
sem1 <- merge(sem1, sem1_tus, by = "numero")


sem2_implant <- sem2_implant %>% dplyr::mutate(tus = ifelse(e560==1, 1, 0))

sem2_tus <- sem2_implant[,c("ID","tus")]
sem2_tus <- sem2_tus %>% dplyr::mutate(h_tus = tus)
sem2_tus <- sem2_tus[order(sem2_tus$ID, sem2_tus$h_tus, decreasing = TRUE), ]
sem2_tus <- sem2_tus %>% distinct(ID, .keep_all = TRUE)
sem2_tus <- sem2_tus[,c("ID","h_tus")]
sem2_implant <- merge(sem2_implant, sem2_tus, by = "ID")


# Categoría de la ocupación

sem1 <- sem1 %>% dplyr::mutate(apr = ifelse(F73 == 1 , 1, 0))
sem1 <- sem1 %>% dplyr::mutate(apu = ifelse(F73 == 2 , 1, 0))
sem1 <- sem1 %>% dplyr::mutate(coo = ifelse(F73 == 3 , 1, 0))
sem1 <- sem1 %>% dplyr::mutate(pat = ifelse(F73 == 4 , 1, 0))
sem1 <- sem1 %>% dplyr::mutate(cps = ifelse(F73 == 5 , 1, 0))
sem1 <- sem1 %>% dplyr::mutate(cpc = ifelse(F73 == 6 , 1, 0))
sem1 <- sem1 %>% dplyr::mutate(mnr = ifelse(F73 == 7 , 1, 0))
sem1 <- sem1 %>% dplyr::mutate(pse = ifelse(F73 == 8 , 1, 0))

sem2_panel <- sem2_panel %>% dplyr::mutate(apr = ifelse(f73 == 1 , 1, 0))
sem2_panel <- sem2_panel %>% dplyr::mutate(apu = ifelse(f73 == 2 , 1, 0))
sem2_panel <- sem2_panel %>% dplyr::mutate(coo = ifelse(f73 == 3 , 1, 0))
sem2_panel <- sem2_panel %>% dplyr::mutate(pat = ifelse(f73 == 4 , 1, 0))
sem2_panel <- sem2_panel %>% dplyr::mutate(cps = ifelse(f73 == 5 , 1, 0))
sem2_panel <- sem2_panel %>% dplyr::mutate(cpc = ifelse(f73 == 6 , 1, 0))
sem2_panel <- sem2_panel %>% dplyr::mutate(mnr = ifelse(f73 == 7 , 1, 0))
sem2_panel <- sem2_panel %>% dplyr::mutate(pse = ifelse(f73 == 8 , 1, 0))

categoria <- c("apr", "apu", "coo", "pat", "cps", "cpc", "mnr", "pse")




# Tipo de ocupación

sem1 <- sem1 %>% dplyr::mutate(mil = ifelse(F71_2 >  0000 & F71_2 < 1000, 1, 0))
sem1 <- sem1 %>% dplyr::mutate(dir = ifelse(F71_2 >= 1000 & F71_2 < 2000, 1, 0))
sem1 <- sem1 %>% dplyr::mutate(pro = ifelse(F71_2 >= 2000 & F71_2 < 3000, 1, 0))
sem1 <- sem1 %>% dplyr::mutate(tec = ifelse(F71_2 >= 3000 & F71_2 < 4000, 1, 0))
sem1 <- sem1 %>% dplyr::mutate(adm = ifelse(F71_2 >= 4000 & F71_2 < 5000, 1, 0))
sem1 <- sem1 %>% dplyr::mutate(ser = ifelse(F71_2 >= 5000 & F71_2 < 6000, 1, 0))
sem1 <- sem1 %>% dplyr::mutate(agr = ifelse(F71_2 >= 6000 & F71_2 < 7000, 1, 0))
sem1 <- sem1 %>% dplyr::mutate(ofi = ifelse(F71_2 >= 7000 & F71_2 < 8000, 1, 0))
sem1 <- sem1 %>% dplyr::mutate(ope = ifelse(F71_2 >= 8000 & F71_2 < 9000, 1, 0))
sem1 <- sem1 %>% dplyr::mutate(ele = ifelse(F71_2 >= 9000 , 1, 0))

sem2_panel <- sem2_panel %>% dplyr::mutate(mil = ifelse(f71_2 >  0000 & f71_2 < 1000, 1, 0))
sem2_panel <- sem2_panel %>% dplyr::mutate(dir = ifelse(f71_2 >= 1000 & f71_2 < 2000, 1, 0))
sem2_panel <- sem2_panel %>% dplyr::mutate(pro = ifelse(f71_2 >= 2000 & f71_2 < 3000, 1, 0))
sem2_panel <- sem2_panel %>% dplyr::mutate(tec = ifelse(f71_2 >= 3000 & f71_2 < 4000, 1, 0))
sem2_panel <- sem2_panel %>% dplyr::mutate(adm = ifelse(f71_2 >= 4000 & f71_2 < 5000, 1, 0))
sem2_panel <- sem2_panel %>% dplyr::mutate(ser = ifelse(f71_2 >= 5000 & f71_2 < 6000, 1, 0))
sem2_panel <- sem2_panel %>% dplyr::mutate(agr = ifelse(f71_2 >= 6000 & f71_2 < 7000, 1, 0))
sem2_panel <- sem2_panel %>% dplyr::mutate(ofi = ifelse(f71_2 >= 7000 & f71_2 < 8000, 1, 0))
sem2_panel <- sem2_panel %>% dplyr::mutate(ope = ifelse(f71_2 >= 8000 & f71_2 < 9000, 1, 0))
sem2_panel <- sem2_panel %>% dplyr::mutate(ele = ifelse(f71_2 >= 9000 , 1, 0))

ocupacion <- c("mil", "dir", "pro", "tec", "adm", "ser", "agr", "ofi", "ope", "ele")


# Rama de actividad

sem1 <- sem1 %>% dplyr::mutate(r1  = ifelse(F72_2 >  0000 & F72_2 < 1000, 1, 0))
sem1 <- sem1 %>% dplyr::mutate(r2  = ifelse(F72_2 >  1000 & F72_2 < 4000, 1, 0))
sem1 <- sem1 %>% dplyr::mutate(r3  = ifelse(F72_2 >  4000 & F72_2 < 4500, 1, 0))
sem1 <- sem1 %>% dplyr::mutate(r4  = ifelse(F72_2 >= 4500 & F72_2 < 4900, 1, 0))
sem1 <- sem1 %>% dplyr::mutate(r5  = ifelse(F72_2 >= 4900 & F72_2 < 5500, 1, 0))
sem1 <- sem1 %>% dplyr::mutate(r6  = ifelse((F72_2 >= 5500 & F72_2 < 5800) | (F72_2 >= 9000 & F72_2 < 9400) | (F72_2 >= 9400 & F72_2 < 9700), 1, 0))
sem1 <- sem1 %>% dplyr::mutate(r7  = ifelse(F72_2 >= 5800 & F72_2 < 6400, 1, 0))
sem1 <- sem1 %>% dplyr::mutate(r8  = ifelse(F72_2 >= 6400 & F72_2 < 6800, 1, 0))
sem1 <- sem1 %>% dplyr::mutate(r9  = ifelse(F72_2 >= 6800 & F72_2 < 6900, 1, 0))
sem1 <- sem1 %>% dplyr::mutate(r10 = ifelse(F72_2 >= 6900 & F72_2 < 7500, 1, 0))
sem1 <- sem1 %>% dplyr::mutate(r11 = ifelse(F72_2 >= 7500 & F72_2 < 8400, 1, 0))
sem1 <- sem1 %>% dplyr::mutate(r12 = ifelse((F72_2 >= 8400 & F72_2 < 8500) | (F72_2 == 9900), 1, 0))
sem1 <- sem1 %>% dplyr::mutate(r13 = ifelse(F72_2 >= 8500 & F72_2 < 8600, 1, 0))
sem1 <- sem1 %>% dplyr::mutate(r14 = ifelse(F72_2 >= 8600 & F72_2 < 9000, 1, 0))
sem1 <- sem1 %>% dplyr::mutate(r17 = ifelse(F72_2 >= 9700 & F72_2 < 9900, 1, 0))

sem2_panel <- sem2_panel %>% dplyr::mutate(r1  = ifelse(f72_2 >  0000 & f72_2 < 1000, 1, 0))
sem2_panel <- sem2_panel %>% dplyr::mutate(r2  = ifelse(f72_2 >  1000 & f72_2 < 4000, 1, 0))
sem2_panel <- sem2_panel %>% dplyr::mutate(r3  = ifelse(f72_2 >  4000 & f72_2 < 4500, 1, 0))
sem2_panel <- sem2_panel %>% dplyr::mutate(r4  = ifelse(f72_2 >= 4500 & f72_2 < 4900, 1, 0))
sem2_panel <- sem2_panel %>% dplyr::mutate(r5  = ifelse(f72_2 >= 4900 & f72_2 < 5500, 1, 0))
sem2_panel <- sem2_panel %>% dplyr::mutate(r6  = ifelse((f72_2 >= 5500 & f72_2 < 5800) | (f72_2 >= 9000 & f72_2 < 9400  | (f72_2 >= 9400 & f72_2 < 9700)), 1, 0))
sem2_panel <- sem2_panel %>% dplyr::mutate(r7  = ifelse(f72_2 >= 5800 & f72_2 < 6400, 1, 0))
sem2_panel <- sem2_panel %>% dplyr::mutate(r8  = ifelse(f72_2 >= 6400 & f72_2 < 6800, 1, 0))
sem2_panel <- sem2_panel %>% dplyr::mutate(r9  = ifelse(f72_2 >= 6800 & f72_2 < 6900, 1, 0))
sem2_panel <- sem2_panel %>% dplyr::mutate(r10 = ifelse(f72_2 >= 6900 & f72_2 < 7500, 1, 0))
sem2_panel <- sem2_panel %>% dplyr::mutate(r11 = ifelse(f72_2 >= 7500 & f72_2 < 8400, 1, 0))
sem2_panel <- sem2_panel %>% dplyr::mutate(r12 = ifelse((f72_2 >= 8400 & f72_2 < 8500) | (f72_2 == 9900), 1, 0))
sem2_panel <- sem2_panel %>% dplyr::mutate(r13 = ifelse(f72_2 >= 8500 & f72_2 < 8600, 1, 0))
sem2_panel <- sem2_panel %>% dplyr::mutate(r14 = ifelse(f72_2 >= 8600 & f72_2 < 9000, 1, 0))
sem2_panel <- sem2_panel %>% dplyr::mutate(r17 = ifelse(f72_2 >= 9700 & f72_2 < 9900, 1, 0))

rama <- c("r1", "r2", "r3", "r4", "r5", "r6", "r7", "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r17")





# Restricciones al empleo

sem1 <- sem1 %>% dplyr::mutate(nre = ifelse(bc_register2 == 1 & bc_subocupado == 0 , 1, 0))
sem1 <- sem1 %>% dplyr::mutate(sub = ifelse(bc_register2 == 0 & bc_subocupado == 1 , 1, 0))
sem1 <- sem1 %>% dplyr::mutate(amb = ifelse(bc_register2 == 1 & bc_subocupado == 1 , 1, 0))
sem1 <- sem1 %>% dplyr::mutate(sin = ifelse(bc_register2 == 0 & bc_subocupado == 0 , 1, 0))

sem2_panel <- sem2_panel %>% dplyr::mutate(nre = ifelse(bc_register2 == 1 & bc_subocupado == 0 , 1, 0))
sem2_panel <- sem2_panel %>% dplyr::mutate(sub = ifelse(bc_register2 == 0 & bc_subocupado == 1 , 1, 0))
sem2_panel <- sem2_panel %>% dplyr::mutate(amb = ifelse(bc_register2 == 1 & bc_subocupado == 1 , 1, 0))
sem2_panel <- sem2_panel %>% dplyr::mutate(sin = ifelse(bc_register2 == 0 & bc_subocupado == 0 , 1, 0))

restricciones <- c("nre", "sub", "amb", "sin")



rm(pobres, quintil, sem1_h, sem2_h, sem1_unipersonales, sem2_implant_unipersonales, sem1_tus, sem2_tus, sem1_afam, sem2_afam,
   sem1_afam_1, sem1_afam_2, sem1_afam_3, sem2_afam_1, sem2_afam_2, sem2_afam_3)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Bases a nivel hogar ###                               

sem1_h <- sem1 %>% distinct(numero, .keep_all = TRUE)
sem2_implant_h <- sem2_implant %>% distinct(ID, .keep_all = TRUE)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Información de muestreo ###


# Ponderador semestral semestre 1:
sem1 <- sem1 %>% dplyr::mutate(pesosem = pesomen / 6)


sem1_svy           <- srvyr::as_survey_design(sem1, ids = numero, weights = pesomen)
sem1_svy2           <- srvyr::as_survey_design(sem1, ids = numero, weights = pesosem)

sem1_h_svy         <- srvyr::as_survey_design(sem1_h, ids = numero, weights = pesomen)

sem2_implant_svy   <- srvyr::as_survey_design(sem2_implant, ids = ID, weights = w_sem)
sem2_implant_h_svy <- srvyr::as_survey_design(sem2_implant_h, ids = ID, weights = w_sem)

sem2_panel_svy   <-  srvyr::as_survey_design(sem2_panel, ids = ID, weights = w)

#sem2_panel_svy     <- svrepdesign(data = sem2_panel,
#                                type = "bootstrap",
#                                 weights =~ w,
#                                 repweights = sem2_panel %>% dplyr::select(dplyr::starts_with("wr")))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Estimación de indicadores ###

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
### 311 Tasa de actividad
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

## Total país


# Total

a_mes <- sem1_svy %>%
  srvyr::filter(E27>=14) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(pea, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

b_mes <- sem2_panel_svy %>%
  srvyr::filter(e27>=14) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(pea, na.rm=T))

b_sem <- mean(as.numeric(b_mes$colname))

c_ano <- mean(c(a_sem, b_sem))
c_ano <- as.data.frame(c_ano)
c_ano <- c_ano %>% dplyr::mutate(aux = 1)
c_ano <- c_ano %>% dplyr::rename(VALOR = c_ano)

# Ascendencia étnico racial

a_afro <- function(x) {
  x <- sem1_svy %>%
    filter(bd_e29_1 == x & E27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pea, na.rm=T))
  x <- mean(x$colname)
}       

a_e_afro <- numeric()

for(i in 1:2){
  a_e_afro[i] <- a_afro(x = i)
}     

b_afro <- function(x) {
  x <- sem2_panel_svy %>%
    filter(bd_e29_1 == x & e27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pea, na.rm=T))
  x <- mean(x$colname)
}       

b_e_afro <- numeric()

for(i in 1:2){
  b_e_afro[i] <- b_afro(x = i)
}     

c_afro <- as.data.frame(cbind(a_e_afro, b_e_afro))
c_afro <- c_afro %>% dplyr::mutate(VALOR = (a_e_afro + b_e_afro)/2)
c_afro <- c_afro %>% dplyr::mutate(aux = c(2,3))
c_afro <- c_afro[,c("VALOR", "aux")]



# Quintil de ingresos

a_quintil <- function(x) {
  x <- sem1_svy %>%
    filter(quintilesy == x & E27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pea, na.rm=T))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

b_quintil <- function(x) {
  x <- sem2_panel_svy %>%
    filter(quintilesy == x & e27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pea, na.rm=T))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}     

c_quintil <- as.data.frame(cbind(a_e_quintil, b_e_quintil))
c_quintil <- c_quintil %>% dplyr::mutate(VALOR = (a_e_quintil + b_e_quintil)/2)
c_quintil <- c_quintil %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil <- c_quintil[,c("VALOR", "aux")]




# Sexo

a_sexo <- function(x) {
  x <- sem1_svy %>%
    filter(bc_pe2 == x & E27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pea, na.rm=T))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}     

b_sexo <- function(x) {
  x <- sem2_panel_svy %>%
    filter(bc_pe2 == x & e27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pea, na.rm=T))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}     


c_sexo <- as.data.frame(cbind(a_e_sexo, b_e_sexo))
c_sexo <- c_sexo %>% dplyr::mutate(VALOR = (a_e_sexo + b_e_sexo)/2)
c_sexo <- c_sexo %>% dplyr::mutate(aux = c(9, 10))
c_sexo <- c_sexo[,c("VALOR", "aux")]


# Tramo de edad

a_edad <- function(x) {
  x <- sem1_svy %>%
    filter(tramo_edad == x & E27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pea, na.rm=T))
  x <- mean(x$colname)
}       

a_e_edad <- numeric()

for(i in 1:5){
  a_e_edad[i] <- a_edad(x = i)
}     

b_edad <- function(x) {
  x <- sem2_panel_svy %>%
    filter(tramo_edad == x & e27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pea, na.rm=T))
  x <- mean(x$colname)
}       

b_e_edad <- numeric()

for(i in 1:5){
  b_e_edad[i] <- b_edad(x = i)
} 

c_edad <- as.data.frame(cbind(a_e_edad, b_e_edad))
c_edad <- c_edad %>% dplyr::mutate(VALOR = (a_e_edad + b_e_edad)/2)
c_edad <- c_edad %>% dplyr::mutate(aux = c(11, 12, 13, 14, 15))
c_edad <- c_edad[,c("VALOR", "aux")]



# Región

a_region <- function(x) {
  x <- sem1_svy %>%
    filter(region_4 == x & E27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pea, na.rm=T))
  x <- mean(x$colname)
}       

a_e_region <- numeric()

for(i in 3:4){
  a_e_region[i] <- a_region(x = i)
}     

b_region <- function(x) {
  x <- sem2_panel_svy %>%
    filter(region_4 == x & e27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pea, na.rm=T))
  x <- mean(x$colname)
}       

b_e_region <- numeric()

for(i in 3:4){
  b_e_region[i] <- b_region(x = i)
}     


c_region <- as.data.frame(cbind(a_e_region, b_e_region))
c_region <- c_region %>% dplyr::mutate(VALOR = (a_e_region + b_e_region)/2)
c_region <- c_region[c(3,4),]
c_region <- c_region %>% dplyr::mutate(aux = c(16, 17))
c_region <- c_region[,c("VALOR", "aux")]


# Condición de pobreza del hogar

a_pobre <- function(x) {
  x <- sem1_svy %>%
    filter(pobre_06 == x & E27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pea, na.rm=T))
  x <- mean(x$colname)
}       

a_e_pobre <- numeric()

for(i in 1:2){
  a_e_pobre[i] <- a_pobre(x = i)
}     

b_pobre <- function(x) {
  x <- sem2_panel_svy %>%
    filter(pobre_06 == x & e27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pea, na.rm=T))
  x <- mean(x$colname)
}       

b_e_pobre <- numeric()

for(i in 1:2){
  b_e_pobre[i] <- b_pobre(x = i)
}     


c_pobre <- as.data.frame(cbind(a_e_pobre, b_e_pobre))
c_pobre <- c_pobre %>% dplyr::mutate(VALOR = (a_e_pobre + b_e_pobre)/2)
c_pobre <- c_pobre %>% dplyr::mutate(aux = c(18, 19))
c_pobre <- c_pobre[,c("VALOR", "aux")]


## País Urbano

# Total

a_mes <- sem1_svy %>%
  srvyr::filter(E27>=14 & (region_4 == 1 | region_4 == 2)) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(pea, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

b_mes <- sem2_panel_svy %>%
  srvyr::filter(e27>=14 & (region_4 == 1 | region_4 == 2)) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(pea, na.rm=T))

b_sem <- mean(as.numeric(b_mes$colname))

c_ano_pu <- mean(c(a_sem, b_sem))
c_ano_pu <- as.data.frame(c_ano_pu)
c_ano_pu <- c_ano_pu %>% dplyr::mutate(aux = 1)
c_ano_pu <- c_ano_pu %>% dplyr::rename(VALOR = c_ano_pu)

# Quintil de ingresos

a_quintil <- function(x) {
  x <- sem1_svy %>%
    filter(quintilesy == x & E27>=14 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pea, na.rm=T))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

b_quintil <- function(x) {
  x <- sem2_panel_svy %>%
    filter(quintilesy == x & e27>=14 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pea, na.rm=T))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}     

c_quintil_pu <- as.data.frame(cbind(a_e_quintil, b_e_quintil))
c_quintil_pu <- c_quintil_pu %>% dplyr::mutate(VALOR = (a_e_quintil + b_e_quintil)/2)
c_quintil_pu <- c_quintil_pu %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil_pu <- c_quintil_pu[,c("VALOR", "aux")]


# Sexo

a_sexo <- function(x) {
  x <- sem1_svy %>%
    filter(bc_pe2 == x & E27>=14 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pea, na.rm=T))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}     

b_sexo <- function(x) {
  x <- sem2_panel_svy %>%
    filter(bc_pe2 == x & e27>=14 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pea, na.rm=T))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}     


c_sexo_pu <- as.data.frame(cbind(a_e_sexo, b_e_sexo))
c_sexo_pu <- c_sexo_pu %>% dplyr::mutate(VALOR = (a_e_sexo + b_e_sexo)/2)
c_sexo_pu <- c_sexo_pu %>% dplyr::mutate(aux = c(9, 10))
c_sexo_pu <- c_sexo_pu[,c("VALOR", "aux")]


# Tramo de edad

a_edad <- function(x) {
  x <- sem1_svy %>%
    filter(tramo_edad == x & E27>=14 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pea, na.rm=T))
  x <- mean(x$colname)
}       

a_e_edad <- numeric()

for(i in 1:5){
  a_e_edad[i] <- a_edad(x = i)
}     

b_edad <- function(x) {
  x <- sem2_panel_svy %>%
    filter(tramo_edad == x & e27>=14 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pea, na.rm=T))
  x <- mean(x$colname)
}       

b_e_edad <- numeric()

for(i in 1:5){
  b_e_edad[i] <- b_edad(x = i)
} 

c_edad_pu <- as.data.frame(cbind(a_e_edad, b_e_edad))
c_edad_pu <- c_edad_pu %>% dplyr::mutate(VALOR = (a_e_edad + b_e_edad)/2)
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

a_mes <- sem1_svy %>%
  srvyr::filter(E27>=14) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(ocup, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

b_mes <- sem2_panel_svy %>%
  srvyr::filter(e27>=14) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(ocup, na.rm=T))

b_sem <- mean(as.numeric(b_mes$colname))

c_ano <- mean(c(a_sem, b_sem))
c_ano <- as.data.frame(c_ano)
c_ano <- c_ano %>% dplyr::mutate(aux = 1)
c_ano <- c_ano %>% dplyr::rename(VALOR = c_ano)

# Ascendencia étnico racial

a_afro <- function(x) {
  x <- sem1_svy %>%
    filter(bd_e29_1 == x & E27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(ocup, na.rm=T))
  x <- mean(x$colname)
}       

a_e_afro <- numeric()

for(i in 1:2){
  a_e_afro[i] <- a_afro(x = i)
}     

b_afro <- function(x) {
  x <- sem2_panel_svy %>%
    filter(bd_e29_1 == x & e27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(ocup, na.rm=T))
  x <- mean(x$colname)
}       

b_e_afro <- numeric()

for(i in 1:2){
  b_e_afro[i] <- b_afro(x = i)
}     

c_afro <- as.data.frame(cbind(a_e_afro, b_e_afro))
c_afro <- c_afro %>% dplyr::mutate(VALOR = (a_e_afro + b_e_afro)/2)
c_afro <- c_afro %>% dplyr::mutate(aux = c(2,3))
c_afro <- c_afro[,c("VALOR", "aux")]



# Quintil de ingresos

a_quintil <- function(x) {
  x <- sem1_svy %>%
    filter(quintilesy == x & E27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(ocup, na.rm=T))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

b_quintil <- function(x) {
  x <- sem2_panel_svy %>%
    filter(quintilesy == x & e27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(ocup, na.rm=T))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}     

c_quintil <- as.data.frame(cbind(a_e_quintil, b_e_quintil))
c_quintil <- c_quintil %>% dplyr::mutate(VALOR = (a_e_quintil + b_e_quintil)/2)
c_quintil <- c_quintil %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil <- c_quintil[,c("VALOR", "aux")]

# Sexo

a_sexo <- function(x) {
  x <- sem1_svy %>%
    filter(bc_pe2 == x & E27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(ocup, na.rm=T))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}     

b_sexo <- function(x) {
  x <- sem2_panel_svy %>%
    filter(bc_pe2 == x & e27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(ocup, na.rm=T))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}     

c_sexo <- as.data.frame(cbind(a_e_sexo, b_e_sexo))
c_sexo <- c_sexo %>% dplyr::mutate(VALOR = (a_e_sexo + b_e_sexo)/2)
c_sexo <- c_sexo %>% dplyr::mutate(aux = c(9, 10))
c_sexo <- c_sexo[,c("VALOR", "aux")]

# Tramo de edad

a_edad <- function(x) {
  x <- sem1_svy %>%
    filter(tramo_edad == x & E27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(ocup, na.rm=T))
  x <- mean(x$colname)
}       

a_e_edad <- numeric()

for(i in 1:5){
  a_e_edad[i] <- a_edad(x = i)
}     

b_edad <- function(x) {
  x <- sem2_panel_svy %>%
    filter(tramo_edad == x & e27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(ocup, na.rm=T))
  x <- mean(x$colname)
}       

b_e_edad <- numeric()

for(i in 1:5){
  b_e_edad[i] <- b_edad(x = i)
} 

c_edad <- as.data.frame(cbind(a_e_edad, b_e_edad))
c_edad <- c_edad %>% dplyr::mutate(VALOR = (a_e_edad + b_e_edad)/2)
c_edad <- c_edad %>% dplyr::mutate(aux = c(11, 12, 13, 14, 15))
c_edad <- c_edad[,c("VALOR", "aux")]



# Región

a_region <- function(x) {
  x <- sem1_svy %>%
    filter(region_4 == x & E27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(ocup, na.rm=T))
  x <- mean(x$colname)
}       

a_e_region <- numeric()

for(i in 3:4){
  a_e_region[i] <- a_region(x = i)
}     

b_region <- function(x) {
  x <- sem2_panel_svy %>%
    filter(region_4 == x & e27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(ocup, na.rm=T))
  x <- mean(x$colname)
}       

b_e_region <- numeric()

for(i in 3:4){
  b_e_region[i] <- b_region(x = i)
}     


c_region <- as.data.frame(cbind(a_e_region, b_e_region))
c_region <- c_region %>% dplyr::mutate(VALOR = (a_e_region + b_e_region)/2)
c_region <- c_region[c(3,4),]
c_region <- c_region %>% dplyr::mutate(aux = c(16, 17))
c_region <- c_region[,c("VALOR", "aux")]


# Condición de pobreza del hogar

a_pobre <- function(x) {
  x <- sem1_svy %>%
    filter(pobre_06 == x & E27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(ocup, na.rm=T))
  x <- mean(x$colname)
}       

a_e_pobre <- numeric()

for(i in 1:2){
  a_e_pobre[i] <- a_pobre(x = i)
}     

b_pobre <- function(x) {
  x <- sem2_panel_svy %>%
    filter(pobre_06 == x & e27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(ocup, na.rm=T))
  x <- mean(x$colname)
}       

b_e_pobre <- numeric()

for(i in 1:2){
  b_e_pobre[i] <- b_pobre(x = i)
}     


c_pobre <- as.data.frame(cbind(a_e_pobre, b_e_pobre))
c_pobre <- c_pobre %>% dplyr::mutate(VALOR = (a_e_pobre + b_e_pobre)/2)
c_pobre <- c_pobre %>% dplyr::mutate(aux = c(18, 19))
c_pobre <- c_pobre[,c("VALOR", "aux")]


## País Urbano

# Total

a_mes <- sem1_svy %>%
  srvyr::filter(E27>=14 & (region_4 == 1 | region_4 == 2)) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(ocup, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

b_mes <- sem2_panel_svy %>%
  srvyr::filter(e27>=14 & (region_4 == 1 | region_4 == 2)) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(ocup, na.rm=T))

b_sem <- mean(as.numeric(b_mes$colname))

c_ano_pu <- mean(c(a_sem, b_sem))
c_ano_pu <- as.data.frame(c_ano_pu)
c_ano_pu <- c_ano_pu %>% dplyr::mutate(aux = 1)
c_ano_pu <- c_ano_pu %>% dplyr::rename(VALOR = c_ano_pu)

# Quintil de ingresos

a_quintil <- function(x) {
  x <- sem1_svy %>%
    filter(quintilesy == x & E27>=14 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(ocup, na.rm=T))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

b_quintil <- function(x) {
  x <- sem2_panel_svy %>%
    filter(quintilesy == x & e27>=14 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(ocup, na.rm=T))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}     

c_quintil_pu <- as.data.frame(cbind(a_e_quintil, b_e_quintil))
c_quintil_pu <- c_quintil_pu %>% dplyr::mutate(VALOR = (a_e_quintil + b_e_quintil)/2)
c_quintil_pu <- c_quintil_pu %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil_pu <- c_quintil_pu[,c("VALOR", "aux")]


# Sexo

a_sexo <- function(x) {
  x <- sem1_svy %>%
    filter(bc_pe2 == x & E27>=14 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(ocup, na.rm=T))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}     

b_sexo <- function(x) {
  x <- sem2_panel_svy %>%
    filter(bc_pe2 == x & e27>=14 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(ocup, na.rm=T))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}     


c_sexo_pu <- as.data.frame(cbind(a_e_sexo, b_e_sexo))
c_sexo_pu <- c_sexo_pu %>% dplyr::mutate(VALOR = (a_e_sexo + b_e_sexo)/2)
c_sexo_pu <- c_sexo_pu %>% dplyr::mutate(aux = c(9, 10))
c_sexo_pu <- c_sexo_pu[,c("VALOR", "aux")]


# Tramo de edad

a_edad <- function(x) {
  x <- sem1_svy %>%
    filter(tramo_edad == x & E27>=14 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(ocup, na.rm=T))
  x <- mean(x$colname)
}       

a_e_edad <- numeric()

for(i in 1:5){
  a_e_edad[i] <- a_edad(x = i)
}     

b_edad <- function(x) {
  x <- sem2_panel_svy %>%
    filter(tramo_edad == x & e27>=14 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(ocup, na.rm=T))
  x <- mean(x$colname)
}       

b_e_edad <- numeric()

for(i in 1:5){
  b_e_edad[i] <- b_edad(x = i)
} 

c_edad_pu <- as.data.frame(cbind(a_e_edad, b_e_edad))
c_edad_pu <- c_edad_pu %>% dplyr::mutate(VALOR = (a_e_edad + b_e_edad)/2)
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

a_mes <- sem1_svy %>%
  srvyr::filter(pea == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(desocup, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

b_mes <- sem2_panel_svy %>%
  srvyr::filter(pea == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(desocup, na.rm=T))

b_sem <- mean(as.numeric(b_mes$colname))

c_ano <- mean(c(a_sem, b_sem))
c_ano <- as.data.frame(c_ano)
c_ano <- c_ano %>% dplyr::mutate(aux = 1)
c_ano <- c_ano %>% dplyr::rename(VALOR = c_ano)

# Ascendencia étnico racial

a_afro <- function(x) {
  x <- sem1_svy %>%
    filter(bd_e29_1 == x & pea == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(desocup, na.rm=T))
  x <- mean(x$colname)
}       

a_e_afro <- numeric()

for(i in 1:2){
  a_e_afro[i] <- a_afro(x = i)
}     

b_afro <- function(x) {
  x <- sem2_panel_svy %>%
    filter(bd_e29_1 == x & pea == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(desocup, na.rm=T))
  x <- mean(x$colname)
}       

b_e_afro <- numeric()

for(i in 1:2){
  b_e_afro[i] <- b_afro(x = i)
}     

c_afro <- as.data.frame(cbind(a_e_afro, b_e_afro))
c_afro <- c_afro %>% dplyr::mutate(VALOR = (a_e_afro + b_e_afro)/2)
c_afro <- c_afro %>% dplyr::mutate(aux = c(2,3))
c_afro <- c_afro[,c("VALOR", "aux")]



# Quintil de ingresos

a_quintil <- function(x) {
  x <- sem1_svy %>%
    filter(quintilesy == x & pea == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(desocup, na.rm=T))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

b_quintil <- function(x) {
  x <- sem2_panel_svy %>%
    filter(quintilesy == x & pea == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(desocup, na.rm=T))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}     

c_quintil <- as.data.frame(cbind(a_e_quintil, b_e_quintil))
c_quintil <- c_quintil %>% dplyr::mutate(VALOR = (a_e_quintil + b_e_quintil)/2)
c_quintil <- c_quintil %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil <- c_quintil[,c("VALOR", "aux")]

# Sexo

a_sexo <- function(x) {
  x <- sem1_svy %>%
    filter(bc_pe2 == x & pea == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(desocup, na.rm=T))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}     

b_sexo <- function(x) {
  x <- sem2_panel_svy %>%
    filter(bc_pe2 == x & pea == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(desocup, na.rm=T))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}     

c_sexo <- as.data.frame(cbind(a_e_sexo, b_e_sexo))
c_sexo <- c_sexo %>% dplyr::mutate(VALOR = (a_e_sexo + b_e_sexo)/2)
c_sexo <- c_sexo %>% dplyr::mutate(aux = c(9, 10))
c_sexo <- c_sexo[,c("VALOR", "aux")]

# Tramo de edad

a_edad <- function(x) {
  x <- sem1_svy %>%
    filter(tramo_edad == x & pea == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(desocup, na.rm=T))
  x <- mean(x$colname)
}       

a_e_edad <- numeric()

for(i in 1:5){
  a_e_edad[i] <- a_edad(x = i)
}     

b_edad <- function(x) {
  x <- sem2_panel_svy %>%
    filter(tramo_edad == x & pea == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(desocup, na.rm=T))
  x <- mean(x$colname)
}       

b_e_edad <- numeric()

for(i in 1:5){
  b_e_edad[i] <- b_edad(x = i)
} 

c_edad <- as.data.frame(cbind(a_e_edad, b_e_edad))
c_edad <- c_edad %>% dplyr::mutate(VALOR = (a_e_edad + b_e_edad)/2)
c_edad <- c_edad %>% dplyr::mutate(aux = c(11, 12, 13, 14, 15))
c_edad <- c_edad[,c("VALOR", "aux")]



# Región

a_region <- function(x) {
  x <- sem1_svy %>%
    filter(region_4 == x & pea == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(desocup, na.rm=T))
  x <- mean(x$colname)
}       

a_e_region <- numeric()

for(i in 3:4){
  a_e_region[i] <- a_region(x = i)
}     

b_region <- function(x) {
  x <- sem2_panel_svy %>%
    filter(region_4 == x & pea == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(desocup, na.rm=T))
  x <- mean(x$colname)
}       

b_e_region <- numeric()

for(i in 3:4){
  b_e_region[i] <- b_region(x = i)
}     


c_region <- as.data.frame(cbind(a_e_region, b_e_region))
c_region <- c_region %>% dplyr::mutate(VALOR = (a_e_region + b_e_region)/2)
c_region <- c_region[c(3,4),]
c_region <- c_region %>% dplyr::mutate(aux = c(16, 17))
c_region <- c_region[,c("VALOR", "aux")]


# Condición de pobreza del hogar

a_pobre <- function(x) {
  x <- sem1_svy %>%
    filter(pobre_06 == x & pea == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(desocup, na.rm=T))
  x <- mean(x$colname)
}       

a_e_pobre <- numeric()

for(i in 1:2){
  a_e_pobre[i] <- a_pobre(x = i)
}     

b_pobre <- function(x) {
  x <- sem2_panel_svy %>%
    filter(pobre_06 == x & pea == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(desocup, na.rm=T))
  x <- mean(x$colname)
}       

b_e_pobre <- numeric()

for(i in 1:2){
  b_e_pobre[i] <- b_pobre(x = i)
}     


c_pobre <- as.data.frame(cbind(a_e_pobre, b_e_pobre))
c_pobre <- c_pobre %>% dplyr::mutate(VALOR = (a_e_pobre + b_e_pobre)/2)
c_pobre <- c_pobre %>% dplyr::mutate(aux = c(18, 19))
c_pobre <- c_pobre[,c("VALOR", "aux")]


## País Urbano

# Total

a_mes <- sem1_svy %>%
  srvyr::filter(pea == 1 & (region_4 == 1 | region_4 == 2)) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(desocup, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

b_mes <- sem2_panel_svy %>%
  srvyr::filter(pea == 1 & (region_4 == 1 | region_4 == 2)) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(desocup, na.rm=T))

b_sem <- mean(as.numeric(b_mes$colname))

c_ano_pu <- mean(c(a_sem, b_sem))
c_ano_pu <- as.data.frame(c_ano_pu)
c_ano_pu <- c_ano_pu %>% dplyr::mutate(aux = 1)
c_ano_pu <- c_ano_pu %>% dplyr::rename(VALOR = c_ano_pu)

# Quintil de ingresos

a_quintil <- function(x) {
  x <- sem1_svy %>%
    filter(quintilesy == x & pea == 1 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(desocup, na.rm=T))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

b_quintil <- function(x) {
  x <- sem2_panel_svy %>%
    filter(quintilesy == x & pea == 1 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(desocup, na.rm=T))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}     

c_quintil_pu <- as.data.frame(cbind(a_e_quintil, b_e_quintil))
c_quintil_pu <- c_quintil_pu %>% dplyr::mutate(VALOR = (a_e_quintil + b_e_quintil)/2)
c_quintil_pu <- c_quintil_pu %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil_pu <- c_quintil_pu[,c("VALOR", "aux")]


# Sexo

a_sexo <- function(x) {
  x <- sem1_svy %>%
    filter(bc_pe2 == x & pea == 1 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(desocup, na.rm=T))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}     

b_sexo <- function(x) {
  x <- sem2_panel_svy %>%
    filter(bc_pe2 == x & pea == 1 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(desocup, na.rm=T))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}     


c_sexo_pu <- as.data.frame(cbind(a_e_sexo, b_e_sexo))
c_sexo_pu <- c_sexo_pu %>% dplyr::mutate(VALOR = (a_e_sexo + b_e_sexo)/2)
c_sexo_pu <- c_sexo_pu %>% dplyr::mutate(aux = c(9, 10))
c_sexo_pu <- c_sexo_pu[,c("VALOR", "aux")]


# Tramo de edad

a_edad <- function(x) {
  x <- sem1_svy %>%
    filter(tramo_edad == x & pea == 1 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(desocup, na.rm=T))
  x <- mean(x$colname)
}       

a_e_edad <- numeric()

for(i in 1:5){
  a_e_edad[i] <- a_edad(x = i)
}     

b_edad <- function(x) {
  x <- sem2_panel_svy %>%
    filter(tramo_edad == x & pea == 1 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(desocup, na.rm=T))
  x <- mean(x$colname)
}       

b_e_edad <- numeric()

for(i in 1:5){
  b_e_edad[i] <- b_edad(x = i)
} 

c_edad_pu <- as.data.frame(cbind(a_e_edad, b_e_edad))
c_edad_pu <- c_edad_pu %>% dplyr::mutate(VALOR = (a_e_edad + b_e_edad)/2)
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

a_mes <- sem1_svy %>%
  srvyr::filter(ocup == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(bc_subocupado, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

b_mes <- sem2_panel_svy %>%
  srvyr::filter(ocup == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(bc_subocupado, na.rm=T))

b_sem <- mean(as.numeric(b_mes$colname))

c_ano <- mean(c(a_sem, b_sem))
c_ano <- as.data.frame(c_ano)
c_ano <- c_ano %>% dplyr::mutate(aux = 1)
c_ano <- c_ano %>% dplyr::rename(VALOR = c_ano)

# Ascendencia étnico racial

a_afro <- function(x) {
  x <- sem1_svy %>%
    filter(bd_e29_1 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_subocupado, na.rm=T))
  x <- mean(x$colname)
}       

a_e_afro <- numeric()

for(i in 1:2){
  a_e_afro[i] <- a_afro(x = i)
}     

b_afro <- function(x) {
  x <- sem2_panel_svy %>%
    filter(bd_e29_1 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_subocupado, na.rm=T))
  x <- mean(x$colname)
}       

b_e_afro <- numeric()

for(i in 1:2){
  b_e_afro[i] <- b_afro(x = i)
}     

c_afro <- as.data.frame(cbind(a_e_afro, b_e_afro))
c_afro <- c_afro %>% dplyr::mutate(VALOR = (a_e_afro + b_e_afro)/2)
c_afro <- c_afro %>% dplyr::mutate(aux = c(2,3))
c_afro <- c_afro[,c("VALOR", "aux")]



# Quintil de ingresos

a_quintil <- function(x) {
  x <- sem1_svy %>%
    filter(quintilesy == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_subocupado, na.rm=T))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

b_quintil <- function(x) {
  x <- sem2_panel_svy %>%
    filter(quintilesy == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_subocupado, na.rm=T))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}     

c_quintil <- as.data.frame(cbind(a_e_quintil, b_e_quintil))
c_quintil <- c_quintil %>% dplyr::mutate(VALOR = (a_e_quintil + b_e_quintil)/2)
c_quintil <- c_quintil %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil <- c_quintil[,c("VALOR", "aux")]

# Sexo

a_sexo <- function(x) {
  x <- sem1_svy %>%
    filter(bc_pe2 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_subocupado, na.rm=T))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}     

b_sexo <- function(x) {
  x <- sem2_panel_svy %>%
    filter(bc_pe2 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_subocupado, na.rm=T))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}     

c_sexo <- as.data.frame(cbind(a_e_sexo, b_e_sexo))
c_sexo <- c_sexo %>% dplyr::mutate(VALOR = (a_e_sexo + b_e_sexo)/2)
c_sexo <- c_sexo %>% dplyr::mutate(aux = c(9, 10))
c_sexo <- c_sexo[,c("VALOR", "aux")]

# Tramo de edad

a_edad <- function(x) {
  x <- sem1_svy %>%
    filter(tramo_edad == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_subocupado, na.rm=T))
  x <- mean(x$colname)
}       

a_e_edad <- numeric()

for(i in 1:5){
  a_e_edad[i] <- a_edad(x = i)
}     

b_edad <- function(x) {
  x <- sem2_panel_svy %>%
    filter(tramo_edad == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_subocupado, na.rm=T))
  x <- mean(x$colname)
}       

b_e_edad <- numeric()

for(i in 1:5){
  b_e_edad[i] <- b_edad(x = i)
} 

c_edad <- as.data.frame(cbind(a_e_edad, b_e_edad))
c_edad <- c_edad %>% dplyr::mutate(VALOR = (a_e_edad + b_e_edad)/2)
c_edad <- c_edad %>% dplyr::mutate(aux = c(11, 12, 13, 14, 15))
c_edad <- c_edad[,c("VALOR", "aux")]



# Región

a_region <- function(x) {
  x <- sem1_svy %>%
    filter(region_4 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_subocupado, na.rm=T))
  x <- mean(x$colname)
}       

a_e_region <- numeric()

for(i in 3:4){
  a_e_region[i] <- a_region(x = i)
}     

b_region <- function(x) {
  x <- sem2_panel_svy %>%
    filter(region_4 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_subocupado, na.rm=T))
  x <- mean(x$colname)
}       

b_e_region <- numeric()

for(i in 3:4){
  b_e_region[i] <- b_region(x = i)
}     


c_region <- as.data.frame(cbind(a_e_region, b_e_region))
c_region <- c_region %>% dplyr::mutate(VALOR = (a_e_region + b_e_region)/2)
c_region <- c_region[c(3,4),]
c_region <- c_region %>% dplyr::mutate(aux = c(16, 17))
c_region <- c_region[,c("VALOR", "aux")]


# Condición de pobreza del hogar

a_pobre <- function(x) {
  x <- sem1_svy %>%
    filter(pobre_06 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_subocupado, na.rm=T))
  x <- mean(x$colname)
}       

a_e_pobre <- numeric()

for(i in 1:2){
  a_e_pobre[i] <- a_pobre(x = i)
}     

b_pobre <- function(x) {
  x <- sem2_panel_svy %>%
    filter(pobre_06 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_subocupado, na.rm=T))
  x <- mean(x$colname)
}       

b_e_pobre <- numeric()

for(i in 1:2){
  b_e_pobre[i] <- b_pobre(x = i)
}     


c_pobre <- as.data.frame(cbind(a_e_pobre, b_e_pobre))
c_pobre <- c_pobre %>% dplyr::mutate(VALOR = (a_e_pobre + b_e_pobre)/2)
c_pobre <- c_pobre %>% dplyr::mutate(aux = c(18, 19))
c_pobre <- c_pobre[,c("VALOR", "aux")]

## País Urbano

# Total

a_mes <- sem1_svy %>%
  srvyr::filter(ocup == 1 & (region_4 == 1 | region_4 == 2)) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(bc_subocupado, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

b_mes <- sem2_panel_svy %>%
  srvyr::filter(ocup == 1 & (region_4 == 1 | region_4 == 2)) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(bc_subocupado, na.rm=T))

b_sem <- mean(as.numeric(b_mes$colname))

c_ano_pu <- mean(c(a_sem, b_sem))
c_ano_pu <- as.data.frame(c_ano_pu)
c_ano_pu <- c_ano_pu %>% dplyr::mutate(aux = 1)
c_ano_pu <- c_ano_pu %>% dplyr::rename(VALOR = c_ano_pu)

# Quintil de ingresos

a_quintil <- function(x) {
  x <- sem1_svy %>%
    filter(quintilesy == x & ocup == 1 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_subocupado, na.rm=T))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

b_quintil <- function(x) {
  x <- sem2_panel_svy %>%
    filter(quintilesy == x & ocup == 1 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_subocupado, na.rm=T))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}     

c_quintil_pu <- as.data.frame(cbind(a_e_quintil, b_e_quintil))
c_quintil_pu <- c_quintil_pu %>% dplyr::mutate(VALOR = (a_e_quintil + b_e_quintil)/2)
c_quintil_pu <- c_quintil_pu %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil_pu <- c_quintil_pu[,c("VALOR", "aux")]


# Sexo

a_sexo <- function(x) {
  x <- sem1_svy %>%
    filter(bc_pe2 == x & ocup == 1 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_subocupado, na.rm=T))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}     

b_sexo <- function(x) {
  x <- sem2_panel_svy %>%
    filter(bc_pe2 == x & ocup == 1 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_subocupado, na.rm=T))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}     


c_sexo_pu <- as.data.frame(cbind(a_e_sexo, b_e_sexo))
c_sexo_pu <- c_sexo_pu %>% dplyr::mutate(VALOR = (a_e_sexo + b_e_sexo)/2)
c_sexo_pu <- c_sexo_pu %>% dplyr::mutate(aux = c(9, 10))
c_sexo_pu <- c_sexo_pu[,c("VALOR", "aux")]


# Tramo de edad

a_edad <- function(x) {
  x <- sem1_svy %>%
    filter(tramo_edad == x & ocup == 1 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_subocupado, na.rm=T))
  x <- mean(x$colname)
}       

a_e_edad <- numeric()

for(i in 1:5){
  a_e_edad[i] <- a_edad(x = i)
}     

b_edad <- function(x) {
  x <- sem2_panel_svy %>%
    filter(tramo_edad == x & ocup == 1 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_subocupado, na.rm=T))
  x <- mean(x$colname)
}       

b_e_edad <- numeric()

for(i in 1:5){
  b_e_edad[i] <- b_edad(x = i)
} 

c_edad_pu <- as.data.frame(cbind(a_e_edad, b_e_edad))
c_edad_pu <- c_edad_pu %>% dplyr::mutate(VALOR = (a_e_edad + b_e_edad)/2)
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

a_mes <- sem1_svy %>%
  srvyr::filter(ocup == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(bc_register, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

b_mes <- sem2_panel_svy %>%
  srvyr::filter(ocup == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(bc_register, na.rm=T))

b_sem <- mean(as.numeric(b_mes$colname))

c_ano <- mean(c(a_sem, b_sem))
c_ano <- as.data.frame(c_ano)
c_ano <- c_ano %>% dplyr::mutate(aux = 1)
c_ano <- c_ano %>% dplyr::rename(VALOR = c_ano)

# Ascendencia étnico racial

a_afro <- function(x) {
  x <- sem1_svy %>%
    filter(bd_e29_1 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register, na.rm=T))
  x <- mean(x$colname)
}       

a_e_afro <- numeric()

for(i in 1:2){
  a_e_afro[i] <- a_afro(x = i)
}     

b_afro <- function(x) {
  x <- sem2_panel_svy %>%
    filter(bd_e29_1 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register, na.rm=T))
  x <- mean(x$colname)
}       

b_e_afro <- numeric()

for(i in 1:2){
  b_e_afro[i] <- b_afro(x = i)
}     

c_afro <- as.data.frame(cbind(a_e_afro, b_e_afro))
c_afro <- c_afro %>% dplyr::mutate(VALOR = (a_e_afro + b_e_afro)/2)
c_afro <- c_afro %>% dplyr::mutate(aux = c(2,3))
c_afro <- c_afro[,c("VALOR", "aux")]



# Quintil de ingresos

a_quintil <- function(x) {
  x <- sem1_svy %>%
    filter(quintilesy == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register, na.rm=T))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

b_quintil <- function(x) {
  x <- sem2_panel_svy %>%
    filter(quintilesy == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register, na.rm=T))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}     

c_quintil <- as.data.frame(cbind(a_e_quintil, b_e_quintil))
c_quintil <- c_quintil %>% dplyr::mutate(VALOR = (a_e_quintil + b_e_quintil)/2)
c_quintil <- c_quintil %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil <- c_quintil[,c("VALOR", "aux")]

# Sexo

a_sexo <- function(x) {
  x <- sem1_svy %>%
    filter(bc_pe2 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register, na.rm=T))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}     

b_sexo <- function(x) {
  x <- sem2_panel_svy %>%
    filter(bc_pe2 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register, na.rm=T))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}     

c_sexo <- as.data.frame(cbind(a_e_sexo, b_e_sexo))
c_sexo <- c_sexo %>% dplyr::mutate(VALOR = (a_e_sexo + b_e_sexo)/2)
c_sexo <- c_sexo %>% dplyr::mutate(aux = c(9, 10))
c_sexo <- c_sexo[,c("VALOR", "aux")]

# Tramo de edad

a_edad <- function(x) {
  x <- sem1_svy %>%
    filter(tramo_edad == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register, na.rm=T))
  x <- mean(x$colname)
}       

a_e_edad <- numeric()

for(i in 1:5){
  a_e_edad[i] <- a_edad(x = i)
}     

b_edad <- function(x) {
  x <- sem2_panel_svy %>%
    filter(tramo_edad == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register, na.rm=T))
  x <- mean(x$colname)
}       

b_e_edad <- numeric()

for(i in 1:5){
  b_e_edad[i] <- b_edad(x = i)
} 

c_edad <- as.data.frame(cbind(a_e_edad, b_e_edad))
c_edad <- c_edad %>% dplyr::mutate(VALOR = (a_e_edad + b_e_edad)/2)
c_edad <- c_edad %>% dplyr::mutate(aux = c(11, 12, 13, 14, 15))
c_edad <- c_edad[,c("VALOR", "aux")]



# Región

a_region <- function(x) {
  x <- sem1_svy %>%
    filter(region_4 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register, na.rm=T))
  x <- mean(x$colname)
}       

a_e_region <- numeric()

for(i in 3:4){
  a_e_region[i] <- a_region(x = i)
}     

b_region <- function(x) {
  x <- sem2_panel_svy %>%
    filter(region_4 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register, na.rm=T))
  x <- mean(x$colname)
}       

b_e_region <- numeric()

for(i in 3:4){
  b_e_region[i] <- b_region(x = i)
}     


c_region <- as.data.frame(cbind(a_e_region, b_e_region))
c_region <- c_region %>% dplyr::mutate(VALOR = (a_e_region + b_e_region)/2)
c_region <- c_region[c(3,4),]
c_region <- c_region %>% dplyr::mutate(aux = c(16, 17))
c_region <- c_region[,c("VALOR", "aux")]


# Condición de pobreza del hogar

a_pobre <- function(x) {
  x <- sem1_svy %>%
    filter(pobre_06 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register, na.rm=T))
  x <- mean(x$colname)
}       

a_e_pobre <- numeric()

for(i in 1:2){
  a_e_pobre[i] <- a_pobre(x = i)
}     

b_pobre <- function(x) {
  x <- sem2_panel_svy %>%
    filter(pobre_06 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register, na.rm=T))
  x <- mean(x$colname)
}       

b_e_pobre <- numeric()

for(i in 1:2){
  b_e_pobre[i] <- b_pobre(x = i)
}     


c_pobre <- as.data.frame(cbind(a_e_pobre, b_e_pobre))
c_pobre <- c_pobre %>% dplyr::mutate(VALOR = (a_e_pobre + b_e_pobre)/2)
c_pobre <- c_pobre %>% dplyr::mutate(aux = c(18, 19))
c_pobre <- c_pobre[,c("VALOR", "aux")]

## País Urbano

# Total

a_mes <- sem1_svy %>%
  srvyr::filter(ocup == 1 & (region_4 == 1 | region_4 == 2)) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(bc_register, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

b_mes <- sem2_panel_svy %>%
  srvyr::filter(ocup == 1 & (region_4 == 1 | region_4 == 2)) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(bc_register, na.rm=T))

b_sem <- mean(as.numeric(b_mes$colname))

c_ano_pu <- mean(c(a_sem, b_sem))
c_ano_pu <- as.data.frame(c_ano_pu)
c_ano_pu <- c_ano_pu %>% dplyr::mutate(aux = 1)
c_ano_pu <- c_ano_pu %>% dplyr::rename(VALOR = c_ano_pu)

# Quintil de ingresos

a_quintil <- function(x) {
  x <- sem1_svy %>%
    filter(quintilesy == x & ocup == 1 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register, na.rm=T))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

b_quintil <- function(x) {
  x <- sem2_panel_svy %>%
    filter(quintilesy == x & ocup == 1 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register, na.rm=T))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}     

c_quintil_pu <- as.data.frame(cbind(a_e_quintil, b_e_quintil))
c_quintil_pu <- c_quintil_pu %>% dplyr::mutate(VALOR = (a_e_quintil + b_e_quintil)/2)
c_quintil_pu <- c_quintil_pu %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil_pu <- c_quintil_pu[,c("VALOR", "aux")]


# Sexo

a_sexo <- function(x) {
  x <- sem1_svy %>%
    filter(bc_pe2 == x & ocup == 1 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register, na.rm=T))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}     

b_sexo <- function(x) {
  x <- sem2_panel_svy %>%
    filter(bc_pe2 == x & ocup == 1 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register, na.rm=T))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}     


c_sexo_pu <- as.data.frame(cbind(a_e_sexo, b_e_sexo))
c_sexo_pu <- c_sexo_pu %>% dplyr::mutate(VALOR = (a_e_sexo + b_e_sexo)/2)
c_sexo_pu <- c_sexo_pu %>% dplyr::mutate(aux = c(9, 10))
c_sexo_pu <- c_sexo_pu[,c("VALOR", "aux")]


# Tramo de edad

a_edad <- function(x) {
  x <- sem1_svy %>%
    filter(tramo_edad == x & ocup == 1 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register, na.rm=T))
  x <- mean(x$colname)
}       

a_e_edad <- numeric()

for(i in 1:5){
  a_e_edad[i] <- a_edad(x = i)
}     

b_edad <- function(x) {
  x <- sem2_panel_svy %>%
    filter(tramo_edad == x & ocup == 1 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register, na.rm=T))
  x <- mean(x$colname)
}       

b_e_edad <- numeric()

for(i in 1:5){
  b_e_edad[i] <- b_edad(x = i)
} 

c_edad_pu <- as.data.frame(cbind(a_e_edad, b_e_edad))
c_edad_pu <- c_edad_pu %>% dplyr::mutate(VALOR = (a_e_edad + b_e_edad)/2)
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

a_mes <- sem1_svy %>%
  srvyr::filter(ocup == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(bc_horas, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

b_mes <- sem2_panel_svy %>%
  srvyr::filter(ocup == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(bc_horas, na.rm=T))

b_sem <- mean(as.numeric(b_mes$colname))

c_ano <- mean(c(a_sem, b_sem))
c_ano <- as.data.frame(c_ano)
c_ano <- c_ano %>% dplyr::mutate(aux = 1)
c_ano <- c_ano %>% dplyr::rename(VALOR = c_ano)

# Ascendencia étnico racial

a_afro <- function(x) {
  x <- sem1_svy %>%
    filter(bd_e29_1 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_horas, na.rm=T))
  x <- mean(x$colname)
}       

a_e_afro <- numeric()

for(i in 1:2){
  a_e_afro[i] <- a_afro(x = i)
}     

b_afro <- function(x) {
  x <- sem2_panel_svy %>%
    filter(bd_e29_1 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_horas, na.rm=T))
  x <- mean(x$colname)
}       

b_e_afro <- numeric()

for(i in 1:2){
  b_e_afro[i] <- b_afro(x = i)
}     

c_afro <- as.data.frame(cbind(a_e_afro, b_e_afro))
c_afro <- c_afro %>% dplyr::mutate(VALOR = (a_e_afro + b_e_afro)/2)
c_afro <- c_afro %>% dplyr::mutate(aux = c(2,3))
c_afro <- c_afro[,c("VALOR", "aux")]



# Quintil de ingresos

a_quintil <- function(x) {
  x <- sem1_svy %>%
    filter(quintilesy == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_horas, na.rm=T))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

b_quintil <- function(x) {
  x <- sem2_panel_svy %>%
    filter(quintilesy == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_horas, na.rm=T))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}     

c_quintil <- as.data.frame(cbind(a_e_quintil, b_e_quintil))
c_quintil <- c_quintil %>% dplyr::mutate(VALOR = (a_e_quintil + b_e_quintil)/2)
c_quintil <- c_quintil %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil <- c_quintil[,c("VALOR", "aux")]

# Sexo

a_sexo <- function(x) {
  x <- sem1_svy %>%
    filter(bc_pe2 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_horas, na.rm=T))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}     

b_sexo <- function(x) {
  x <- sem2_panel_svy %>%
    filter(bc_pe2 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_horas, na.rm=T))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}     

c_sexo <- as.data.frame(cbind(a_e_sexo, b_e_sexo))
c_sexo <- c_sexo %>% dplyr::mutate(VALOR = (a_e_sexo + b_e_sexo)/2)
c_sexo <- c_sexo %>% dplyr::mutate(aux = c(9, 10))
c_sexo <- c_sexo[,c("VALOR", "aux")]

# Tramo de edad

a_edad <- function(x) {
  x <- sem1_svy %>%
    filter(tramo_edad == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_horas, na.rm=T))
  x <- mean(x$colname)
}       

a_e_edad <- numeric()

for(i in 1:5){
  a_e_edad[i] <- a_edad(x = i)
}     

b_edad <- function(x) {
  x <- sem2_panel_svy %>%
    filter(tramo_edad == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_horas, na.rm=T))
  x <- mean(x$colname)
}       

b_e_edad <- numeric()

for(i in 1:5){
  b_e_edad[i] <- b_edad(x = i)
} 

c_edad <- as.data.frame(cbind(a_e_edad, b_e_edad))
c_edad <- c_edad %>% dplyr::mutate(VALOR = (a_e_edad + b_e_edad)/2)
c_edad <- c_edad %>% dplyr::mutate(aux = c(11, 12, 13, 14, 15))
c_edad <- c_edad[,c("VALOR", "aux")]



# Región

a_region <- function(x) {
  x <- sem1_svy %>%
    filter(region_4 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_horas, na.rm=T))
  x <- mean(x$colname)
}       

a_e_region <- numeric()

for(i in 3:4){
  a_e_region[i] <- a_region(x = i)
}     

b_region <- function(x) {
  x <- sem2_panel_svy %>%
    filter(region_4 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_horas, na.rm=T))
  x <- mean(x$colname)
}       

b_e_region <- numeric()

for(i in 3:4){
  b_e_region[i] <- b_region(x = i)
}     


c_region <- as.data.frame(cbind(a_e_region, b_e_region))
c_region <- c_region %>% dplyr::mutate(VALOR = (a_e_region + b_e_region)/2)
c_region <- c_region[c(3,4),]
c_region <- c_region %>% dplyr::mutate(aux = c(16, 17))
c_region <- c_region[,c("VALOR", "aux")]


# Condición de pobreza del hogar

a_pobre <- function(x) {
  x <- sem1_svy %>%
    filter(pobre_06 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_horas, na.rm=T))
  x <- mean(x$colname)
}       

a_e_pobre <- numeric()

for(i in 1:2){
  a_e_pobre[i] <- a_pobre(x = i)
}     

b_pobre <- function(x) {
  x <- sem2_panel_svy %>%
    filter(pobre_06 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_horas, na.rm=T))
  x <- mean(x$colname)
}       

b_e_pobre <- numeric()

for(i in 1:2){
  b_e_pobre[i] <- b_pobre(x = i)
}     


c_pobre <- as.data.frame(cbind(a_e_pobre, b_e_pobre))
c_pobre <- c_pobre %>% dplyr::mutate(VALOR = (a_e_pobre + b_e_pobre)/2)
c_pobre <- c_pobre %>% dplyr::mutate(aux = c(18, 19))
c_pobre <- c_pobre[,c("VALOR", "aux")]

## País Urbano

# Total

a_mes <- sem1_svy %>%
  srvyr::filter(ocup == 1 & (region_4 == 1 | region_4 == 2)) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(bc_horas, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

b_mes <- sem2_panel_svy %>%
  srvyr::filter(ocup == 1 & (region_4 == 1 | region_4 == 2)) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(bc_horas, na.rm=T))

b_sem <- mean(as.numeric(b_mes$colname))

c_ano_pu <- mean(c(a_sem, b_sem))
c_ano_pu <- as.data.frame(c_ano_pu)
c_ano_pu <- c_ano_pu %>% dplyr::mutate(aux = 1)
c_ano_pu <- c_ano_pu %>% dplyr::rename(VALOR = c_ano_pu)

# Quintil de ingresos

a_quintil <- function(x) {
  x <- sem1_svy %>%
    filter(quintilesy == x & ocup == 1 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_horas, na.rm=T))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

b_quintil <- function(x) {
  x <- sem2_panel_svy %>%
    filter(quintilesy == x & ocup == 1 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_horas, na.rm=T))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}     

c_quintil_pu <- as.data.frame(cbind(a_e_quintil, b_e_quintil))
c_quintil_pu <- c_quintil_pu %>% dplyr::mutate(VALOR = (a_e_quintil + b_e_quintil)/2)
c_quintil_pu <- c_quintil_pu %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil_pu <- c_quintil_pu[,c("VALOR", "aux")]


# Sexo

a_sexo <- function(x) {
  x <- sem1_svy %>%
    filter(bc_pe2 == x & ocup == 1 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_horas, na.rm=T))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}     

b_sexo <- function(x) {
  x <- sem2_panel_svy %>%
    filter(bc_pe2 == x & ocup == 1 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_horas, na.rm=T))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}     


c_sexo_pu <- as.data.frame(cbind(a_e_sexo, b_e_sexo))
c_sexo_pu <- c_sexo_pu %>% dplyr::mutate(VALOR = (a_e_sexo + b_e_sexo)/2)
c_sexo_pu <- c_sexo_pu %>% dplyr::mutate(aux = c(9, 10))
c_sexo_pu <- c_sexo_pu[,c("VALOR", "aux")]


# Tramo de edad

a_edad <- function(x) {
  x <- sem1_svy %>%
    filter(tramo_edad == x & ocup == 1 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_horas, na.rm=T))
  x <- mean(x$colname)
}       

a_e_edad <- numeric()

for(i in 1:5){
  a_e_edad[i] <- a_edad(x = i)
}     

b_edad <- function(x) {
  x <- sem2_panel_svy %>%
    filter(tramo_edad == x & ocup == 1 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_horas, na.rm=T))
  x <- mean(x$colname)
}       

b_e_edad <- numeric()

for(i in 1:5){
  b_e_edad[i] <- b_edad(x = i)
} 

c_edad_pu <- as.data.frame(cbind(a_e_edad, b_e_edad))
c_edad_pu <- c_edad_pu %>% dplyr::mutate(VALOR = (a_e_edad + b_e_edad)/2)
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

a_mes <- sem1_svy %>%
  srvyr::filter(ocup == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

b_mes <- sem2_implant_svy %>%
  srvyr::filter(ocup == 1 & f85>0) %>%
  srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))

b_sem <- mean(as.numeric(b_mes$colname))

c_ano <- mean(c(a_sem, b_sem))
c_ano <- as.data.frame(c_ano)
c_ano <- c_ano %>% dplyr::mutate(aux = 1)
c_ano <- c_ano %>% dplyr::rename(VALOR = c_ano)

# Ascendencia étnico racial

a_afro <- function(x) {
  x <- sem1_svy %>%
    filter(bd_e29_1 == x & ocup == 1 & F85>0) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

a_e_afro <- numeric()

for(i in 1:2){
  a_e_afro[i] <- a_afro(x = i)
}     

b_afro <- function(x) {
  x <- sem2_implant_svy %>%
    filter(bd_e29_1 == x & ocup == 1 & f85>0) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

b_e_afro <- numeric()

for(i in 1:2){
  b_e_afro[i] <- b_afro(x = i)
}     

c_afro <- as.data.frame(cbind(a_e_afro, b_e_afro))
c_afro <- c_afro %>% dplyr::mutate(VALOR = (a_e_afro + b_e_afro)/2)
c_afro <- c_afro %>% dplyr::mutate(aux = c(2,3))
c_afro <- c_afro[,c("VALOR", "aux")]



# Quintil de ingresos

a_quintil <- function(x) {
  x <- sem1_svy %>%
    filter(quintilesy == x & ocup == 1 & F85>0) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

b_quintil <- function(x) {
  x <- sem2_implant_svy %>%
    filter(quintilesy == x & ocup == 1 & f85>0) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}     

c_quintil <- as.data.frame(cbind(a_e_quintil, b_e_quintil))
c_quintil <- c_quintil %>% dplyr::mutate(VALOR = (a_e_quintil + b_e_quintil)/2)
c_quintil <- c_quintil %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil <- c_quintil[,c("VALOR", "aux")]

# Sexo

a_sexo <- function(x) {
  x <- sem1_svy %>%
    filter(bc_pe2 == x & ocup == 1 & F85>0) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}     

b_sexo <- function(x) {
  x <- sem2_implant_svy %>%
    filter(bc_pe2 == x & ocup == 1 & f85>0) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}     

c_sexo <- as.data.frame(cbind(a_e_sexo, b_e_sexo))
c_sexo <- c_sexo %>% dplyr::mutate(VALOR = (a_e_sexo + b_e_sexo)/2)
c_sexo <- c_sexo %>% dplyr::mutate(aux = c(9, 10))
c_sexo <- c_sexo[,c("VALOR", "aux")]

# Tramo de edad

a_edad <- function(x) {
  x <- sem1_svy %>%
    filter(tramo_edad == x & ocup == 1 & F85>0) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

a_e_edad <- numeric()

for(i in 1:5){
  a_e_edad[i] <- a_edad(x = i)
}     

b_edad <- function(x) {
  x <- sem2_implant_svy %>%
    filter(tramo_edad == x & ocup == 1 & f85>0) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

b_e_edad <- numeric()

for(i in 1:5){
  b_e_edad[i] <- b_edad(x = i)
} 

c_edad <- as.data.frame(cbind(a_e_edad, b_e_edad))
c_edad <- c_edad %>% dplyr::mutate(VALOR = (a_e_edad + b_e_edad)/2)
c_edad <- c_edad %>% dplyr::mutate(aux = c(11, 12, 13, 14, 15))
c_edad <- c_edad[,c("VALOR", "aux")]



# Región

a_region <- function(x) {
  x <- sem1_svy %>%
    filter(region_4 == x & ocup == 1 & F85>0) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

a_e_region <- numeric()

for(i in 3:4){
  a_e_region[i] <- a_region(x = i)
}     

b_region <- function(x) {
  x <- sem2_implant_svy %>%
    filter(region_4 == x & ocup == 1 & f85>0) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

b_e_region <- numeric()

for(i in 3:4){
  b_e_region[i] <- b_region(x = i)
}     


c_region <- as.data.frame(cbind(a_e_region, b_e_region))
c_region <- c_region %>% dplyr::mutate(VALOR = (a_e_region + b_e_region)/2)
c_region <- c_region[c(3,4),]
c_region <- c_region %>% dplyr::mutate(aux = c(16, 17))
c_region <- c_region[,c("VALOR", "aux")]


# Condición de pobreza del hogar

a_pobre <- function(x) {
  x <- sem1_svy %>%
    filter(pobre_06 == x & ocup == 1 & F85>0) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

a_e_pobre <- numeric()

for(i in 1:2){
  a_e_pobre[i] <- a_pobre(x = i)
}     

b_pobre <- function(x) {
  x <- sem2_implant_svy %>%
    filter(pobre_06 == x & ocup == 1 & f85>0) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

b_e_pobre <- numeric()

for(i in 1:2){
  b_e_pobre[i] <- b_pobre(x = i)
}     


c_pobre <- as.data.frame(cbind(a_e_pobre, b_e_pobre))
c_pobre <- c_pobre %>% dplyr::mutate(VALOR = (a_e_pobre + b_e_pobre)/2)
c_pobre <- c_pobre %>% dplyr::mutate(aux = c(18, 19))
c_pobre <- c_pobre[,c("VALOR", "aux")]

## País Urbano

# Total

a_mes <- sem1_svy %>%
  srvyr::filter(ocup == 1 & (region_4 == 1 | region_4 == 2) & F85>0) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

b_mes <- sem2_implant_svy %>%
  srvyr::filter(ocup == 1 & (region_4 == 1 | region_4 == 2) & f85>0) %>%
  srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))

b_sem <- mean(as.numeric(b_mes$colname))

c_ano_pu <- mean(c(a_sem, b_sem))
c_ano_pu <- as.data.frame(c_ano_pu)
c_ano_pu <- c_ano_pu %>% dplyr::mutate(aux = 1)
c_ano_pu <- c_ano_pu %>% dplyr::rename(VALOR = c_ano_pu)

# Quintil de ingresos

a_quintil <- function(x) {
  x <- sem1_svy %>%
    filter(quintilesy == x & ocup == 1 & (region_4 == 1 | region_4 == 2) & F85>0) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

b_quintil <- function(x) {
  x <- sem2_implant_svy %>%
    filter(quintilesy == x & ocup == 1 & (region_4 == 1 | region_4 == 2) & f85>0) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}     

c_quintil_pu <- as.data.frame(cbind(a_e_quintil, b_e_quintil))
c_quintil_pu <- c_quintil_pu %>% dplyr::mutate(VALOR = (a_e_quintil + b_e_quintil)/2)
c_quintil_pu <- c_quintil_pu %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil_pu <- c_quintil_pu[,c("VALOR", "aux")]


# Sexo

a_sexo <- function(x) {
  x <- sem1_svy %>%
    filter(bc_pe2 == x & ocup == 1 & (region_4 == 1 | region_4 == 2) & F85>0) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}     

b_sexo <- function(x) {
  x <- sem2_implant_svy %>%
    filter(bc_pe2 == x & ocup == 1 & (region_4 == 1 | region_4 == 2) & f85>0) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}     


c_sexo_pu <- as.data.frame(cbind(a_e_sexo, b_e_sexo))
c_sexo_pu <- c_sexo_pu %>% dplyr::mutate(VALOR = (a_e_sexo + b_e_sexo)/2)
c_sexo_pu <- c_sexo_pu %>% dplyr::mutate(aux = c(9, 10))
c_sexo_pu <- c_sexo_pu[,c("VALOR", "aux")]


# Tramo de edad

a_edad <- function(x) {
  x <- sem1_svy %>%
    filter(tramo_edad == x & ocup == 1 & (region_4 == 1 | region_4 == 2) & F85>0) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

a_e_edad <- numeric()

for(i in 1:5){
  a_e_edad[i] <- a_edad(x = i)
}     

b_edad <- function(x) {
  x <- sem2_implant_svy %>%
    filter(tramo_edad == x & ocup == 1 & (region_4 == 1 | region_4 == 2) & f85>0) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

b_e_edad <- numeric()

for(i in 1:5){
  b_e_edad[i] <- b_edad(x = i)
} 

c_edad_pu <- as.data.frame(cbind(a_e_edad, b_e_edad))
c_edad_pu <- c_edad_pu %>% dplyr::mutate(VALOR = (a_e_edad + b_e_edad)/2)
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

a_mes <- sem1_svy %>%
  srvyr::filter(ocup == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(salario_insuf_smn, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

b_mes <- sem2_implant_svy %>%
  srvyr::filter(ocup == 1) %>%
  srvyr::summarise(colname = srvyr::survey_mean(salario_insuf_smn, na.rm=T))

b_sem <- mean(as.numeric(b_mes$colname))

c_ano <- mean(c(a_sem, b_sem))
c_ano <- as.data.frame(c_ano)
c_ano <- c_ano %>% dplyr::mutate(aux = 1)
c_ano <- c_ano %>% dplyr::rename(VALOR = c_ano)

# Ascendencia étnico racial

a_afro <- function(x) {
  x <- sem1_svy %>%
    filter(bd_e29_1 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

a_e_afro <- numeric()

for(i in 1:2){
  a_e_afro[i] <- a_afro(x = i)
}     

b_afro <- function(x) {
  x <- sem2_implant_svy %>%
    filter(bd_e29_1 == x & ocup == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

b_e_afro <- numeric()

for(i in 1:2){
  b_e_afro[i] <- b_afro(x = i)
}     

c_afro <- as.data.frame(cbind(a_e_afro, b_e_afro))
c_afro <- c_afro %>% dplyr::mutate(VALOR = (a_e_afro + b_e_afro)/2)
c_afro <- c_afro %>% dplyr::mutate(aux = c(2,3))
c_afro <- c_afro[,c("VALOR", "aux")]



# Quintil de ingresos

a_quintil <- function(x) {
  x <- sem1_svy %>%
    filter(quintilesy == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

b_quintil <- function(x) {
  x <- sem2_implant_svy %>%
    filter(quintilesy == x & ocup == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}     

c_quintil <- as.data.frame(cbind(a_e_quintil, b_e_quintil))
c_quintil <- c_quintil %>% dplyr::mutate(VALOR = (a_e_quintil + b_e_quintil)/2)
c_quintil <- c_quintil %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil <- c_quintil[,c("VALOR", "aux")]

# Sexo

a_sexo <- function(x) {
  x <- sem1_svy %>%
    filter(bc_pe2 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}     

b_sexo <- function(x) {
  x <- sem2_implant_svy %>%
    filter(bc_pe2 == x & ocup == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}     

c_sexo <- as.data.frame(cbind(a_e_sexo, b_e_sexo))
c_sexo <- c_sexo %>% dplyr::mutate(VALOR = (a_e_sexo + b_e_sexo)/2)
c_sexo <- c_sexo %>% dplyr::mutate(aux = c(9, 10))
c_sexo <- c_sexo[,c("VALOR", "aux")]

# Tramo de edad

a_edad <- function(x) {
  x <- sem1_svy %>%
    filter(tramo_edad == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

a_e_edad <- numeric()

for(i in 1:5){
  a_e_edad[i] <- a_edad(x = i)
}     

b_edad <- function(x) {
  x <- sem2_implant_svy %>%
    filter(tramo_edad == x & ocup == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

b_e_edad <- numeric()

for(i in 1:5){
  b_e_edad[i] <- b_edad(x = i)
} 

c_edad <- as.data.frame(cbind(a_e_edad, b_e_edad))
c_edad <- c_edad %>% dplyr::mutate(VALOR = (a_e_edad + b_e_edad)/2)
c_edad <- c_edad %>% dplyr::mutate(aux = c(11, 12, 13, 14, 15))
c_edad <- c_edad[,c("VALOR", "aux")]



# Región

a_region <- function(x) {
  x <- sem1_svy %>%
    filter(region_4 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

a_e_region <- numeric()

for(i in 3:4){
  a_e_region[i] <- a_region(x = i)
}     

b_region <- function(x) {
  x <- sem2_implant_svy %>%
    filter(region_4 == x & ocup == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

b_e_region <- numeric()

for(i in 3:4){
  b_e_region[i] <- b_region(x = i)
}     


c_region <- as.data.frame(cbind(a_e_region, b_e_region))
c_region <- c_region %>% dplyr::mutate(VALOR = (a_e_region + b_e_region)/2)
c_region <- c_region[c(3,4),]
c_region <- c_region %>% dplyr::mutate(aux = c(16, 17))
c_region <- c_region[,c("VALOR", "aux")]


# Condición de pobreza del hogar

a_pobre <- function(x) {
  x <- sem1_svy %>%
    filter(pobre_06 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

a_e_pobre <- numeric()

for(i in 1:2){
  a_e_pobre[i] <- a_pobre(x = i)
}     

b_pobre <- function(x) {
  x <- sem2_implant_svy %>%
    filter(pobre_06 == x & ocup == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

b_e_pobre <- numeric()

for(i in 1:2){
  b_e_pobre[i] <- b_pobre(x = i)
}     


c_pobre <- as.data.frame(cbind(a_e_pobre, b_e_pobre))
c_pobre <- c_pobre %>% dplyr::mutate(VALOR = (a_e_pobre + b_e_pobre)/2)
c_pobre <- c_pobre %>% dplyr::mutate(aux = c(18, 19))
c_pobre <- c_pobre[,c("VALOR", "aux")]

## País Urbano

# Total

a_mes <- sem1_svy %>%
  srvyr::filter(ocup == 1 & (region_4 == 1 | region_4 == 2)) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(salario_insuf_smn, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

b_mes <- sem2_implant_svy %>%
  srvyr::filter(ocup == 1 & (region_4 == 1 | region_4 == 2)) %>%
  srvyr::summarise(colname = srvyr::survey_mean(salario_insuf_smn, na.rm=T))

b_sem <- mean(as.numeric(b_mes$colname))

c_ano_pu <- mean(c(a_sem, b_sem))
c_ano_pu <- as.data.frame(c_ano_pu)
c_ano_pu <- c_ano_pu %>% dplyr::mutate(aux = 1)
c_ano_pu <- c_ano_pu %>% dplyr::rename(VALOR = c_ano_pu)

# Quintil de ingresos

a_quintil <- function(x) {
  x <- sem1_svy %>%
    filter(quintilesy == x & ocup == 1 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

b_quintil <- function(x) {
  x <- sem2_implant_svy %>%
    filter(quintilesy == x & ocup == 1 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}     

c_quintil_pu <- as.data.frame(cbind(a_e_quintil, b_e_quintil))
c_quintil_pu <- c_quintil_pu %>% dplyr::mutate(VALOR = (a_e_quintil + b_e_quintil)/2)
c_quintil_pu <- c_quintil_pu %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil_pu <- c_quintil_pu[,c("VALOR", "aux")]


# Sexo

a_sexo <- function(x) {
  x <- sem1_svy %>%
    filter(bc_pe2 == x & ocup == 1 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}     

b_sexo <- function(x) {
  x <- sem2_implant_svy %>%
    filter(bc_pe2 == x & ocup == 1 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}     


c_sexo_pu <- as.data.frame(cbind(a_e_sexo, b_e_sexo))
c_sexo_pu <- c_sexo_pu %>% dplyr::mutate(VALOR = (a_e_sexo + b_e_sexo)/2)
c_sexo_pu <- c_sexo_pu %>% dplyr::mutate(aux = c(9, 10))
c_sexo_pu <- c_sexo_pu[,c("VALOR", "aux")]


# Tramo de edad

a_edad <- function(x) {
  x <- sem1_svy %>%
    filter(tramo_edad == x & ocup == 1 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

a_e_edad <- numeric()

for(i in 1:5){
  a_e_edad[i] <- a_edad(x = i)
}     

b_edad <- function(x) {
  x <- sem2_implant_svy %>%
    filter(tramo_edad == x & ocup == 1 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

b_e_edad <- numeric()

for(i in 1:5){
  b_e_edad[i] <- b_edad(x = i)
} 

c_edad_pu <- as.data.frame(cbind(a_e_edad, b_e_edad))
c_edad_pu <- c_edad_pu %>% dplyr::mutate(VALOR = (a_e_edad + b_e_edad)/2)
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

a_mes <- sem1_svy %>%
  srvyr::filter(ocup == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(salario_insuf, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

b_mes <- sem2_implant_svy %>%
  srvyr::filter(ocup == 1) %>%
  srvyr::summarise(colname = srvyr::survey_mean(salario_insuf, na.rm=T))

b_sem <- mean(as.numeric(b_mes$colname))

c_ano <- mean(c(a_sem, b_sem))
c_ano <- as.data.frame(c_ano)
c_ano <- c_ano %>% dplyr::mutate(aux = 1)
c_ano <- c_ano %>% dplyr::rename(VALOR = c_ano)

# Ascendencia étnico racial

a_afro <- function(x) {
  x <- sem1_svy %>%
    filter(bd_e29_1 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf, na.rm=T))
  x <- mean(x$colname)
}       

a_e_afro <- numeric()

for(i in 1:2){
  a_e_afro[i] <- a_afro(x = i)
}     

b_afro <- function(x) {
  x <- sem2_implant_svy %>%
    filter(bd_e29_1 == x & ocup == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf, na.rm=T))
  x <- mean(x$colname)
}       

b_e_afro <- numeric()

for(i in 1:2){
  b_e_afro[i] <- b_afro(x = i)
}     

c_afro <- as.data.frame(cbind(a_e_afro, b_e_afro))
c_afro <- c_afro %>% dplyr::mutate(VALOR = (a_e_afro + b_e_afro)/2)
c_afro <- c_afro %>% dplyr::mutate(aux = c(2,3))
c_afro <- c_afro[,c("VALOR", "aux")]



# Quintil de ingresos

a_quintil <- function(x) {
  x <- sem1_svy %>%
    filter(quintilesy == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf, na.rm=T))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

b_quintil <- function(x) {
  x <- sem2_implant_svy %>%
    filter(quintilesy == x & ocup == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf, na.rm=T))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}     

c_quintil <- as.data.frame(cbind(a_e_quintil, b_e_quintil))
c_quintil <- c_quintil %>% dplyr::mutate(VALOR = (a_e_quintil + b_e_quintil)/2)
c_quintil <- c_quintil %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil <- c_quintil[,c("VALOR", "aux")]

# Sexo

a_sexo <- function(x) {
  x <- sem1_svy %>%
    filter(bc_pe2 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf, na.rm=T))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}     

b_sexo <- function(x) {
  x <- sem2_implant_svy %>%
    filter(bc_pe2 == x & ocup == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf, na.rm=T))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}     

c_sexo <- as.data.frame(cbind(a_e_sexo, b_e_sexo))
c_sexo <- c_sexo %>% dplyr::mutate(VALOR = (a_e_sexo + b_e_sexo)/2)
c_sexo <- c_sexo %>% dplyr::mutate(aux = c(9, 10))
c_sexo <- c_sexo[,c("VALOR", "aux")]

# Tramo de edad

a_edad <- function(x) {
  x <- sem1_svy %>%
    filter(tramo_edad == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf, na.rm=T))
  x <- mean(x$colname)
}       

a_e_edad <- numeric()

for(i in 1:5){
  a_e_edad[i] <- a_edad(x = i)
}     

b_edad <- function(x) {
  x <- sem2_implant_svy %>%
    filter(tramo_edad == x & ocup == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf, na.rm=T))
  x <- mean(x$colname)
}       

b_e_edad <- numeric()

for(i in 1:5){
  b_e_edad[i] <- b_edad(x = i)
} 

c_edad <- as.data.frame(cbind(a_e_edad, b_e_edad))
c_edad <- c_edad %>% dplyr::mutate(VALOR = (a_e_edad + b_e_edad)/2)
c_edad <- c_edad %>% dplyr::mutate(aux = c(11, 12, 13, 14, 15))
c_edad <- c_edad[,c("VALOR", "aux")]



# Región

a_region <- function(x) {
  x <- sem1_svy %>%
    filter(region_4 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf, na.rm=T))
  x <- mean(x$colname)
}       

a_e_region <- numeric()

for(i in 3:4){
  a_e_region[i] <- a_region(x = i)
}     

b_region <- function(x) {
  x <- sem2_implant_svy %>%
    filter(region_4 == x & ocup == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf, na.rm=T))
  x <- mean(x$colname)
}       

b_e_region <- numeric()

for(i in 3:4){
  b_e_region[i] <- b_region(x = i)
}     


c_region <- as.data.frame(cbind(a_e_region, b_e_region))
c_region <- c_region %>% dplyr::mutate(VALOR = (a_e_region + b_e_region)/2)
c_region <- c_region[c(3,4),]
c_region <- c_region %>% dplyr::mutate(aux = c(16, 17))
c_region <- c_region[,c("VALOR", "aux")]


# Condición de pobreza del hogar

a_pobre <- function(x) {
  x <- sem1_svy %>%
    filter(pobre_06 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf, na.rm=T))
  x <- mean(x$colname)
}       

a_e_pobre <- numeric()

for(i in 1:2){
  a_e_pobre[i] <- a_pobre(x = i)
}     

b_pobre <- function(x) {
  x <- sem2_implant_svy %>%
    filter(pobre_06 == x & ocup == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf, na.rm=T))
  x <- mean(x$colname)
}       

b_e_pobre <- numeric()

for(i in 1:2){
  b_e_pobre[i] <- b_pobre(x = i)
}     


c_pobre <- as.data.frame(cbind(a_e_pobre, b_e_pobre))
c_pobre <- c_pobre %>% dplyr::mutate(VALOR = (a_e_pobre + b_e_pobre)/2)
c_pobre <- c_pobre %>% dplyr::mutate(aux = c(18, 19))
c_pobre <- c_pobre[,c("VALOR", "aux")]

## País Urbano

# Total

a_mes <- sem1_svy %>%
  srvyr::filter(ocup == 1 & (region_4 == 1 | region_4 == 2)) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(salario_insuf, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

b_mes <- sem2_implant_svy %>%
  srvyr::filter(ocup == 1 & (region_4 == 1 | region_4 == 2)) %>%
  srvyr::summarise(colname = srvyr::survey_mean(salario_insuf, na.rm=T))

b_sem <- mean(as.numeric(b_mes$colname))

c_ano_pu <- mean(c(a_sem, b_sem))
c_ano_pu <- as.data.frame(c_ano_pu)
c_ano_pu <- c_ano_pu %>% dplyr::mutate(aux = 1)
c_ano_pu <- c_ano_pu %>% dplyr::rename(VALOR = c_ano_pu)

# Quintil de ingresos

a_quintil <- function(x) {
  x <- sem1_svy %>%
    filter(quintilesy == x & ocup == 1 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

b_quintil <- function(x) {
  x <- sem2_implant_svy %>%
    filter(quintilesy == x & ocup == 1 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}     

c_quintil_pu <- as.data.frame(cbind(a_e_quintil, b_e_quintil))
c_quintil_pu <- c_quintil_pu %>% dplyr::mutate(VALOR = (a_e_quintil + b_e_quintil)/2)
c_quintil_pu <- c_quintil_pu %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil_pu <- c_quintil_pu[,c("VALOR", "aux")]


# Sexo

a_sexo <- function(x) {
  x <- sem1_svy %>%
    filter(bc_pe2 == x & ocup == 1 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf, na.rm=T))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}     

b_sexo <- function(x) {
  x <- sem2_implant_svy %>%
    filter(bc_pe2 == x & ocup == 1 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf, na.rm=T))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}     


c_sexo_pu <- as.data.frame(cbind(a_e_sexo, b_e_sexo))
c_sexo_pu <- c_sexo_pu %>% dplyr::mutate(VALOR = (a_e_sexo + b_e_sexo)/2)
c_sexo_pu <- c_sexo_pu %>% dplyr::mutate(aux = c(9, 10))
c_sexo_pu <- c_sexo_pu[,c("VALOR", "aux")]


# Tramo de edad

a_edad <- function(x) {
  x <- sem1_svy %>%
    filter(tramo_edad == x & ocup == 1 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf, na.rm=T))
  x <- mean(x$colname)
}       

a_e_edad <- numeric()

for(i in 1:5){
  a_e_edad[i] <- a_edad(x = i)
}     

b_edad <- function(x) {
  x <- sem2_implant_svy %>%
    filter(tramo_edad == x & ocup == 1 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf, na.rm=T))
  x <- mean(x$colname)
}       

b_e_edad <- numeric()

for(i in 1:5){
  b_e_edad[i] <- b_edad(x = i)
} 

c_edad_pu <- as.data.frame(cbind(a_e_edad, b_e_edad))
c_edad_pu <- c_edad_pu %>% dplyr::mutate(VALOR = (a_e_edad + b_e_edad)/2)
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

a_mes_varones <- sem1_svy %>%
  srvyr::filter(ocup == 1 & bc_pe2 == 1 & F85>0) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))

a_sem_varones <- mean(as.numeric(a_mes_varones$colname))

a_mes_mujeres <- sem1_svy %>%
  srvyr::filter(ocup == 1 & bc_pe2 == 2 & F85>0) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))

a_sem_mujeres <- mean(as.numeric(a_mes_mujeres$colname))

b_mes_varones <- sem2_implant_svy %>%
  srvyr::filter(ocup == 1 & bc_pe2 == 1 & f85>0) %>%
  srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))

b_sem_varones <- mean(as.numeric(b_mes_varones$colname))

b_mes_mujeres <- sem2_implant_svy %>%
  srvyr::filter(ocup == 1 & bc_pe2 == 2 & f85>0) %>%
  srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))

b_sem_mujeres <- mean(as.numeric(b_mes_mujeres$colname))

c_ano <- as.data.frame(t(c(a_sem_varones, b_sem_varones, a_sem_mujeres, b_sem_mujeres)))
c_ano <- c_ano %>% dplyr::mutate(c_ano_varones = (V1 + V2) / 2, 
                                 c_ano_mujeres = (V3 + V4) / 2)
c_ano <- c_ano %>% dplyr::mutate(VALOR = c_ano_mujeres / c_ano_varones)
c_ano <- c_ano %>% dplyr::mutate(aux = 1)
c_ano <- c_ano[, c("VALOR", "aux")]




# Ascendencia étnico racial

a_afro_varones <- function(x) {
  x <- sem1_svy %>%
    filter(bd_e29_1 == x & ocup == 1  & bc_pe2 == 1 & F85>0) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

a_e_afro_varones <- numeric()

for(i in 1:2){
  a_e_afro_varones[i] <- a_afro_varones(x = i)
}     

a_afro_mujeres <- function(x) {
  x <- sem1_svy %>%
    filter(bd_e29_1 == x & ocup == 1  & bc_pe2 == 2 & F85>0) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

a_e_afro_mujeres <- numeric()

for(i in 1:2){
  a_e_afro_mujeres[i] <- a_afro_mujeres(x = i)
} 

b_afro_varones <- function(x) {
  x <- sem2_implant_svy %>%
    filter(bd_e29_1 == x & ocup == 1  & bc_pe2 == 1 & f85>0) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

b_e_afro_varones <- numeric()

for(i in 1:2){
  b_e_afro_varones[i] <- b_afro_varones(x = i)
}  

b_afro_mujeres <- function(x) {
  x <- sem2_implant_svy %>%
    filter(bd_e29_1 == x & ocup == 1  & bc_pe2 == 2 & f85>0) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

b_e_afro_mujeres <- numeric()

for(i in 1:2){
  b_e_afro_mujeres[i] <- b_afro_mujeres(x = i)
} 

c_afro <- as.data.frame(cbind(a_e_afro_varones, b_e_afro_varones, a_e_afro_mujeres, b_e_afro_mujeres))
c_afro <- c_afro %>% dplyr::mutate(c_ano_varones = (a_e_afro_varones + b_e_afro_varones)/2,
                                   c_ano_mujeres = (a_e_afro_mujeres + b_e_afro_mujeres)/2)
c_afro <- c_afro %>% dplyr::mutate(VALOR = c_ano_mujeres / c_ano_varones)
c_afro <- c_afro %>% dplyr::mutate(aux = c(2,3))
c_afro <- c_afro[,c("VALOR", "aux")]



# Quintil de ingresos

a_quintil_varones <- function(x) {
  x <- sem1_svy %>%
    filter(quintilesy == x & ocup == 1  & bc_pe2 == 1 & F85>0) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

a_e_quintil_varones <- numeric()

for(i in 1:5){
  a_e_quintil_varones[i] <- a_quintil_varones(x = i)
}     

a_quintil_mujeres <- function(x) {
  x <- sem1_svy %>%
    filter(quintilesy == x & ocup == 1  & bc_pe2 == 2 & F85>0) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

a_e_quintil_mujeres <- numeric()

for(i in 1:5){
  a_e_quintil_mujeres[i] <- a_quintil_mujeres(x = i)
}  


b_quintil_varones <- function(x) {
  x <- sem2_implant_svy %>%
    filter(quintilesy == x & ocup == 1 & bc_pe2 == 1 & f85>0) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

b_e_quintil_varones <- numeric()

for(i in 1:5){
  b_e_quintil_varones[i] <- b_quintil_varones(x = i)
}    


b_quintil_mujeres <- function(x) {
  x <- sem2_implant_svy %>%
    filter(quintilesy == x & ocup == 1  & bc_pe2 == 2 & f85>0) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

b_e_quintil_mujeres <- numeric()

for(i in 1:5){
  b_e_quintil_mujeres[i] <- b_quintil_mujeres(x = i)
}  

c_quintil <- as.data.frame(cbind(a_e_quintil_varones, b_e_quintil_varones, a_e_quintil_mujeres, b_e_quintil_mujeres))
c_quintil <- c_quintil %>% dplyr::mutate(c_ano_varones = (a_e_quintil_varones + b_e_quintil_varones)/2,
                                   c_ano_mujeres = (a_e_quintil_mujeres + b_e_quintil_mujeres)/2)
c_quintil <- c_quintil %>% dplyr::mutate(VALOR = c_ano_mujeres / c_ano_varones)
c_quintil <- c_quintil %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil <- c_quintil[,c("VALOR", "aux")]


# Tramo de edad

a_edad_varones <- function(x) {
  x <- sem1_svy %>%
    filter(tramo_edad == x & ocup == 1 & bc_pe2 == 1 & F85>0) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

a_e_edad_varones <- numeric()

for(i in 1:5){
  a_e_edad_varones[i] <- a_edad_varones(x = i)
}  

a_edad_mujeres <- function(x) {
  x <- sem1_svy %>%
    filter(tramo_edad == x & ocup == 1 & bc_pe2 == 2 & F85>0) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

a_e_edad_mujeres <- numeric()

for(i in 1:5){
  a_e_edad_mujeres[i] <- a_edad_mujeres(x = i)
} 

b_edad_varones <- function(x) {
  x <- sem2_implant_svy %>%
    filter(tramo_edad == x & ocup == 1 & bc_pe2 == 1 & f85>0) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

b_e_edad_varones <- numeric()

for(i in 1:5){
  b_e_edad_varones[i] <- b_edad_varones(x = i)
} 

b_edad_mujeres <- function(x) {
  x <- sem2_implant_svy %>%
    filter(tramo_edad == x & ocup == 1 & bc_pe2 == 2 & f85>0) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

b_e_edad_mujeres <- numeric()

for(i in 1:5){
  b_e_edad_mujeres[i] <- b_edad_mujeres(x = i)
} 

c_edad <- as.data.frame(cbind(a_e_edad_varones, b_e_edad_varones, a_e_edad_mujeres, b_e_edad_mujeres))
c_edad <- c_edad %>% dplyr::mutate(c_ano_varones = (a_e_edad_varones + b_e_edad_varones)/2,
                                   c_ano_mujeres = (a_e_edad_mujeres + b_e_edad_mujeres)/2)
c_edad <- c_edad %>% dplyr::mutate(VALOR = c_ano_mujeres / c_ano_varones)
c_edad <- c_edad %>% dplyr::mutate(aux = c(11, 12, 13, 14, 15))
c_edad <- c_edad[,c("VALOR", "aux")]



# Región

a_region_varones <- function(x) {
  x <- sem1_svy %>%
    filter(region_4 == x & ocup == 1 & bc_pe2 == 1 & F85>0) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

a_e_region_varones <- numeric()

for(i in 3:4){
  a_e_region_varones[i] <- a_region_varones(x = i)
}     

a_region_mujeres <- function(x) {
  x <- sem1_svy %>%
    filter(region_4 == x & ocup == 1 & bc_pe2 == 2 & F85>0) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

a_e_region_mujeres <- numeric()

for(i in 3:4){
  a_e_region_mujeres[i] <- a_region_mujeres(x = i)
}     

b_region_varones <- function(x) {
  x <- sem2_implant_svy %>%
    filter(region_4 == x & ocup == 1 & bc_pe2 == 1 & f85>0) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

b_e_region_varones <- numeric()

for(i in 3:4){
  b_e_region_varones[i] <- b_region_varones(x = i)
}     

b_region_mujeres <- function(x) {
  x <- sem2_implant_svy %>%
    filter(region_4 == x & ocup == 1 & bc_pe2 == 2 & f85>0) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

b_e_region_mujeres <- numeric()

for(i in 3:4){
  b_e_region_mujeres[i] <- b_region_mujeres(x = i)
}     


c_region <- as.data.frame(cbind(a_e_region_varones, b_e_region_varones, a_e_region_mujeres, b_e_region_mujeres))
c_region <- c_region %>% dplyr::mutate(c_ano_varones = (a_e_region_varones + b_e_region_varones)/2,
                                       c_ano_mujeres = (a_e_region_mujeres + b_e_region_mujeres)/2)
c_region <- c_region %>% dplyr::mutate(VALOR = c_ano_mujeres / c_ano_varones)
c_region <- c_region[c(3,4),]
c_region <- c_region %>% dplyr::mutate(aux = c(16, 17))
c_region <- c_region[,c("VALOR", "aux")]


# Condición de pobreza del hogar

a_pobre_varones <- function(x) {
  x <- sem1_svy %>%
    filter(pobre_06 == x & ocup == 1 & bc_pe2 == 1 & F85>0) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

a_e_pobre_varones <- numeric()

for(i in 1:2){
  a_e_pobre_varones[i] <- a_pobre_varones(x = i)
}     

a_pobre_mujeres <- function(x) {
  x <- sem1_svy %>%
    filter(pobre_06 == x & ocup == 1 & bc_pe2 == 1 & F85>0) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

a_e_pobre_mujeres <- numeric()

for(i in 1:2){
  a_e_pobre_mujeres[i] <- a_pobre_mujeres(x = i)
} 

b_pobre_varones <- function(x) {
  x <- sem2_implant_svy %>%
    filter(pobre_06 == x & ocup == 1 & bc_pe2 == 1 & f85>0) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

b_e_pobre_varones <- numeric()

for(i in 1:2){
  b_e_pobre_varones[i] <- b_pobre_varones(x = i)
}     

b_pobre_mujeres <- function(x) {
  x <- sem2_implant_svy %>%
    filter(pobre_06 == x & ocup == 1 & bc_pe2 == 1 & f85>0) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

b_e_pobre_mujeres <- numeric()

for(i in 1:2){
  b_e_pobre_mujeres[i] <- b_pobre_mujeres(x = i)
}   

c_pobre <- as.data.frame(cbind(a_e_pobre_varones, b_e_pobre_varones, a_e_pobre_mujeres, b_e_pobre_mujeres))
c_pobre <- c_pobre %>% dplyr::mutate(c_ano_varones = (a_e_pobre_varones + b_e_pobre_varones)/2,
                                       c_ano_mujeres = (a_e_pobre_mujeres + b_e_pobre_mujeres)/2)
c_pobre <- c_pobre %>% dplyr::mutate(VALOR = c_ano_mujeres / c_ano_varones)
c_pobre <- c_pobre %>% dplyr::mutate(aux = c(18, 19))
c_pobre <- c_pobre[,c("VALOR", "aux")]

## País Urbano

# Total

a_mes_varones <- sem1_svy %>%
  srvyr::filter(ocup == 1 & (region_4 == 1 | region_4 == 2) & bc_pe2 == 1 & F85>0) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))

a_sem_varones <- mean(as.numeric(a_mes_varones$colname))

a_mes_mujeres <- sem1_svy %>%
  srvyr::filter(ocup == 1 & (region_4 == 1 | region_4 == 2) & bc_pe2 == 2 & F85>0) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))

a_sem_mujeres <- mean(as.numeric(a_mes_mujeres$colname))

b_mes_varones <- sem2_implant_svy %>%
  srvyr::filter(ocup == 1 & (region_4 == 1 | region_4 == 2) & bc_pe2 == 1 & f85>0) %>%
  srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))

b_sem_varones <- mean(as.numeric(b_mes_varones$colname))

b_mes_mujeres <- sem2_implant_svy %>%
  srvyr::filter(ocup == 1 & (region_4 == 1 | region_4 == 2) & bc_pe2 == 2 & f85>0) %>%
  srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))

b_sem_mujeres <- mean(as.numeric(b_mes_mujeres$colname))

c_ano_pu <- as.data.frame(t(c(a_sem_varones, b_sem_varones, a_sem_mujeres, b_sem_mujeres)))
c_ano_pu <- c_ano_pu %>% dplyr::mutate(c_ano_varones = (V1 + V2) / 2, 
                                 c_ano_mujeres = (V3 + V4) / 2)
c_ano_pu <- c_ano_pu %>% dplyr::mutate(VALOR = c_ano_mujeres / c_ano_varones)
c_ano_pu <- c_ano_pu %>% dplyr::mutate(aux = 1)
c_ano_pu <- c_ano_pu[, c("VALOR", "aux")]


# Quintil de ingresos

a_quintil_varones <- function(x) {
  x <- sem1_svy %>%
    filter(quintilesy == x & ocup == 1 & (region_4 == 1 | region_4 == 2) & bc_pe2 == 1 & F85>0) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

a_e_quintil_varones <- numeric()

for(i in 1:5){
  a_e_quintil_varones[i] <- a_quintil_varones(x = i)
}  

a_quintil_mujeres <- function(x) {
  x <- sem1_svy %>%
    filter(quintilesy == x & ocup == 1 & (region_4 == 1 | region_4 == 2) & bc_pe2 == 2 & F85>0) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

a_e_quintil_mujeres <- numeric()

for(i in 1:5){
  a_e_quintil_mujeres[i] <- a_quintil_mujeres(x = i)
}     

b_quintil_varones <- function(x) {
  x <- sem2_implant_svy %>%
    filter(quintilesy == x & ocup == 1 & (region_4 == 1 | region_4 == 2) & bc_pe2 == 1 & f85>0) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

b_e_quintil_varones <- numeric()

for(i in 1:5){
  b_e_quintil_varones[i] <- b_quintil_varones(x = i)
}     

b_quintil_mujeres <- function(x) {
  x <- sem2_implant_svy %>%
    filter(quintilesy == x & ocup == 1 & (region_4 == 1 | region_4 == 2) & bc_pe2 == 2 & f85>0) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

b_e_quintil_mujeres <- numeric()

for(i in 1:5){
  b_e_quintil_mujeres[i] <- b_quintil_mujeres(x = i)
} 

c_quintil_pu <- as.data.frame(cbind(a_e_quintil_varones, b_e_quintil_varones, a_e_quintil_mujeres, b_e_quintil_mujeres))
c_quintil_pu <- c_quintil_pu %>% dplyr::mutate(c_ano_varones = (a_e_quintil_varones + b_e_quintil_varones)/2,
                                               c_ano_mujeres = (a_e_quintil_mujeres + b_e_quintil_mujeres)/2)
c_quintil_pu <- c_quintil_pu %>% dplyr::mutate(VALOR = c_ano_mujeres / c_ano_varones)
c_quintil_pu <- c_quintil_pu %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil_pu <- c_quintil_pu[,c("VALOR", "aux")]



# Tramo de edad

a_edad_varones <- function(x) {
  x <- sem1_svy %>%
    filter(tramo_edad == x & ocup == 1 & (region_4 == 1 | region_4 == 2) & bc_pe2 == 1 & F85>0) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

a_e_edad_varones <- numeric()

for(i in 1:5){
  a_e_edad_varones[i] <- a_edad_varones(x = i)
}     

a_edad_mujeres <- function(x) {
  x <- sem1_svy %>%
    filter(tramo_edad == x & ocup == 1 & (region_4 == 1 | region_4 == 2) & bc_pe2 == 2 & F85>0) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

a_e_edad_mujeres <- numeric()

for(i in 1:5){
  a_e_edad_mujeres[i] <- a_edad_mujeres(x = i)
}     



b_edad_varones <- function(x) {
  x <- sem2_implant_svy %>%
    filter(tramo_edad == x & ocup == 1 & (region_4 == 1 | region_4 == 2) & bc_pe2 == 1 & f85>0) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

b_e_edad_varones <- numeric()

for(i in 1:5){
  b_e_edad_varones[i] <- b_edad_varones(x = i)
} 

b_edad_mujeres <- function(x) {
  x <- sem2_implant_svy %>%
    filter(tramo_edad == x & ocup == 1 & (region_4 == 1 | region_4 == 2) & bc_pe2 == 2 & f85>0) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

b_e_edad_mujeres <- numeric()

for(i in 1:5){
  b_e_edad_mujeres[i] <- b_edad_mujeres(x = i)
} 

c_edad_pu <- as.data.frame(cbind(a_e_edad_varones, b_e_edad_varones, a_e_edad_mujeres, b_e_edad_mujeres))
c_edad_pu <- c_edad_pu %>% dplyr::mutate(c_ano_varones = (a_e_edad_varones + b_e_edad_varones)/2,
                                         c_ano_mujeres = (a_e_edad_mujeres + b_e_edad_mujeres)/2)
c_edad_pu <- c_edad_pu %>% dplyr::mutate(VALOR = c_ano_mujeres / c_ano_varones)
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

a_mes <- sem1_svy %>%
  srvyr::filter(tramo_edad == 5) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(jub, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

b_mes <- sem2_implant_svy %>%
  srvyr::filter(tramo_edad == 5) %>%
  srvyr::summarise(colname = srvyr::survey_mean(jub, na.rm=T))

b_sem <- mean(as.numeric(b_mes$colname))

c_ano <- mean(c(a_sem, b_sem))
c_ano <- as.data.frame(c_ano)
c_ano <- c_ano %>% dplyr::mutate(aux = 1)
c_ano <- c_ano %>% dplyr::rename(VALOR = c_ano)

# Ascendencia étnico racial

a_afro <- function(x) {
  x <- sem1_svy %>%
    filter(bd_e29_1 == x & tramo_edad == 5) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub, na.rm=T))
  x <- mean(x$colname)
}       

a_e_afro <- numeric()

for(i in 1:2){
  a_e_afro[i] <- a_afro(x = i)
}     

b_afro <- function(x) {
  x <- sem2_implant_svy %>%
    filter(bd_e29_1 == x & tramo_edad == 5) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub, na.rm=T))
  x <- mean(x$colname)
}       

b_e_afro <- numeric()

for(i in 1:2){
  b_e_afro[i] <- b_afro(x = i)
}     

c_afro <- as.data.frame(cbind(a_e_afro, b_e_afro))
c_afro <- c_afro %>% dplyr::mutate(VALOR = (a_e_afro + b_e_afro)/2)
c_afro <- c_afro %>% dplyr::mutate(aux = c(2,3))
c_afro <- c_afro[,c("VALOR", "aux")]



# Quintil de ingresos

a_quintil <- function(x) {
  x <- sem1_svy %>%
    filter(quintilesy == x & tramo_edad == 5) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub, na.rm=T))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

b_quintil <- function(x) {
  x <- sem2_implant_svy %>%
    filter(quintilesy == x & tramo_edad == 5) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub, na.rm=T))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}     

c_quintil <- as.data.frame(cbind(a_e_quintil, b_e_quintil))
c_quintil <- c_quintil %>% dplyr::mutate(VALOR = (a_e_quintil + b_e_quintil)/2)
c_quintil <- c_quintil %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil <- c_quintil[,c("VALOR", "aux")]

# Sexo

a_sexo <- function(x) {
  x <- sem1_svy %>%
    filter(bc_pe2 == x & tramo_edad == 5) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub, na.rm=T))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}     

b_sexo <- function(x) {
  x <- sem2_implant_svy %>%
    filter(bc_pe2 == x & tramo_edad == 5) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub, na.rm=T))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}     

c_sexo <- as.data.frame(cbind(a_e_sexo, b_e_sexo))
c_sexo <- c_sexo %>% dplyr::mutate(VALOR = (a_e_sexo + b_e_sexo)/2)
c_sexo <- c_sexo %>% dplyr::mutate(aux = c(9, 10))
c_sexo <- c_sexo[,c("VALOR", "aux")]



# Región

a_region <- function(x) {
  x <- sem1_svy %>%
    filter(region_4 == x & tramo_edad == 5) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub, na.rm=T))
  x <- mean(x$colname)
}       

a_e_region <- numeric()

for(i in 3:4){
  a_e_region[i] <- a_region(x = i)
}     

b_region <- function(x) {
  x <- sem2_implant_svy %>%
    filter(region_4 == x & tramo_edad == 5) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub, na.rm=T))
  x <- mean(x$colname)
}       

b_e_region <- numeric()

for(i in 3:4){
  b_e_region[i] <- b_region(x = i)
}     


c_region <- as.data.frame(cbind(a_e_region, b_e_region))
c_region <- c_region %>% dplyr::mutate(VALOR = (a_e_region + b_e_region)/2)
c_region <- c_region[c(3,4),]
c_region <- c_region %>% dplyr::mutate(aux = c(16, 17))
c_region <- c_region[,c("VALOR", "aux")]


# Condición de pobreza del hogar

a_pobre <- function(x) {
  x <- sem1_svy %>%
    filter(pobre_06 == x & tramo_edad == 5) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub, na.rm=T))
  x <- mean(x$colname)
}       

a_e_pobre <- numeric()

for(i in 1:2){
  a_e_pobre[i] <- a_pobre(x = i)
}     

b_pobre <- function(x) {
  x <- sem2_implant_svy %>%
    filter(pobre_06 == x & tramo_edad == 5) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub, na.rm=T))
  x <- mean(x$colname)
}       

b_e_pobre <- numeric()

for(i in 1:2){
  b_e_pobre[i] <- b_pobre(x = i)
}     


c_pobre <- as.data.frame(cbind(a_e_pobre, b_e_pobre))
c_pobre <- c_pobre %>% dplyr::mutate(VALOR = (a_e_pobre + b_e_pobre)/2)
c_pobre <- c_pobre %>% dplyr::mutate(aux = c(18, 19))
c_pobre <- c_pobre[,c("VALOR", "aux")]

## País Urbano

# Total

a_mes <- sem1_svy %>%
  srvyr::filter(tramo_edad == 5 & (region_4 == 1 | region_4 == 2)) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(jub, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

b_mes <- sem2_implant_svy %>%
  srvyr::filter(tramo_edad == 5 & (region_4 == 1 | region_4 == 2)) %>%
  srvyr::summarise(colname = srvyr::survey_mean(jub, na.rm=T))

b_sem <- mean(as.numeric(b_mes$colname))

c_ano_pu <- mean(c(a_sem, b_sem))
c_ano_pu <- as.data.frame(c_ano_pu)
c_ano_pu <- c_ano_pu %>% dplyr::mutate(aux = 1)
c_ano_pu <- c_ano_pu %>% dplyr::rename(VALOR = c_ano_pu)

# Quintil de ingresos

a_quintil <- function(x) {
  x <- sem1_svy %>%
    filter(quintilesy == x & tramo_edad == 5 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub, na.rm=T))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

b_quintil <- function(x) {
  x <- sem2_implant_svy %>%
    filter(quintilesy == x & tramo_edad == 5 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub, na.rm=T))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}     

c_quintil_pu <- as.data.frame(cbind(a_e_quintil, b_e_quintil))
c_quintil_pu <- c_quintil_pu %>% dplyr::mutate(VALOR = (a_e_quintil + b_e_quintil)/2)
c_quintil_pu <- c_quintil_pu %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil_pu <- c_quintil_pu[,c("VALOR", "aux")]


# Sexo

a_sexo <- function(x) {
  x <- sem1_svy %>%
    filter(bc_pe2 == x & tramo_edad == 5 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub, na.rm=T))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}     

b_sexo <- function(x) {
  x <- sem2_implant_svy %>%
    filter(bc_pe2 == x & tramo_edad == 5 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub, na.rm=T))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}     


c_sexo_pu <- as.data.frame(cbind(a_e_sexo, b_e_sexo))
c_sexo_pu <- c_sexo_pu %>% dplyr::mutate(VALOR = (a_e_sexo + b_e_sexo)/2)
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

a_mes <- sem1_svy %>%
  srvyr::filter(tramo_edad == 5) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(pens, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

b_mes <- sem2_implant_svy %>%
  srvyr::filter(tramo_edad == 5) %>%
  srvyr::summarise(colname = srvyr::survey_mean(pens, na.rm=T))

b_sem <- mean(as.numeric(b_mes$colname))

c_ano <- mean(c(a_sem, b_sem))
c_ano <- as.data.frame(c_ano)
c_ano <- c_ano %>% dplyr::mutate(aux = 1)
c_ano <- c_ano %>% dplyr::rename(VALOR = c_ano)

# Ascendencia étnico racial

a_afro <- function(x) {
  x <- sem1_svy %>%
    filter(bd_e29_1 == x & tramo_edad == 5) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens, na.rm=T))
  x <- mean(x$colname)
}       

a_e_afro <- numeric()

for(i in 1:2){
  a_e_afro[i] <- a_afro(x = i)
}     

b_afro <- function(x) {
  x <- sem2_implant_svy %>%
    filter(bd_e29_1 == x & tramo_edad == 5) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens, na.rm=T))
  x <- mean(x$colname)
}       

b_e_afro <- numeric()

for(i in 1:2){
  b_e_afro[i] <- b_afro(x = i)
}     

c_afro <- as.data.frame(cbind(a_e_afro, b_e_afro))
c_afro <- c_afro %>% dplyr::mutate(VALOR = (a_e_afro + b_e_afro)/2)
c_afro <- c_afro %>% dplyr::mutate(aux = c(2,3))
c_afro <- c_afro[,c("VALOR", "aux")]



# Quintil de ingresos

a_quintil <- function(x) {
  x <- sem1_svy %>%
    filter(quintilesy == x & tramo_edad == 5) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens, na.rm=T))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

b_quintil <- function(x) {
  x <- sem2_implant_svy %>%
    filter(quintilesy == x & tramo_edad == 5) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens, na.rm=T))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}     

c_quintil <- as.data.frame(cbind(a_e_quintil, b_e_quintil))
c_quintil <- c_quintil %>% dplyr::mutate(VALOR = (a_e_quintil + b_e_quintil)/2)
c_quintil <- c_quintil %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil <- c_quintil[,c("VALOR", "aux")]

# Sexo

a_sexo <- function(x) {
  x <- sem1_svy %>%
    filter(bc_pe2 == x & tramo_edad == 5) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens, na.rm=T))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}     

b_sexo <- function(x) {
  x <- sem2_implant_svy %>%
    filter(bc_pe2 == x & tramo_edad == 5) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens, na.rm=T))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}     

c_sexo <- as.data.frame(cbind(a_e_sexo, b_e_sexo))
c_sexo <- c_sexo %>% dplyr::mutate(VALOR = (a_e_sexo + b_e_sexo)/2)
c_sexo <- c_sexo %>% dplyr::mutate(aux = c(9, 10))
c_sexo <- c_sexo[,c("VALOR", "aux")]



# Región

a_region <- function(x) {
  x <- sem1_svy %>%
    filter(region_4 == x & tramo_edad == 5) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens, na.rm=T))
  x <- mean(x$colname)
}       

a_e_region <- numeric()

for(i in 3:4){
  a_e_region[i] <- a_region(x = i)
}     

b_region <- function(x) {
  x <- sem2_implant_svy %>%
    filter(region_4 == x & tramo_edad == 5) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens, na.rm=T))
  x <- mean(x$colname)
}       

b_e_region <- numeric()

for(i in 3:4){
  b_e_region[i] <- b_region(x = i)
}     


c_region <- as.data.frame(cbind(a_e_region, b_e_region))
c_region <- c_region %>% dplyr::mutate(VALOR = (a_e_region + b_e_region)/2)
c_region <- c_region[c(3,4),]
c_region <- c_region %>% dplyr::mutate(aux = c(16, 17))
c_region <- c_region[,c("VALOR", "aux")]


# Condición de pobreza del hogar

a_pobre <- function(x) {
  x <- sem1_svy %>%
    filter(pobre_06 == x & tramo_edad == 5) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens, na.rm=T))
  x <- mean(x$colname)
}       

a_e_pobre <- numeric()

for(i in 1:2){
  a_e_pobre[i] <- a_pobre(x = i)
}     

b_pobre <- function(x) {
  x <- sem2_implant_svy %>%
    filter(pobre_06 == x & tramo_edad == 5) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens, na.rm=T))
  x <- mean(x$colname)
}       

b_e_pobre <- numeric()

for(i in 1:2){
  b_e_pobre[i] <- b_pobre(x = i)
}     


c_pobre <- as.data.frame(cbind(a_e_pobre, b_e_pobre))
c_pobre <- c_pobre %>% dplyr::mutate(VALOR = (a_e_pobre + b_e_pobre)/2)
c_pobre <- c_pobre %>% dplyr::mutate(aux = c(18, 19))
c_pobre <- c_pobre[,c("VALOR", "aux")]

## País Urbano

# Total

a_mes <- sem1_svy %>%
  srvyr::filter(tramo_edad == 5 & (region_4 == 1 | region_4 == 2)) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(pens, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

b_mes <- sem2_implant_svy %>%
  srvyr::filter(tramo_edad == 5 & (region_4 == 1 | region_4 == 2)) %>%
  srvyr::summarise(colname = srvyr::survey_mean(pens, na.rm=T))

b_sem <- mean(as.numeric(b_mes$colname))

c_ano_pu <- mean(c(a_sem, b_sem))
c_ano_pu <- as.data.frame(c_ano_pu)
c_ano_pu <- c_ano_pu %>% dplyr::mutate(aux = 1)
c_ano_pu <- c_ano_pu %>% dplyr::rename(VALOR = c_ano_pu)

# Quintil de ingresos

a_quintil <- function(x) {
  x <- sem1_svy %>%
    filter(quintilesy == x & tramo_edad == 5 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens, na.rm=T))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

b_quintil <- function(x) {
  x <- sem2_implant_svy %>%
    filter(quintilesy == x & tramo_edad == 5 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens, na.rm=T))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}     

c_quintil_pu <- as.data.frame(cbind(a_e_quintil, b_e_quintil))
c_quintil_pu <- c_quintil_pu %>% dplyr::mutate(VALOR = (a_e_quintil + b_e_quintil)/2)
c_quintil_pu <- c_quintil_pu %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil_pu <- c_quintil_pu[,c("VALOR", "aux")]


# Sexo

a_sexo <- function(x) {
  x <- sem1_svy %>%
    filter(bc_pe2 == x & tramo_edad == 5 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens, na.rm=T))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}     

b_sexo <- function(x) {
  x <- sem2_implant_svy %>%
    filter(bc_pe2 == x & tramo_edad == 5 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens, na.rm=T))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}     


c_sexo_pu <- as.data.frame(cbind(a_e_sexo, b_e_sexo))
c_sexo_pu <- c_sexo_pu %>% dplyr::mutate(VALOR = (a_e_sexo + b_e_sexo)/2)
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

a_mes <- sem1_svy %>%
  srvyr::filter(tramo_edad == 5) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(nijubpens, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

b_mes <- sem2_implant_svy %>%
  srvyr::filter(tramo_edad == 5) %>%
  srvyr::summarise(colname = srvyr::survey_mean(nijubpens, na.rm=T))

b_sem <- mean(as.numeric(b_mes$colname))

c_ano <- mean(c(a_sem, b_sem))
c_ano <- as.data.frame(c_ano)
c_ano <- c_ano %>% dplyr::mutate(aux = 1)
c_ano <- c_ano %>% dplyr::rename(VALOR = c_ano)

# Ascendencia étnico racial

a_afro <- function(x) {
  x <- sem1_svy %>%
    filter(bd_e29_1 == x & tramo_edad == 5) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nijubpens, na.rm=T))
  x <- mean(x$colname)
}       

a_e_afro <- numeric()

for(i in 1:2){
  a_e_afro[i] <- a_afro(x = i)
}     

b_afro <- function(x) {
  x <- sem2_implant_svy %>%
    filter(bd_e29_1 == x & tramo_edad == 5) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nijubpens, na.rm=T))
  x <- mean(x$colname)
}       

b_e_afro <- numeric()

for(i in 1:2){
  b_e_afro[i] <- b_afro(x = i)
}     

c_afro <- as.data.frame(cbind(a_e_afro, b_e_afro))
c_afro <- c_afro %>% dplyr::mutate(VALOR = (a_e_afro + b_e_afro)/2)
c_afro <- c_afro %>% dplyr::mutate(aux = c(2,3))
c_afro <- c_afro[,c("VALOR", "aux")]



# Quintil de ingresos

a_quintil <- function(x) {
  x <- sem1_svy %>%
    filter(quintilesy == x & tramo_edad == 5) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nijubpens, na.rm=T))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

b_quintil <- function(x) {
  x <- sem2_implant_svy %>%
    filter(quintilesy == x & tramo_edad == 5) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nijubpens, na.rm=T))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}     

c_quintil <- as.data.frame(cbind(a_e_quintil, b_e_quintil))
c_quintil <- c_quintil %>% dplyr::mutate(VALOR = (a_e_quintil + b_e_quintil)/2)
c_quintil <- c_quintil %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil <- c_quintil[,c("VALOR", "aux")]

# Sexo

a_sexo <- function(x) {
  x <- sem1_svy %>%
    filter(bc_pe2 == x & tramo_edad == 5) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nijubpens, na.rm=T))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}     

b_sexo <- function(x) {
  x <- sem2_implant_svy %>%
    filter(bc_pe2 == x & tramo_edad == 5) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nijubpens, na.rm=T))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}     

c_sexo <- as.data.frame(cbind(a_e_sexo, b_e_sexo))
c_sexo <- c_sexo %>% dplyr::mutate(VALOR = (a_e_sexo + b_e_sexo)/2)
c_sexo <- c_sexo %>% dplyr::mutate(aux = c(9, 10))
c_sexo <- c_sexo[,c("VALOR", "aux")]



# Región

a_region <- function(x) {
  x <- sem1_svy %>%
    filter(region_4 == x & tramo_edad == 5) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nijubpens, na.rm=T))
  x <- mean(x$colname)
}       

a_e_region <- numeric()

for(i in 3:4){
  a_e_region[i] <- a_region(x = i)
}     

b_region <- function(x) {
  x <- sem2_implant_svy %>%
    filter(region_4 == x & tramo_edad == 5) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nijubpens, na.rm=T))
  x <- mean(x$colname)
}       

b_e_region <- numeric()

for(i in 3:4){
  b_e_region[i] <- b_region(x = i)
}     


c_region <- as.data.frame(cbind(a_e_region, b_e_region))
c_region <- c_region %>% dplyr::mutate(VALOR = (a_e_region + b_e_region)/2)
c_region <- c_region[c(3,4),]
c_region <- c_region %>% dplyr::mutate(aux = c(16, 17))
c_region <- c_region[,c("VALOR", "aux")]


# Condición de pobreza del hogar

a_pobre <- function(x) {
  x <- sem1_svy %>%
    filter(pobre_06 == x & tramo_edad == 5) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nijubpens, na.rm=T))
  x <- mean(x$colname)
}       

a_e_pobre <- numeric()

for(i in 1:2){
  a_e_pobre[i] <- a_pobre(x = i)
}     

b_pobre <- function(x) {
  x <- sem2_implant_svy %>%
    filter(pobre_06 == x & tramo_edad == 5) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nijubpens, na.rm=T))
  x <- mean(x$colname)
}       

b_e_pobre <- numeric()

for(i in 1:2){
  b_e_pobre[i] <- b_pobre(x = i)
}     


c_pobre <- as.data.frame(cbind(a_e_pobre, b_e_pobre))
c_pobre <- c_pobre %>% dplyr::mutate(VALOR = (a_e_pobre + b_e_pobre)/2)
c_pobre <- c_pobre %>% dplyr::mutate(aux = c(18, 19))
c_pobre <- c_pobre[,c("VALOR", "aux")]

## País Urbano

# Total

a_mes <- sem1_svy %>%
  srvyr::filter(tramo_edad == 5 & (region_4 == 1 | region_4 == 2)) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(nijubpens, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

b_mes <- sem2_implant_svy %>%
  srvyr::filter(tramo_edad == 5 & (region_4 == 1 | region_4 == 2)) %>%
  srvyr::summarise(colname = srvyr::survey_mean(nijubpens, na.rm=T))

b_sem <- mean(as.numeric(b_mes$colname))

c_ano_pu <- mean(c(a_sem, b_sem))
c_ano_pu <- as.data.frame(c_ano_pu)
c_ano_pu <- c_ano_pu %>% dplyr::mutate(aux = 1)
c_ano_pu <- c_ano_pu %>% dplyr::rename(VALOR = c_ano_pu)

# Quintil de ingresos

a_quintil <- function(x) {
  x <- sem1_svy %>%
    filter(quintilesy == x & tramo_edad == 5 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nijubpens, na.rm=T))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

b_quintil <- function(x) {
  x <- sem2_implant_svy %>%
    filter(quintilesy == x & tramo_edad == 5 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nijubpens, na.rm=T))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}     

c_quintil_pu <- as.data.frame(cbind(a_e_quintil, b_e_quintil))
c_quintil_pu <- c_quintil_pu %>% dplyr::mutate(VALOR = (a_e_quintil + b_e_quintil)/2)
c_quintil_pu <- c_quintil_pu %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil_pu <- c_quintil_pu[,c("VALOR", "aux")]


# Sexo

a_sexo <- function(x) {
  x <- sem1_svy %>%
    filter(bc_pe2 == x & tramo_edad == 5 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nijubpens, na.rm=T))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}     

b_sexo <- function(x) {
  x <- sem2_implant_svy %>%
    filter(bc_pe2 == x & tramo_edad == 5 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nijubpens, na.rm=T))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}     


c_sexo_pu <- as.data.frame(cbind(a_e_sexo, b_e_sexo))
c_sexo_pu <- c_sexo_pu %>% dplyr::mutate(VALOR = (a_e_sexo + b_e_sexo)/2)
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

a_mes <- sem1_svy %>%
  srvyr::filter(jub == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(jub_insuf, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

b_mes <- sem2_implant_svy %>%
  srvyr::filter(jub == 1) %>%
  srvyr::summarise(colname = srvyr::survey_mean(jub_insuf, na.rm=T))

b_sem <- mean(as.numeric(b_mes$colname))

c_ano <- mean(c(a_sem, b_sem))
c_ano <- as.data.frame(c_ano)
c_ano <- c_ano %>% dplyr::mutate(aux = 1)
c_ano <- c_ano %>% dplyr::rename(VALOR = c_ano)

# Ascendencia étnico racial

a_afro <- function(x) {
  x <- sem1_svy %>%
    filter(bd_e29_1 == x & jub == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub_insuf, na.rm=T))
  x <- mean(x$colname)
}       

a_e_afro <- numeric()

for(i in 1:2){
  a_e_afro[i] <- a_afro(x = i)
}     

b_afro <- function(x) {
  x <- sem2_implant_svy %>%
    filter(bd_e29_1 == x & jub == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub_insuf, na.rm=T))
  x <- mean(x$colname)
}       

b_e_afro <- numeric()

for(i in 1:2){
  b_e_afro[i] <- b_afro(x = i)
}     

c_afro <- as.data.frame(cbind(a_e_afro, b_e_afro))
c_afro <- c_afro %>% dplyr::mutate(VALOR = (a_e_afro + b_e_afro)/2)
c_afro <- c_afro %>% dplyr::mutate(aux = c(2,3))
c_afro <- c_afro[,c("VALOR", "aux")]



# Quintil de ingresos

a_quintil <- function(x) {
  x <- sem1_svy %>%
    filter(quintilesy == x & jub == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub_insuf, na.rm=T))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

b_quintil <- function(x) {
  x <- sem2_implant_svy %>%
    filter(quintilesy == x & jub == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub_insuf, na.rm=T))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}     

c_quintil <- as.data.frame(cbind(a_e_quintil, b_e_quintil))
c_quintil <- c_quintil %>% dplyr::mutate(VALOR = (a_e_quintil + b_e_quintil)/2)
c_quintil <- c_quintil %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil <- c_quintil[,c("VALOR", "aux")]

# Sexo

a_sexo <- function(x) {
  x <- sem1_svy %>%
    filter(bc_pe2 == x & jub == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub_insuf, na.rm=T))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}     

b_sexo <- function(x) {
  x <- sem2_implant_svy %>%
    filter(bc_pe2 == x & jub == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub_insuf, na.rm=T))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}     

c_sexo <- as.data.frame(cbind(a_e_sexo, b_e_sexo))
c_sexo <- c_sexo %>% dplyr::mutate(VALOR = (a_e_sexo + b_e_sexo)/2)
c_sexo <- c_sexo %>% dplyr::mutate(aux = c(9, 10))
c_sexo <- c_sexo[,c("VALOR", "aux")]



# Región

a_region <- function(x) {
  x <- sem1_svy %>%
    filter(region_4 == x & jub == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub_insuf, na.rm=T))
  x <- mean(x$colname)
}       

a_e_region <- numeric()

for(i in 3:4){
  a_e_region[i] <- a_region(x = i)
}     

b_region <- function(x) {
  x <- sem2_implant_svy %>%
    filter(region_4 == x & jub == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub_insuf, na.rm=T))
  x <- mean(x$colname)
}       

b_e_region <- numeric()

for(i in 3:4){
  b_e_region[i] <- b_region(x = i)
}     


c_region <- as.data.frame(cbind(a_e_region, b_e_region))
c_region <- c_region %>% dplyr::mutate(VALOR = (a_e_region + b_e_region)/2)
c_region <- c_region[c(3,4),]
c_region <- c_region %>% dplyr::mutate(aux = c(16, 17))
c_region <- c_region[,c("VALOR", "aux")]


# Condición de pobreza del hogar

a_pobre <- function(x) {
  x <- sem1_svy %>%
    filter(pobre_06 == x & jub == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub_insuf, na.rm=T))
  x <- mean(x$colname)
}       

a_e_pobre <- numeric()

for(i in 1:2){
  a_e_pobre[i] <- a_pobre(x = i)
}     

b_pobre <- function(x) {
  x <- sem2_implant_svy %>%
    filter(pobre_06 == x & jub == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub_insuf, na.rm=T))
  x <- mean(x$colname)
}       

b_e_pobre <- numeric()

for(i in 1:2){
  b_e_pobre[i] <- b_pobre(x = i)
}     


c_pobre <- as.data.frame(cbind(a_e_pobre, b_e_pobre))
c_pobre <- c_pobre %>% dplyr::mutate(VALOR = (a_e_pobre + b_e_pobre)/2)
c_pobre <- c_pobre %>% dplyr::mutate(aux = c(18, 19))
c_pobre <- c_pobre[,c("VALOR", "aux")]

## País Urbano

# Total

a_mes <- sem1_svy %>%
  srvyr::filter(jub == 1 & (region_4 == 1 | region_4 == 2)) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(jub_insuf, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

b_mes <- sem2_implant_svy %>%
  srvyr::filter(jub == 1 & (region_4 == 1 | region_4 == 2)) %>%
  srvyr::summarise(colname = srvyr::survey_mean(jub_insuf, na.rm=T))

b_sem <- mean(as.numeric(b_mes$colname))

c_ano_pu <- mean(c(a_sem, b_sem))
c_ano_pu <- as.data.frame(c_ano_pu)
c_ano_pu <- c_ano_pu %>% dplyr::mutate(aux = 1)
c_ano_pu <- c_ano_pu %>% dplyr::rename(VALOR = c_ano_pu)

# Quintil de ingresos

a_quintil <- function(x) {
  x <- sem1_svy %>%
    filter(quintilesy == x & jub == 1 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub_insuf, na.rm=T))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

b_quintil <- function(x) {
  x <- sem2_implant_svy %>%
    filter(quintilesy == x & jub == 1 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub_insuf, na.rm=T))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}     

c_quintil_pu <- as.data.frame(cbind(a_e_quintil, b_e_quintil))
c_quintil_pu <- c_quintil_pu %>% dplyr::mutate(VALOR = (a_e_quintil + b_e_quintil)/2)
c_quintil_pu <- c_quintil_pu %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil_pu <- c_quintil_pu[,c("VALOR", "aux")]


# Sexo

a_sexo <- function(x) {
  x <- sem1_svy %>%
    filter(bc_pe2 == x & jub == 1 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub_insuf, na.rm=T))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}     

b_sexo <- function(x) {
  x <- sem2_implant_svy %>%
    filter(bc_pe2 == x & jub == 1 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub_insuf, na.rm=T))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}     


c_sexo_pu <- as.data.frame(cbind(a_e_sexo, b_e_sexo))
c_sexo_pu <- c_sexo_pu %>% dplyr::mutate(VALOR = (a_e_sexo + b_e_sexo)/2)
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

a_mes <- sem1_svy %>%
  srvyr::filter(pens == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(pens_insuf, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

b_mes <- sem2_implant_svy %>%
  srvyr::filter(pens == 1) %>%
  srvyr::summarise(colname = srvyr::survey_mean(pens_insuf, na.rm=T))

b_sem <- mean(as.numeric(b_mes$colname))

c_ano <- mean(c(a_sem, b_sem))
c_ano <- as.data.frame(c_ano)
c_ano <- c_ano %>% dplyr::mutate(aux = 1)
c_ano <- c_ano %>% dplyr::rename(VALOR = c_ano)

# Ascendencia étnico racial

a_afro <- function(x) {
  x <- sem1_svy %>%
    filter(bd_e29_1 == x & pens == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens_insuf, na.rm=T))
  x <- mean(x$colname)
}       

a_e_afro <- numeric()

for(i in 1:2){
  a_e_afro[i] <- a_afro(x = i)
}     

b_afro <- function(x) {
  x <- sem2_implant_svy %>%
    filter(bd_e29_1 == x & pens == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens_insuf, na.rm=T))
  x <- mean(x$colname)
}       

b_e_afro <- numeric()

for(i in 1:2){
  b_e_afro[i] <- b_afro(x = i)
}     

c_afro <- as.data.frame(cbind(a_e_afro, b_e_afro))
c_afro <- c_afro %>% dplyr::mutate(VALOR = (a_e_afro + b_e_afro)/2)
c_afro <- c_afro %>% dplyr::mutate(aux = c(2,3))
c_afro <- c_afro[,c("VALOR", "aux")]



# Quintil de ingresos

a_quintil <- function(x) {
  x <- sem1_svy %>%
    filter(quintilesy == x & pens == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens_insuf, na.rm=T))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

b_quintil <- function(x) {
  x <- sem2_implant_svy %>%
    filter(quintilesy == x & pens == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens_insuf, na.rm=T))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}     

c_quintil <- as.data.frame(cbind(a_e_quintil, b_e_quintil))
c_quintil <- c_quintil %>% dplyr::mutate(VALOR = (a_e_quintil + b_e_quintil)/2)
c_quintil <- c_quintil %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil <- c_quintil[,c("VALOR", "aux")]

# Sexo

a_sexo <- function(x) {
  x <- sem1_svy %>%
    filter(bc_pe2 == x & pens == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens_insuf, na.rm=T))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}     

b_sexo <- function(x) {
  x <- sem2_implant_svy %>%
    filter(bc_pe2 == x & pens == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens_insuf, na.rm=T))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}     

c_sexo <- as.data.frame(cbind(a_e_sexo, b_e_sexo))
c_sexo <- c_sexo %>% dplyr::mutate(VALOR = (a_e_sexo + b_e_sexo)/2)
c_sexo <- c_sexo %>% dplyr::mutate(aux = c(9, 10))
c_sexo <- c_sexo[,c("VALOR", "aux")]



# Región

a_region <- function(x) {
  x <- sem1_svy %>%
    filter(region_4 == x & pens == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens_insuf, na.rm=T))
  x <- mean(x$colname)
}       

a_e_region <- numeric()

for(i in 3:4){
  a_e_region[i] <- a_region(x = i)
}     

b_region <- function(x) {
  x <- sem2_implant_svy %>%
    filter(region_4 == x & pens == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens_insuf, na.rm=T))
  x <- mean(x$colname)
}       

b_e_region <- numeric()

for(i in 3:4){
  b_e_region[i] <- b_region(x = i)
}     


c_region <- as.data.frame(cbind(a_e_region, b_e_region))
c_region <- c_region %>% dplyr::mutate(VALOR = (a_e_region + b_e_region)/2)
c_region <- c_region[c(3,4),]
c_region <- c_region %>% dplyr::mutate(aux = c(16, 17))
c_region <- c_region[,c("VALOR", "aux")]


# Condición de pobreza del hogar

a_pobre <- function(x) {
  x <- sem1_svy %>%
    filter(pobre_06 == x & pens == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens_insuf, na.rm=T))
  x <- mean(x$colname)
}       

a_e_pobre <- numeric()

for(i in 1:2){
  a_e_pobre[i] <- a_pobre(x = i)
}     

b_pobre <- function(x) {
  x <- sem2_implant_svy %>%
    filter(pobre_06 == x & pens == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens_insuf, na.rm=T))
  x <- mean(x$colname)
}       

b_e_pobre <- numeric()

for(i in 1:2){
  b_e_pobre[i] <- b_pobre(x = i)
}     


c_pobre <- as.data.frame(cbind(a_e_pobre, b_e_pobre))
c_pobre <- c_pobre %>% dplyr::mutate(VALOR = (a_e_pobre + b_e_pobre)/2)
c_pobre <- c_pobre %>% dplyr::mutate(aux = c(18, 19))
c_pobre <- c_pobre[,c("VALOR", "aux")]

## País Urbano

# Total

a_mes <- sem1_svy %>%
  srvyr::filter(pens == 1 & (region_4 == 1 | region_4 == 2)) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(pens_insuf, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

b_mes <- sem2_implant_svy %>%
  srvyr::filter(pens == 1 & (region_4 == 1 | region_4 == 2)) %>%
  srvyr::summarise(colname = srvyr::survey_mean(pens_insuf, na.rm=T))

b_sem <- mean(as.numeric(b_mes$colname))

c_ano_pu <- mean(c(a_sem, b_sem))
c_ano_pu <- as.data.frame(c_ano_pu)
c_ano_pu <- c_ano_pu %>% dplyr::mutate(aux = 1)
c_ano_pu <- c_ano_pu %>% dplyr::rename(VALOR = c_ano_pu)

# Quintil de ingresos

a_quintil <- function(x) {
  x <- sem1_svy %>%
    filter(quintilesy == x & pens == 1 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens_insuf, na.rm=T))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

b_quintil <- function(x) {
  x <- sem2_implant_svy %>%
    filter(quintilesy == x & pens == 1 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens_insuf, na.rm=T))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}     

c_quintil_pu <- as.data.frame(cbind(a_e_quintil, b_e_quintil))
c_quintil_pu <- c_quintil_pu %>% dplyr::mutate(VALOR = (a_e_quintil + b_e_quintil)/2)
c_quintil_pu <- c_quintil_pu %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil_pu <- c_quintil_pu[,c("VALOR", "aux")]


# Sexo

a_sexo <- function(x) {
  x <- sem1_svy %>%
    filter(bc_pe2 == x & pens == 1 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens_insuf, na.rm=T))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}     

b_sexo <- function(x) {
  x <- sem2_implant_svy %>%
    filter(bc_pe2 == x & pens == 1 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens_insuf, na.rm=T))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}     


c_sexo_pu <- as.data.frame(cbind(a_e_sexo, b_e_sexo))
c_sexo_pu <- c_sexo_pu %>% dplyr::mutate(VALOR = (a_e_sexo + b_e_sexo)/2)
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

a_mes <- sem1_svy %>%
  srvyr::filter(jub == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(jub_insuf_smn, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

b_mes <- sem2_implant_svy %>%
  srvyr::filter(jub == 1) %>%
  srvyr::summarise(colname = srvyr::survey_mean(jub_insuf_smn, na.rm=T))

b_sem <- mean(as.numeric(b_mes$colname))

c_ano <- mean(c(a_sem, b_sem))
c_ano <- as.data.frame(c_ano)
c_ano <- c_ano %>% dplyr::mutate(aux = 1)
c_ano <- c_ano %>% dplyr::rename(VALOR = c_ano)

# Ascendencia étnico racial

a_afro <- function(x) {
  x <- sem1_svy %>%
    filter(bd_e29_1 == x & jub == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

a_e_afro <- numeric()

for(i in 1:2){
  a_e_afro[i] <- a_afro(x = i)
}     

b_afro <- function(x) {
  x <- sem2_implant_svy %>%
    filter(bd_e29_1 == x & jub == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

b_e_afro <- numeric()

for(i in 1:2){
  b_e_afro[i] <- b_afro(x = i)
}     

c_afro <- as.data.frame(cbind(a_e_afro, b_e_afro))
c_afro <- c_afro %>% dplyr::mutate(VALOR = (a_e_afro + b_e_afro)/2)
c_afro <- c_afro %>% dplyr::mutate(aux = c(2,3))
c_afro <- c_afro[,c("VALOR", "aux")]



# Quintil de ingresos

a_quintil <- function(x) {
  x <- sem1_svy %>%
    filter(quintilesy == x & jub == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

b_quintil <- function(x) {
  x <- sem2_implant_svy %>%
    filter(quintilesy == x & jub == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}     

c_quintil <- as.data.frame(cbind(a_e_quintil, b_e_quintil))
c_quintil <- c_quintil %>% dplyr::mutate(VALOR = (a_e_quintil + b_e_quintil)/2)
c_quintil <- c_quintil %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil <- c_quintil[,c("VALOR", "aux")]

# Sexo

a_sexo <- function(x) {
  x <- sem1_svy %>%
    filter(bc_pe2 == x & jub == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}     

b_sexo <- function(x) {
  x <- sem2_implant_svy %>%
    filter(bc_pe2 == x & jub == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}     

c_sexo <- as.data.frame(cbind(a_e_sexo, b_e_sexo))
c_sexo <- c_sexo %>% dplyr::mutate(VALOR = (a_e_sexo + b_e_sexo)/2)
c_sexo <- c_sexo %>% dplyr::mutate(aux = c(9, 10))
c_sexo <- c_sexo[,c("VALOR", "aux")]



# Región

a_region <- function(x) {
  x <- sem1_svy %>%
    filter(region_4 == x & jub == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

a_e_region <- numeric()

for(i in 3:4){
  a_e_region[i] <- a_region(x = i)
}     

b_region <- function(x) {
  x <- sem2_implant_svy %>%
    filter(region_4 == x & jub == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

b_e_region <- numeric()

for(i in 3:4){
  b_e_region[i] <- b_region(x = i)
}     


c_region <- as.data.frame(cbind(a_e_region, b_e_region))
c_region <- c_region %>% dplyr::mutate(VALOR = (a_e_region + b_e_region)/2)
c_region <- c_region[c(3,4),]
c_region <- c_region %>% dplyr::mutate(aux = c(16, 17))
c_region <- c_region[,c("VALOR", "aux")]


# Condición de pobreza del hogar

a_pobre <- function(x) {
  x <- sem1_svy %>%
    filter(pobre_06 == x & jub == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

a_e_pobre <- numeric()

for(i in 1:2){
  a_e_pobre[i] <- a_pobre(x = i)
}     

b_pobre <- function(x) {
  x <- sem2_implant_svy %>%
    filter(pobre_06 == x & jub == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

b_e_pobre <- numeric()

for(i in 1:2){
  b_e_pobre[i] <- b_pobre(x = i)
}     


c_pobre <- as.data.frame(cbind(a_e_pobre, b_e_pobre))
c_pobre <- c_pobre %>% dplyr::mutate(VALOR = (a_e_pobre + b_e_pobre)/2)
c_pobre <- c_pobre %>% dplyr::mutate(aux = c(18, 19))
c_pobre <- c_pobre[,c("VALOR", "aux")]

## País Urbano

# Total

a_mes <- sem1_svy %>%
  srvyr::filter(jub == 1 & (region_4 == 1 | region_4 == 2)) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(jub_insuf_smn, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

b_mes <- sem2_implant_svy %>%
  srvyr::filter(jub == 1 & (region_4 == 1 | region_4 == 2)) %>%
  srvyr::summarise(colname = srvyr::survey_mean(jub_insuf_smn, na.rm=T))

b_sem <- mean(as.numeric(b_mes$colname))

c_ano_pu <- mean(c(a_sem, b_sem))
c_ano_pu <- as.data.frame(c_ano_pu)
c_ano_pu <- c_ano_pu %>% dplyr::mutate(aux = 1)
c_ano_pu <- c_ano_pu %>% dplyr::rename(VALOR = c_ano_pu)

# Quintil de ingresos

a_quintil <- function(x) {
  x <- sem1_svy %>%
    filter(quintilesy == x & jub == 1 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

b_quintil <- function(x) {
  x <- sem2_implant_svy %>%
    filter(quintilesy == x & jub == 1 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}     

c_quintil_pu <- as.data.frame(cbind(a_e_quintil, b_e_quintil))
c_quintil_pu <- c_quintil_pu %>% dplyr::mutate(VALOR = (a_e_quintil + b_e_quintil)/2)
c_quintil_pu <- c_quintil_pu %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil_pu <- c_quintil_pu[,c("VALOR", "aux")]


# Sexo

a_sexo <- function(x) {
  x <- sem1_svy %>%
    filter(bc_pe2 == x & jub == 1 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}     

b_sexo <- function(x) {
  x <- sem2_implant_svy %>%
    filter(bc_pe2 == x & jub == 1 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::summarise(colname = srvyr::survey_mean(jub_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}     


c_sexo_pu <- as.data.frame(cbind(a_e_sexo, b_e_sexo))
c_sexo_pu <- c_sexo_pu %>% dplyr::mutate(VALOR = (a_e_sexo + b_e_sexo)/2)
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

a_mes <- sem1_svy %>%
  srvyr::filter(pens == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(pens_insuf_smn, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

b_mes <- sem2_implant_svy %>%
  srvyr::filter(pens == 1) %>%
  srvyr::summarise(colname = srvyr::survey_mean(pens_insuf_smn, na.rm=T))

b_sem <- mean(as.numeric(b_mes$colname))

c_ano <- mean(c(a_sem, b_sem))
c_ano <- as.data.frame(c_ano)
c_ano <- c_ano %>% dplyr::mutate(aux = 1)
c_ano <- c_ano %>% dplyr::rename(VALOR = c_ano)

# Ascendencia étnico racial

a_afro <- function(x) {
  x <- sem1_svy %>%
    filter(bd_e29_1 == x & pens == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

a_e_afro <- numeric()

for(i in 1:2){
  a_e_afro[i] <- a_afro(x = i)
}     

b_afro <- function(x) {
  x <- sem2_implant_svy %>%
    filter(bd_e29_1 == x & pens == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

b_e_afro <- numeric()

for(i in 1:2){
  b_e_afro[i] <- b_afro(x = i)
}     

c_afro <- as.data.frame(cbind(a_e_afro, b_e_afro))
c_afro <- c_afro %>% dplyr::mutate(VALOR = (a_e_afro + b_e_afro)/2)
c_afro <- c_afro %>% dplyr::mutate(aux = c(2,3))
c_afro <- c_afro[,c("VALOR", "aux")]



# Quintil de ingresos

a_quintil <- function(x) {
  x <- sem1_svy %>%
    filter(quintilesy == x & pens == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

b_quintil <- function(x) {
  x <- sem2_implant_svy %>%
    filter(quintilesy == x & pens == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}     

c_quintil <- as.data.frame(cbind(a_e_quintil, b_e_quintil))
c_quintil <- c_quintil %>% dplyr::mutate(VALOR = (a_e_quintil + b_e_quintil)/2)
c_quintil <- c_quintil %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil <- c_quintil[,c("VALOR", "aux")]

# Sexo

a_sexo <- function(x) {
  x <- sem1_svy %>%
    filter(bc_pe2 == x & pens == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}     

b_sexo <- function(x) {
  x <- sem2_implant_svy %>%
    filter(bc_pe2 == x & pens == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}     

c_sexo <- as.data.frame(cbind(a_e_sexo, b_e_sexo))
c_sexo <- c_sexo %>% dplyr::mutate(VALOR = (a_e_sexo + b_e_sexo)/2)
c_sexo <- c_sexo %>% dplyr::mutate(aux = c(9, 10))
c_sexo <- c_sexo[,c("VALOR", "aux")]



# Región

a_region <- function(x) {
  x <- sem1_svy %>%
    filter(region_4 == x & pens == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

a_e_region <- numeric()

for(i in 3:4){
  a_e_region[i] <- a_region(x = i)
}     

b_region <- function(x) {
  x <- sem2_implant_svy %>%
    filter(region_4 == x & pens == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

b_e_region <- numeric()

for(i in 3:4){
  b_e_region[i] <- b_region(x = i)
}     


c_region <- as.data.frame(cbind(a_e_region, b_e_region))
c_region <- c_region %>% dplyr::mutate(VALOR = (a_e_region + b_e_region)/2)
c_region <- c_region[c(3,4),]
c_region <- c_region %>% dplyr::mutate(aux = c(16, 17))
c_region <- c_region[,c("VALOR", "aux")]


# Condición de pobreza del hogar

a_pobre <- function(x) {
  x <- sem1_svy %>%
    filter(pobre_06 == x & pens == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

a_e_pobre <- numeric()

for(i in 1:2){
  a_e_pobre[i] <- a_pobre(x = i)
}     

b_pobre <- function(x) {
  x <- sem2_implant_svy %>%
    filter(pobre_06 == x & pens == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

b_e_pobre <- numeric()

for(i in 1:2){
  b_e_pobre[i] <- b_pobre(x = i)
}     


c_pobre <- as.data.frame(cbind(a_e_pobre, b_e_pobre))
c_pobre <- c_pobre %>% dplyr::mutate(VALOR = (a_e_pobre + b_e_pobre)/2)
c_pobre <- c_pobre %>% dplyr::mutate(aux = c(18, 19))
c_pobre <- c_pobre[,c("VALOR", "aux")]

## País Urbano

# Total

a_mes <- sem1_svy %>%
  srvyr::filter(pens == 1 & (region_4 == 1 | region_4 == 2)) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(pens_insuf_smn, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

b_mes <- sem2_implant_svy %>%
  srvyr::filter(pens == 1 & (region_4 == 1 | region_4 == 2)) %>%
  srvyr::summarise(colname = srvyr::survey_mean(pens_insuf_smn, na.rm=T))

b_sem <- mean(as.numeric(b_mes$colname))

c_ano_pu <- mean(c(a_sem, b_sem))
c_ano_pu <- as.data.frame(c_ano_pu)
c_ano_pu <- c_ano_pu %>% dplyr::mutate(aux = 1)
c_ano_pu <- c_ano_pu %>% dplyr::rename(VALOR = c_ano_pu)

# Quintil de ingresos

a_quintil <- function(x) {
  x <- sem1_svy %>%
    filter(quintilesy == x & pens == 1 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

b_quintil <- function(x) {
  x <- sem2_implant_svy %>%
    filter(quintilesy == x & pens == 1 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}     

c_quintil_pu <- as.data.frame(cbind(a_e_quintil, b_e_quintil))
c_quintil_pu <- c_quintil_pu %>% dplyr::mutate(VALOR = (a_e_quintil + b_e_quintil)/2)
c_quintil_pu <- c_quintil_pu %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil_pu <- c_quintil_pu[,c("VALOR", "aux")]


# Sexo

a_sexo <- function(x) {
  x <- sem1_svy %>%
    filter(bc_pe2 == x & pens == 1 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}     

b_sexo <- function(x) {
  x <- sem2_implant_svy %>%
    filter(bc_pe2 == x & pens == 1 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pens_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}     


c_sexo_pu <- as.data.frame(cbind(a_e_sexo, b_e_sexo))
c_sexo_pu <- c_sexo_pu %>% dplyr::mutate(VALOR = (a_e_sexo + b_e_sexo)/2)
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

a_mes <- sem1_svy %>%
  srvyr::filter(desocup == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(seguroparo, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

b_mes <- sem2_panel_svy %>%
  srvyr::filter(desocup == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(seguroparo, na.rm=T))

b_sem <- mean(as.numeric(b_mes$colname))

c_ano <- mean(c(a_sem, b_sem))
c_ano <- as.data.frame(c_ano)
c_ano <- c_ano %>% dplyr::mutate(aux = 1)
c_ano <- c_ano %>% dplyr::rename(VALOR = c_ano)

# Ascendencia étnico racial

a_afro <- function(x) {
  x <- sem1_svy %>%
    filter(bd_e29_1 == x & desocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(seguroparo, na.rm=T))
  x <- mean(x$colname)
}       

a_e_afro <- numeric()

for(i in 1:2){
  a_e_afro[i] <- a_afro(x = i)
}     

b_afro <- function(x) {
  x <- sem2_panel_svy %>%
    filter(bd_e29_1 == x & desocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(seguroparo, na.rm=T))
  x <- mean(x$colname)
}       

b_e_afro <- numeric()

for(i in 1:2){
  b_e_afro[i] <- b_afro(x = i)
}     

c_afro <- as.data.frame(cbind(a_e_afro, b_e_afro))
c_afro <- c_afro %>% dplyr::mutate(VALOR = (a_e_afro + b_e_afro)/2)
c_afro <- c_afro %>% dplyr::mutate(aux = c(2,3))
c_afro <- c_afro[,c("VALOR", "aux")]



# Quintil de ingresos

a_quintil <- function(x) {
  x <- sem1_svy %>%
    filter(quintilesy == x & desocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(seguroparo, na.rm=T))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

b_quintil <- function(x) {
  x <- sem2_panel_svy %>%
    filter(quintilesy == x & desocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(seguroparo, na.rm=T))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}     

c_quintil <- as.data.frame(cbind(a_e_quintil, b_e_quintil))
c_quintil <- c_quintil %>% dplyr::mutate(VALOR = (a_e_quintil + b_e_quintil)/2)
c_quintil <- c_quintil %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil <- c_quintil[,c("VALOR", "aux")]

# Sexo

a_sexo <- function(x) {
  x <- sem1_svy %>%
    filter(bc_pe2 == x & desocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(seguroparo, na.rm=T))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}     

b_sexo <- function(x) {
  x <- sem2_panel_svy %>%
    filter(bc_pe2 == x & desocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(seguroparo, na.rm=T))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}     

c_sexo <- as.data.frame(cbind(a_e_sexo, b_e_sexo))
c_sexo <- c_sexo %>% dplyr::mutate(VALOR = (a_e_sexo + b_e_sexo)/2)
c_sexo <- c_sexo %>% dplyr::mutate(aux = c(9, 10))
c_sexo <- c_sexo[,c("VALOR", "aux")]

# Tramo de edad

a_edad <- function(x) {
  x <- sem1_svy %>%
    filter(tramo_edad == x & desocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(seguroparo, na.rm=T))
  x <- mean(x$colname)
}       

a_e_edad <- numeric()

for(i in 1:5){
  a_e_edad[i] <- a_edad(x = i)
}     

b_edad <- function(x) {
  x <- sem2_panel_svy %>%
    filter(tramo_edad == x & desocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(seguroparo, na.rm=T))
  x <- mean(x$colname)
}       

b_e_edad <- numeric()

for(i in 1:5){
  b_e_edad[i] <- b_edad(x = i)
} 

c_edad <- as.data.frame(cbind(a_e_edad, b_e_edad))
c_edad <- c_edad %>% dplyr::mutate(VALOR = (a_e_edad + b_e_edad)/2)
c_edad <- c_edad %>% dplyr::mutate(aux = c(11, 12, 13, 14, 15))
c_edad <- c_edad[,c("VALOR", "aux")]



# Región

a_region <- function(x) {
  x <- sem1_svy %>%
    filter(region_4 == x & desocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(seguroparo, na.rm=T))
  x <- mean(x$colname)
}       

a_e_region <- numeric()

for(i in 3:4){
  a_e_region[i] <- a_region(x = i)
}     

b_region <- function(x) {
  x <- sem2_panel_svy %>%
    filter(region_4 == x & desocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(seguroparo, na.rm=T))
  x <- mean(x$colname)
}       

b_e_region <- numeric()

for(i in 3:4){
  b_e_region[i] <- b_region(x = i)
}     


c_region <- as.data.frame(cbind(a_e_region, b_e_region))
c_region <- c_region %>% dplyr::mutate(VALOR = (a_e_region + b_e_region)/2)
c_region <- c_region[c(3,4),]
c_region <- c_region %>% dplyr::mutate(aux = c(16, 17))
c_region <- c_region[,c("VALOR", "aux")]


# Condición de pobreza del hogar

a_pobre <- function(x) {
  x <- sem1_svy %>%
    filter(pobre_06 == x & desocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(seguroparo, na.rm=T))
  x <- mean(x$colname)
}       

a_e_pobre <- numeric()

for(i in 1:2){
  a_e_pobre[i] <- a_pobre(x = i)
}     

b_pobre <- function(x) {
  x <- sem2_panel_svy %>%
    filter(pobre_06 == x & desocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(seguroparo, na.rm=T))
  x <- mean(x$colname)
}       

b_e_pobre <- numeric()

for(i in 1:2){
  b_e_pobre[i] <- b_pobre(x = i)
}     


c_pobre <- as.data.frame(cbind(a_e_pobre, b_e_pobre))
c_pobre <- c_pobre %>% dplyr::mutate(VALOR = (a_e_pobre + b_e_pobre)/2)
c_pobre <- c_pobre %>% dplyr::mutate(aux = c(18, 19))
c_pobre <- c_pobre[,c("VALOR", "aux")]


## País Urbano

# Total

a_mes <- sem1_svy %>%
  srvyr::filter(desocup == 1 & (region_4 == 1 | region_4 == 2)) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(seguroparo, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

b_mes <- sem2_panel_svy %>%
  srvyr::filter(desocup == 1 & (region_4 == 1 | region_4 == 2)) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(seguroparo, na.rm=T))

b_sem <- mean(as.numeric(b_mes$colname))

c_ano_pu <- mean(c(a_sem, b_sem))
c_ano_pu <- as.data.frame(c_ano_pu)
c_ano_pu <- c_ano_pu %>% dplyr::mutate(aux = 1)
c_ano_pu <- c_ano_pu %>% dplyr::rename(VALOR = c_ano_pu)

# Quintil de ingresos

a_quintil <- function(x) {
  x <- sem1_svy %>%
    filter(quintilesy == x & desocup == 1 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(seguroparo, na.rm=T))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

b_quintil <- function(x) {
  x <- sem2_panel_svy %>%
    filter(quintilesy == x & desocup == 1 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(seguroparo, na.rm=T))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}     

c_quintil_pu <- as.data.frame(cbind(a_e_quintil, b_e_quintil))
c_quintil_pu <- c_quintil_pu %>% dplyr::mutate(VALOR = (a_e_quintil + b_e_quintil)/2)
c_quintil_pu <- c_quintil_pu %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil_pu <- c_quintil_pu[,c("VALOR", "aux")]


# Sexo

a_sexo <- function(x) {
  x <- sem1_svy %>%
    filter(bc_pe2 == x & desocup == 1 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(seguroparo, na.rm=T))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}     

b_sexo <- function(x) {
  x <- sem2_panel_svy %>%
    filter(bc_pe2 == x & desocup == 1 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(seguroparo, na.rm=T))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}     


c_sexo_pu <- as.data.frame(cbind(a_e_sexo, b_e_sexo))
c_sexo_pu <- c_sexo_pu %>% dplyr::mutate(VALOR = (a_e_sexo + b_e_sexo)/2)
c_sexo_pu <- c_sexo_pu %>% dplyr::mutate(aux = c(9, 10))
c_sexo_pu <- c_sexo_pu[,c("VALOR", "aux")]


# Tramo de edad

a_edad <- function(x) {
  x <- sem1_svy %>%
    filter(tramo_edad == x & desocup == 1 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(seguroparo, na.rm=T))
  x <- mean(x$colname)
}       

a_e_edad <- numeric()

for(i in 1:5){
  a_e_edad[i] <- a_edad(x = i)
}     

b_edad <- function(x) {
  x <- sem2_panel_svy %>%
    filter(tramo_edad == x & desocup == 1 & (region_4 == 1 | region_4 == 2)) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(seguroparo, na.rm=T))
  x <- mean(x$colname)
}       

b_e_edad <- numeric()

for(i in 1:5){
  b_e_edad[i] <- b_edad(x = i)
} 

c_edad_pu <- as.data.frame(cbind(a_e_edad, b_e_edad))
c_edad_pu <- c_edad_pu %>% dplyr::mutate(VALOR = (a_e_edad + b_e_edad)/2)
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

a_mes <- sem1_h_svy %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(h_afam_total, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))
a_sem <- as.data.frame(a_sem)

b_sem<- sem2_implant_h_svy %>%
  srvyr::summarise(colname = srvyr::survey_mean(h_afam_total, na.rm=T))

b_sem<- select(b_sem, colname)

c_ano <- cbind(a_sem, b_sem)
c_ano <- c_ano %>% dplyr::mutate(VALOR = (a_sem + colname) /2)
c_ano <- c_ano %>% dplyr::mutate(aux = 1)
c_ano<- select(c_ano, c(VALOR, aux))


# Quintil de ingresos

a_quintil <- function(x) {
  x <- sem1_h_svy %>%
    filter(quintilesy == x) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(h_afam_total, na.rm=T))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

b_quintil <- function(x) {
  x <- sem2_implant_h_svy %>%
    filter(quintilesy == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(h_afam_total, na.rm=T))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}     

c_quintil <- as.data.frame(cbind(a_e_quintil, b_e_quintil))
c_quintil <- c_quintil %>% dplyr::mutate(VALOR = (a_e_quintil + b_e_quintil)/2)
c_quintil <- c_quintil %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil <- c_quintil[,c("VALOR", "aux")]


# Región

a_region <- function(x) {
  x <- sem1_h_svy %>%
    filter(region_4 == x) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(h_afam_total, na.rm=T))
  x <- mean(x$colname)
}       

a_e_region <- numeric()

for(i in 3:4){
  a_e_region[i] <- a_region(x = i)
}     

b_region <- function(x) {
  x <- sem2_implant_h_svy %>%
    filter(region_4 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(h_afam_total, na.rm=T))
  x <- mean(x$colname)
}       

b_e_region <- numeric()

for(i in 3:4){
  b_e_region[i] <- b_region(x = i)
}     


c_region <- as.data.frame(cbind(a_e_region, b_e_region))
c_region <- c_region %>% dplyr::mutate(VALOR = (a_e_region + b_e_region)/2)
c_region <- c_region[c(3,4),]
c_region <- c_region %>% dplyr::mutate(aux = c(16, 17))
c_region <- c_region[,c("VALOR", "aux")]


# Sexo de jefe/a hogar

a_sexojefe <- function(x) {
  x <- sem1_h_svy %>%
    filter(sexojefe == x) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(h_afam_total, na.rm=T))
  x <- mean(x$colname)
}       

a_e_sexojefe <- numeric()

for(i in 1:2){
  a_e_sexojefe[i] <- a_sexojefe(x = i)
}     

b_sexojefe <- function(x) {
  x <- sem2_implant_h_svy %>%
    filter(sexojefe == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(h_afam_total, na.rm=T))
  x <- mean(x$colname)
}       

b_e_sexojefe <- numeric()

for(i in 1:2){
  b_e_sexojefe[i] <- b_sexojefe(x = i)
}     


c_sexojefe <- as.data.frame(cbind(a_e_sexojefe, b_e_sexojefe))
c_sexojefe <- c_sexojefe %>% dplyr::mutate(VALOR = (a_e_sexojefe + b_e_sexojefe)/2)
c_sexojefe <- c_sexojefe %>% dplyr::mutate(aux = c(20, 21))
c_sexojefe <- c_sexojefe[,c("VALOR", "aux")]

# Condición de pobreza del hogar

a_pobre <- function(x) {
  x <- sem1_h_svy %>%
    filter(pobre_06 == x) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(h_afam_total, na.rm=T))
  x <- mean(x$colname)
}       

a_e_pobre <- numeric()

for(i in 1:2){
  a_e_pobre[i] <- a_pobre(x = i)
}     

b_pobre <- function(x) {
  x <- sem2_implant_h_svy %>%
    filter(pobre_06 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(h_afam_total, na.rm=T))
  x <- mean(x$colname)
}       

b_e_pobre <- numeric()

for(i in 1:2){
  b_e_pobre[i] <- b_pobre(x = i)
}     


c_pobre <- as.data.frame(cbind(a_e_pobre, b_e_pobre))
c_pobre <- c_pobre %>% dplyr::mutate(VALOR = (a_e_pobre + b_e_pobre)/2)
c_pobre <- c_pobre %>% dplyr::mutate(aux = c(18, 19))
c_pobre <- c_pobre[,c("VALOR", "aux")]

# País Urbano

a_mes <- sem1_h_svy %>%
  filter(region_4 == 1 | region_4 == 2) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(h_afam_total, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))
a_sem <- as.data.frame(a_sem)

b_sem<- sem2_implant_h_svy %>%
  filter(region_4 == 1 | region_4 == 2) %>%
  srvyr::summarise(colname = srvyr::survey_mean(h_afam_total, na.rm=T))

b_sem<- select(b_sem, colname)

c_ano_pu <- cbind(a_sem, b_sem)
c_ano_pu <- c_ano_pu %>% dplyr::mutate(VALOR = (a_sem + colname) /2)
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

a_mes <- sem1_h_svy %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(h_afam_pe, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))
a_sem <- as.data.frame(a_sem)

b_sem<- sem2_implant_h_svy %>%
  srvyr::summarise(colname = srvyr::survey_mean(h_afam_pe, na.rm=T))

b_sem<- select(b_sem, colname)

c_ano <- cbind(a_sem, b_sem)
c_ano <- c_ano %>% dplyr::mutate(VALOR = (a_sem + colname) /2)
c_ano <- c_ano %>% dplyr::mutate(aux = 1)
c_ano<- select(c_ano, c(VALOR, aux))

# Quintil de ingresos

a_quintil <- function(x) {
  x <- sem1_h_svy %>%
    filter(quintilesy == x) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(h_afam_pe, na.rm=T))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

b_quintil <- function(x) {
  x <- sem2_implant_h_svy %>%
    filter(quintilesy == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(h_afam_pe, na.rm=T))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}     

c_quintil <- as.data.frame(cbind(a_e_quintil, b_e_quintil))
c_quintil <- c_quintil %>% dplyr::mutate(VALOR = (a_e_quintil + b_e_quintil)/2)
c_quintil <- c_quintil %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil <- c_quintil[,c("VALOR", "aux")]


# Región

a_region <- function(x) {
  x <- sem1_h_svy %>%
    filter(region_4 == x) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(h_afam_pe, na.rm=T))
  x <- mean(x$colname)
}       

a_e_region <- numeric()

for(i in 3:4){
  a_e_region[i] <- a_region(x = i)
}     

b_region <- function(x) {
  x <- sem2_implant_h_svy %>%
    filter(region_4 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(h_afam_pe, na.rm=T))
  x <- mean(x$colname)
}       

b_e_region <- numeric()

for(i in 3:4){
  b_e_region[i] <- b_region(x = i)
}     


c_region <- as.data.frame(cbind(a_e_region, b_e_region))
c_region <- c_region %>% dplyr::mutate(VALOR = (a_e_region + b_e_region)/2)
c_region <- c_region[c(3,4),]
c_region <- c_region %>% dplyr::mutate(aux = c(16, 17))
c_region <- c_region[,c("VALOR", "aux")]


# Sexo de jefe/a hogar

a_sexojefe <- function(x) {
  x <- sem1_h_svy %>%
    filter(sexojefe == x) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(h_afam_pe, na.rm=T))
  x <- mean(x$colname)
}       

a_e_sexojefe <- numeric()

for(i in 1:2){
  a_e_sexojefe[i] <- a_sexojefe(x = i)
}     

b_sexojefe <- function(x) {
  x <- sem2_implant_h_svy %>%
    filter(sexojefe == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(h_afam_pe, na.rm=T))
  x <- mean(x$colname)
}       

b_e_sexojefe <- numeric()

for(i in 1:2){
  b_e_sexojefe[i] <- b_sexojefe(x = i)
}     


c_sexojefe <- as.data.frame(cbind(a_e_sexojefe, b_e_sexojefe))
c_sexojefe <- c_sexojefe %>% dplyr::mutate(VALOR = (a_e_sexojefe + b_e_sexojefe)/2)
c_sexojefe <- c_sexojefe %>% dplyr::mutate(aux = c(20, 21))
c_sexojefe <- c_sexojefe[,c("VALOR", "aux")]

# Condición de pobreza del hogar

a_pobre <- function(x) {
  x <- sem1_h_svy %>%
    filter(pobre_06 == x) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(h_afam_pe, na.rm=T))
  x <- mean(x$colname)
}       

a_e_pobre <- numeric()

for(i in 1:2){
  a_e_pobre[i] <- a_pobre(x = i)
}     

b_pobre <- function(x) {
  x <- sem2_implant_h_svy %>%
    filter(pobre_06 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(h_afam_pe, na.rm=T))
  x <- mean(x$colname)
}       

b_e_pobre <- numeric()

for(i in 1:2){
  b_e_pobre[i] <- b_pobre(x = i)
}     


c_pobre <- as.data.frame(cbind(a_e_pobre, b_e_pobre))
c_pobre <- c_pobre %>% dplyr::mutate(VALOR = (a_e_pobre + b_e_pobre)/2)
c_pobre <- c_pobre %>% dplyr::mutate(aux = c(18, 19))
c_pobre <- c_pobre[,c("VALOR", "aux")]


# País Urbano

a_mes <- sem1_h_svy %>%
  filter(region_4 == 1 | region_4 == 2) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(h_afam_pe, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))
a_sem <- as.data.frame(a_sem)

b_sem<- sem2_implant_h_svy %>%
  filter(region_4 == 1 | region_4 == 2) %>%
  srvyr::summarise(colname = srvyr::survey_mean(h_afam_pe, na.rm=T))

b_sem<- select(b_sem, colname)

c_ano_pu <- cbind(a_sem, b_sem)
c_ano_pu <- c_ano_pu %>% dplyr::mutate(VALOR = (a_sem + colname) /2)
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

a_mes <- sem1_h_svy %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(h_tus, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))
a_sem <- as.data.frame(a_sem)

b_sem<- sem2_implant_h_svy %>%
  srvyr::summarise(colname = srvyr::survey_mean(h_tus, na.rm=T))

b_sem<- select(b_sem, colname)

c_ano <- cbind(a_sem, b_sem)
c_ano <- c_ano %>% dplyr::mutate(VALOR = (a_sem + colname) /2)
c_ano <- c_ano %>% dplyr::mutate(aux = 1)
c_ano<- select(c_ano, c(VALOR, aux))

# Quintil de ingresos

a_quintil <- function(x) {
  x <- sem1_h_svy %>%
    filter(quintilesy == x) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(h_tus, na.rm=T))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

b_quintil <- function(x) {
  x <- sem2_implant_h_svy %>%
    filter(quintilesy == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(h_tus, na.rm=T))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}     

c_quintil <- as.data.frame(cbind(a_e_quintil, b_e_quintil))
c_quintil <- c_quintil %>% dplyr::mutate(VALOR = (a_e_quintil + b_e_quintil)/2)
c_quintil <- c_quintil %>% dplyr::mutate(aux = c(4, 5, 6, 7, 8))
c_quintil <- c_quintil[,c("VALOR", "aux")]


# Región

a_region <- function(x) {
  x <- sem1_h_svy %>%
    filter(region_4 == x) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(h_tus, na.rm=T))
  x <- mean(x$colname)
}       

a_e_region <- numeric()

for(i in 3:4){
  a_e_region[i] <- a_region(x = i)
}     

b_region <- function(x) {
  x <- sem2_implant_h_svy %>%
    filter(region_4 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(h_tus, na.rm=T))
  x <- mean(x$colname)
}       

b_e_region <- numeric()

for(i in 3:4){
  b_e_region[i] <- b_region(x = i)
}     


c_region <- as.data.frame(cbind(a_e_region, b_e_region))
c_region <- c_region %>% dplyr::mutate(VALOR = (a_e_region + b_e_region)/2)
c_region <- c_region[c(3,4),]
c_region <- c_region %>% dplyr::mutate(aux = c(16, 17))
c_region <- c_region[,c("VALOR", "aux")]


# Sexo de jefe/a hogar

a_sexojefe <- function(x) {
  x <- sem1_h_svy %>%
    filter(sexojefe == x) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(h_tus, na.rm=T))
  x <- mean(x$colname)
}       

a_e_sexojefe <- numeric()

for(i in 1:2){
  a_e_sexojefe[i] <- a_sexojefe(x = i)
}     

b_sexojefe <- function(x) {
  x <- sem2_implant_h_svy %>%
    filter(sexojefe == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(h_tus, na.rm=T))
  x <- mean(x$colname)
}       

b_e_sexojefe <- numeric()

for(i in 1:2){
  b_e_sexojefe[i] <- b_sexojefe(x = i)
}     


c_sexojefe <- as.data.frame(cbind(a_e_sexojefe, b_e_sexojefe))
c_sexojefe <- c_sexojefe %>% dplyr::mutate(VALOR = (a_e_sexojefe + b_e_sexojefe)/2)
c_sexojefe <- c_sexojefe %>% dplyr::mutate(aux = c(20, 21))
c_sexojefe <- c_sexojefe[,c("VALOR", "aux")]

# Condición de pobreza del hogar

a_pobre <- function(x) {
  x <- sem1_h_svy %>%
    filter(pobre_06 == x) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(h_tus, na.rm=T))
  x <- mean(x$colname)
}       

a_e_pobre <- numeric()

for(i in 1:2){
  a_e_pobre[i] <- a_pobre(x = i)
}     

b_pobre <- function(x) {
  x <- sem2_implant_h_svy %>%
    filter(pobre_06 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(h_tus, na.rm=T))
  x <- mean(x$colname)
}       

b_e_pobre <- numeric()

for(i in 1:2){
  b_e_pobre[i] <- b_pobre(x = i)
}     


c_pobre <- as.data.frame(cbind(a_e_pobre, b_e_pobre))
c_pobre <- c_pobre %>% dplyr::mutate(VALOR = (a_e_pobre + b_e_pobre)/2)
c_pobre <- c_pobre %>% dplyr::mutate(aux = c(18, 19))
c_pobre <- c_pobre[,c("VALOR", "aux")]

# País urbano

a_mes <- sem1_h_svy %>%
  filter(region_4 == 1 | region_4 == 2) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(h_tus, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))
a_sem <- as.data.frame(a_sem)

b_sem<- sem2_implant_h_svy %>%
  filter(region_4 == 1 | region_4 == 2) %>%
  srvyr::summarise(colname = srvyr::survey_mean(h_tus, na.rm=T))

b_sem<- select(b_sem, colname)

c_ano_pu <- cbind(a_sem, b_sem)
c_ano_pu <- c_ano_pu %>% dplyr::mutate(VALOR = (a_sem + colname) /2)
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

base <- sem1_svy2 %>% filter(ocup == 1)

a_sem <- sapply(base$variables %>% select(categoria), function(x){
  base %>%
    filter(ocup == 1) %>%
    srvyr::summarise(VALOR_1 = srvyr::survey_mean(x, na.rm = TRUE)) 
})

a_sem <- data.frame(t(a_sem))

base <- sem2_panel_svy %>% filter(ocup == 1)

b_sem <- sapply(base$variables %>% select(categoria), function(x){
  base %>%
    srvyr::summarise(VALOR_2 = srvyr::survey_mean(x, na.rm = TRUE)) 
})

b_sem <- data.frame(t(b_sem))

c_ano <- cbind(a_sem, b_sem)
c_ano <- c_ano %>% dplyr::mutate(VALOR = (as.numeric(VALOR_1) + as.numeric(VALOR_2)) /2)
c_ano <- c_ano %>% dplyr::mutate(aux = 1)
c_ano <- c_ano %>% dplyr::mutate(CATEGORÍOCUP = Categoria)
c_ano <- c_ano[,c("VALOR", "aux", "CATEGORÍOCUP")]

# Varones

base <- sem1_svy2 %>% filter(ocup == 1 & bc_pe2 == 1)

a_sem <- sapply(base$variables %>% select(categoria), function(x){
  base %>%
    filter(ocup == 1) %>%
    srvyr::summarise(VALOR_1 = srvyr::survey_mean(x, na.rm = TRUE)) 
})

a_sem <- data.frame(t(a_sem))

base <- sem2_panel_svy %>% filter(ocup == 1 & bc_pe2 == 1)

b_sem <- sapply(base$variables %>% select(categoria), function(x){
  base %>%
    srvyr::summarise(VALOR_2 = srvyr::survey_mean(x, na.rm = TRUE)) 
})

b_sem <- data.frame(t(b_sem))

c_varones <- cbind(a_sem, b_sem)
c_varones <- c_varones %>% dplyr::mutate(VALOR = (as.numeric(VALOR_1) + as.numeric(VALOR_2)) /2)
c_varones <- c_varones %>% dplyr::mutate(aux = 9)
c_varones <- c_varones %>% dplyr::mutate(CATEGORÍOCUP = Categoria)
c_varones <- c_varones[,c("VALOR", "aux", "CATEGORÍOCUP")]

# Mujeres

base <- sem1_svy2 %>% filter(ocup == 1 & bc_pe2 == 2)

a_sem <- sapply(base$variables %>% select(categoria), function(x){
  base %>%
    filter(ocup == 1) %>%
    srvyr::summarise(VALOR_1 = srvyr::survey_mean(x, na.rm = TRUE)) 
})

a_sem <- data.frame(t(a_sem))

base <- sem2_panel_svy %>% filter(ocup == 1 & bc_pe2 == 2)

b_sem <- sapply(base$variables %>% select(categoria), function(x){
  base %>%
    srvyr::summarise(VALOR_2 = srvyr::survey_mean(x, na.rm = TRUE)) 
})

b_sem <- data.frame(t(b_sem))

c_mujeres <- cbind(a_sem, b_sem)
c_mujeres <- c_mujeres %>% dplyr::mutate(VALOR = (as.numeric(VALOR_1) + as.numeric(VALOR_2)) /2)
c_mujeres <- c_mujeres %>% dplyr::mutate(aux = 10)
c_mujeres <- c_mujeres %>% dplyr::mutate(CATEGORÍOCUP = Categoria)
c_mujeres <- c_mujeres[,c("VALOR", "aux", "CATEGORÍOCUP")]



CODIND       <- 317
NOMINDICADOR <- "Distribución de personas ocupadas por categoría de la ocupación"

VALOR        <- rbind(c_ano, c_varones, c_mujeres)
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

base <- sem1_svy2 %>% filter(ocup == 1)

a_sem <- sapply(base$variables %>% select(ocupacion), function(x){
  base %>%
    filter(ocup == 1) %>%
    srvyr::summarise(VALOR_1 = srvyr::survey_mean(x, na.rm = TRUE)) 
})

a_sem <- data.frame(t(a_sem))

base <- sem2_panel_svy %>% filter(ocup == 1)

b_sem <- sapply(base$variables %>% select(ocupacion), function(x){
  base %>%
    srvyr::summarise(VALOR_2 = srvyr::survey_mean(x, na.rm = TRUE)) 
})

b_sem <- data.frame(t(b_sem))

c_ano <- cbind(a_sem, b_sem)
c_ano <- c_ano %>% dplyr::mutate(VALOR = (as.numeric(VALOR_1) + as.numeric(VALOR_2)) /2)
c_ano <- c_ano %>% dplyr::mutate(aux = 1)
c_ano <- c_ano %>% dplyr::mutate(TIPO_DE_OCUPACION = Ocupacion)
c_ano <- c_ano[,c("VALOR", "aux", "TIPO_DE_OCUPACION")]

# Varones

base <- sem1_svy2 %>% filter(ocup == 1 & bc_pe2 == 1)

a_sem <- sapply(base$variables %>% select(ocupacion), function(x){
  base %>%
    filter(ocup == 1) %>%
    srvyr::summarise(VALOR_1 = srvyr::survey_mean(x, na.rm = TRUE)) 
})

a_sem <- data.frame(t(a_sem))

base <- sem2_panel_svy %>% filter(ocup == 1 & bc_pe2 == 1)

b_sem <- sapply(base$variables %>% select(ocupacion), function(x){
  base %>%
    srvyr::summarise(VALOR_2 = srvyr::survey_mean(x, na.rm = TRUE)) 
})

b_sem <- data.frame(t(b_sem))

c_varones <- cbind(a_sem, b_sem)
c_varones <- c_varones %>% dplyr::mutate(VALOR = (as.numeric(VALOR_1) + as.numeric(VALOR_2)) /2)
c_varones <- c_varones %>% dplyr::mutate(aux = 9)
c_varones <- c_varones %>% dplyr::mutate(TIPO_DE_OCUPACION = Ocupacion)
c_varones <- c_varones[,c("VALOR", "aux", "TIPO_DE_OCUPACION")]

# Mujeres

base <- sem1_svy2 %>% filter(ocup == 1 & bc_pe2 == 2)

a_sem <- sapply(base$variables %>% select(ocupacion), function(x){
  base %>%
    filter(ocup == 1) %>%
    srvyr::summarise(VALOR_1 = srvyr::survey_mean(x, na.rm = TRUE)) 
})

a_sem <- data.frame(t(a_sem))

base <- sem2_panel_svy %>% filter(ocup == 1 & bc_pe2 == 2)

b_sem <- sapply(base$variables %>% select(ocupacion), function(x){
  base %>%
    srvyr::summarise(VALOR_2 = srvyr::survey_mean(x, na.rm = TRUE)) 
})

b_sem <- data.frame(t(b_sem))

c_mujeres <- cbind(a_sem, b_sem)
c_mujeres <- c_mujeres %>% dplyr::mutate(VALOR = (as.numeric(VALOR_1) + as.numeric(VALOR_2)) /2)
c_mujeres <- c_mujeres %>% dplyr::mutate(aux = 10)
c_mujeres <- c_mujeres %>% dplyr::mutate(TIPO_DE_OCUPACION = Ocupacion)
c_mujeres <- c_mujeres[,c("VALOR", "aux", "TIPO_DE_OCUPACION")]



CODIND       <- 318
NOMINDICADOR <- "Distribución de personas ocupadas por tipo de ocupación (CIUO-08)"

VALOR        <- rbind(c_ano, c_varones, c_mujeres)
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

base <- sem1_svy2 %>% filter(ocup == 1)

a_sem <- sapply(base$variables %>% select(rama), function(x){
  base %>%
    filter(ocup == 1) %>%
    srvyr::summarise(VALOR_1 = srvyr::survey_mean(x, na.rm = TRUE)) 
})

a_sem <- data.frame(t(a_sem))

base <- sem2_panel_svy %>% filter(ocup == 1)

b_sem <- sapply(base$variables %>% select(rama), function(x){
  base %>%
    srvyr::summarise(VALOR_2 = srvyr::survey_mean(x, na.rm = TRUE)) 
})

b_sem <- data.frame(t(b_sem))

c_ano <- cbind(a_sem, b_sem)
c_ano <- c_ano %>% dplyr::mutate(VALOR = (as.numeric(VALOR_1) + as.numeric(VALOR_2)) /2)
c_ano <- c_ano %>% dplyr::mutate(aux = 1)
c_ano <- c_ano %>% dplyr::mutate(RAMA_DE_ACTIVIDAD = Rama)
c_ano <- c_ano[,c("VALOR", "aux", "RAMA_DE_ACTIVIDAD")]

# Varones

base <- sem1_svy2 %>% filter(ocup == 1 & bc_pe2 == 1)

a_sem <- sapply(base$variables %>% select(rama), function(x){
  base %>%
    filter(ocup == 1) %>%
    srvyr::summarise(VALOR_1 = srvyr::survey_mean(x, na.rm = TRUE)) 
})

a_sem <- data.frame(t(a_sem))

base <- sem2_panel_svy %>% filter(ocup == 1 & bc_pe2 == 1)

b_sem <- sapply(base$variables %>% select(rama), function(x){
  base %>%
    srvyr::summarise(VALOR_2 = srvyr::survey_mean(x, na.rm = TRUE)) 
})

b_sem <- data.frame(t(b_sem))

c_varones <- cbind(a_sem, b_sem)
c_varones <- c_varones %>% dplyr::mutate(VALOR = (as.numeric(VALOR_1) + as.numeric(VALOR_2)) /2)
c_varones <- c_varones %>% dplyr::mutate(aux = 9)
c_varones <- c_varones %>% dplyr::mutate(RAMA_DE_ACTIVIDAD = Rama)
c_varones <- c_varones[,c("VALOR", "aux", "RAMA_DE_ACTIVIDAD")]

# Mujeres

base <- sem1_svy2 %>% filter(ocup == 1 & bc_pe2 == 2)

a_sem <- sapply(base$variables %>% select(rama), function(x){
  base %>%
    filter(ocup == 1) %>%
    srvyr::summarise(VALOR_1 = srvyr::survey_mean(x, na.rm = TRUE)) 
})

a_sem <- data.frame(t(a_sem))

base <- sem2_panel_svy %>% filter(ocup == 1 & bc_pe2 == 2)

b_sem <- sapply(base$variables %>% select(rama), function(x){
  base %>%
    srvyr::summarise(VALOR_2 = srvyr::survey_mean(x, na.rm = TRUE)) 
})

b_sem <- data.frame(t(b_sem))

c_mujeres <- cbind(a_sem, b_sem)
c_mujeres <- c_mujeres %>% dplyr::mutate(VALOR = (as.numeric(VALOR_1) + as.numeric(VALOR_2)) /2)
c_mujeres <- c_mujeres %>% dplyr::mutate(aux = 10)
c_mujeres <- c_mujeres %>% dplyr::mutate(RAMA_DE_ACTIVIDAD = Rama)
c_mujeres <- c_mujeres[,c("VALOR", "aux", "RAMA_DE_ACTIVIDAD")]



CODIND       <- 319
NOMINDICADOR <- "Distribución de personas ocupadas por rama de actividad (CIIU Rev. 4)"

VALOR        <- rbind(c_ano, c_varones, c_mujeres)
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

base <- sem1_svy2 %>% filter(ocup == 1)

a_sem <- sapply(base$variables %>% select(restricciones), function(x){
  base %>%
    filter(ocup == 1) %>%
    srvyr::summarise(VALOR_1 = srvyr::survey_mean(x, na.rm = TRUE)) 
})

a_sem <- data.frame(t(a_sem))

base <- sem2_panel_svy %>% filter(ocup == 1)

b_sem <- sapply(base$variables %>% select(restricciones), function(x){
  base %>%
    srvyr::summarise(VALOR_2 = srvyr::survey_mean(x, na.rm = TRUE)) 
})

b_sem <- data.frame(t(b_sem))

c_ano <- cbind(a_sem, b_sem)
c_ano <- c_ano %>% dplyr::mutate(VALOR = (as.numeric(VALOR_1) + as.numeric(VALOR_2)) /2)
c_ano <- c_ano %>% dplyr::mutate(aux = 1)
c_ano <- c_ano %>% dplyr::mutate(RESTRICCIONES_AL_EMPLEO = Restricciones)
c_ano <- c_ano[,c("VALOR", "aux", "RESTRICCIONES_AL_EMPLEO")]

# Varones

base <- sem1_svy2 %>% filter(ocup == 1 & bc_pe2 == 1)

a_sem <- sapply(base$variables %>% select(restricciones), function(x){
  base %>%
    filter(ocup == 1) %>%
    srvyr::summarise(VALOR_1 = srvyr::survey_mean(x, na.rm = TRUE)) 
})

a_sem <- data.frame(t(a_sem))

base <- sem2_panel_svy %>% filter(ocup == 1 & bc_pe2 == 1)

b_sem <- sapply(base$variables %>% select(restricciones), function(x){
  base %>%
    srvyr::summarise(VALOR_2 = srvyr::survey_mean(x, na.rm = TRUE)) 
})

b_sem <- data.frame(t(b_sem))

c_varones <- cbind(a_sem, b_sem)
c_varones <- c_varones %>% dplyr::mutate(VALOR = (as.numeric(VALOR_1) + as.numeric(VALOR_2)) /2)
c_varones <- c_varones %>% dplyr::mutate(aux = 9)
c_varones <- c_varones %>% dplyr::mutate(RESTRICCIONES_AL_EMPLEO = Restricciones)
c_varones <- c_varones[,c("VALOR", "aux", "RESTRICCIONES_AL_EMPLEO")]

# Mujeres

base <- sem1_svy2 %>% filter(ocup == 1 & bc_pe2 == 2)

a_sem <- sapply(base$variables %>% select(restricciones), function(x){
  base %>%
    filter(ocup == 1) %>%
    srvyr::summarise(VALOR_1 = srvyr::survey_mean(x, na.rm = TRUE)) 
})

a_sem <- data.frame(t(a_sem))

base <- sem2_panel_svy %>% filter(ocup == 1 & bc_pe2 == 2)

b_sem <- sapply(base$variables %>% select(restricciones), function(x){
  base %>%
    srvyr::summarise(VALOR_2 = srvyr::survey_mean(x, na.rm = TRUE)) 
})

b_sem <- data.frame(t(b_sem))

c_mujeres <- cbind(a_sem, b_sem)
c_mujeres <- c_mujeres %>% dplyr::mutate(VALOR = (as.numeric(VALOR_1) + as.numeric(VALOR_2)) /2)
c_mujeres <- c_mujeres %>% dplyr::mutate(aux = 10)
c_mujeres <- c_mujeres %>% dplyr::mutate(RESTRICCIONES_AL_EMPLEO = Restricciones)
c_mujeres <- c_mujeres[,c("VALOR", "aux", "RESTRICCIONES_AL_EMPLEO")]



CODIND       <- 320
NOMINDICADOR <- "Distribución de personas ocupadas por restricciones al empleo"

VALOR        <- rbind(c_ano, c_varones, c_mujeres)
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

Empleo_2021 <- rbind(t_01, t_02, t_03, t_04, t_05, t_06, t_07, t_08, t_09, t_10, t_11, t_12, t_13, t_14, t_15, t_16, t_17, t_18, t_19, t_20, t_21, t_22, t_23, t_24, t_25)
rio::export(Empleo_2021, "Empleo_2021.xlsx")
Empleo_2021  <- rio::import("C:/Users/Usuario/Dropbox/1. Unidad de Métodos y Acceso a Datos/1. Observatorio UMAD/PISO II_MOTORES/2021/ECH 2021/Empleo_2021.xlsx")


## Piso I


piso_1  <- rio::import("C:/Users/Usuario/Dropbox/1. Unidad de Métodos y Acceso a Datos/1. Observatorio UMAD/PISO III_INDICADORES/GITHUB UMAD/Piso-I-Empleo/Data/Base_motor_empleo.xls")



Empleo_2021 <-  Empleo_2021  %>% mutate(AUX = case_when(NOMINDICADOR == "TASA DE ACTIVIDAD" & EDAD == "14 a 18" ~ "TASA DE ACTIVIDAD DE PERSONAS DE 14 A 18 AÑOS",
                                                        NOMINDICADOR == "TASA DE ACTIVIDAD" & EDAD == "19 a 24" ~ "TASA DE ACTIVIDAD DE PERSONAS DE 19 A 24 AÑOS",
                                                        NOMINDICADOR == "TASA DE ACTIVIDAD" & EDAD == "25 a 29" ~ "TASA DE ACTIVIDAD DE PERSONAS DE 25 A 29 AÑOS",
                                                        NOMINDICADOR == "TASA DE ACTIVIDAD" & EDAD == "30 a 64" ~ "TASA DE ACTIVIDAD DE PERSONAS DE 30 A 64 AÑOS",
                                                        NOMINDICADOR == "TASA DE ACTIVIDAD" & EDAD == "65 y más" ~ "TASA DE ACTIVIDAD DE PERSONAS DE 65 AÑOS Y MÁS",
                                                        NOMINDICADOR == "Distribución de personas ocupadas por restricciones al empleo" & RESTRICCIONES_AL_EMPLEO == "No registradas"  ~ "PORCENTAJE DE PERSONAS OCUPADAS NO REGISTRADAS",
                                                        NOMINDICADOR == "Distribución de personas ocupadas por restricciones al empleo" & RESTRICCIONES_AL_EMPLEO == "No registradas y subempleadas"  ~  "PORCENTAJE DE PERSONAS OCUPADAS NO REGISTRADAS Y SUBEMPLEADAS",
                                                        NOMINDICADOR == "Distribución de personas ocupadas por restricciones al empleo" & RESTRICCIONES_AL_EMPLEO == "Sin restricciones al empleo" ~ "PORCENTAJE DE PERSONAS OCUPADAS SIN RESTRICCIONES AL EMPLEO",
                                                        NOMINDICADOR == "Distribución de personas ocupadas por restricciones al empleo" & RESTRICCIONES_AL_EMPLEO == "Subempleadas" ~ "PORCENTAJE DE PERSONAS OCUPADAS SUBEMPLEADAS",
                                                        TRUE ~ NOMINDICADOR))
Empleo_2021 <- select(Empleo_2021, -NOMINDICADOR)
Empleo_2021 <- Empleo_2021  %>%  rename(NOMINDICADOR = AUX)

Empleo_2021 <-  Empleo_2021  %>% mutate(AUX = case_when(NOMINDICADOR == "PORCENTAJE DE OCUPADOS NO REGISTRADOS EN SEGURIDAD SOCIAL EN OCUPACIÓN PRINCIPAL" ~ round((1-as.numeric(VALOR)),1),
                                                        TRUE ~ VALOR))
Empleo_2021 <- select(Empleo_2021, -VALOR)
Empleo_2021 <- Empleo_2021  %>%  rename(VALOR = AUX)                                                        

Empleo_2021 <- subset(Empleo_2021, select=c(CODIND,	NOMINDICADOR,	SEXO,	ASCENDENCIA,	QUINTIL, DEPARTAMENTO, URBANORURALUY,	PAIS,	FECHA,	FECHA2,	VALOR,	RESPONSABLE,	JERARQUIA,	SECTORPRODUCTIVO,	SECTORINSTITUCIONAL,	ACTIVIDADESECONOMICAS,	DIVISIÓNECONOMICA,	CONCEPTO,	CATEGORÍOCUP,	RELACIONFAMILIAR,	EDAD, SITUACION_HOGAR, RAMA_DE_ACTIVIDAD, TIPO_DE_OCUPACION, RESTRICCIONES_AL_EMPLEO, SEXO_JEFE, NOTA_INDICADOR))
  
Empleo_2021 <-  Empleo_2021  %>% rename("QUINTIL DE INGRESO" = QUINTIL,
                                        "DEPARTAMENTO UY" = DEPARTAMENTO, 
                                        "PAÍS" = PAIS,
                                        "JERARQUÍA" = JERARQUIA, 
                                        "CATEGORÍA DE OCUPACIÓN RURAL" = CATEGORÍOCUP,
                                        "RELACIÓN FAMILIAR" = RELACIONFAMILIAR)                                                        

piso_1  <- piso_1  %>% dplyr::mutate(SITUACION_HOGAR = "", 
                                     RAMA_DE_ACTIVIDAD = "", 
                                     TIPO_DE_OCUPACION = "", 
                                     RESTRICCIONES_AL_EMPLEO = "", 
                                     SEXO_JEFE = "", 
                                     NOTA_INDICADOR = "")                                                       
                                                        
                                                       
                                                       


names <-names(piso_1)
names (Empleo_2021) = names



piso_1 <- piso_1 %>% mutate(pobr =  0,
                            edad = 0,
                            sjef = 0)
Empleo_2021 <- Empleo_2021 %>% mutate(pobr = ifelse(is.na(SITUACION_HOGAR), 0, 1),
                                      edad = ifelse(is.na(EDAD), 0, 1),
                                      sjef = ifelse(is.na(SEXO_JEFE), 0, 1))



piso_1 <- rbind(piso_1, Empleo_2021)

rio::export(piso_1, "C:/Users/Usuario/Dropbox/1. Unidad de Métodos y Acceso a Datos/1. Observatorio UMAD/PISO III_INDICADORES/GITHUB UMAD/Piso-I-Empleo/Data/Base_motor_empleo_2792022.xlsx")




## Piso II

baseweb   <- rio::import("Empleo-Salarios-Transferencias_31082022 (1).xlsx")
baseweb  <-  baseweb %>% filter(CODIND != 322 & CODIND != 336 & CODIND != 337 & CODIND != 315 & CODIND != 321) # Elimino los indicadores que se re-procesaron

# Revisión de indicadores previos a 2021

revision  <- rio::import("C:/Users/Usuario/Dropbox/1. Unidad de Métodos y Acceso a Datos/1. Observatorio UMAD/PISO III_INDICADORES/GITHUB UMAD/Piso-I-Empleo/Data/Base_motor_empleo.xls")
revision  <-  revision %>% filter(CODIND == 322 | CODIND == 336 | CODIND == 337)

revision  <-  revision %>% mutate(FECHA = case_when(FECHA == 6 ~ 2006,
                                                    FECHA == 7 ~ 2007,
                                                    FECHA == 8 ~ 2008,
                                                    FECHA == 9 ~ 2009,
                                                    FECHA == 10 ~ 2010,
                                                    FECHA == 11 ~ 2011,
                                                    FECHA == 12 ~ 2012,
                                                    FECHA == 13 ~ 2013,
                                                    FECHA == 14 ~ 2014,
                                                    FECHA == 15 ~ 2015,
                                                    FECHA == 16 ~ 2016,
                                                    FECHA == 17 ~ 2017,
                                                    FECHA == 18 ~ 2018,
                                                    FECHA == 19 ~ 2019,
                                                    FECHA == 20 ~ 2020,
                                                    TRUE ~ FECHA))

revision$FECHA_REC <- as.Date(paste0(revision$FECHA,"-01-01"),tryFormats = "%Y-%m-%d")

revision <- revision  %>% mutate(URBANORURALUY = case_when(URBANORURALUY == "TODOS" ~ "TOTAL PAÍS",
                                                           TRUE ~ URBANORURALUY))
revision <- revision  %>% mutate(REGIÓN_original = URBANORURALUY)
revision <- revision  %>% mutate(URB_TOT = case_when(URBANORURALUY == "URBANO (MÁS DE 5.000 HABITANTES)"  ~ "U",
                                                     TRUE ~ "T"))

revision <- revision  %>% mutate(EDAD = case_when(NOMINDICADOR == "PORCENTAJE DE OCUPADOS DE 14 A 18 AÑOS QUE PERCIBEN UN INGRESO POR DEBAJO DEL SMN" ~ "14 a 18",
                                                  NOMINDICADOR == "PORCENTAJE DE OCUPADOS DE 19 A 24 AÑOS QUE PERCIBEN UN INGRESO POR DEBAJO DEL SMN" ~ "19 a 24",
                                                  NOMINDICADOR == "PORCENTAJE DE OCUPADOS DE 25 A 29 AÑOS QUE PERCIBEN UN INGRESO POR DEBAJO DEL SMN" ~ "25 a 29",
                                                  NOMINDICADOR == "PORCENTAJE DE OCUPADOS DE 30 A 64 AÑOS QUE PERCIBEN UN INGRESO POR DEBAJO DEL SMN" ~ "30 a 64",  
                                                  NOMINDICADOR == "PORCENTAJE DE OCUPADOS DE 65 AÑOS Y MÁS QUE PERCIBEN UN INGRESO POR DEBAJO DEL SMN" ~ "65 y más", 
                                                  TRUE ~ ""))

revision <- revision  %>% mutate(SITUACIÓN_HOGAR = case_when(NOMINDICADOR == "PORCENTAJE DE OCUPADOS EN HOGARES EN SITUACIÓN DE POBREZA QUE PERCIBEN UN INGRESO POR DEBAJO DEL SMN" ~ "En situación de pobreza",
                                                             NOMINDICADOR == "PORCENTAJE DE OCUPADOS EN HOGARES NO EN SITUACIÓN DE POBREZA QUE PERCIBEN UN INGRESO POR DEBAJO DEL SMN" ~ "No en situación de pobreza",
                                                             NOMINDICADOR == "PORCENTAJE DE PERSONAS EN HOGARES EN SITUACIÓN DE POBREZA QUE PERCIBEN JUBILACIONES POR DEBAJO DEL SALARIO MÍNIMO NACIONAL" ~ "En situación de pobreza",
                                                             NOMINDICADOR == "PORCENTAJE DE PERSONAS EN HOGARES NO EN SITUACIÓN DE POBREZA QUE PERCIBEN JUBILACIONES POR DEBAJO DEL SALARIO MÍNIMO NACIONAL" ~ "No en situación de pobreza",
                                                             NOMINDICADOR == "PORCENTAJE DE PERSONAS EN HOGARES EN SITUACIÓN DE POBREZA QUE PERCIBEN PENSIONES POR DEBAJO DEL SALARIO MÍNIMO NACIONAL" ~ "En situación de pobreza",
                                                             NOMINDICADOR == "PORCENTAJE DE PERSONAS EN HOGARES NO EN SITUACIÓN DE POBREZA QUE PERCIBEN PENSIONES POR DEBAJO DEL SALARIO MÍNIMO NACIONAL" ~ "No en situación de pobreza",
                                                            TRUE ~ ""))

revision <- revision  %>% mutate(NOMINDICADOR = case_when(CODIND == 322 ~ "Porcentaje de ocupados que perciben un ingreso por debajo del SMN",
                                                          CODIND == 336 ~ "Porcentaje personas que perciben jubilaciones por debajo del salario mínimo nacional",
                                                          CODIND == 337 ~ "Porcentaje personas que perciben pensiones por debajo del salario mínimo nacional"))


revision <- revision  %>% mutate(CORTE = case_when(SEXO == "VARONES" | SEXO == "MUJERES" ~ "SEXO",
                                                   ASCENDENCIA == "AFRO" | ASCENDENCIA == "NO AFRO" ~ " ASCENDENCIA",
                                                   `QUINTIL DE INGRESO`  == "QUINTIL 1" | 
                                                   `QUINTIL DE INGRESO`  == "QUINTIL 2" |
                                                   `QUINTIL DE INGRESO`  == "QUINTIL 3" |
                                                   `QUINTIL DE INGRESO`  == "QUINTIL 4" |
                                                   `QUINTIL DE INGRESO`  == "QUINTIL 5" ~ "QUINTIL_DE_INGRESO",
                                                   EDAD != "" ~ "EDAD",
                                                   SITUACIÓN_HOGAR != "" ~ "SITUACIÓN_HOGAR", 
                                                   URBANORURALUY == "RURAL DISPERSO" |
                                                  URBANORURALUY == "URBANO (MENOS DE 5.000 HABITANTES)" ~ "REGIÓN",
                                                   TRUE ~ "TOTAL"))
                                                   
revision <- revision  %>% mutate(CORTE = case_when(CORTE == "TOTAL" & URBANORURALUY == "TOTAL PAÍS" ~ "REGIÓN",                                                   
                                                   TRUE ~ CORTE))

revision <- revision  %>% mutate(SEXO = case_when(SEXO == "TODOS" ~ "", 
                                                  TRUE ~ SEXO), 
                                 ASCENDENCIA = case_when(ASCENDENCIA == "TODOS" ~ "", 
                                                  TRUE ~ ASCENDENCIA), 
                                 `QUINTIL DE INGRESO` = case_when(`QUINTIL DE INGRESO` == "TODOS" ~ "", 
                                                  TRUE ~ `QUINTIL DE INGRESO`))

revision <- revision  %>% mutate(REGIÓN = case_when(URBANORURALUY == "RURAL DISPERSO"  ~ "RURAL DISPERSO",
                                                    URBANORURALUY == "URBANO (MENOS DE 5.000 HABITANTES)"  ~ "URBANO (MENOS DE 5.000 HABITANTES)",
                                                    CORTE == "REGIÓN" & URBANORURALUY == "TOTAL PAÍS" ~ "TOTAL PAÍS",
                                                    TRUE ~ ""))


revision <- revision  %>% mutate(JERARQUÍA = ifelse(SEXO == "" & ASCENDENCIA == "" & `QUINTIL DE INGRESO` == "" & EDAD == "" & SITUACIÓN_HOGAR == "" & 
                                                         (REGIÓN == "" | REGIÓN == "TOTAL PAÍS"), 1, 0),
                                 JERARQUIA_CAT = 1)
  
revision <- revision  %>% mutate(CORTE_2 = "",
                                 CORTE_3 = "",
                                 FRECUENCIA = "", 
                                 JEFATURA = "", 
                                 RESTRICCIONES_AL_EMPLEO = "",
                                 RAMA_DE_ACTIVIDAD = "",
                                 TIPO_DE_OCUPACIÓN = "",
                                 CATEGORÍA_DE_OCUPACIÓN = "",
                                 SECTOR_PRODUCTIVO = "",
                                 SECTOR_INSTITUCIONAL = "",
                                 ACTIVIDADES_ECONÓMICAS = "",
                                 DIVISIÓN_ECONÓMICA = "",
                                 PÚBLICO = "",
                                 CONCEPTO = "",
                                 DEPARTAMENTO_UY = "",
                                 RELACIÓN_FAMILIAR = "",
                                 `SECTOR PÚBLICO` = "")

duplicacion <- revision %>% filter(CORTE == "REGIÓN" & REGIÓN == "TOTAL PAÍS")
duplicacion <- duplicacion %>% mutate(CORTE =  "TOTAL", 
                                REGIÓN = "")

revision <- rbind(revision, duplicacion)


revision <- subset(revision, select=c(CODIND,	NOMINDICADOR, PAÍS, FECHA, FECHA_REC, VALOR, RESPONSABLE, JERARQUÍA, JERARQUIA_CAT, CORTE, CORTE_2, CORTE_3, FRECUENCIA,
                                      JEFATURA, RESTRICCIONES_AL_EMPLEO, RAMA_DE_ACTIVIDAD, TIPO_DE_OCUPACIÓN,  CATEGORÍA_DE_OCUPACIÓN, SITUACIÓN_HOGAR, 
                                      SEXO,	ASCENDENCIA,	`QUINTIL DE INGRESO`, DEPARTAMENTO_UY, URB_TOT, REGIÓN, REGIÓN_original, SECTOR_PRODUCTIVO, SECTOR_INSTITUCIONAL,
                                      ACTIVIDADES_ECONÓMICAS, DIVISIÓN_ECONÓMICA, `SECTOR PÚBLICO`, CONCEPTO, RELACIÓN_FAMILIAR, EDAD))
                                      
names <-names(baseweb)
names (revision) = names


baseweb <- rbind(baseweb, revision)

# Segunda revisión: hay dos indicadores que están bien en piso I pero mal en piso II

piso_1  <- rio::import("C:/Users/Usuario/Dropbox/1. Unidad de Métodos y Acceso a Datos/1. Observatorio UMAD/PISO III_INDICADORES/GITHUB UMAD/Piso-I-Empleo/Data/Base_motor_empleo.xls")
revision2  <-  piso_1 %>% filter(CODIND == 315 | CODIND == 321) #Indicadores a reemplazar en base web piso II

revision2$NOMINDICADOR <- casefold(revision2$NOMINDICADOR, upper = FALSE) 


aux <- as.data.frame(substr(revision2$NOMINDICADOR, start = 1, stop = 1))
names (aux) = "V1"
aux$V1 <- casefold(aux$V1, upper = TRUE) 

aux2 <- as.data.frame(substr(revision2$NOMINDICADOR, start = 2, stop = 100000000))
names (aux2) = "V2"

aux3 <- cbind(aux,aux2)
aux3$NOMINDICADOR_2 <- paste0(aux3$V1,aux3$V2)

aux3 <- select(aux3, NOMINDICADOR_2)
revision2 <- cbind(revision2,aux3)

revision2 <- select(revision2, -NOMINDICADOR)
revision2 <- rename(revision2, NOMINDICADOR = NOMINDICADOR_2)

revision2$FECHA_REC <- as.Date(paste0(revision2$FECHA,"-01-01"),tryFormats = "%Y-%m-%d")

revision2 <- revision2  %>% mutate(URBANORURALUY = case_when(URBANORURALUY == "TODOS" ~ "TOTAL PAÍS",
                                                           TRUE ~ URBANORURALUY))

revision2 <- revision2  %>% mutate(REGIÓN_original = URBANORURALUY)
revision2 <- revision2  %>% mutate(URB_TOT = case_when(URBANORURALUY == "URBANO (MÁS DE 5.000 HABITANTES)"  ~ "U",
                                                     TRUE ~ "T"))

revision2 <- revision2  %>% mutate(EDAD = case_when(NOMINDICADOR == "Ingreso por hora promedio en ocupación principal de personas de 14 a 18 años (def. base 2010)" |
                                                    NOMINDICADOR == "Porcentaje de ocupados de personas de 14 a 18 años no registrados en seguridad social en ocupación principal"  ~ "14 a 18",
                                                  NOMINDICADOR == "Ingreso por hora promedio en ocupación principal de personas de 19 a 24 años (def. base 2010)" |
                                                    NOMINDICADOR == "Porcentaje de ocupados de personas de 19 a 24 años no registrados en seguridad social en ocupación principal"  ~ "19 a 24",
                                                  NOMINDICADOR == "Ingreso por hora promedio en ocupación principal de personas de 25 a 29 años (def. base 2010)" |
                                                    NOMINDICADOR == "Porcentaje de ocupados de personas de 25 a 29 años no registrados en seguridad social en ocupación principal"  ~ "25 a 29",
                                                  NOMINDICADOR == "Ingreso por hora promedio en ocupación principal de personas de 30 a 64 años (def. base 2010)" |
                                                    NOMINDICADOR == "Porcentaje de ocupados de personas de 30 a 64 años no registrados en seguridad social en ocupación principal"  ~ "30 a 64",  
                                                  NOMINDICADOR == "Ingreso por hora promedio en ocupación principal de personas de 65 años y más (def. base 2010)" |
                                                    NOMINDICADOR == "Porcentaje de ocupados de personas de 65 años y más no registrados en seguridad social en ocupación principal" ~ "65 y más",
                                                  TRUE ~ ""))

revision2 <- revision2  %>% mutate(SITUACIÓN_HOGAR = case_when(NOMINDICADOR == "Ingreso por hora promedio en ocupación principal de personas en hogares en situación de pobreza (def. base 2010)" ~ "En situación de pobreza",
                                                             NOMINDICADOR == "Ingreso por hora promedio en ocupación principal de personas en hogares no en situación de pobreza (def. base 2010)" ~ "No en situación de pobreza",
                                                             NOMINDICADOR == "Porcentaje de ocupados no registrados en seguridad social en ocupación principal en hogares en situación de pobreza" ~ "En situación de pobreza",
                                                             NOMINDICADOR == "Porcentaje de ocupados no registrados en seguridad social en ocupación principal en hogares no en situación de pobreza" ~ "No en situación de pobreza",
                                                             TRUE ~ ""))

revision2 <- revision2  %>% mutate(NOMINDICADOR = case_when(CODIND == 315 ~ "Porcentaje de ocupados no registrados en seguridad social en ocupación principal",
                                                          CODIND == 321 ~ "Ingreso por hora promedio en ocupación principal (def. base 2010)"))


revision2 <- revision2  %>% mutate(CORTE = case_when(SEXO == "VARONES" | SEXO == "MUJERES" ~ "SEXO",
                                                   ASCENDENCIA == "AFRO" | ASCENDENCIA == "NO AFRO" ~ " ASCENDENCIA",
                                                   `QUINTIL DE INGRESO`  == "QUINTIL 1" | 
                                                     `QUINTIL DE INGRESO`  == "QUINTIL 2" |
                                                     `QUINTIL DE INGRESO`  == "QUINTIL 3" |
                                                     `QUINTIL DE INGRESO`  == "QUINTIL 4" |
                                                     `QUINTIL DE INGRESO`  == "QUINTIL 5" ~ "QUINTIL_DE_INGRESO",
                                                   EDAD != "" ~ "EDAD",
                                                   SITUACIÓN_HOGAR != "" ~ "SITUACIÓN_HOGAR", 
                                                   URBANORURALUY == "RURAL DISPERSO" |
                                                     URBANORURALUY == "URBANO (MENOS DE 5.000 HABITANTES)" ~ "REGIÓN",
                                                   TRUE ~ "TOTAL"))

revision2 <- revision2  %>% mutate(CORTE = case_when(CORTE == "TOTAL" & URBANORURALUY == "TOTAL PAÍS" ~ "REGIÓN",                                                   
                                                   TRUE ~ CORTE))

revision2 <- revision2  %>% mutate(SEXO = case_when(SEXO == "TODOS" ~ "", 
                                                  TRUE ~ SEXO), 
                                 ASCENDENCIA = case_when(ASCENDENCIA == "TODOS" ~ "", 
                                                         TRUE ~ ASCENDENCIA), 
                                 `QUINTIL DE INGRESO` = case_when(`QUINTIL DE INGRESO` == "TODOS" ~ "", 
                                                                  TRUE ~ `QUINTIL DE INGRESO`))

revision2 <- revision2  %>% mutate(REGIÓN = case_when(URBANORURALUY == "RURAL DISPERSO"  ~ "RURAL DISPERSO",
                                                    URBANORURALUY == "URBANO (MENOS DE 5.000 HABITANTES)"  ~ "URBANO (MENOS DE 5.000 HABITANTES)",
                                                    CORTE == "REGIÓN" & URBANORURALUY == "TOTAL PAÍS" ~ "TOTAL PAÍS",
                                                    TRUE ~ ""))




revision2 <- revision2  %>% mutate(CORTE_2 = "",
                                 CORTE_3 = "",
                                 FRECUENCIA = "", 
                                 JEFATURA = "", 
                                 RESTRICCIONES_AL_EMPLEO = "",
                                 RAMA_DE_ACTIVIDAD = "",
                                 TIPO_DE_OCUPACIÓN = "",
                                 CATEGORÍA_DE_OCUPACIÓN = "",
                                 SECTOR_PRODUCTIVO = "",
                                 SECTOR_INSTITUCIONAL = "",
                                 ACTIVIDADES_ECONÓMICAS = "",
                                 DIVISIÓN_ECONÓMICA = "",
                                 PÚBLICO = "",
                                 CONCEPTO = "",
                                 DEPARTAMENTO_UY = "",
                                 RELACIÓN_FAMILIAR = "",
                                 `SECTOR PÚBLICO` = "")

duplicacion <- revision2 %>% filter(CORTE == "REGIÓN" & REGIÓN == "TOTAL PAÍS")
duplicacion <- duplicacion %>% mutate(CORTE =  "TOTAL", 
                                      REGIÓN = "")

revision2 <- rbind(revision2, duplicacion)

revision2 <- revision2  %>% mutate(JERARQUÍA = ifelse(CORTE == "TOTAL", 1, 0),
                                   JERARQUIA_CAT = 1)

revision2 <- revision2  %>%  mutate(VALOR = case_when(CODIND == 315 ~  round((1-as.numeric(VALOR))*100,1),
                                                      CODIND == 321 ~ VALOR))
  


revision2 <- subset(revision2, select=c(CODIND,	NOMINDICADOR, PAÍS, FECHA, FECHA_REC, VALOR, RESPONSABLE, JERARQUÍA, JERARQUIA_CAT, CORTE, CORTE_2, CORTE_3, FRECUENCIA,
                                      JEFATURA, RESTRICCIONES_AL_EMPLEO, RAMA_DE_ACTIVIDAD, TIPO_DE_OCUPACIÓN,  CATEGORÍA_DE_OCUPACIÓN, SITUACIÓN_HOGAR, 
                                      SEXO,	ASCENDENCIA,	`QUINTIL DE INGRESO`, DEPARTAMENTO_UY, URB_TOT, REGIÓN, REGIÓN_original, SECTOR_PRODUCTIVO, SECTOR_INSTITUCIONAL,
                                      ACTIVIDADES_ECONÓMICAS, DIVISIÓN_ECONÓMICA, `SECTOR PÚBLICO`, CONCEPTO, RELACIÓN_FAMILIAR, EDAD))

names <-names(baseweb)
names (revision2) = names


baseweb <- rbind(baseweb, revision2)

# Incorporación de data 2021

Empleo_2021  <- rio::import("C:/Users/Usuario/Dropbox/1. Unidad de Métodos y Acceso a Datos/1. Observatorio UMAD/PISO II_MOTORES/2021/ECH 2021/Empleo_2021.xlsx")

Empleo_2021$NOMINDICADOR <- casefold(Empleo_2021$NOMINDICADOR, upper = FALSE) 


aux <- as.data.frame(substr(Empleo_2021$NOMINDICADOR, start = 1, stop = 1))
names (aux) = "V1"
aux$V1 <- casefold(aux$V1, upper = TRUE) 

aux2 <- as.data.frame(substr(Empleo_2021$NOMINDICADOR, start = 2, stop = 100000000))
names (aux2) = "V2"

aux3 <- cbind(aux,aux2)
aux3$NOMINDICADOR_2 <- paste0(aux3$V1,aux3$V2)

aux3 <- aux3   %>%  mutate(NOMINDICADOR_2 = case_when (NOMINDICADOR_2 == "Distribución de personas ocupadas por rama de actividad (ciiu rev. 4)" ~ "Distribución de personas ocupadas por rama de actividad (CIIU Rev. 4)",
                                                       NOMINDICADOR_2 == "Distribución de personas ocupadas por tipo de ocupación (ciuo-08)" ~ "Distribución de personas ocupadas por tipo de ocupación (CIUO-08)",
                                                       NOMINDICADOR_2 == "Porcentaje de ocupados que perciben un ingreso por debajo del smn" ~ "Porcentaje de ocupados que perciben un ingreso por debajo del SMN",
                           TRUE ~ NOMINDICADOR_2))

aux3 <- select(aux3, NOMINDICADOR_2)
Empleo_2021 <- cbind(Empleo_2021,aux3)

Empleo_2021 <- select(Empleo_2021, -NOMINDICADOR)
Empleo_2021 <- rename(Empleo_2021, NOMINDICADOR = NOMINDICADOR_2)


Empleo_2021$FECHA_REC <- as.Date(paste0(Empleo_2021$FECHA,"-01-01"),tryFormats = "%Y-%m-%d")


Empleo_2021 <- Empleo_2021  %>% mutate(URBANORURALUY = case_when(URBANORURALUY == "TODOS" ~ "TOTAL PAÍS",
                                                           TRUE ~ URBANORURALUY))


Empleo_2021 <- Empleo_2021  %>% mutate(REGIÓN_original = URBANORURALUY)
Empleo_2021 <- Empleo_2021  %>% mutate(URB_TOT = case_when(URBANORURALUY == "URBANO (MÁS DE 5.000 HABITANTES)"  ~ "U",
                                                     TRUE ~ "T"))

Empleo_2021 <- Empleo_2021  %>% mutate(CORTE = case_when(SEXO == "VARONES" | SEXO == "MUJERES" ~ "SEXO",
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

Empleo_2021 <- Empleo_2021  %>% mutate(CORTE = case_when(CORTE == "TOTAL" & URBANORURALUY == "TOTAL PAÍS" ~ "REGIÓN",                                                   
                                                   TRUE ~ CORTE))

Empleo_2021 <- Empleo_2021  %>% mutate(SEXO = case_when(SEXO == "TODOS" ~ "", 
                                                  TRUE ~ SEXO), 
                                 ASCENDENCIA = case_when(ASCENDENCIA == "TODOS" ~ "", 
                                                         TRUE ~ ASCENDENCIA), 
                                 QUINTIL = case_when(QUINTIL == "TODOS" ~ "", 
                                                                  TRUE ~ QUINTIL))

Empleo_2021 <- Empleo_2021  %>% mutate(REGIÓN = case_when(URBANORURALUY == "RURAL DISPERSO"  ~ "RURAL DISPERSO",
                                                    URBANORURALUY == "URBANO (MENOS DE 5.000 HABITANTES)"  ~ "URBANO (MENOS DE 5.000 HABITANTES)",
                                                    CORTE == "REGIÓN" & URBANORURALUY == "TOTAL PAÍS" ~ "TOTAL PAÍS",
                                                    TRUE ~ ""))


Empleo_2021 <- Empleo_2021  %>% mutate(JERARQUÍA = ifelse(SEXO == "" & ASCENDENCIA == "" & QUINTIL == "" & EDAD == "" & SITUACION_HOGAR == "" & 
                                                      (REGIÓN == "" | REGIÓN == "TOTAL PAÍS"), 1, 0),
                                 JERARQUIA_CAT = 1)

Empleo_2021 <- Empleo_2021  %>% mutate(CORTE_2 = "",
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

duplicacion <- Empleo_2021 %>% filter(CORTE == "REGIÓN" & REGIÓN == "TOTAL PAÍS")
duplicacion <- duplicacion %>% mutate(CORTE =  "TOTAL", 
                                      REGIÓN = "")

Empleo_2021 <- rbind(Empleo_2021, duplicacion)


Empleo_2021 <- subset(Empleo_2021, select=c(CODIND,	NOMINDICADOR, PAIS, FECHA, FECHA_REC, VALOR, RESPONSABLE, JERARQUÍA, JERARQUIA_CAT, CORTE, CORTE_2, CORTE_3, FRECUENCIA,
                                      SEXO_JEFE, RESTRICCIONES_AL_EMPLEO, RAMA_DE_ACTIVIDAD, TIPO_DE_OCUPACION,  CATEGORÍOCUP, SITUACION_HOGAR, 
                                      SEXO,	ASCENDENCIA,	QUINTIL, DEPARTAMENTO, URB_TOT, REGIÓN, REGIÓN_original, SECTOR_PRODUCTIVO, SECTOR_INSTITUCIONAL,
                                      ACTIVIDADES_ECONÓMICAS, DIVISIÓN_ECONÓMICA, `SECTOR PÚBLICO`, CONCEPTO, RELACIÓN_FAMILIAR, EDAD))

names <-names(baseweb)
names (Empleo_2021) = names


baseweb_2021 <- rbind(baseweb, Empleo_2021)

rio::export(revision, "Exportaciones/revision_2022.xlsx")
rio::export(revision2, "Exportaciones/revision_2022_2.xlsx")
rio::export(Empleo_2021, "Exportaciones/Empleo_2021_piso II.xlsx")
rio::export(t_22, "Exportaciones/t22.xlsx")
rio::export(t_23, "Exportaciones/t23.xlsx")


## Corrección de corte región para la web


baseweb_2022  <- rio::import("C:/Users/Usuario/Dropbox/1. Unidad de Métodos y Acceso a Datos/1. Observatorio UMAD/PISO II_MOTORES/2021/ECH 2021/Empleo-Salarios-Transferencias_05102022.xlsx")
urbano  <- baseweb_2022 %>% filter(CORTE == "TOTAL" & REGIÓN_original == "URBANO (MÁS DE 5.000 HABITANTES)")
urbano <- urbano  %>% mutate(CORTE = "REGIÓN",
                             REGIÓN = "URBANO (MÁS DE 5.000 HABITANTES)",
                             JERARQUÍA = 0,
                             URB_TOT = "T")

urbano <- subset(urbano, select=c(CODIND,	NOMINDICADOR, PAÍS, FECHA, FECHA_REC, VALOR, RESPONSABLE, JERARQUÍA, JERARQUIA_CAT, CORTE, CORTE_2, CORTE_3, FRECUENCIA,
                                            JEFATURA, RESTRICCIONES_AL_EMPLEO, RAMA_DE_ACTIVIDAD, TIPO_DE_OCUPACIÓN,  CATEGORÍA_DE_OCUPACIÓN, SITUACIÓN_HOGAR, 
                                            SEXO,	ASCENDENCIA,	QUINTIL_DE_INGRESO, DEPARTAMENTO_UY, URB_TOT, REGIÓN, REGIÓN_original, SECTOR_PRODUCTIVO, SECTOR_INSTITUCIONAL,
                                            ACTIVIDADES_ECONÓMICAS, DIVISIÓN_ECONÓMICA, `SECTOR PÚBLICO`, CONCEPTO, RELACIÓN_FAMILIAR, EDAD))
                           
rio::export(urbano, "Exportaciones/urbano.xlsx")

