# ***********************************************************************
#
# FUNDAR - IMPACTO DE PANDEMIA COVID 19 EN COBERTURA DE SALUD
#
# Fecha: Septiembre de 2021
#
# Autor: Alen Jimenez. E-mail: jimenezalen[at]gmail.com
#
# Objetivo: Analisis de la base.
#
# Bases utilizadas: - base
#
# ***********************************************************************

# INTRODUCCION: limpiamos la memoria y cargamos los paquetes

rm(list = ls())
library(pacman)
p_load(tidyverse, shiny, #basicos
       rio, readxl, openxlsx, haven, googledrive, #import y export
       GGally, grid, ggrepel, ggpubr, patchwork, colorspace, ggflags, ggthemes, treemapify, #graficos
       jtools, huxtable, stargazer, #formato tablas de regresion
       Hmisc, lubridate, collapse, imputeTS, pracma, expss, 
       DataCombine, fmsb, stringi, countrycode, extrafont, fastDummies,
       data.table)
#font_import() #importamos fuentes para obtener la letra Calibri
loadfonts(device = "win") #cargamos las fuentes
options(scipen = 999) #para que no use notacion cientifica 
Sys.setlocale("LC_TIME", "English") #para cambiar idioma default de fechas
'%notin%' <- Negate('%in%') #funcion complementaria de %in%

# ****************************************************************************************

# DIRECTORIO Y RUTAS

#seteamos directorio principal

setwd("D:/Desktop/fundar/eje4_encuestas")

# ****************************************************************************************

# FUNCIONES PARA GRAFICAR

source("codigo/funciones_para_graficos.R")

# ****************************************************************************************

# CARGAMOS LAS BASES

eph_ind_19_3 <- import("bases/eph/Usu_individual_T319.txt", dec = ",")

eph_ind_20_3 <- import("bases/eph/usu_individual_T320.txt", dec = ",")

eph <- plyr::rbind.fill(eph_ind_19_3, eph_ind_20_3)

# ****************************************************************************************

# EDICION DE BASES DE DATOS

# Creamos una variable que indique el numero de fila o individuo en la base

eph$id_str <- paste(eph$CODUSU, eph$NRO_HOGAR, eph$COMPONENTE, sep = "_")

eph$id <- eph %>% group_indices(eph$id_str)

eph <- arrange(eph, id)

# Creamos variable tiempo

eph$time <- paste("T", eph$TRIMESTRE, "_", eph$ANO4, sep = "")

# Transformamos ESTADO a factor

'CONDICIÓN DE ACTIVIDAD
0 = Entrevista individual no realizada (no respuesta al Cuestionario Individual)
1 = Ocupado
2 = Desocupado
3 = Inactivo
4 = Menor de 10 años'

eph$ESTADO <- as.factor(eph$ESTADO)

# Creamos variable binaria indicativa de ESTADO = ocupado

eph$ocupado <- 0
eph$ocupado[eph$ESTADO == 1] <- 1
eph$ocupado[is.na(eph$ESTADO) == TRUE] <- NA

# Creamos variable binaria indicativa de PEA (Poblacion economicamente activa)

'La PEA se define segun el INDEC como la suma de ocupados y desocupados.'

eph$pea <- 0
eph$pea[eph$ESTADO %in% c(1,2)] <- 1
eph$pea[is.na(eph$ESTADO) == TRUE] <- NA

# Transformamos PP04B_COD a factor

'A que se dedica o produce el negocio/empresa/institucion?
(Ver Clasificador de Actividades Economicas para Encuestas Sociodemograficas del Mercosur 
- CAES- MERCOSUR)'

eph$PP04B_COD <- as.factor(eph$PP04B_COD)

# Transformamos a factor conjunto de variables independientes

'vindsel <- c("MAS_500", "CH04", "REGION")

for(i in vindsel){
        eph[i] <- as.factor(eph[i])
        eph_ind_20_3[i] <- as.factor(eph_ind_20_3[i])
}
'
# Binaria Posee Cobertura de Salud (CH08)

'Tiene algun tipo de cobertura medica por la que paga o le descuentan?
1 = Obra social (incluye PAMI)
2 = Mutual / Prepaga / Servicio de emergencia
3 = Planes y seguros publicos
4 = No paga ni le descuentan
9 = Ns./Nr.
12 = Obra social y mutual/prepaga/servicio de emergencia
13 = Obra social y Planes y Seguros Publicos
23 = Mutual/prepaga/servicio de emergencia/ Planes y Seguros Publicos
123 = obra social, mutual/prepaga/servicio de emergencia y Planes y Seguros Publicos'

cubierto <- c(1, 2, 12, 13, 23, 123)

descubierto <- c(3, 4)

eph$cubierto <- NA
eph$cubierto[eph$CH08 %in% descubierto] <- 0
eph$cubierto[eph$CH08 %in% cubierto] <- 1

# Construimos categorias de edad

eph$edad_cat <- NA
eph$edad_cat[eph$CH06 < 18] <- 1
eph$edad_cat[eph$CH06 %in% 18:49] <- 2
eph$edad_cat[eph$CH06 > 49] <- 3

# Horas trabajadas en Ocupacion Principal y Secundarias Ultima semana

'docuementacion: "Los codigos 9, 99, 999, 9999 corresponden, salvo indicacion en contrario, 
a la categoria No sabe/No responde."'

eph$PP3E_TOT[eph$PP3E_TOT == 999] <- NA
eph$PP3F_TOT[eph$PP3F_TOT == 999] <- NA
eph$horas_trab <- eph$PP3E_TOT + eph$PP3F_TOT
eph$horas_trab[eph$horas_trab == 0] <- NA

eph$horas_trab_pea <- eph$horas_trab
eph$horas_trab_pea[eph$ESTADO == 2] <- 0

# Ingreso de ocupacion ppal (P21) como proporcion del ingreso total (P47T)

'documentacion: "los montos de ingreso, en cuyo caso la no respuesta se 
identifica con el código -9."'

eph$P21[eph$P21 == -9] <- NA
eph$P47T[eph$P47T == -9] <- NA
eph$ingppal_tot <- eph$P21 / eph$P47T

# Ingreso laboral por hora (T_VI: ingreso no laboral)

eph$T_VI[eph$T_VI == -9] <- NA
eph$inglab <- eph$P47T - eph$T_VI

#transformamos montos nominales a montos reales: base Agosto 2019

'En el cuestionario la pregunta sobre ingresos se hace sobre ingresos mensuales.
Como trabajamos los trimestres 3 (meses 7, 8 y 9), consideramos el mes de Agosto 
como el de referencia para el trimestre.
Segun el informe https://www.indec.gob.ar/uploads/informesdeprensa/ipc_09_20D39002C437.pdf,
la variacion Agosto 2020-Agosto 2019 fue del 40.7%'

eph$inglab[eph$time == "T3_2020"] <- eph$inglab[eph$time == "T3_2020"] / (1.407)

'al hacer el calculo, consideramos que las horas trabajas se miden en semanas y los ingresos
laborales se miden por mes'

eph$horas_trab_mes <- eph$horas_trab*4

eph$inglab_hora <- eph$inglab / (eph$horas_trab*4)

'svyratio(~inglab, ~horas_trab, design = subset(eph_svy, CH04 ==1), na.rm = TRUE)
svyratio(~inglab, ~horas_trab, design = subset(eph_svy, CH04 ==2), na.rm = TRUE)

svyratio(~inglab, ~horas_trab, design = subset(eph_ind_20_3_svy, CH04 ==1), na.rm = TRUE)
svyratio(~inglab, ~horas_trab, design = subset(eph_ind_20_3_svy, CH04 ==2), na.rm = TRUE)
'
# Cantidad de Ocupaciones

'cambiamos los 0 a 1 porque la distribucion no presenta 1s
segun documentacion, el codigo 0 identifica los casos a los cuales 
no les corresponde la secuencia analizada.'

eph$PP03D[eph$ESTADO != 1] <- NA
eph$PP03D[eph$ESTADO == 1 & eph$PP03D == 0] <- 1

# Clasificador de actividades MERCOSUR

eph$clf_act <- NA
#Resto
eph$clf_act[is.na(eph$PP04B_COD) == FALSE] <- 8
#Servicios de la administracion publica y prestación publica de servicios a la comunidad
eph$clf_act[eph$PP04B_COD == 8401] <- 1
#Comercio de alimentos, bebidas y tabaco; Servicios de expendio de comidas y bebidas, excepto por vendedores ambulantes
eph$clf_act[eph$PP04B_COD %in% c(4803,5601)] <- 2
#Ensenianza inicial y primaria, secundaria, terciaria, universitaria y de postgrado
eph$clf_act[eph$PP04B_COD == 8501] <- 3
#Actividades de los hogares como empleadores de personal domestico
eph$clf_act[eph$PP04B_COD == 9700] <- 4
#Actividades de atencion a la salud humana
eph$clf_act[eph$PP04B_COD == 8600] <- 5
#Construccion
eph$clf_act[eph$PP04B_COD == 4000] <- 6
#Comercio de mercaderías n.c.p. incluso mercaderias usadas
eph$clf_act[eph$PP04B_COD == 4807] <- 7

# Clasificador de actividades: relacionadas a covid y no relacionadas a covid

covid_norel <- c(8401, 8501, 8600)

eph$act_covid <- NA
eph$act_covid[eph$PP04B_COD %in% covid_norel] <- 0
eph$act_covid[eph$PP04B_COD %notin% covid_norel] <- 1

# Programas Sociales / Seguro de Desempleo

'V4_M: Monto del ingreso por seguro de desempleo
V5_M: Monto del ingreso por subsidio o ayuda social (en dinero) del gobierno, iglesias, etc.'

ayuda_no <- (eph$V4_M == 0)&(eph$V5_M == 0)

ayuda_si <- (eph$V4_M > 0)|(eph$V5_M > 0)

eph$ayuda_estatal <- NA

eph$ayuda_estatal[ayuda_no] <- 0

eph$ayuda_estatal[ayuda_si] <- 1

# ****************************************************************************************

# EMPLEO DE LOS PESOS REPLICADOS CON LA BASE DE DATOS PARA USUARIOS
#Tiene que hacerse una vez que se prepararon todas las variables a ser usadas para el analisis

library(survey)

eph_svy <- svydesign(data = eph,
                     id = ~1,
                     weights = ~PONDERA,
                     strata = ~time)

# ****************************************************************************************

#informes para verificar calculos

'https://www.indec.gob.ar/indec/web/Institucional-Indec-InformesTecnicos-58'

# ****************************************************************************************

# Funcion para construir tablas de promedios

'
Funcion que arma una tabla, donde las filas refieren a categorias de una variable 
independiente y las columnas refieren a una variable dependiente.

Para categoria de variable independiente hay dos filas: una con el promedio y otra con el 
desvio estandar de la variable dependiente para esa categoria de la independiente.

El numero de columnas esta determinado por el numero de categorias de la variable dependiente
y el numero de bases cuyos datos se quieren comparar. Por ejemplo, la variable dependiente
ESTADO cuenta con 5 categorias, si queremos comparar la base de III19 vs III20, entonces van a 
haber 1 + (5*2) columnas (una mas para las categorias de la variable independiente.)

Nota: la varind debe ser numerica, no factor, si no tira error.'

tabla <- function(base, #base diseniada
                   periodo1, #periodo 1 de comparacion
                   periodo2, #periodo 2 de comparacion
                   vardep, #variable dependiente, string
                   varind, #variable independiente, string
                   varind_etiq, #lista de etiquetas categorias de var independiente
                   tgroups_etiq) #lista de etiquetas para cada base (base0 y base1)
                  
        {
        
        # Transformamos a formato formula los strings de vardep y varind
        
        vardep_frm <- as.formula(paste("~", vardep, sep = ""))
        
        varind_frm <- as.formula(paste("~", varind, sep = ""))
        
        # Calculamos el promedio y error estandar para cada categoria de varind, 
        # para cada periodo
        
        means_1 <- svyby(vardep_frm, 
                         design = subset(base, time == periodo1), 
                         by = varind_frm, 
                         FUN = svymean, 
                         na.rm.by = TRUE,
                         na.rm.all = TRUE,
                         na.rm = TRUE)
        
        means_2 <- svyby(vardep_frm, 
                         design = subset(base, time == periodo2), 
                         by = varind_frm, 
                         FUN = svymean, 
                         na.rm.by = TRUE,
                         na.rm.all = TRUE,
                         na.rm = TRUE)
        
        # Categorias de variable independiente: codificacion y tamanios
        
        varind_cats <- sort(unique(base$variables[varind])[,1])
        
        varind_numcat <- length(varind_cats)
        
        # Armamos base auxiliar con diferencias entre periodos
        
        test_results <- data.frame(varind = varind_cats,
                                   diff = NA)
        
        # Armamos formula para hacer ejercicio de diferencia de medias
        
        test_frm <- as.formula(paste(vardep,"~","time", sep = ""))
        
        # Para cada categoria de varind, test de diferencias entre periodos
        
        for(i in 1:varind_numcat){
                
                #Tomamos solo las filas de la categoria de varind correspondiente
                
                logical_aux <- base$variables[varind] == varind_cats[i]
                
                #Realizamos el test
                test <- summary(svyglm(test_frm, design = subset(base, logical_aux)))
                
                #Extraemos diferencia, error estandar y p-value
                test_beta <- round(test$coefficients[[2]], 2)
                test_se <- test$coefficients[[4]]
                test_pvalue <- test$coefficients[[8]]
                
                #Armamos simbolo de significancia estadistica en base al p-value
                simbolo <- ""
                
                if(test_pvalue < 0.01){
                        simbolo <- "***"
                } else{
                        if(test_pvalue < 0.05){
                                simbolo <- "**"
                        } else{
                                if(test_pvalue < 0.10){
                                        simbolo <- "***"
                                } else{
                                        simbolo <- ""
                                }
                        }
                }
                
                #Completamos el data frame creado mas arriba con los resultados del test
                test_results[i, 2] <- paste(test_beta, simbolo, sep = "")
                
                test_results[i, 3] <- test_se
        }
        
        # Armamos base final juntando los datos del calculo de medias, errores estandar, 
        # diferencias y significancia estadistica
        
        base_final <- data.frame(varind = sort(rep(varind_cats, 2)),
                                 diff = NA)
        
        for(i in 1:varind_numcat){
                base_final[2*i-1, 2] <- means_1[i, 2]
                
                base_final[2*i, 2] <- means_1[i, 3]
                
                base_final[2*i-1, 3] <- means_2[i, 2]
                
                base_final[2*i, 3] <- means_2[i, 3]
                
                base_final[2*i-1, 4] <- test_results[i, 2]
                
                base_final[2*i, 4] <- test_results[i, 3]
        }
        
        # Nombre de columnas de la base final
        
        colnames(base_final) <- c(varind_etiq, tgroups_etiq, "Diferencia entre períodos")
        
        return(base_final)
}

# ****************************************************************************************


tabla_ratio <- function(base, #base diseniada
                        numerador, #numerador de ratio a estimar
                        denominador, #denominador de ratio a estimar
                        varind, #variable independiente, string
                        varind_etiq) #etiqueta de nombre de variable
        {
        # Transformamos a formato formula los strings de numerador, denominador y varind
        
        numer_frm <- as.formula(paste("~", numerador, sep = ""))
        
        denom_frm <- as.formula(paste("~", denominador, sep = ""))
        
        varind_frm <- as.formula(paste("~", varind, sep = ""))
        
        # Calculamos el promedio y error estandar para cada categoria de varind, 
        # para cada periodo
        
        logical_1 <- (is.na(base$variables[numerador][,1]) == FALSE)&
                (is.na(base$variables[denominador][,1]) == FALSE)&
                (is.na(base$variables[varind][,1]) == FALSE)
        
        base_final <- data.frame()
        
        # Categorias de variable independiente: codificacion y tamanios
        
        varind_cats <- sort(unique(base$variables[varind])[,1])
        
        varind_numcat <- length(varind_cats)
        
        for(i in 1:varind_numcat){
                
                logical_2 <- logical_1 & (base$variables[varind][,1] == varind_cats[i])
                
                ratios_1 <- svyby(numer_frm,
                                  by = ~time,
                                  denominator = denom_frm,
                                  design = subset(base, logical_2),
                                  svyratio,
                                  covmat = TRUE)
                
                diferencia <- svycontrast(ratios_1, c( -1, 1))
                
                ci_low_90 <- confint(diferencia, level = 0.90)[1]
                ci_upp_90 <- confint(diferencia, level = 0.90)[2]
                ci_low_95 <- confint(diferencia, level = 0.95)[1]
                ci_upp_95 <- confint(diferencia, level = 0.95)[2]
                ci_low_99 <- confint(diferencia, level = 0.99)[1]
                ci_upp_99 <- confint(diferencia, level = 0.99)[2]
                
                if((ci_low_99 < 0 & 0 < ci_upp_99) == FALSE){
                        mensaje <- "***"
                } else {
                        if((ci_low_95 < 0 & 0 < ci_upp_95) == FALSE){
                                mensaje <- "**"
                        } else {
                                if((ci_low_90 < 0 & 0 < ci_upp_90) == FALSE){
                                        mensaje <- "*"
                                } else {mensaje <- ""}
                        }
                }
                
                ratios_2 <- data.table::transpose(ratios_1, 
                                                  make.names = "time")
                ratios_2[,"Diferencia"] <- NA
                
                ratios_2[1,"Diferencia"] <- paste(round(as.numeric(diferencia),2),
                                                  mensaje,
                                                  sep = "")
                
                ratios_2[2,"Diferencia"] <- SE(diferencia)[1]
                
                ratios_2[, varind_etiq] <- varind_cats[i]
                
                ratios_2 <- ratios_2[,c(4,1,2,3)]
                
                base_final <- base_final %>% plyr::rbind.fill(ratios_2)
        }
        
        return(base_final)
}

# ****************************************************************************************

# ESTADO OCUPACIONAL: PROPORCION DE OCUPADOS

# Abrimos excel

wb <- createWorkbook()

# Desagregado por Sexo

base_aux <- tabla(base = eph_svy,
                  periodo1 = "T3_2019",
                  periodo2 = "T3_2020",
                  varind = "CH04",
                  vardep = "ocupado",
                  varind_etiq = "Sexo",
                  tgroups_etiq = c("T3-19", "T3-20"))


addWorksheet(wb, 
             sheetName = "sexo") #creamos pestania

writeData(wb,
          "sexo",
          base_aux) #asignamos data frame a pestania

# Desagregado por Edad

base_aux <- tabla(base = eph_svy,
                  periodo1 = "T3_2019",
                  periodo2 = "T3_2020", 
                  varind = "edad_cat",
                  vardep = "ocupado",
                  varind_etiq = "Categoría de Edad",
                  tgroups_etiq = c("T3-19", "T3-20"))

addWorksheet(wb, 
             sheetName = "edad") #creamos pestania

writeData(wb,
          "edad",
          base_aux) #asignamos data frame a pestania

# Desagregado por Region

base_aux <- tabla(base = eph_svy,
                  periodo1 = "T3_2019",
                  periodo2 = "T3_2020", 
                  varind = "REGION",
                  vardep = "ocupado",
                  varind_etiq = "Región Geográfica",
                  tgroups_etiq = c("T3-19", "T3-20"))

addWorksheet(wb, 
             sheetName = "region") #creamos pestania

writeData(wb,
          "region",
          base_aux) #asignamos data frame a pestania

#No hacemos por tipo de actividad economica porque todos los que tiene actividad economica
#son del ocupado == Ocupados

# Guardamos excel

saveWorkbook(wb, 
             "doc/output/descriptivos/tablas_eph_proporcion_ocupados.xlsx",
             overwrite = TRUE) #guardamos excel

# ****************************************************************************************

# INGRESO LABORAL DE OCUPACION PRINCIPAL COMO PROPORCION DEL TOTAL

# Abrimos excel

wb <- createWorkbook()

# Desagregado por Sexo

base_aux <- tabla_ratio(base = eph_svy,
                        numerador = "P21",
                        denominador = "P47T",
                        varind = "CH04",
                        varind_etiq = "Sexo")

addWorksheet(wb, 
             sheetName = "sexo") #creamos pestania

writeData(wb,
          "sexo",
          base_aux) #asignamos data frame a pestania

# Desagregado por Edad

base_aux <- tabla_ratio(base = eph_svy,
                        numerador = "P21",
                        denominador = "P47T",
                        varind = "edad_cat",
                        varind_etiq = "Categoría de Edad")

addWorksheet(wb, 
             sheetName = "edad") #creamos pestania

writeData(wb,
          "edad",
          base_aux) #asignamos data frame a pestania

# Desagregado por Region

base_aux <- tabla_ratio(base = eph_svy,
                        numerador = "P21",
                        denominador = "P47T",
                        varind = "REGION",
                        varind_etiq = "Región Geográfica")

addWorksheet(wb, 
             sheetName = "region") #creamos pestania

writeData(wb,
          "region",
          base_aux) #asignamos data frame a pestania

# Desagregado por Actividad Economica

base_aux <- tabla_ratio(base = eph_svy,
                        numerador = "P21",
                        denominador = "P47T",
                        varind = "clf_act",
                        varind_etiq = "Rama de Actividad")

addWorksheet(wb, 
             sheetName = "clasificacion_actividad") #creamos pestania

writeData(wb,
          "clasificacion_actividad",
          base_aux) #asignamos data frame a pestania

# Desagregado por Clasificacion COVID o no de Actividad

base_aux <- tabla_ratio(base = eph_svy,
                        numerador = "P21",
                        denominador = "P47T",
                        varind = "act_covid",
                        varind_etiq = "Rama de Actividad - COVID")

addWorksheet(wb, 
             sheetName = "clasificacion_actividad_covid") #creamos pestania

writeData(wb,
          "clasificacion_actividad_covid",
          base_aux) #asignamos data frame a pestania

# Guardamos excel

saveWorkbook(wb, 
             "doc/output/descriptivos/tablas_eph_proporcion_ingreso_laboral_ppal_sobre_total.xlsx",
             overwrite = TRUE) #guardamos excel

# ****************************************************************************************

# HORAS TRABAJADAS

# Abrimos excel

wb <- createWorkbook()

# Desagregado por Sexo

base_aux <- tabla(base = eph_svy,
                  periodo1 = "T3_2019",
                  periodo2 = "T3_2020", 
                  varind = "CH04",
                  vardep = "horas_trab",
                  varind_etiq = "Sexo",
                  tgroups_etiq = c("T3-19", "T3-20"))

addWorksheet(wb, 
             sheetName = "sexo") #creamos pestania

writeData(wb,
          "sexo",
          base_aux) #asignamos data frame a pestania

# Desagregado por Edad

base_aux <- tabla(base = eph_svy,
                  periodo1 = "T3_2019",
                  periodo2 = "T3_2020", 
                  varind = "edad_cat",
                  vardep = "horas_trab",
                  varind_etiq = "Categoría de Edad",
                  tgroups_etiq = c("T3-19", "T3-20"))

addWorksheet(wb, 
             sheetName = "edad") #creamos pestania

writeData(wb,
          "edad",
          base_aux) #asignamos data frame a pestania

# Desagregado por Region

base_aux <- tabla(base = eph_svy,
                  periodo1 = "T3_2019",
                  periodo2 = "T3_2020", 
                  varind = "REGION",
                  vardep = "horas_trab",
                  varind_etiq = "Región Geográfica",
                  tgroups_etiq = c("T3-19", "T3-20"))

addWorksheet(wb, 
             sheetName = "region") #creamos pestania

writeData(wb,
          "region",
          base_aux) #asignamos data frame a pestania

# Desagregado por Actividad Economica

base_aux <- tabla(base = eph_svy,
                  periodo1 = "T3_2019",
                  periodo2 = "T3_2020", 
                  varind = "clf_act",
                  vardep = "horas_trab",
                  varind_etiq = "Rama de Actividad",
                  tgroups_etiq = c("T3-19", "T3-20"))

addWorksheet(wb, 
             sheetName = "clasificacion_actividad") #creamos pestania

writeData(wb,
          "clasificacion_actividad",
          base_aux) #asignamos data frame a pestania

# Desagregado por Clasificacion COVID o no de Actividad

base_aux <- tabla(base = eph_svy,
                  periodo1 = "T3_2019",
                  periodo2 = "T3_2020", 
                  varind = "act_covid",
                  vardep = "horas_trab",
                  varind_etiq = "Rama de Actividad-COVID",
                  tgroups_etiq = c("T3-19", "T3-20"))

addWorksheet(wb, 
             sheetName = "clasificacion_actividad_covid") #creamos pestania

writeData(wb,
          "clasificacion_actividad_covid",
          base_aux) #asignamos data frame a pestania

# Guardamos excel

saveWorkbook(wb, 
             "doc/output/descriptivos/tablas_eph_horas_trabajadas.xlsx",
             overwrite = TRUE) #guardamos excel

# ****************************************************************************************

# HORAS TRABAJADAS PEA

# Abrimos excel

wb <- createWorkbook()

# Desagregado por Sexo

base_aux <- tabla(base = eph_svy,
                  periodo1 = "T3_2019",
                  periodo2 = "T3_2020", 
                  varind = "CH04",
                  vardep = "horas_trab_pea",
                  varind_etiq = "Sexo",
                  tgroups_etiq = c("T3-19", "T3-20"))

addWorksheet(wb, 
             sheetName = "sexo") #creamos pestania

writeData(wb,
          "sexo",
          base_aux) #asignamos data frame a pestania

# Desagregado por Edad

base_aux <- tabla(base = eph_svy,
                  periodo1 = "T3_2019",
                  periodo2 = "T3_2020", 
                  varind = "edad_cat",
                  vardep = "horas_trab_pea",
                  varind_etiq = "Categoría de Edad",
                  tgroups_etiq = c("T3-19", "T3-20"))

addWorksheet(wb, 
             sheetName = "edad") #creamos pestania

writeData(wb,
          "edad",
          base_aux) #asignamos data frame a pestania

# Desagregado por Region

base_aux <- tabla(base = eph_svy,
                  periodo1 = "T3_2019",
                  periodo2 = "T3_2020", 
                  varind = "REGION",
                  vardep = "horas_trab_pea",
                  varind_etiq = "Región Geográfica",
                  tgroups_etiq = c("T3-19", "T3-20"))

addWorksheet(wb, 
             sheetName = "region") #creamos pestania

writeData(wb,
          "region",
          base_aux) #asignamos data frame a pestania

# Desagregado por Actividad Economica

base_aux <- tabla(base = eph_svy,
                  periodo1 = "T3_2019",
                  periodo2 = "T3_2020", 
                  varind = "clf_act",
                  vardep = "horas_trab_pea",
                  varind_etiq = "Rama de Actividad",
                  tgroups_etiq = c("T3-19", "T3-20"))

addWorksheet(wb, 
             sheetName = "clasificacion_actividad") #creamos pestania

writeData(wb,
          "clasificacion_actividad",
          base_aux) #asignamos data frame a pestania

# Desagregado por Clasificacion COVID o no de Actividad

base_aux <- tabla(base = eph_svy,
                  periodo1 = "T3_2019",
                  periodo2 = "T3_2020", 
                  varind = "act_covid",
                  vardep = "horas_trab_pea",
                  varind_etiq = "Rama de Actividad-COVID",
                  tgroups_etiq = c("T3-19", "T3-20"))

addWorksheet(wb, 
             sheetName = "clasificacion_actividad_covid") #creamos pestania

writeData(wb,
          "clasificacion_actividad_covid",
          base_aux) #asignamos data frame a pestania

# Guardamos excel

saveWorkbook(wb, 
             "doc/output/descriptivos/tablas_eph_horas_trabajadas_pea.xlsx",
             overwrite = TRUE) #guardamos excel

# ****************************************************************************************

# INGRESO LABORAL POR HORA TRABAJADA

# Abrimos excel

wb <- createWorkbook()

# Desagregado por Sexo

base_aux <- tabla_ratio(base = eph_svy,
                  numerador = "inglab",
                  denominador = "horas_trab_mes",
                  varind = "CH04",
                  varind_etiq = "Sexo")

addWorksheet(wb, 
             sheetName = "sexo") #creamos pestania

writeData(wb,
          "sexo",
          base_aux) #asignamos data frame a pestania

# Desagregado por Edad

base_aux <- tabla_ratio(base = eph_svy,
                  numerador = "inglab",
                  denominador = "horas_trab_mes",
                  varind = "edad_cat",
                  varind_etiq = "Categoría de Edad")

addWorksheet(wb, 
             sheetName = "edad") #creamos pestania

writeData(wb,
          "edad",
          base_aux) #asignamos data frame a pestania

# Desagregado por Region

base_aux <- tabla_ratio(base = eph_svy,
                        numerador = "inglab",
                        denominador = "horas_trab_mes",
                        varind = "REGION",
                        varind_etiq = "Región Geográfica")

addWorksheet(wb, 
             sheetName = "region") #creamos pestania

writeData(wb,
          "region",
          base_aux) #asignamos data frame a pestania

# Desagregado por Actividad Economica

base_aux <- tabla_ratio(base = eph_svy,
                        numerador = "inglab",
                        denominador = "horas_trab_mes",
                        varind = "clf_act",
                        varind_etiq = "Rama de Actividad")

addWorksheet(wb, 
             sheetName = "clasificacion_actividad") #creamos pestania

writeData(wb,
          "clasificacion_actividad",
          base_aux) #asignamos data frame a pestania

# Desagregado por Clasificacion COVID o no de Actividad

base_aux <- tabla_ratio(base = eph_svy,
                        numerador = "inglab",
                        denominador = "horas_trab_mes",
                        varind = "act_covid",
                        varind_etiq = "Rama de Actividad - COVID")

addWorksheet(wb, 
             sheetName = "clasificacion_actividad_covid") #creamos pestania

writeData(wb,
          "clasificacion_actividad_covid",
          base_aux) #asignamos data frame a pestania

# Guardamos excel

saveWorkbook(wb, 
             "doc/output/descriptivos/tablas_eph_ingreso_laboral_horario.xlsx",
             overwrite = TRUE) #guardamos excel

# ****************************************************************************************

# POSESION DE ALGUNA COBERTURA DE SALUD

# Abrimos excel

wb <- createWorkbook()

# Desagregado por Sexo

base_aux <- tabla(base = eph_svy,
                  periodo1 = "T3_2019",
                  periodo2 = "T3_2020", 
                  varind = "CH04",
                  vardep = "cubierto",
                  varind_etiq = "Sexo",
                  tgroups_etiq = c("T3-19", "T3-20"))

addWorksheet(wb, 
             sheetName = "sexo") #creamos pestania

writeData(wb,
          "sexo",
          base_aux) #asignamos data frame a pestania

# Desagregado por Edad

base_aux <- tabla(base = eph_svy,
                  periodo1 = "T3_2019",
                  periodo2 = "T3_2020", 
                  varind = "edad_cat",
                  vardep = "cubierto",
                  varind_etiq = "Categoría de Edad",
                  tgroups_etiq = c("T3-19", "T3-20"))

addWorksheet(wb, 
             sheetName = "edad") #creamos pestania

writeData(wb,
          "edad",
          base_aux) #asignamos data frame a pestania

# Desagregado por Region

base_aux <- tabla(base = eph_svy,
                  periodo1 = "T3_2019",
                  periodo2 = "T3_2020", 
                  varind = "REGION",
                  vardep = "cubierto",
                  varind_etiq = "Región Geográfica",
                  tgroups_etiq = c("T3-19", "T3-20"))

addWorksheet(wb, 
             sheetName = "region") #creamos pestania

writeData(wb,
          "region",
          base_aux) #asignamos data frame a pestania

# Desagregado por Actividad Economica

base_aux <- tabla(base = eph_svy,
                  periodo1 = "T3_2019",
                  periodo2 = "T3_2020", 
                  varind = "clf_act",
                  vardep = "cubierto",
                  varind_etiq = "Rama de Actividad",
                  tgroups_etiq = c("T3-19", "T3-20"))

addWorksheet(wb, 
             sheetName = "clasificacion_actividad") #creamos pestania

writeData(wb,
          "clasificacion_actividad",
          base_aux) #asignamos data frame a pestania

# Desagregado por Clasificacion COVID o no de Actividad

base_aux <- tabla(base = eph_svy,
                  periodo1 = "T3_2019",
                  periodo2 = "T3_2020", 
                  varind = "act_covid",
                  vardep = "cubierto",
                  varind_etiq = "Rama de Actividad-COVID",
                  tgroups_etiq = c("T3-19", "T3-20"))

addWorksheet(wb, 
             sheetName = "clasificacion_actividad_covid") #creamos pestania

writeData(wb,
          "clasificacion_actividad_covid",
          base_aux) #asignamos data frame a pestania

# Guardamos excel

saveWorkbook(wb, 
             "doc/output/descriptivos/tablas_eph_cobertura_salud.xlsx",
             overwrite = TRUE) #guardamos excel

# ****************************************************************************************

# RECIBE SEGURO DE DESEMPLEO O PROGRAMA SOCIAL

# Abrimos excel

wb <- createWorkbook()

# Desagregado por Sexo

base_aux <- tabla(base = eph_svy,
                  periodo1 = "T3_2019",
                  periodo2 = "T3_2020", 
                  varind = "CH04",
                  vardep = "ayuda_estatal",
                  varind_etiq = "Sexo",
                  tgroups_etiq = c("T3-19", "T3-20"))

addWorksheet(wb, 
             sheetName = "sexo") #creamos pestania

writeData(wb,
          "sexo",
          base_aux) #asignamos data frame a pestania

# Desagregado por Edad

base_aux <- tabla(base = eph_svy,
                  periodo1 = "T3_2019",
                  periodo2 = "T3_2020", 
                  varind = "edad_cat",
                  vardep = "ayuda_estatal",
                  varind_etiq = "Categoría de Edad",
                  tgroups_etiq = c("T3-19", "T3-20"))

addWorksheet(wb, 
             sheetName = "edad") #creamos pestania

writeData(wb,
          "edad",
          base_aux) #asignamos data frame a pestania

# Desagregado por Region

base_aux <- tabla(base = eph_svy,
                  periodo1 = "T3_2019",
                  periodo2 = "T3_2020", 
                  varind = "REGION",
                  vardep = "ayuda_estatal",
                  varind_etiq = "Región Geográfica",
                  tgroups_etiq = c("T3-19", "T3-20"))

addWorksheet(wb, 
             sheetName = "region") #creamos pestania

writeData(wb,
          "region",
          base_aux) #asignamos data frame a pestania

# Desagregado por Actividad Economica

base_aux <- tabla(base = eph_svy,
                  periodo1 = "T3_2019",
                  periodo2 = "T3_2020", 
                  varind = "clf_act",
                  vardep = "ayuda_estatal",
                  varind_etiq = "Rama de Actividad",
                  tgroups_etiq = c("T3-19", "T3-20"))

addWorksheet(wb, 
             sheetName = "clasificacion_actividad") #creamos pestania

writeData(wb,
          "clasificacion_actividad",
          base_aux) #asignamos data frame a pestania

# Desagregado por Clasificacion COVID o no de Actividad

base_aux <- tabla(base = eph_svy,
                  periodo1 = "T3_2019",
                  periodo2 = "T3_2020", 
                  varind = "act_covid",
                  vardep = "ayuda_estatal",
                  varind_etiq = "Rama de Actividad-COVID",
                  tgroups_etiq = c("T3-19", "T3-20"))

addWorksheet(wb, 
             sheetName = "clasificacion_actividad_covid") #creamos pestania

writeData(wb,
          "clasificacion_actividad_covid",
          base_aux) #asignamos data frame a pestania

# Guardamos excel

saveWorkbook(wb, 
             "doc/output/descriptivos/tablas_eph_ayuda_estatal.xlsx",
             overwrite = TRUE) #guardamos excel

# ****************************************************************************************
# Categoria Ocupacional - Mercosur

svymean(~PP04B_COD, design = subset(eph_ind_19_3_svy, 
                                    is.na(PP04B_COD) == FALSE))

svymean(~PP04B_COD, design = subset(eph_ind_20_3_svy, 
                                    is.na(PP04B_COD) == FALSE))

"PP04D_COD" #Código de ocupación Clasificador Nacional de Ocupaciones - CNO - versión 2001

