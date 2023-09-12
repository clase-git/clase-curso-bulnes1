if (!require("dplyr")) install.packages("dplyr")
if (!require("data.table")) install.packages("data.table")
if (!require("purrr")) install.packages("purrr")
if (!require("downloader")) install.packages("downloader")
if (!require("microbenchmark")) install.packages("microbenchmark")
if (!require("readr")) install.packages("readr")
if (!require("tidyverse")) install.packages("tidyverse")

library(dplyr)
library(data.table)
library(purrr)
library(microbenchmark)
library(readr)
library(downloader)
library(stringr)

source("functions.R")



#EJERCICIO 1. Descargar archivos

#--Se define directorio 

directory <- "data_pf/"

urls <- c("https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi-2021---personas.csv?sfvrsn=d03ae552_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2020/esi-2020---personas.csv?sfvrsn=fb1f7e0c_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2019/esi-2019---personas.csv?sfvrsn=9eb52870_8&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2018/esi-2018---personas.csv?sfvrsn=a5de2b27_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2017/esi-2017---personas.csv?sfvrsn=d556c5a1_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2016/esi-2016---personas.csv?sfvrsn=81beb5a_6&download=true"
)



file_names <- map_chr(urls, extract_name)

walk2(urls, file_names, ~ download_esi_data(.x, .y, "data_pf/"))


#================================================================

#EJERCICIO 2. Cargar archivos


files <- list.files("data_pf/", full.names = T)

names <- str_extract_all(files, "[^/]+\\.csv")  %>% str_replace_all("---","_")

esi <- map(files, read_esi_data) %>% set_names(names)

version <- str_extract_all(names,"[a-zA-Z]{3}-(\\d{4})") %>%
  str_replace_all("-", "_") %>% 
  unlist()

esi <- map2(esi, version, ~mutate(.x, version = .y))

#================================================================

#EJERCICIO 3. Obtener datos

#3a. Tabla que contiene 3 columnas( version, n_personas, y n_hogares)

info_tabla <- map_dfr(esi, extraer_info)

info_tabla

#3b. Tabla con mín, max, media, mediana, p10 y p90 de variable ponderada

esi <- map(esi, ~ mutate(.x, id_identificacion_exp = ponderar(id_identificacion, fact_cal_esi)))

tabla_resumen <- map_df(esi, resumen_estadistico)

tabla_resumen

##Respuesta: Se observa un dato muy alto en la media de la versión esi_2020



#3c. Tabla para contabilizar el numero de estratos con una sola unidad primaria de muestreo (conglomerado)

num_estratos <- map_dfr(esi, contar_estratos)

num_estratos


#3d. Tabla con mín, max, media, mediana, p10 y p90 de ing_t_p

estadisticas_ingresos <- map_dfr(esi, calcular_estadisticas_ingresos)

estadisticas_ingresos

#===============================================================================

#EJERCICIO 4. Mejorando el código


#4a. Calcular prromedio con purrr

promedio_purrr <- esi %>%
  map_df(~ .x %>%
           group_by(version) %>%
           summarise(promedio_ing_t_p = mean(ing_t_p)))

print(promedio_purrr)

#4b. Apilar tablas y calcular promedios con Group_by y summarise

Group_and_Summarise <-  esi %>%
  map(~.x %>% select(version, ing_t_p)) %>%
  rbindlist() %>%
  group_by(version) %>%
  summarise( promedio = mean(ing_t_p))

Group_and_Summarise

#4c. Calcular promedio con purrr, usando función con data.table

promedio_purrr_data_table <- map_dfr(esi, calcular_promedio_data_table)

promedio_purrr_data_table


#4d. Apilar tablas y calcular promedio con data.table

datos_seleccionados <- map(esi, seleccionar_y_convertir_data_table)

# Apilar los dataframes seleccionados en un solo data.table
datos_apilados <- rbindlist(datos_seleccionados)

# Calcular el promedio de ing_t_p agrupando por version
promedio_data_table_apilado <- datos_apilados[, .(promedio_ing_t_p = mean(ing_t_p)), by = version]

promedio_data_table_apilado

#--  aplicar paquete microbenchmark para calcular el tiempo de cada  ejercicio

mbm_resultados <- microbenchmark(
  promedio_purrr,
  Group_and_Summarise,
  promedio_purrr_data_table,
  promedio_data_table_apilado,
  times = 5  
)

mbm_resultados

##Respuesta: Según los datos de microbenchmark, el método más eficiente es calcular el promedio solo con purrr,
##el método más lento es el calculo utilizando Group_by y summarise, los otros dos métodos tambien son rápidos pero
##no tanto como el primero, quizás se debe a que primero se deben apilar las tablas y luego realizar el cálculo.
