
## Paquetes necesarios ##

if (!require("dplyr")) install.packages("dplyr")
if (!require("stringr")) install.packages("stringr")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("purrr")) install.packages("purrr")
if (!require("data.table")) install.packages("data.table")
if (!require("microbenchmark")) install.packages("microbenchmark")
if (!require("survey")) install.packages("survey")

## librerias necesarias ##

library(dplyr)
library(stringr)
library(tidyverse)
library(purrr)
library(data.table)
library(microbenchmark)
library(survey)





if(!file.exists("data")) {
  dir.create("data")
}

### Cargando Funciones##

source("functions.R")
options(scipen = 999)

### Ejercicio 1 #####

urls <- c("https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi-2021---personas.csv?sfvrsn=d03ae552_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2020/esi-2020---personas.csv?sfvrsn=fb1f7e0c_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2019/esi-2019---personas.csv?sfvrsn=9eb52870_8&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2018/esi-2018---personas.csv?sfvrsn=a5de2b27_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2017/esi-2017---personas.csv?sfvrsn=d556c5a1_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2016/esi-2016---personas.csv?sfvrsn=81beb5a_6&download=true"
)


file_names <- map(urls, extract_names)

download_esi_data(url = urls, file_name = file_names, directory = "data")  


### Ejercicio 2 #####

ruta <- paste0("data/",file_names)

esi_data <- read_esi_data(ruta)

names(esi_data) <- str_extract(file_names, pattern = "\\d+") %>%
  str_replace_all(pattern = "^", replacement = "esi_")


### Ejercicio 3 #####

esi_data <- esi_data %>% map(~.x %>% mutate(version = paste0("esi_",ano_encuesta)))
esi_data$esi_2019[, fact_cal_esi := as.numeric(str_replace_all(fact_cal_esi, pattern = ",", replacement = "."))]
esi_data$esi_2019[, ing_t_p := as.numeric(str_replace_all(ing_t_p, pattern = ",", replacement = "."))]
esi_data$esi_2016[, ing_t_p := as.numeric(str_replace_all(ing_t_p, pattern = ",", replacement = "."))]


##Tabla que contenga 3 columnas: version, n_personas (idrph) y n_hogares (id_identificacion). En la columna version debes usar la siguiente estructura: esi_{año}

Tabla1 <- esi_data %>%
  map(~.x %>% select("version", "idrph", "id_identificacion")) %>%
  rbindlist() %>%
  group_by(version) %>%
  summarise(n_personas = n_distinct(idrph), n_hogares = n_distinct(id_identificacion))

Tabla1

##Tabla que contenga mínimo, máximo, media, mediana, p10 y p90 del factor de expansión (fact_cal_esi) a nivel de hogar (id_identificacion) para cada versión, debes incluir la columna versión. ¿Se observan algunos pesos de muestreo atípicos?

Tabla2 <- esi_data %>%
  map(~.x %>% select("version", "id_identificacion", "fact_cal_esi")) %>%
  rbindlist() %>%
  group_by(version) %>%
  summarise(n_hogares = n_distinct(id_identificacion),
            minimo = min(as.numeric(fact_cal_esi), na.rm = TRUE),
            maximo = max(as.numeric(fact_cal_esi), na.rm = TRUE),
            media = mean(as.numeric(fact_cal_esi), na.rm = TRUE),
            mediana = median(as.numeric(fact_cal_esi), na.rm = TRUE),
            p10 = quantile(as.numeric(fact_cal_esi), 0.1, na.rm = TRUE),
            p90 = quantile(as.numeric(fact_cal_esi), 0.9, na.rm = TRUE))

Tabla2

##Tabla que contenga el número de estratos (estrato) con una sola unidad primaria de muestro (conglomerado). Debes incluir la columna versión.

Tabla3 <- esi_data %>%
  map(~.x %>% select("version", "estrato", "conglomerado")) %>%
  rbindlist() %>%
  group_by(version, conglomerado) %>%
  count(estrato) %>%
  filter(n == 1) %>%
  ungroup() %>%
  group_by(version) %>%
  summarise(n_estratos = sum(n))

Tabla3


##Tabla que contenga mínimo, máximo, media, mediana, p10 y p90 de los ingresos del trabajo principal a nivel de personas (ing_t_p) para cada versión.

esi_data_unica <- esi_data %>%
  map(~.x %>% mutate(ing_exp = ing_t_p*fact_cal_esi, N = sum(fact_cal_esi)) %>%
        select("version", "idrph", "fact_cal_esi", "ing_t_p", "ing_exp", "N")) %>%
  rbindlist()

Tabla4 <- esi_data_unica %>%
  group_by(version) %>%
  summarise(n_personas = n_distinct(idrph),
            suma_fact_exp = sum(ing_exp),
            N,
            minimo = min(ing_t_p, na.rm = TRUE),
            maximo = max(ing_t_p, na.rm = TRUE),
            media = sum(ing_exp)/N,
            mediana = median(ing_exp, na.rm = TRUE),
            p10 = quantile(as.numeric(ing_t_p), 0.1, na.rm = TRUE),
            p90 = quantile(as.numeric(ing_t_p), 0.9, na.rm = TRUE)) %>%
  distinct()

Tabla4


### Ejercicio 4 #####

##Lista de tablas: calcular promedio con herramientas de purrr (como en el ejercicio anterior)

results <- microbenchmark(
  Only_purr = esi_data %>% map(~.x %>% summarise(only_purr = mean(ing_t_p))),

##Tablas apiladas: calcular promedio con group_by() %>% summarise() (apila una tabla sobre otra en un dataframe)
    
  Group_and_Summarise = esi_data %>%
    map(~.x %>% select("ing_t_p", "version")) %>%
    rbindlist() %>%
    group_by(version) %>%
    summarise( promedio = mean(ing_t_p)),
 
##Lista de tablas: calcular promedio con herramientas de purrr, utilizando una función creada por ti, que utilice data.table

  Mi_promedio = esi_data %>% 
    map(~.x %>% summarise( mi_promedio = mi_promedio(.x,ing_t_p))), 

##Tablas apiladas: calcular promedio con data.table

  data_mean = esi_data %>%
    map(~.x %>% select("ing_t_p", "version")) %>%
    rbindlist() %>%
    .[, mean(ing_t_p), by = version],
  
  times = 5)
results


##¿Existen diferencias importantes entre las distintas estrategias ¿Hay alguna más eficiente que otra? ¿Usar group_by versus map hace alguna diferencia?

##R: Purrr reduce el tiempo de procesamiento.
##R: Purrr pareciera ser más eficiente.
##R: Map toma menos tiempo.





