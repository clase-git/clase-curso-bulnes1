rm(list = ls(all.names = T), envir = environment()) # se elimina lista de objetos creados y se limpia el ambiente
cat("\014")                                         # se limpia la consola
gc()                                                # se limpia de memoria los objetos eliminados
cat("\014")                                         # se limpia la consola

options(repos = c(CRAN = "http://cran.rstudio.com")) # se seleeciona página de descarga para paquetes
options(scipen = 999)                                # se elimina notación científica

# Tarea final; curso INE R Intermedio -------------------------------------

# Carga de librería -------------------------------------------------------

if (!require("dplyr")) install.packages("dplyr"); library(dplyr)
if (!require("stringr")) install.packages("stringr"); library(stringr)
if (!require("tidyverse")) install.packages("tidyverse"); library(tidyverse)
if (!require("purrr")) install.packages("purrr"); library(purrr)
if (!require("data.table")) install.packages("data.table"); library(data.table)
if (!require("microbenchmark")) install.packages("microbenchmark"); library(microbenchmark)
if (!require("survey")) install.packages("survey"); library(survey)


# Creación carpeta data ---------------------------------------------------

if(!file.exists("data")) {
  dir.create("data")
}

bases <- "./data/"

# carga de funciones desde script functions -------------------------------

source("./functions.R")

# Ejercicio 1 -------------------------------------------------------------

urls <- c("https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi-2021---personas.csv?sfvrsn=d03ae552_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2020/esi-2020---personas.csv?sfvrsn=fb1f7e0c_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2019/esi-2019---personas.csv?sfvrsn=9eb52870_8&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2018/esi-2018---personas.csv?sfvrsn=a5de2b27_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2017/esi-2017---personas.csv?sfvrsn=d556c5a1_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2016/esi-2016---personas.csv?sfvrsn=81beb5a_6&download=true"
)

file_names <- map_chr(urls, extract_names)
map2(urls, file_names, download_esi_data, directory = bases)


# Ejercicio 2 -------------------------------------------------------------

BBDD_esi <- read_esi_data(bases)

names(BBDD_esi) <- str_extract(file_names, pattern = "\\d+") %>%
  str_replace_all(pattern = "^", replacement = "esi_")


BBDD_esi <- BBDD_esi %>% map(~.x %>% mutate(version = paste0("esi_",ano_encuesta)))
BBDD_esi$esi_2019[, fact_cal_esi := as.numeric(str_replace_all(fact_cal_esi, pattern = ",", replacement = "."))]
BBDD_esi$esi_2019[, ing_t_p := as.numeric(str_replace_all(ing_t_p, pattern = ",", replacement = "."))]
BBDD_esi$esi_2016[, ing_t_p := as.numeric(str_replace_all(ing_t_p, pattern = ",", replacement = "."))]

Tabla1 <- BBDD_esi %>%
  map(~.x %>% select("version", "idrph", "id_identificacion")) %>%
  rbindlist() %>%
  group_by(version) %>%
  summarise(n_personas = n_distinct(idrph), n_hogares = n_distinct(id_identificacion))

Tabla2 <- BBDD_esi %>%
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

Tabla3 <- BBDD_esi %>%
  map(~.x %>% select("version", "estrato", "conglomerado")) %>%
  rbindlist() %>%
  group_by(version, conglomerado) %>%
  count(estrato) %>%
  filter(n == 1) %>%
  ungroup() %>%
  group_by(version) %>%
  summarise(n_estratos = sum(n))

BBDD_esi_consolidada <- BBDD_esi %>%
  map(~.x %>% mutate(ing_exp = ing_t_p*fact_cal_esi, N = sum(fact_cal_esi)) %>%
        select("version", "idrph", "fact_cal_esi", "ing_t_p", "ing_exp", "N")) %>%
  rbindlist()

Tabla4 <- BBDD_esi_consolidada %>%
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


print(Tabla1)
print(Tabla2)
print(Tabla3)
print(Tabla4)

# Pregunta 4 --------------------------------------------------------------

results <- microbenchmark(
  Tabla1_2 = map_df(BBDD_esi, promedio_with_purr),
  Tabla2_2 = esi_df %>% group_by(version) %>% summarise(promedio_ingresos = mean(ing_t_p, na.rm = T)),
  Tabla3_2 = map_df(BBDD_esi, promedio_with_data_table)
  ,times = 5,unit = 'seconds')




