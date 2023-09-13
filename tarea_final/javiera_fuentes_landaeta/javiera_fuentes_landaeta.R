###Tarea Final
rm(list = ls())

if(!require("dplyr")) install.packages("dplyr") 
if(!require("data.table")) install.packages("data.table") 
if(!require("purrr")) install.packages("purrr")
if(!require("stringr")) install.packages("stringr")

library(dplyr)
library(purrr) 
library(data.table)

#Ejercicio 1
urls <- c("https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi-2021---personas.csv?sfvrsn=d03ae552_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2020/esi-2020---personas.csv?sfvrsn=fb1f7e0c_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2019/esi-2019---personas.csv?sfvrsn=9eb52870_8&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2018/esi-2018---personas.csv?sfvrsn=a5de2b27_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2017/esi-2017---personas.csv?sfvrsn=d556c5a1_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2016/esi-2016---personas.csv?sfvrsn=81beb5a_6&download=true"
)

extract_name <- function (urls){
  urls %>% 
    str_extract(pattern = "esi-20[:digit:]{2}---personas.csv")
}

map(urls, extract_name)

download_esi_data <- function(url, file_name) {
  dest_dir <- "/Tarea Final"
  directory <- file.path(dest_dir, file_name)
  download.file(url, directory)
}



map(urls, download_esi_data)

#Ejercicio 2

read_esi_data <- function(df){
  fread(df)
}

files <- list.files("/Tarea Final", full.names = T) 

esi <-map(files,read_esi_data) %>% 
  set_names(str_extract(files,pattern = "esi-[:digit:]{4}"))


#Ejercicio 3

esi <- map(esi, ~mutate(.,version=paste0("esi_",ano_encuesta)))

tabla_1 <- function (esi) {
  esi %>% select(version, idrph, id_identificacion)
}

map(esi, tabla_1)

tabla_2 <- function (esi){
  esi %>% select(version, idrph, id_identificacion) %>% 
    group_by(version, id_identificacion) %>% 
    summarise(mínimo = min(fact_cal_esi), máximo = max(fact_cal_esi),
              media = mean(fact_cal_esi), mediana = median (fact_cal_esi),
              percentil_10 = quantile(fact_cal_esi, 0.1), 
              percentil_90 = quantile(fact_cal_esi, 0.9))
}

map(esi, tabla_2)

tabla_3 <- function (esi) {
  esi %>% select(version, estrato, conglomerado) %>% 
    summarise(conglomerados_n = n_distinct(conglomerado, na.rm=FALSE)) %>% 
    filter(conglomerados_n==1)
}

map(esi, tabla_3)

tabla_4 <- function (esi) {
  esi %>% select(version, idrph, ing_t_p)
  group_by(version, idrph) %>% 
    summarise(mínimo = min(ing_t_p), máximo = max(ing_t_p),
              media = mean(ing_t_p), mediana = median (ing_t_p),
              percentil_10 = quantile(ing_t_p, 0.1), 
              percentil_90 = quantile(ing_t_p, 0.9))
}

map(esi, tabla_4)

