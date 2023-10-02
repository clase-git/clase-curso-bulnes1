rm(list = ls())

if(!require("dplyr")) install.packages("dplyr") 
if(!require("data.table")) install.packages("data.table") 
if(!require("purrr")) install.packages("purrr")
if(!require("stringr")) install.packages("stringr")
if(!require("readr")) install.packages("readr")

library(dplyr)
library(purrr) 
library(data.table)
library(stringr)
library(readr)
#EJERCICIO 1: DESCARGAR ARCHIVOS

urls <- c("https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi-2021---personas.csv?sfvrsn=d03ae552_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2020/esi-2020---personas.csv?sfvrsn=fb1f7e0c_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2019/esi-2019---personas.csv?sfvrsn=9eb52870_8&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2018/esi-2018---personas.csv?sfvrsn=a5de2b27_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2017/1esi-2017---personas.csv?sfvrsn=d556c5a1_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2016/esi-2016---personas.csv?sfvrsn=81beb5a_6&download=true"
)


extract_names<-function(urls){
  str_extract(urls, pattern = "esi-20[:digit:]{2}---personas.csv")
  }
file_names<-map(urls, extract_names)
file_names


download_esi_data<-function(urls, file_names, directory){
  if (!dir.exists(directory)) {
    dir.create(directory)
  }
  download.file(urls, paste0(directory, "/", file_names))
  }

purrr::walk2(urls, file_names, download_esi_data, directory = "data")


#EJERCICIO 2
file_path<-list.files("data/", full.names = T)
read_esi_data <- function(file_path) {
fread(file_path) ##All controls such as sep, colClasses and nrows are automatically detected.
}

esi<-map(file_path, read_esi_data)

#EJERCICIO 3
#tabla 1

esi <- map(esi, ~mutate(.,version=paste0("esi_",ano_encuesta)))

create_total_table <- function(data) {

  data$id_identificacion <- as.integer(data$id_identificacion)
  total_table <- data |> 
    bind_rows() |> 
    group_by(version) |> 
    summarise(n_personas = n(), n_hogares = n_distinct(id_identificacion))
  

  
  return(total_table)
}
create_total_table(esi)




create_total_table <- function(data) {
  # Convertir la columna id_identificacion al tipo de datos integer
  data$id_identificacion <- as.integer(data$id_identificacion)
  
  # Crear la tabla
  total_table <- data |>
    bind_rows() |>
    group_by(version) |>
    summarise(n_personas = n(), n_hogares = n_distinct(id_identificacion))
  
  # Agregar la versión con el formato esi_{año}
  total_table$version <- paste0("esi_", total_table$version)
  
  return(total_table)
}
esi <- map(esi, ~mutate(.,version=paste0("esi_",ano_encuesta)))
esi<-map(esi, ~mutate(., id_identificacion= as.integer(id_identificacion)))

tabla1 <- esi |>
  bind_rows() |> 
  group_by(ano_encuesta) |> 
  summarize(n_personas = n(), n_hogares = n_distinct(id_identificacion))
tabla2 <- esi  |> 
  bind_rows() |>
  group_by(ano_encuesta) |>
  summarize(
    min = min(fact_cal_esi),
    max = max(fact_cal_esi),
    mean = mean(fact_cal_esi),
    median = median(fact_cal_esi),
    p10 = quantile(fact_cal_esi, 0.1),
    p90 = quantile(fact_cal_esi, 0.9))
 ##los valores maximos de 2018 y 2019 son casi el triple que en los años siguientes.
tabla3 <- esi |>
  bind_rows() |>
  group_by(version, estrato) |>
  mutate(n_conglomerados = n_distinct(conglomerado)) |>
  filter(n_conglomerados == 1) |>
  summarize(n_estratos = n())
tabla4 <- esi |>
  bind_rows() |>
  group_by(version) |>
  summarize(
    min = min(ing_t_p),
    max = max(ing_t_p),
    mean = mean(ing_t_p),
    median = median(ing_t_p),
    p10 = quantile(ing_t_p, 0.1),
    p90 = quantile(ing_t_p, 0.9)
  )
