
# Master

# Limpiar y remover -------------------------------------------------------

cat("\014") 
rm(list = ls())

options(encoding = "utf-8", scipen=999)


# Paquetes a utilizar -----------------------------------------------------

if(!require("tidyverse")) install.packages("tidyverse")
if(!require("purrr")) install.packages("purrr")
if(!require("readr")) install.packages("readr")
if(!require("data.table")) install.packages("data.table")


# Crea carpetas -----------------------------------------------------------

if (!dir.exists ("data")) {
  dir.create("data", recursive = TRUE)
}


# Carga funciones ---------------------------------------------------------

source('functions.R')


# Descargar archivos ------------------------------------------------------

urls <- c("https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi-2021---personas.csv?sfvrsn=d03ae552_4&download=true",
          "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2020/esi-2020---personas.csv?sfvrsn=fb1f7e0c_4&download=true",
          "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2019/esi-2019---personas.csv?sfvrsn=9eb52870_8&download=true",
          "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2018/esi-2018---personas.csv?sfvrsn=a5de2b27_6&download=true",
          "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2017/esi-2017---personas.csv?sfvrsn=d556c5a1_6&download=true",
          "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2016/esi-2016---personas.csv?sfvrsn=81beb5a_6&download=true"
)


# Encontrar nombres
file_names <- map_chr(urls, extract_name)

# Descargar archivos
map2(urls, file_names, download_esi_data)


# Leer archivos -----------------------------------------------------------

lista <- list.files(path = "data/", pattern = "esi-20\\d{2}", full.names = TRUE)
varios_esi <- map(lista, read_esi_data)


# Obtener datos -----------------------------------------------------------

anio <- str_extract_all(file_names,"\\d{4}") %>% unlist()
names(varios_esi) <-  paste0("esi_", anio)  
nombres_lista <-  imap(varios_esi, ~.y) %>% unlist()
varios_esi <- map2(varios_esi,nombres_lista,~mutate(.x,version = .y))

esi_df = varios_esi %>% bind_rows()


# Tabla_1: que contenga 3 columnas: version, n_personas, n_hogares

tabla_1 <- esi_df %>% select(version, idrph, id_identificacion) %>% 
                      group_by(version) %>% 
                      mutate(n_personas = n()) %>%
                      ungroup() %>% 
                      group_by(version, id_identificacion) %>% 
                      mutate(hogares = n()) %>% 
                      distinct(version, .keep_all = TRUE) %>% 
                      ungroup() %>% 
                      group_by(version) %>% 
                      mutate(n_hogares = n()) %>% 
                      ungroup() %>%
                      select(version, n_personas, n_hogares) %>% 
                      distinct(version, .keep_all = TRUE) %>% 
                      print()
  

# Tabla_2: que contenga mínimo, máximo, media, mediana, p10, p90 del factor de expansión a nivel hogar, para cada versión

esi_df <- esi_df %>% mutate(fact_cal_esi = as.numeric(esi_df$fact_cal_esi))
percentil <- quantile(esi_df$fact_cal_esi, probs = c(0.10, 0.30, 0.50, 0.70, 0.90), type = 7, na.rm = TRUE)

esi_df %>% group_by(version) %>% 
           summarise(minimo = min(fact_cal_esi, na.rm = TRUE),
                     maximo = max(fact_cal_esi, na.rm = TRUE),
                     media = mean(fact_cal_esi, na.rm = TRUE),
                     mediana = median(fact_cal_esi, na.rm = TRUE),
                     p10 = percentil[1],
                     p90 = percentil[5]
                     )


aggregate(fact_cal_esi ~ version, esi_df, quantile)


 
