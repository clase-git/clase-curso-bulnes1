# ESI
# 
# Tarea Final - Curso Intermedio ------------------------------------------
# Gabriel Retamales Muñoz

# Funciones y librerías ---------------------------------------------------

#Librerías

if(!require("dplyr")) install.packages("dplyr")
if(!require("purrr")) install.packages("purrr")
if(!require("data.table")) install.packages("data.table")

suppressPackageStartupMessages({
  library(purrr)
  library(dplyr)
  library(data.table)
})

#Funciones
source("./functions.R")

#Directorio de destino
directorio <- "./data"

# Ejercicio 1 --------------------------------------------------------------------

urls <- c("https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi-2021---personas.csv?sfvrsn=d03ae552_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2020/esi-2020---personas.csv?sfvrsn=fb1f7e0c_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2019/esi-2019---personas.csv?sfvrsn=9eb52870_8&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2018/esi-2018---personas.csv?sfvrsn=a5de2b27_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2017/1esi-2017---personas.csv?sfvrsn=d556c5a1_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2016/esi-2016---personas.csv?sfvrsn=81beb5a_6&download=true"
)

# Mapeo para descargar datos

file_names <- map_chr(urls, extract_nombre)
map2(urls, file_names, download_esi, directory = directorio)
 

# Ejercicio 2 -------------------------------------------------------------


datos_esi <- read_esi_data(directorio)
#Para leer alguna tabla de modo singular podemos usar View(esi_data_list[[numero del vector]])
#O podemos también usar el nombre del archivo.. por ejemplo esi_data_list["esi_2021_personad"]



# Ejercicio 3 -------------------------------------------------------------


csv_nombres <- list.files(directorio, pattern = "\\.csv$", full.names = TRUE)

# Extraer el año de los nombres de archivo y crear la tabla
tabla1 <- map2_df(datos_esi, csv_nombres, ~ {
  year <- substr(basename(.y), 5, 8)
  summarise(.x, version = paste0("esi_", year), n_personas = n(), n_hogares = n_distinct(id_identificacion))
})

# Crear la tabla con estadísticas del factor de expansión
tabla2 <- map2_df(datos_esi, csv_nombres, ~ {
  year <- substr(basename(.y), 5, 8)
  summarise(.x, version = paste0("esi_", year), 
            min_fact = min(fact_cal_esi), max_fact = max(fact_cal_esi), 
            media_fact = mean(fact_cal_esi), 
            mediana_fact = median(fact_cal_esi), 
            p10_fact = quantile(fact_cal_esi, probs = 0.10), 
            p90_fact = quantile(fact_cal_esi, probs = 0.90))
})

# Crear la tabla con el número de estratos con un solo conglomerado
tabla3 <- map2_df(datos_esi, csv_nombres, ~ {
  year <- substr(basename(.y), 5, 8)
  num_estratos_unidad <- sum(table(.x$estrato) == 1)
  data.frame(version = paste0("esi_", year), num_estratos_unidad)
})


# Crear la tabla con estadísticas de ingresos del trabajo principal
tabla4 <- map2_df(datos_esi, csv_nombres, ~ {
  year <- substr(basename(.y), 5, 8)
  summarise(.x, version = paste0("esi_", year), 
            min_ingresos = min(ing_t_p), max_ingresos = max(ing_t_p), 
            media_ingresos = mean(ing_t_p), 
            mediana_ingresos = median(ing_t_p), 
            p10_ingresos = quantile(ing_t_p, probs = 0.10), 
            p90_ingresos = quantile(ing_t_p, probs = 0.90))
})

print(tabla1)
print(tabla2)
print(tabla3)
print(tabla4)


# Ejercicio 4 -------------------------------------------------------------

if(!require("microbenchmark")) install.packages("microbenchmark")
library(microbenchmark)

#Prueba 1: Lista de tablas con purrr
microbenchmark(
  purrr_mean = mean(map_dbl(datos_esi, ~mean(.x$ing_t_p))),
  times = 5
)

# Prueba 3: Lista de tablas con de purrr y data.table
microbenchmark(
  purrr_dt_mean = mean(map_dbl(datos_esi, ~mean(.x$ing_t_p))),
  times = 5
)


#Prueba 2 y 4 no me salieron :')
#
#


