
# Tarea final Curso Intermedio R, INE -------------------------------------

# Estefanía Pizarro Arias


# Limpiar ambiente y cargar paquetes --------------------------------------
rm(list = ls())
graphics.off()


library(tidyverse)
library(data.table)
library(microbenchmark)

# Llamar funciones --------------------------------------------------------

source("r codes/functions.R")


# Ejercicio 1: Descarga de archivos--------------------------------------------------------
# 
urls <- c("https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi-2021---personas.csv?sfvrsn=d03ae552_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2020/esi-2020---personas.csv?sfvrsn=fb1f7e0c_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2019/esi-2019---personas.csv?sfvrsn=9eb52870_8&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2018/esi-2018---personas.csv?sfvrsn=a5de2b27_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2017/esi-2017---personas.csv?sfvrsn=d556c5a1_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2016/esi-2016---personas.csv?sfvrsn=81beb5a_6&download=true"
)

## Aplicar funcion "file names"
file_names <- urls %>% map_chr(~extract_names(.x))

## Descarga de archivos
pmap(list(urls, file_names,"data/"), download_esi_data)


# Ejercicio 2: Leer archivos -------------------------------------------------------------
path.origin <- "data/"
ruta <- str_c(path.origin, file_names)

agno <-  list.files("data/") %>% 
  str_extract(pattern = "[[:digit:]]{4}") 

esi <- map(ruta, read_esi_data) %>% 
  set_names(paste0("esi_",agno))


# Ejercicio 3: Obtener datos -------------------------------------------------------------

esi <- map2(esi,agno,~mutate(.x,version = glue::glue("esi_{.y}")))

esi_df <- esi %>% bind_rows()

# Tabla 1: Tabla que contenga 3 columnas: version, n_personas (idrph) y n_hogares (id_identificacion).
# En la columna version debes usar la siguiente estructura: esi_{año}
esi_df %>% 
  group_by(version) %>% 
  summarise(n_personas = n_distinct(idrph),
            n_hogares  = n_distinct(id_identificacion))

# Tabla 2: Tabla que contenga mínimo, máximo, media, mediana, p10 y p90 del factor de expansión 
# (fact_cal_esi) a nivel de hogar (id_identificacion) para cada versión, debes incluir la columna
# versión. ¿Se observan algunos pesos de muestreo atípicos?
esi_df %>% 
  group_by(version) %>% 
  summarise(min = min(fact_cal_esi, na.rm = T),
            max = max(fact_cal_esi, na.rm = T),
            media = mean(fact_cal_esi, na.rm = T),
            mediana = median(fact_cal_esi, na.rm = T),
            p10 = quantile(fact_cal_esi, probs = 0.1, na.rm = T),
            p90 = quantile(fact_cal_esi, probs = 0.9, na.rm = T))

ggplot(esi_df, aes (y = fact_cal_esi, x = version)) +
  geom_boxplot()+
  ylim(c(0,750))

# Tabla 3: Tabla que contenga el número de estratos (estrato) con una sola unidad primaria de 
# muestro (conglomerado). Debes incluir la columna versión

# Filtrar los estratos con una sola unidad primaria de muestreo (conglomerado)
estratos_unidad_unica <- esi_df %>%
  group_by(version, estrato) %>%
  summarise(n_conglomerados = n_distinct(conglomerado)) %>%
  filter(n_conglomerados == 1)

# Contar el número de estratos con una sola unidad primaria de muestreo por versión
estratos_unidad_unica %>%
  group_by(version) %>%
  summarise(numero_estratos = n())


# Tabla 4: Tabla que contenga mínimo, máximo, media, mediana, p10 y p90 de los ingresos del 
# trabajo principal a nivel de personas (ing_t_p) para cada versión.
esi_df %>% 
  group_by(version) %>% 
  summarise(min = min(ing_t_p, na.rm = T),
            max = max(ing_t_p, na.rm = T),
            media = mean(ing_t_p, na.rm = T),
            mediana = median(ing_t_p, na.rm = T),
            p10 = quantile(ing_t_p, probs = 0.1, na.rm = T),
            p90 = quantile(ing_t_p, probs = 0.9, na.rm = T))


# Ejercicio 4: Mejorando el código -------------------------------------------------------

  
## Lista de tablas: promedio con herramientas de purrr
map_df(esi, promedio_purr)

## Tablas apiladas: promedio con group_by y summarise
esi_df %>% 
  group_by(version) %>% 
  summarise(promedio_ingresos = mean(ing_t_p, na.rm = T))

## Lista de tablas: promedio con herramientas de purrr usando funcion con data.table
map_df(esi, promedio_data_table)

## Tablas apiladas: promedio con data table
esi_dt <- as.data.table(esi_df)
esi_dt[, .(promedio_ingresos = mean(ing_t_p, na.rm = T)), by = .(version)]


results <- microbenchmark(
  p1 = map_df(esi, promedio_purr),
  p2 = esi_df %>% group_by(version) %>% summarise(promedio_ingresos = mean(ing_t_p, na.rm = T)),
  p3 = map_df(esi, promedio_data_table),
  p4 = esi_dt[, .(promedio_ingresos = mean(ing_t_p, na.rm = T)), by = .(version)]
,times = 5,unit = 'seconds')

boxplot(results)
autoplot(results)

# ¿Existen diferencias importantes entre las distintas estrategias? 
# R: si, el uso de la tercera estrategia muestra un proceso mas lento (menos optimo)

# ¿Hay alguna más eficiente que otra? 
# R: La forma más eficiente es mediante el uso de tablas apiladas con data.table

# ¿Usar group_by versus map hace alguna diferencia?
# R: si se comparan las estrategias 1 y 2 (map y group by respectivamente), para este caso 
# es más optimo el uso de group_by
