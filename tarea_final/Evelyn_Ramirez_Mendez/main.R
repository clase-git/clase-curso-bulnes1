rm(list=ls())

if(!require("tidyverse")) install.packages("tidyverse")
if(!require("purrr")) install.packages("purrr")
if (!require("data.table")) install.packages("data.table")
if(!require("microbenchmark")) install.packages("microbenchmark")

source("functions.R")

# Ejercicio 1: descargar archivos -------------------------------------------------------------------------------------------

urls <- c("https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi-2021---personas.csv?sfvrsn=d03ae552_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2020/esi-2020---personas.csv?sfvrsn=fb1f7e0c_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2019/esi-2019---personas.csv?sfvrsn=9eb52870_8&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2018/esi-2018---personas.csv?sfvrsn=a5de2b27_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2017/esi-2017---personas.csv?sfvrsn=d556c5a1_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2016/esi-2016---personas.csv?sfvrsn=81beb5a_6&download=true")

file_names <- map(urls,extract_names)

#Esta fue la prueba con un solo archivo:
# download_esi_data("https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi-2021---personas.csv?sfvrsn=d03ae552_4&download=true","esi-2021---personas.csv","data")

map(urls,download_esi_data)

# Ejercicio 2: leer archivos ------------------------------------------------------------------------------------------------

#read_esi_data("data/esi-2021---personas.csv")
files <- list.files("data", full.names = T) 
esi <-map(files,read_esi_data) %>% set_names(str_extract(files,pattern = "esi-[[:digit:]]{4}"))

# Ejercicio 3: obtener datos ------------------------------------------------------------------------------------------------

esi <- map(esi,~mutate(.x,id_identificacion = as.numeric(id_identificacion),
                        idrph = as.numeric(idrph),
                          version=paste0("esi_",ano_encuesta)))
tabla1 <- esi %>% 
  map(~.x %>% select("version", "idrph", "id_identificacion")) %>%
  rbindlist() %>% 
  group_by(version) %>% 
  summarise(n_personas=n_distinct(idrph),
            n_hogares=n_distinct(id_identificacion))
tabla1

tabla2 <- esi %>% 
  map(~.x %>% select("version", "id_identificacion", "fact_cal_esi")) %>%
  rbindlist() %>%
  group_by(version) %>% 
  summarise(n_hogares = n_distinct(id_identificacion),
            min=min(fact_cal_esi),
            max=max(fact_cal_esi),
            media=mean(fact_cal_esi),
            mediana=median(fact_cal_esi),
            p10=quantile(fact_cal_esi,0.10),
            p90=quantile(fact_cal_esi,0.90))
tabla2

tabla3 <- esi%>%
  map(~.x %>% select("version", "estrato", "conglomerado")) %>%
  rbindlist() %>%
  group_by(version, estrato) %>%
  summarise(cant_conglomerados = n_distinct(conglomerado)) %>%
  filter(cant_conglomerados == 1) %>% 
  ungroup() %>%
  group_by(version) %>% 
  summarise(n_estratos=n_distinct(estrato))
tabla3  # Hasta el 2020 sólo 4 estratos tenían 1 conglomerado. En 2021 5 estratos.

tabla4_0 <- esi%>%
  map(~.x %>% mutate(ing_exp = ing_t_p*fact_cal_esi) %>%
        select("version", "idrph", "fact_cal_esi", "ing_t_p", "ing_exp")) %>%
  rbindlist() 

options(scipen = 999) # para evitar la notación científica

tabla4 <- tabla4_0 %>% 
  group_by(version) %>%
  summarise(n_personas = n_distinct(idrph),
            N = sum(fact_cal_esi),
            suma_fact_exp = sum(ing_exp, na.rm = T),
            minimo = min(ing_t_p, na.rm = TRUE),
            maximo = max(ing_t_p, na.rm = TRUE),
            media = mean(ing_exp),
            mediana = median(ing_exp, na.rm = TRUE),
            p10 = quantile(as.numeric(ing_t_p), 0.1, na.rm = TRUE),
            p90 = quantile(as.numeric(ing_t_p), 0.9, na.rm = TRUE)) %>%
  distinct()
tabla4

# Ejercicio 4: Mejorando el código ------------------------------------------------------------------------------------------

# 1. Lista de tablas
map_df(esi, ~mean(.x$ing_t_p))

# 2. Tablas apiladas
esi_df <- esi %>% bind_rows()
esi_df %>% 
  group_by(version) %>% 
  summarise(media_ingresos = mean(ing_t_p, na.rm = T))

# 3. Lista de tablas: funcion data table
map_dfr(esi, fn_media_dt)

# 4. Tablas apiladas
esi_dt <- as.data.table(esi_df)

esi_dt[,.(media_ingresos = mean(ing_t_p, na.rm = T)), by = .(version)]

res <- microbenchmark(e1 = map_df(esi, ~mean(.x$ing_t_p)),
                    e2 = esi_df %>% 
                      group_by(version) %>% 
                      summarise(media_ingresos = mean(ing_t_p, na.rm = T)),
                    e3 = map_dfr(esi, fn_media_dt),
                    e4 = esi_dt[,.(media_ingresos = mean(ing_t_p, na.rm = T)), by = .(version)], 
                    times = 5, unit = "second")
res
boxplot(res) # el proceso que menos demora es utilizando data.table (e4)





