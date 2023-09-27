### TRABAJO FINAL CURSO R INTERMEDIO ###
### JULIO LUNA CHAPARRO ###


### Se limpian de memoria elementos de proyectos anteriores

rm(list = ls())

### Verificar instalación de paquetes necesarios para utilizar las librerías ####

if (!require('tidyverse')) install.packages('tidyverse')
if (!require('downloader')) install.packages(('downloader'))
if (!require('data.table')) install.packages(('data.table'))
if (!require('microbenchmark')) install.packages(('microbenchmark'))


library('tidyverse')
library('downloader')
library('data.table')
library('microbenchmark')


### Se define fuente de las funciones que se ocuparan ####

source(file = "functions.R", encoding = "UTF-8")




# Ejercicio 1:descargar archivos ------------------------------------------

urls <- c("https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi-2021---personas.csv?sfvrsn=d03ae552_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2020/esi-2020---personas.csv?sfvrsn=fb1f7e0c_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2019/esi-2019---personas.csv?sfvrsn=9eb52870_8&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2018/esi-2018---personas.csv?sfvrsn=a5de2b27_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2017/esi-2017---personas.csv?sfvrsn=d556c5a1_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2016/esi-2016---personas.csv?sfvrsn=81beb5a_6&download=true"
)

#Se utiliza la función extract_name para obtener un vector llamado file_names con los nombres de los archivos

files_names <- extract_name(urls)

#Se utiliza la función download_esi_data para descargar los archivos

walk2(urls, files_names, ~download_esi_data(urls, files_names, directory = 'data'))






# Ejercicio 2: leer archivos ----------------------------------------------


esi <- read_esi_data("data", files_names)





# Ejercicio 3: obtener datos ----------------------------------------------

version <- str_extract_all(files_names, "esi_[0-9]{4}") %>% unlist()

esi <- map2(esi, version, ~mutate(.x, version = .y))

###Tabla 1  
###Tabla que contenga 3 columnas: version, n_personas (idrph) y n_hogares (id_identificacion).
###En la columna version debes usar la siguiente estructura: esi_{año}. Ejemplo: esi_2017

tabla_1 <- map(esi, ~mutate(.x, 
                            n_personas = n_distinct(idrph), 
                            n_hogares = n_distinct(id_identificacion)) %>%
                 distinct(version, n_personas, n_hogares)) %>%
  bind_rows()

###Tabla 2
###Tabla que contenga mínimo, máximo, media, mediana, p10 y p90 del factor de expansión (fact_cal_esi) 
###a nivel de hogar (id_identificacion) para cada versión, debes incluir la columna versión.

tabla_2 <-  map(esi, ~.x %>% 
                  select(version,fact_cal_esi,id_identificacion) %>% 
                  distinct() %>%
                  reframe(version,
                          minimo = min(fact_cal_esi,na.rm=T),
                          maximo = max(fact_cal_esi,na.rm=T),
                          media = mean(fact_cal_esi, na.rm=T),
                          mediana = median(fact_cal_esi,na.rm=T),
                          p_10 = quantile(fact_cal_esi, probs = 0.1,na.rm=T),
                          p_90 = quantile(fact_cal_esi, probs = 0.9,na.rm=T),
                  ) %>% 
                  distinct())%>%
  bind_rows()

###Tabla 3
###Tabla que contenga el número de estratos (estrato) con una sola unidad primaria de muestro (conglomerado). 
###Debes incluir la columna versión.

tabla_3 <- map(esi, ~.x %>% 
                 select(version, estrato, conglomerado) %>%
                 distinct() %>% 
                 group_by(estrato) %>% 
                 mutate(n_conglomerado = n()) %>% 
                 ungroup() %>% 
                 subset(n_conglomerado == 1) %>% 
                 reframe(version,
                         n_estrato= n()) %>% 
                 distinct()) %>% 
  bind_rows()


###Tabla 4
###Tabla que contenga mínimo, máximo, media, mediana, p10 y p90 de los ingresos del trabajo principal 
###a nivel de personas (ing_t_p) para cada versión.

tabla_4 <- map(esi, ~select(.x, version, fact_cal_esi, ing_t_p) %>%
                 mutate(ing_t_p = ifelse(ing_t_p == 0, NA, ing_t_p)) %>%    
                 mutate(ing_exp = ing_t_p*fact_cal_esi) %>%
                 reframe(version,
                         minimo = min(ing_exp,na.rm=T),
                         maximo = max(ing_exp,na.rm=T),
                         media = mean(ing_exp, na.rm=T),
                         mediana = median(ing_exp,na.rm=T),
                         p_10 = quantile(ing_exp, probs = 0.1,na.rm=T),
                         p_90 = quantile(ing_exp, probs = 0.9,na.rm=T),
                 ) %>% 
                 distinct())%>% 
  bind_rows()



# Ejercicio 4: mejorando el código ----------------------------------------

#En prueba_2. 
#Se produce error:Can't combine `..1$id_identificacion` <double> and `..2$id_identificacion` <integer64>.
#Se realiza la siguiente corrección
### esi[[1]] <- map_at(esi[[1]], 'id_identificacion', as.integer)
### esi[[3]] <- map_at(esi[[3]], 'id_identificacion', as.integer)


tiempo_1 <- microbenchmark(
  
  prueba_1 <- map(esi, ~select(.x, version, ing_t_p) %>%      
                    mutate(media = mean(ing_t_p, na.rm = T)) %>%
                    distinct(version, media)) %>% 
    bind_rows(),
  
  times = 5)


#Se aplica corrección en prueba_2
esi[[1]] <- map_at(esi[[1]], 'id_identificacion', as.integer)
esi[[3]] <- map_at(esi[[3]], 'id_identificacion', as.integer)

tiempo_2 <- microbenchmark(
  
  prueba_2 <- esi %>% bind_rows() %>% 
    group_by(version) %>% 
    reframe(promedio = mean(ing_t_p, na.rm = T)),
  
  times = 5)


tiempo_3 <- microbenchmark(
  
  prueba_3 <- map_dfr(esi , ~ table_media(.x,ing_t_d,version),.id = "version"),
  
  times = 5)





tiempo_4 <- microbenchmark(
  
  ###Se entrega archivo a la fecha de fallar el disco duro, se iniciaba trabajo en éste punto###
  
  
  
  prueba_4 <- bind_rows(esi)[, .(promedio = mean(ing_t_p, na.rm = T)), by = version],
  
  
  
  
  times = 5)