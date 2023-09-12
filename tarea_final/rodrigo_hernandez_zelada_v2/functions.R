#FUNCIONES

#--Función para extraer el nombre de los archviso desde las urls

extract_name <- function(url){
  file_name <- str_extract(url, "[^/]+\\.csv")
  return(file_name)
}

#--Función para descargar archivos, se usa download porque no presenta problemas con links de tipo 'https'

download_esi_data <- function(url, file_name, directory) {
  download(url, file.path(directory, file_name))
}


#--Se crea función utilizando fread de librería data.table

read_esi_data <- function(file_path) {
  fread(file_path)
}


#--Se crea funcipon para extraer información solicitada 

extraer_info <- function(datos) {
  datos %>%
    group_by(version) %>%           
    summarize(
      n_personas = n(),          
      n_hogares = n_distinct(id_identificacion) 
    )
}

#--Funcion para ponderar  las variables

ponderar <- function (var, fact){
  {{var}}*{{fact}}
}


# Crear una función para resumir los estadísticos deseados

resumen_estadistico <- function(datos) {
  datos %>%
    group_by(version) %>%
    summarise(
      min_id_exp = as.double(min(id_identificacion_exp)),
      max_id_exp = as.double(max(id_identificacion_exp)),
      media_id_exp = as.double(mean(id_identificacion_exp)),
      mediana_id_exp = as.double(median(id_identificacion_exp)),
      p10_id_exp = as.double(quantile(id_identificacion_exp, 0.10)),
      p90_id_exp = as.double(quantile(id_identificacion_exp, 0.90))
    )
}

#--Función para contabilizar los estratos con una sola unidad primaria de muestreo

contar_estratos <- function(datos) {
  datos %>%
    group_by(version, estrato) %>%
    summarise(n_conglomerados = n_distinct(conglomerado)) %>%
    filter(n_conglomerados == 1) %>%
    summarise(n_estratos = n())
}

#Funcion para calcular resumenes estadísticos de ing_t_p

calcular_estadisticas_ingresos <- function(datos) {
  datos %>%
    group_by(version) %>%
    summarise(
      min_ing_t_p = min(ing_t_p, na.rm = TRUE),
      max_ing_t_p = max(ing_t_p, na.rm = TRUE),
      media_ing_t_p = mean(ing_t_p, na.rm = TRUE),
      mediana_ing_t_p = median(ing_t_p, na.rm = TRUE),
      p10_ing_t_p = quantile(ing_t_p, 0.1, na.rm = TRUE),
      p90_ing_t_p = quantile(ing_t_p, 0.9, na.rm = TRUE)
    )
}




#Funcion con data.table. Primnero convierte la tabla en data.table y luego obtiene el promedio

calcular_promedio_data_table <- function(data) {
  
  dt <- as.data.table(data)
  
  promedio <- dt[, .(promedio_ing_t_p = mean(ing_t_p, na.rm = TRUE)), by = version]
  
  return(as.data.frame(promedio))
}

#--Funcion que selecciona variables version e ing_t_p y  convierte en data.table

seleccionar_y_convertir_data_table <- function(data) {
  dt <- as.data.table(data[, .(version, ing_t_p)])
  return(dt)
}














