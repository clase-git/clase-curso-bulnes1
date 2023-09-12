
# Funciones ---------------------------------------------------------------

## Funcion para extraer nombres
extract_names <- function(url){
  str_extract(url, pattern = "esi-\\d{4}---[a-z]{8}.csv")
}

# Funcion para descargar data
download_esi_data <- function(url, file_name, directory){
  data <- fread(url)
  fwrite(data, file = paste0(directory, file_name))
  rm(data)
}

## Funcion para leer archivos
read_esi_data <- function(ruta){
  read_csv(ruta)
}

## Funcion de promedio
promedio_purr <- function(datos){
  datos %>% 
    group_by(version) %>% 
    summarise(promedio_ingresos = mean(ing_t_p, na.rm = T))
}

## Funcion de promedio con data table
promedio_data_table <- function(datos){
  datos_dt <- as.data.table(datos)
  datos_dt[, .(promedio_ingresos = mean(ing_t_p, na.rm = T)), by = .(version)]
}


