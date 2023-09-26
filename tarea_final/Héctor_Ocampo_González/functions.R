# Funciones tarea final ---------------------------------------------------

extract_names <- function(url) {
  str_extract(url, regex("esi-\\d{4}---[a-z]{8}.csv"))}

download_esi_data <- function(url, file_name, directory){
  data <- fread(url)
  fwrite(data, file = paste0(directory, file_name))
  rm(data)
}

read_esi_data <- function(directorio){
  csv_files = list.files(directorio, pattern = "\\.csv$", full.names = TRUE)
  data_list <- setNames(map(csv_files, fread), tools::file_path_sans_ext(basename(csv_files))) 
  
  return(data_list)  
}

promedio_with_purr <- function(datos){
  datos %>% 
    group_by(version) %>% 
    summarise(promedio_ingresos = mean(ing_t_p, na.rm = T))
}

## Funcion de promedio con data table
promedio_with_data_table <- function(datos){
  datos_datatable <- as.data.table(datos)
  datos_datatable[, .(promedio_ingresos = mean(ing_t_p, na.rm = T)), by = .(version)]
}

