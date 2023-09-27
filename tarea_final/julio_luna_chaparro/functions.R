
# Funciones ---------------------------------------------------------------


# Función 1: Extraer nombres de archivos ----------------------------------

extract_name <- function(vector) {
  patron <- ".*/(.*\\.csv)(\\?.*)" 
  gsub(patron, "\\1", vector) %>% 
    str_replace(pattern = "-", replacement = "_") %>% 
    str_replace(pattern = "---", replacement = "_")
}

# Función 2: Descargar archivos -------------------------------------------

download_esi_data <- function(url, file_names, directory){
  if(!dir.exists(directory)) dir.create(directory)
  ruta_descarga <- paste0(directory, "/", file_names)
  download.file(url, ruta_descarga)
}



# Función 3: Leer archivos ------------------------------------------------

read_esi_data<- function(ruta, lista) {
  lectura <- map(lista, ~paste0({{ruta}}, "/", {{lista}}))[[1]] %>% unlist()
  nombres_df <- lista %>% str_extract(., "esi_[[:digit:]]{4}")
  cargar_esi <- lectura %>% 
    map(fread) %>% 
    set_names(nombres_df) 
}


# Función 4: Calcular promedio en data.table

table_media <- function(data, columna, group_var) {
  columna <- as.character(substitute(columna))
  data <- as.data.table(data)
  media <- data[, mean(eval(parse(text = columna)), na.rm = TRUE)]
  return(media)
}

