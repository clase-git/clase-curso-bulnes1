
# Funciones ---------------------------------------------------------------

# Extraer el nombre del archivo de una URL
extract_nombre <- function(url) {
  basename <- sub("\\?.*$", "", basename(url))
  return(basename)
}

#descarga de datos
download_esi <- function(url, file_name, directory) {
  full_url <- url
  full_path <- file.path(directory, file_name)
  
  # Verificamos para no descargar 2 veces el archivo.
  if (!file.exists(full_path)) {
    download.file(full_url, full_path, mode = "wb")
  } else {
    # Si ya existe, notificamos.
    cat(paste("El archivo", file_name, "ya existe en la carpeta", directory, "\n"))
  }
}

#lectura de datos en una variable ejercicio2
read_esi_data <- function(directorio){
  csv_files = list.files(directorio, pattern = "\\.csv$", full.names = TRUE)
  data_list <- setNames(map(csv_files, fread), tools::file_path_sans_ext(basename(csv_files))) 
  
  return(data_list)  
}


