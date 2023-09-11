extract_names <- function(x) str_extract(x, regex("(?!\\n)[^\\/\\?#]+(?=[^\\/]*$)"))

download_esi_data <- function(url, file_name, directory) walk2(url, paste0(directory,"/", file_name), safely(~ download.file(.x , .y, mode = "wb")))

read_esi_data <- function(ruta) map(ruta, fread)

mi_promedio <- function(data, variable) data[, mean(variable)]
    


