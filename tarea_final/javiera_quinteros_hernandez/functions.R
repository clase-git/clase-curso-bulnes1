
# Funciones


# Función extraer nombre --------------------------------------------------

extract_name <- function(url){
  
  if(str_detect(url, "esi-20\\d{2}---personas.csv") == T){
    
    name <- str_extract(url,"esi-20\\d{2}---personas.csv")
    print(name)
    
  } else {
    
    x
    
  }
  
}

# si se necesitara un nombre que abarcara más años pordría usarse --> esi-\\d{4}---personas\\.csv


# Función descargar archivos ----------------------------------------------

download_esi_data <- function(link, file_names, directory = 'data'){
  
  download.file(
    url = link,
    destfile = paste0(directory,"/", file_names)
     )
  
}



# Cargar archivos ---------------------------------------------------------

read_esi_data <- function(lista) {
  
  esi <- read.csv2(lista, header = TRUE, sep = ",")
  
}






