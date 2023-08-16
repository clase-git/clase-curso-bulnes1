# Tarea R Intermedio ------------------------------------------------------
##   Tarea1. funciones

library(dplyr)
library(tidyverse)

### ejercicio 1

get_cv <- function(val, val_na = FALSE) {
#booleano indicador
  if (val_na){
    val <- val[!is.na(val)]
  }
  
#Funcion de cv = sd/media
  valor_med <- mean(val)
  sd_val <- sd(val)
  
  cv <- (sd_val/valor_med) * 100
  
  return(cv)
}


### ejercicio 2

build_address <- function(street, number, apartment = NULL) {
  num_regex <- "\\d+"
  
  # Extraer número usando expresión regular
  num <- regmatches(number, regexec(num_regex, number))[[1]]
  
  # eliminar texto del string y espacios
  street <- gsub("número|num|num.|núm.|nm.|n°|n|n.|n°.", "", street)
  street <- gsub("^\\s+|\\s+$", "", street)
  
  # Construir la dirección con el formato correcto
  address <- paste(street, num)
  
  #Si la dirección lleva depatarmento, extrae el número y borra texto.
  if (!is.null(apartment) && !is.na(apartment)) {
    if(grepl("depto\\.|departamento", apartment)){
     apartment <- gsub("depto\\.|departamento", "", apartment)
     apartment_f <- paste("depto.", apartment)
     address <- paste(address, apartment_f)
     
    }
    
    #(else) si lleva departamento, pero no lleva texto, pega número 
    else { 
      apartment_f = paste("depto.", apartment)
      address = paste(address, apartment_f)
                             
    }
  } #si no tiene depto, no pega nada. (street+number)
  
  return(address)
}


#ejercicio 3

df <- tribble(~calle,              ~numero,      ~depto,
              "calle Hemingway",   "num 345",    "depto. 345",
              "av. Albert Camus",  "número 123", "123",
              "Manuel Rojas",      "234",        "departamento 231",
              "Nicanor Parra",     "678",        NA
)

df_result <- df %>% 
  rowwise %>% 
  mutate(adress = build_address(calle, numero, depto))
  