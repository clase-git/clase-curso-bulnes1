rm(list = ls())
library(tidyverse)


##Ejercico 1

get_cv <- function(n, na.rm =T){
  cv = sd(n,na.rm = na.rm) / mean(n, na.rm = na.rm)
  return(cv)
}

get_cv(n = sample(10, 4))




##Ejercicio 2##

library("stringr")

build_address <- function(street, number, apartament = NULL) {
  
  
  patron_street <- c("calle", "avenida", "av", "avda", "pasaje", "psje", ".+\\.") #Identificar patrone 
  patron_street <- str_c(patron_street, collapse = "|")                            #Transforma el vector a longitud 1 separados por |
  
  
  clean_street <- sub(x=street, pattern=patron_street, replacement="", ignore.case = T)
  clean_street <- sub(x=clean_street, pattern="^ ", replacement="")               #^para buscar la coincidencia al inicio de la cadena.
  clean_street <- sub(x=clean_street, pattern=" $", replacement="")               #$para buscar la coincidencia al final de la cadena
  
  
  
    
  clean_number <- sub(x=number, pattern= ".+\\D", replacement="") #Eliminar todo lo que no es número    
  
  clean_apartament <- sub(x=apartament, pattern=".+\\D", replacement="")
  
  if (is.null(apartament)) {
    
    print(tolower(paste0(clean_street, " ", clean_number)))
    
    
    
  } else { 
    
    
    
    print(tolower(paste0(clean_street, " ", clean_number, ", depto. ", clean_apartament)))
    
  }
  
  
}

street <- "calle Los Alerces"
number <- "número 123"
build_address(street, number)






##Ejercicio 3##


library(dplyr)   

df <- tribble(~calle,              ~numero,      ~depto,
              "calle Hemingway",   "num 345",    "depto. 345",
              "av. Albert Camus",  "número 123", "123",
              "Manuel Rojas",      "234",        "departamento 231",
              "Nicanor Parra",     "678",        NULL
) 
df

df <- df %>% 
  rowwise() %>%
  mutate(dirección = build_address(calle, numero, depto))
df

