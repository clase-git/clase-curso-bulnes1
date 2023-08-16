
# Tarea Sesion 1 Curso R Intermedio ---------------------------------------
# Estefanía Pizarro Arias
# eapizarroa@ine.gob.cl


# Ambiente y librerias -----------------------------------------------------
rm(list = ls())
graphics.off()

pacman::p_load(tidyverse)

# Ejercicio 1: Funcion para obtener CV -----------------------------------------------

get_cv <- function(vector                    ## Vector de longitud n (puede contener valores NA)
                   , remove_na = FALSE){     ## Remover valores NA: Falso por defecto
  

  ## Si se deja la condicional de remover NA se evalua el vector y se filtran los valores NA,
  ## dejandolos fuera
  if(remove_na == TRUE){
    vector <- vector[!is.na(vector)]
  }
  
  mean_value  <- mean(vector)  # Funcion aplicará para media de vector mayores a 0, si la media estimada es 0 se cae la funcion
  sd_value    <- sd(vector)
  cv_value    <- (sd_value/mean_value)*100   # Expresar resultado en %
 
  return(cv_value) 
}

vect1 <- c(1, 2, 3, 7, 5)
get_cv(vect1)  

vect2 <- c(10, 20, 30, NA, 50)
get_cv(vect2, remove_na = TRUE) 


# Ejercicio 2: Funcion construir direccion -------------------------------------------
build_address <- function(street
                          , number
                          , apartment = NULL){

  ## Calle
  calle <- str_to_lower(street) %>% 
    str_replace(., "(?i)^(av\\.|calle|avenida|pasaje)\\s+", "")

  ## Numeracion 
  numero <- number %>% 
    str_remove(., pattern = "\\.") %>%  # remueve puntuacion si existiese
    str_split(., pattern = "\\s") %>% 
    unlist() %>% 
    str_subset(., "[:digit:]")
  
  # Construir la dirección base
  dir_base <- paste(calle, numero,  sep = " ")
  
  if (!is.null(apartment)) {
    
    ## Departamento
    depto <- apartment %>% 
      str_remove(., pattern = "\\.") %>%  # remueve puntuacion si existiese
      str_split(., pattern = "\\s") %>% 
      unlist() %>% 
      str_subset(., "[:digit:]")
    
    depto <- paste("depto.", depto)
    dir <- paste(dir_base, depto, sep = ", ")
    
  } else {
    
    dir <- dir_base
  }
  
  return(dir)
}
  


# Ejercicio 3: Aplicar funcion de construir direccion ---------------------
df <- tribble(~calle,              ~numero,      ~depto,
              "calle Hemingway",   "num 345",    "depto. 345",
              "av. Albert Camus",  "número 123", "123",
              "Manuel Rojas",      "234",        "departamento 231",
              "Nicanor Parra",     "678",        NULL
) 
df %>% 
  rowwise() %>% 
  mutate(direccion = build_address(street = calle, number = numero, apartment = depto))


