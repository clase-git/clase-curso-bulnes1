
# Nombre: Javiera Quinteros

# TAREA 1 -----------------------------------------------------------------
library(tidyverse)

# Ejercicio 1 -------------------------------------------------------------

# Construye una función llamada get_cv que calcule el coeficiente de variación de un vector. get_cv debe recibir 2 parámetros:
# un vector de tamaño n
# un valor boolean que indique si los valores NA deben ser removidos. Por defecto, la función no debe remover dichos valores.
# En la construcción de la función no está permitido utilizar la función cv, pero sí mean y sd
# La fórmula para calcular el coeficiente de variación es cv = sd/x

get_cv <- function(n){
  
  cv = sd(n)/mean(n)
  
  if(sum(is.na(n) == TRUE)>0) {
    message("Los valores NA deben ser removidos")
  }
  
  return(cv)
}

get_cv(n = c(25, 3, 16, 48, 77)) # 0.8639357
get_cv(n = c(25, NA, 16, 48, 77)) # Los valores NA deben ser removidos



# Ejercicio 2 -------------------------------------------------------------

# Crea una función llamada build_address que construya una dirección en base a algunos inputs, siguiendo un formato establecido.
# build_address recibe 2 parámetros obligatorios y uno opcional.
# Parámetros obligatorios:
# *street: string que contiene el nombre de la calle
# *number: string que contiene el número de la dirección
# El parámetro opcional es apartment y contiene el número del departamento.

build_address <- function(street, number, apartment){
  
street <- str_remove(street, "calle|avenida|pasaje|av?.")  
number <- str_extract(number, "[0-9]+")
apartment <- str_extract(apartment, "[0-9]+")

if(is.null(apartment)){
  direccion <- paste0(street, " ", number)
} else{
  direccion <- paste0(street, " ", number, ", ", "depto.", apartment)
}
return(direccion)
}

build_address(street = "pasaje Hemingway", number = "nro 555", apartment = NULL)

#(sum(is.null(apartment) == TRUE)>0)



# Ejercicio 3 -------------------------------------------------------------

# Utilice la función creada sobre el siguiente dataframe, generando una nueva columna llamada dirección.

df <- tribble(~calle,              ~numero,      ~depto,
              "calle Hemingway",   "num 345",    "depto. 345",
              "av. Albert Camus",  "número 123", "123",
              "Manuel Rojas",      "234",        "departamento 231",
              "Nicanor Parra",     "678",        NULL
) 
df


df <- df %>% 
      rowwise() %>% 
      mutate(direccion = build_address(street = calle, number = numero, apartment = depto)) %>% 
      ungroup() %>% 
      mutate(direccion = str_squish(direccion))
df



