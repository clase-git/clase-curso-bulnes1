#Primera Tarea

rm(list = ls())
library(tidyverse)
library(stringr)

#Ejercicio 1

get_cv <- function (n) {
  cv <- sd(n, na.rm=TRUE)/mean(n, na.rm = TRUE)
  return(cv) 
}

#Ejercicio 2
build_address  <- function (street, number, apartment) {
  street <- str_replace_all(street, "av.", "")
  street <- str_replace_all(street, "avenida", "")
  street <- str_replace_all(street, "Avenida", "")
  street <- str_replace_all(street, "Av.", "")
  street <- str_replace_all(street, "calle", "")
  streer <- str_replace_all(street, "Calle", "")
  number <- str_replace_all(number, "num", "")
  number <- str_replace_all(number, "numero", "")
  number <- str_replace_all(number, "número", "")
  apartment <- str_replace_all(apartment, "Depto", "")
  apartment <- str_replace_all(apartment, "depto.", "")
  apartment <- str_replace_all(apartment, "departamento", "")
  if (is.null(apartment)){
    address <- paste(street, number, sep = " ",)
  } else {
    address <- paste(street, number, "depto.", apartment, sep = " ")
  }
  return(address)
}

#Ejercicio 3

df <- tribble(~calle,              ~numero,      ~depto,
              "calle Hemingway",   "num 345",    "depto. 345",
              "av. Albert Camus",  "número 123", "123",
              "Manuel Rojas",      "234",        "departamento 231",
              "Nicanor Parra",     "678",        NULL
) 
View(df)

df <- df %>%  mutate (direccion = build_address (street = calle, number = numero, 
                                                 apartment = depto))
print(df)

