library(dplyr)

#Tarea 

#Actividad 1: Construye una función llamada get_cv que calcule el coeficiente 
#de variación de un vector. get_cv debe recibir 2 parámetros



#Cree una función para realizar el cálculo del CV

calculo_cv <- function(x){
  mean_value <- mean(x)
  sd_value <- sd(x)
  
  cv <- (sd_value / mean_value) * 100
  
  return(cv)
}

#Código para obtener el  CV con o sin  NA

get_cv <- function(vector, remove_na = FALSE) {
  if (remove_na) {
    vector <- vector[!is.na(vector)]
  }
  
  x <- calculo_cv(vector)
  
  return(x)
}

#Ejemplos

cv_con_NA <- get_cv(c(10, 20, NA, 30, 40, NA, 50, 60))
cv_con_NA

cv_sin_NA <- get_cv(c(10, 20, NA, 30, 40, NA, 50, 60), remove_na = TRUE)
cv_sin_NA


#Actividad 2: 
#Crea una función llamada build_address que construya una dirección en base a algunos inputs, siguiendo un formato establecido. 
#build_address recibe 2 parámetros obligatorios y uno opcional.


#Cree una funcion para  limiar cada uno de los parámetros

#Función para limpiar number

limpiar_number <- function(number){
  number_patron <- "(numero|num|número|n)[[:space:]]*"
  number_correcto  <- gsub(number_patron, "", number, ignore.case = TRUE)
  return(number_correcto)
}

#Función para limpiar calle

limpiar_calle <- function(calle) {
  calle_patron <- "(avenida|av\\.|calle|pasaje)?[[:space:]]*([[:alpha:]]+[[:space:]]*[[:alpha:]]*)"
  calle_limpia <- gsub(calle_patron, "\\2", calle, ignore.case = TRUE)
  return(calle_limpia)
}


#Función para limpiar apartament

limpiar_depto <- function(depto) {
  depto_patron <- "(depto\\.|departamento)?[[:space:]]*([[:digit:]]+)"
  depto_limpio <- gsub(depto_patron, "depto. \\2", depto, ignore.case = TRUE)
  return(depto_limpio)
}


#Función principal que llama los 3 parámetros (2 obligatorios y uno opcional)

build_address <- function(street, number, apartment) {
  
  if (!is.null(apartment)) {
    apartment <- limpiar_depto(apartment)
    number <- limpiar_number(number)
    street <- limpiar_calle(street)
    full_address <- paste(street, number, apartment, sep = ", ")
  } else {
    number <- limpiar_number(number)
    street <- limpiar_calle(street)
    full_address <- paste(street, number, sep = " ")
  }
  
  return(full_address)
}


#Actividad 3:
#Utilice la función creada sobre el siguiente dataframe, generando una nueva columna llamada dirección.

df <- tribble(~calle,              ~numero,      ~depto,
              "calle Hemingway",   "num 345",    "depto. 345",
              "av. Albert Camus",  "número 123", "123",
              "Manuel Rojas",      "234",        "departamento 231",
              "Nicanor Parra",     "678",        NULL
) 
df


#nuevo df con la función aplicada.

df2 <- df %>%
  mutate(dirección = build_address(calle, numero, depto))

df2 
 
