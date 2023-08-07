rm(list = ls(all.names = T), envir = environment())
cat("\014")
gc()
cat("\014")

# Tarea 1 R Intermedio ----------------------------------------------------

options(repos = c(CRAN = "http://cran.rstudio.com"))
options(scipen = 999)  

# Librerías ---------------------------------------------------------------

if (!require("tidyverse")) install.packages("tidyverse"); library(tidyverse)
if (!require("dplyr")) install.packages("dplyr"); library(dplyr)
if (!require("ggplot2"))  install.packages("ggplot2"); library(ggplot2)
if (!require("openxlsx"))  install.packages("openxlsx"); library(openxlsx)

# Ejercicio 1

# Construye una función llamada get_cv que calcule el coeficiente de variación de un vector. 
# get_cv debe recibir 2 parámetros: un vector de tamaño n y un valor boolean que indique si los valores NA deben ser removidos. 
# Por defecto, la función no debe remover dichos valores.
# En la construcción de la función no está permitido utilizar la función cv, pero sí mean y sd.

get_cv <- function(vector, remove_na = FALSE) {
  if (remove_na) {
    vector <- vector[!is.na(vector)]
  }
  
  n <- length(vector)
  
  if (n <= 1) {
    stop("El vector ingresado debe contener al menos dos valores para calcular el coeficiente de variación (CV).")
  }
  
  media_valores <- mean(vector, na.rm = TRUE)
  desv_valores <- sd(vector, na.rm = TRUE)
  
  coeficiente_variacion <- (desv_valores / media_valores) * 100
  
  return(coeficiente_variacion)
}


# Ejemplo de uso

Ejemplo_vector <- c(10, 15, 20, 25, 30)
Resultado_CV <- get_cv(Ejemplo_vector)
Resultado_CV

cat("El coeficiente de variación es:", Resultado_CV, "%\n")

Ejemplo_vector2 <- c(10, NA, 20, 25, 30)
Resultado2_CV <- get_cv(Ejemplo_vector2)
Resultado2_CV

cat("El coeficiente de variación es:", Resultado2_CV, "%\n")

# Ejercicio 2

# Crea una función llamada build_address que construya una dirección en base a algunos inputs, siguiendo un formato establecido. 
# build_address recibe 2 parámetros obligatorios y uno opcional.

# Parámetros obligatorios:

# street: string que contiene el nombre de la calle
# number: string que contiene el número de la dirección
# El parámetro opcional es apartment y contiene el número del departamento.


build_address <- function(street, number, apartment = NULL) {
  # Convierte nombre de calle y número a minúsculas
  street <- tolower(street)
  number <- tolower(number)
  
  # formato solicitado de dirección
  address <- paste(street, number, sep = " ")
  
  # Si se proporciona un número de departamento, agregarlo a la dirección
  if (!is.null(apartment)) {
    apartment <- tolower(apartment)
    address <- paste(address, apartment, sep = " ")
  }
  
  return(address)
}

# Ejemplo de uso 

street <- "calle Los Alerces"
number <- "número 123"
resultado <- build_address(street, number)
cat("Dirección:", resultado, "\n")

# ejemplo de uso, incorporando el número de depto

apartment <- "Dpto. 4"
resultado_n_depto <- build_address(street, number, apartment)
cat("Dirección con departamento:", resultado_n_depto, "\n")

# Ejercicio 3


# Dataframe df

df <- tribble(
  ~calle,              ~numero,      ~depto,
  "Olegario Lazo",   "num 689",    "depto. 13",
  "Av. Manso de Velasco",  "número 650", "10",
  "Manuel Rodriguez",      "687",        "departamento 1",
  "San Lucas",     "0338",        NA
)

# Aplicando la función build_address al dataframe "df" y creación de columna 'dirección'

df <- df %>%
  mutate(dirección = mapply(build_address, calle, numero, depto, SIMPLIFY = TRUE))

# Imprimir el dataframe resultante
print(df)



sumar_xyz <- function(x, y, z) {
  x + y + z
}
sumar_xyz(1, 2)
