
# EJERCICIO 1 -------------------------------------------------------------

###Construir función get_cv que retorna coeficiente de variación de un vector
library(tidyverse)


#Función
get_cv <- function (vector_n, na.rm = F) {
  if(na.rm){
    vector_n <- vector_n[!is.na(vector_n)]
  }
  sd_vector_n <- sd(vector_n, na.rm = T)         #desviación estandar
  mean_vector_n <- mean(vector_n, na.rm = T)     #promedio
  c_v = (sd_vector_n/mean_vector_n)*100          #coeficiente de variación
  return(c_v)
}

#Prueba de funcionamiento
vector_prueba <- c(2, 4, 8, 16, 32, NA)

c_v_prueba <- get_cv(vector_prueba)
print(c_v_prueba)


# EJERCICIO 2 -------------------------------------------------------------

###Función que construya una dirección con un formato particular
library(tidyverse)

#Función
build_address <- function(street, number, apartment) {
  
  #todo a minúsculas, apartment corresponde a un número o NULL
  street <- tolower(street)
  number <- tolower(number)
  #elimina de la dirección palabras calle, avenida o pasaje
  street <- str_replace(street, "calle ", "")
  street <- str_replace(street, "avenida ", "")
  street <- str_replace(street, "pasaje ", "")
  #elimina de la dirección palabras variantes de lo anterior
  street <- str_replace(street, "av(|.) ", "")
  street <- str_replace(street, "avda(|.) ", "")
  street <- str_replace(street, "pje(|.) ", "")
  street <- str_replace(street, "psje(|.) ", "")
  
  #elimina del número de la dirección palabras número o numero
  number <- str_replace(number, "n(u|ú)mero ", "")
  #elimina del número de la dirección palabras variantes de lo anterior
  number <- str_replace(number, "n(u|ú)m(|.) ", "")
  number <- str_replace(number, "n(u|ú)(|.) ", "")
  number <- str_replace(number, "nro(|.) ", "")
  number <- str_replace(number, "n. ", "")
  number <- str_replace(number, "n ", "")
  number <- str_replace(number, "# ", "")
  number <- str_replace(number, "#", "")
  
  #En caso de número de departamento distinto de NULL
  if (!is.null(apartment)){
    
    address_p1 <- paste(street, number, sep = " ")
    address_p2 <- paste(", depto.", apartment, sep = " ")
    address <- paste0(address_p1, address_p2)
  }
  
  else {
    
    #da formato a dirección sin ingreso de número de departamento
    
    address  <- paste(street, number, sep = " ")
  }
  
  
  return(address)
}

#Prueba de funcionamiento sin ingreso de número de departamento
street <- "psje Los Cardenales"
number <- "Núm 2215"
apartment <- NULL                #INPUT OPCIONAL sólo números

prueba <- build_address(street, number, apartment)
print(prueba)

#Prueba de funcionamiento con ingreso de número de departamento
street <- "AVda. PEDRO AGUIRRE CERDA"
number <- "Nro #0256"
apartment <- 501                #INPUT OPCIONAL sólo números

prueba <- build_address(street, number, apartment)
print(prueba)



# EJERCICIO 3 -------------------------------------------------------------

###PARA SIGUIENTE DATAFRAME, GENERAR COLUMNA DIRECCIÓN,
###MEDIANTE FUNCION EJERCICIO ANTERIOR
library(tidyverse)

df <- tribble(~calle, ~numero, ~depto,
              "calle Hemingway", "num 345", "345",
              "av. Albert Camus", "número 123", "123",
              "Manuel Rojas", "234", "231",
              "Nicanor Parra", "678", NULL
)


# Aplicamos la función build_address al dataframe df y creamos la columna "dirección"

df_addresse <- df %>%
  mutate(dirección = build_address(calle, numero, depto))

# Mostramos el dataframe con la nueva columna "dirección"
print(df_addresse)


#Función arreglo de dirección por casos NULL
arreglo_address <- function(street, number, apartment, dirección) {
  
  dirección <- str_replace(dirección, ", depto. NULL", "")
  
}

df_addresse_arreglada <- df_addresse %>% 
  mutate(dirección_arreglada = arreglo_address(street, number, apartment, dirección))

print(df_addresse_arreglada)