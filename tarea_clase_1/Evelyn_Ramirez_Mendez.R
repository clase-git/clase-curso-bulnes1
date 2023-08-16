rm(list=ls())


# Ejercicio 1 ---------------------------------------------------------------------------------------------------------------

get_cv <- function(vector,boolean=F){
  if(boolean==TRUE){
    cv=sd(vector, na.rm = T)/mean(vector, na.rm = T)
    return(cv)
  } else if (boolean==FALSE)
  {cv=sd(vector)/mean(vector)
  return(cv)
  }
}

get_cv(c(1,2,4,50,60,30,NA,40,NA),T)


# Ejercicio 2 ---------------------------------------------------------------------------------------------------------------

library(stringr)

build_address <- function(street, number,apartment=NULL){
  street = str_remove(street,"calle|Calle|Av.|av.|Pasaje|psje|Psaje")
  number = str_extract_all(number,"\\d+")
  if (is.null(apartment) == T) {
    address = paste(street,number)
  } else if (is.null(apartment) == F){
    apartment = str_extract_all(apartment,"\\d+")
    address = paste(street,number,", depto",apartment)
  }
  return(address)

}
street <- "calle Los Alerces"
number <- "numero 123"
apartment <- "depRTAMETNTO 10"
build_address(street, number,apartment)
build_address(street, number)

# Ejercicio 3 ---------------------------------------------------------------------------------------------------------------

df <- tribble(~calle,              ~numero,      ~depto,
              "calle Hemingway",   "num 345",    "depto. 345",
              "av. Albert Camus",  "número 123", "123",
              "Manuel Rojas",      "234",        "departamento 231",
              "Nicanor Parra",     "678",        NULL
) 

df

df <- df %>% 
  rowwise() %>% 
  mutate(dirección=build_address(calle,numero,depto))



