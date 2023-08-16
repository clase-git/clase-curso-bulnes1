library(tidyverse)
library(stringr)

##ejercicio 1
get_cv<-function(n){
  eliminar=if(sum(is.na(n))>0){
    TRUE
  } else if (sum(is.na(n))==0){
    FALSE
  }
  eliminar=F
  cv=ifelse(is.na(sd(n, na.rm=eliminar)/mean(n,na.rm=eliminar)), "los valores NA deben ser removidos",sd(n, na.rm=eliminar)/mean(n,na.rm=eliminar))
  return(cv)
}
n = c(2, 400, 98, 33, 56)
n_2 = c(NA, NA, 22, 48, NA)
get_cv(n)
get_cv(n_2)

##ejercicio 2
build_addres<-function(street,number, apartment){
  street= str_remove_all(street, "avenida |calle |av. |pasaje ")
  number= str_extract_all(number, "\\d+")
  apartment=str_extract_all(apartment, "\\d+")
  direccion=paste(street,number,"depto.",apartment)
  return(direccion)}
build_addres(street, number, apartment)

##ejercicio 3
df <- tribble(~calle,              ~numero,      ~depto,
              "calle Hemingway",   "num 345",    "depto. 345",
              "av. Albert Camus",  "número 123", "123",
              "Manuel Rojas",      "234",        "departamento 231",
              "Nicanor Parra",     "678",        NULL
)
df
df<-df |>
  rowwise() |>
  mutate(Direccion=build_addres(calle,numero,depto))
View(df)
