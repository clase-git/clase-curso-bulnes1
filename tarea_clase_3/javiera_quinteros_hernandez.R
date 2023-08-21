
# Tarea 3 -----------------------------------------------------------------

# NO PUDE ENCONTRAR EL ARCHIVO QUE DICE LA PPT. ASÍ QUE USÉ EL .csv


library(tidyverse)
library(readr)
casen <- read_csv("data/casen.csv")

# library(feather)
# library(tidyverse)
# casen = read_feather("data/casen_2020_edit.feather")


# Ejercicio 1 -------------------------------------------------------------

seleccion <- casen %>% select(matches("^o\\d|oficio|rama|^v\\d"))

names(seleccion)


# Ejercicio 2 -------------------------------------------------------------

data <- casen %>% select(o9a, o9b, o24) %>% 
                  filter(!is.na(o9a))

limpia_glosas <- function(x){
  
str_to_lower(x) 
data %>% mutate(x = str_remove_all(x, "[:punct:]|\\d")) %>% 
         mutate(x = str_squish(x))
  
}

z <- map_df(data, limpia_glosas)






