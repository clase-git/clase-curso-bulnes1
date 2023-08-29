
# Tarea Sesion 3 Strings y Regex ------------------------------------------

# Estefanía Pizarro Arias


# Limpieza ambiente y carga de paquetes -----------------------------------
rm(list = ls())
graphics.off()

library(feather)
library(tidyverse)


# Cargar data -------------------------------------------------------------
casen <- read_feather("Data/casen_2020_edit.feather")


# Ejercicio 1 -------------------------------------------------------------
# Modulo ocupacion
mod_ocup <- casen %>% 
  dplyr::select(matches("^o"), contains("rama"))

# Modulo vivienda
mod_viv <- casen %>% 
  dplyr::select(matches("^v"))

# Ejercicio 2 -------------------------------------------------------------

fn_proceso_glosa <- function(df, var){
  
  df %>% 
    mutate(glo_procesada = str_to_lower({{var}}),
           glo_procesada = str_remove_all(glo_procesada, pattern = "[:punct:]"),
           glo_procesada = str_remove_all(glo_procesada, pattern = "[:digit:]"),
           glo_procesada = str_replace_all(glo_procesada, pattern = "\\s+", " "),
           glo_procesada = corpus(glo_procesada),
           glo_procesada = removeWords(glo_procesada, stopwords("spanish")),
           glo_procesada = as.character(glo_procesada),
           glo_procesada = iconv(glo_procesada, to = "ASCII//TRANSLIT"))
}

casen <- casen %>% dplyr::select(id_vivienda, folio, id_persona, o9a, o9b, o24)

fn_proceso_glosa(df = casen, var = o9b)
