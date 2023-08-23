
# EJERCICIO 1 -------------------------------------------------------------

library(tidyverse)
library(haven)
library(stringr)

#Base en: https://observatorio.ministeriodesarrollosocial.gob.cl
#/storage/docs/casen/2020/Casen_en_Pandemia_2020_STATA_revisada2022_09.dta.zip

Casen_2020_STATA <- read_dta("~/Curso_R/R_intermedio/clase_4/Base/Casen_2020_STATA.dta")

#Según libro códigos: https://observatorio.ministeriodesarrollosocial.gob.cl
#/storage/docs/casen/2020/Libro_de_codigos_Base_de_Datos_Casen_en_Pandemia_2020.pdf

modulo_o <- select(Casen_2020_STATA, o1:o36)
modulo_v <- select(Casen_2020_STATA, v1:v29)

tab_ocu_viv <- cbind(modulo_o, modulo_v)       #Se obtienen 185437 obs. de 52 variables


#Utilizando expresiones regulares

#Se define expresión con ayuda de ChatGPT

def_regex <- "\\b(?:o[\\d_a-zA-Z]*|oficio[\\d_]+[0-9a-zA-Z]*|rama[\\d_a-zA-Z]*|v[\\d_a-zA-Z]+)\\b"


tab_ocu_viv_regex <- Casen_2020_STATA %>% 
  select(matches(def_regex))                   #Se obtienen 185437 obs. de 52 variables




# EJERCICIO 2 -------------------------------------------------------------

library(tm)
library(tidyverse)

###Función que procesa cada glosa
procesa_glosa <- function(text) {
  # Poner en minúscula
  text <- tolower(text)
  
  # Remover signos de puntuación y caracteres especiales
  text <- gsub("[[:punct:]]", " ", text)
  
  # Remover números
  text <- gsub("[[:digit:]]", "", text)
  
  # Extraer espacios adicionales
  text <- gsub("\\s+", " ", text)
  
  # Extraer palabras de 3 o menos letras
  ###probado de muchas formas y no funciona###
  #text <- gsub("\\s[A-Za-z]{1,3}]\\s", "", text)
  #text <- gsub("^[A-Za-z]{1,3}\\s", "", text)
  #text <- gsub("\\s[A-Za-z]{1,3}$", "", text)
  
  # Crear una lista de stopwords
  stopwords <- stopwords("es")
  
  # Dividir el texto en palabras
  words <- unlist(strsplit(text, "\\s+"))
  
  # Remover stopwords
  filtered_words <- words[!(words %in% stopwords)]
  
  # Reconstruir el texto procesado
  text_procesado <- paste(filtered_words, collapse = " ")     
  
  return(text_procesado)
}

###Función que recibe como argumentos una base y una variable a procesar
procesar_var <- function(base, var) {
  result_proceso <- base %>%
    rowwise() %>% 
    mutate(procesada  = procesa_glosa({{ var }})) %>% 
    return(result_proceso)
}






###  Uso de la función procesar_var ###

result_proceso_var_o24 <- procesar_var(Casen_2020_STATA, o24)

ver_resultado <- result_proceso_var_o24 %>% 
  select(o24, procesada)

print(ver_resultado, n = 20)

# A tibble: 185,437 × 2
# Rowwise: 
###o24                                          procesada   
##<chr>                                        <chr>                               
##1 "VENDEDOR(A) DE MAQUILLAJE EN LA FERIA"      "vendedor maquillaje feria"         
##2 ""                                           ""                                  
##3 "HOTELERÍA"                                  "hotelería"                         
##4 ""                                           ""                                  
##5 "HACE PASTELERIA"                            "hace pasteleria"                   
##6 ""                                           ""                                  
##7 "AEROPUERTO"                                 "aeropuerto"                        
##8 ""                                           ""                                  
##9 ""                                           ""                                  
##10 ""                                           ""                                  
##11 "REPRODUCCIÓN DE HARINA DE PESCADO Y ACEITE" "reproducción harina pescado aceite"
##12 "RESTAURANTE"                                "restaurante"                       
##13 ""                                           ""                                  
##14 ""                                           ""                                  
##15 "CONSTRUCCIÓN DE CASA"                       "construcción casa"                 
##16 "CASA PARTICULAR"                            "casa particular"                   
##17 ""                                           ""                                  
##18 ""                                           ""                                  
##19 ""                                           ""                                  
##20 "VENTAS DE ART. ARTESANALES"                 "ventas art artesanales"            
# ℹ 185,417 more rows







