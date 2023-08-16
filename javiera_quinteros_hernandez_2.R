
# Tarea 2 -----------------------------------------------------------------

library(tidyverse);library(purrr)
library(gapminder); library(ggplot2)

# Ejercicio 1 -------------------------------------------------------------


gapminder_list <- split(gapminder, gapminder$year)

sum_something <- function(data, group_var, var){
  
  data %>% group_by({{group_var}}) %>% 
           summarise({{var}} := sum({{var}}))
  
}

tabla <- sum_something(gapminder, group_var = continent, var = pop)

plot_table <- function(data, x_var, y_var, titulo){
  
  tabla %>% ggplot(aes(x = {{x_var}}, y = {{y_var}})) +
            geom_bar(stat = "identity") +
            labs (title = titulo)
  
}


plot_with_purrr <- function(lista){
  
  plots_by_year <- lista %>% 
                   map(~sum_something(.x, group_var = continent, var = pop)) %>% 
                   imap(~plot_table(.x, continent, pop, 
                                    glue::glue ("Población mundial, según continente, año {.y}") ))
  
  return(plots_by_year)
}
plot_with_purrr(lista = gapminder_list)



# Ejercicio 2 -------------------------------------------------------------


plot_table <- function(data, x_var, y_var, titulo, subtitulo){
  
  tabla %>% ggplot(aes(x = {{x_var}}, y = {{y_var}})) +
            geom_bar(stat = "identity") +
            labs (title = titulo, subtitle = subtitulo)
  
}
  
# gapminder_list %>% imap(~plot_table(tabla, x_var=continent, y_var = pop, 
#                                     titulo = "Población mundial, según continente", 
#                                     subtitulo = paste0("año",{.y})))



plot_with_purrr <- function(lista){
  
  plots_by_year <- lista %>% 
                   map(~sum_something(.x, group_var = continent, var = pop)) %>% 
                   imap(~plot_table(.x, continent, pop, 
                                    glue::glue ("Población mundial, según continente"),
                                    glue::glue ("año {.y}")))
  
  return(plots_by_year)
}
plot_with_purrr(lista = gapminder_list)



# Ejercicio 3 -------------------------------------------------------------


nested_map <- function(v1, v2){
  
 x <- map2(v1, v2, ~print(paste0(.x, v2))) %>% 
      map2(v1, v2, ~print(paste0(v1, .y)))
  
}

nested_map(v1 = 1:3, v2 = 5:7)

# No logré hacerlo con vectores de distinto largo, me falta el 18, 28 y 38


