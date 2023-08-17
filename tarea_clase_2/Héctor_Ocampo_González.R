rm(list = ls(all.names = T), envir = environment())
cat("\014")
gc()
cat("\014")

# Tarea 2 Héctor Ocampo González ------------------------------------------

options(repos = c(CRAN = "http://cran.rstudio.com"))
options(scipen = 999) 

# Librerías ---------------------------------------------------------------

if (!require("tidyverse")) install.packages("tidyverse"); library(tidyverse)
if (!require("ggplot2"))  install.packages("ggplot2"); library(ggplot2)
if (!require("gapminder")) install.packages("gapminder"); library(gapminder)
if (!require("purrr")) install.packages("purrr"); library(purrr)

# Ejercicio 1 -------------------------------------------------------------

## El siguiente código genera un resultado muy similar al del último ejercicio revisado en la clase. 
## La diferencia es que la implementación es mediante un ciclo for. Adicionalmente, se agrega una funcionalidad 
## que agrega al título el año correspondiente.


sum_something <- function(data, group_var, var) {
  data %>% 
    group_by({{group_var}}) %>% 
    summarise(n = sum({{var}}))
}

plot_table <- function(table, group_var, var,  titulo) { 
  ggplot(table, aes(x = {{group_var}}, y = {{var}} )) + 
    geom_bar(stat = "identity") +
    labs(title = titulo)
}

gapminder_list <- split(gapminder, gapminder$year)

plot_with_for <- function(tablas){
  plots <- list(vector(length = length(tablas) ))
  i <- 1
  for (plot in tablas) {
    table <- sum_something(plot, continent, pop)
    plots[[i]] <- plot_table(table, continent, n, paste("Población mundial, según continente. Año", plot$year[1] )  )
    i <-  i + 1
  }
  return(plots)
}

plots <- plot_with_for(gapminder_list)

print(plots)

## La tarea consiste en llegar al mismo resultado, pero utilizando únicamente las herramientas de purrr. 
## Crea una función llamada plot_with_purrr que reciba una lista de tablas y devuelva una lista de gráficos

## Pista: La función imap puede ser de gran ayuda

# Solución ejercicio 1 ----------------------------------------------------

plot_with_purrr <- function(tablas) {
  plots <- tablas %>%
    imap(~ {
      a <- sum_something(.x, continent, pop)
      plot_title <- paste("Población mundial, según continente. Año", .y)
      plot_table(a, continent, n, plot_title)
    })
  return(plots)
}

plots <- plot_with_purrr(gapminder_list)

print(plots)

# Ejercicio 2 -------------------------------------------------------------

## Modifica la función plot_table para que el año del gráfico aparezca en el subtítulo y no en el título. 
## La función modificada debería recibir un parámetro extra llamado subtitulo, que permita agregar el año al subtítulo del gráfico.

## Una vez que hayas modificado tu función, utilízala dentro de plot_with_purrr. 
## Cada gráfico debería tener el año correspondiente en el subtítulo.

# Solución ejercicio 2 ----------------------------------------------------

plot_table <- function(table, group_var, var, titulo, subtitulo) {
  ggplot(table, aes(x = {{group_var}}, y = {{var}})) +
    geom_bar(stat = "identity") +
    labs(title = titulo, subtitle = subtitulo)
}

plot_with_purrr <- function(tablas) {
  plots <- tablas %>%
    imap(~ {
      a <- sum_something(.x, continent, pop)
      plot_title <- paste("Población mundial, según continente.")
      plot_subtitle <- paste("Año", .y)
      plot_table(a, continent, n, plot_title, plot_subtitle)
    })
  return(plots)
}

plots_modificado <- plot_with_purrr(gapminder_list)

print(plots_modificado)

# Ejercicio 3 -------------------------------------------------------------

## El siguiente for anidado genera pares de x e y. 
## El ejercicio consiste en escribir una función llamada nested_map que utilice una sintaxis de purrr. 
## La función debe recibir dos vectores numéricos (de igual o distinto largo) e imprimir pares de número.

## Es posible que la sintaxis llegue a ser un poco confusa. 
## Reflexiona sobre la pertinencia de purrr para tareas de este tipo.

nested_for <- function(v1, v2) {
  for (x in v1) {
    for (y in v2){
      print(paste(x, y))
    }
  }
}

nested_for(1:3, 5:8)


# Solución ejercicio 3 ----------------------------------------------------

## La pertinencia de purrr para tareas de tipo bucle anidados no es la más óptima; dado que purrr está más
## enfocado en operaciones funcionales y manejo de datos. Esto último también provoca que el código utilizado
## sea menos "elegante". 


nested_map <- function(v1, v2) {
  map2(v1, v2, ~ {
    map(v2, function(y) {
      paste(.x, y)
    })
  }) %>%
    unlist() %>%
    print()
}

nested_map(1:3, 5:8)




