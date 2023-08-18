library(ggplot2)
library(purrr)
library(dplyr)
library(gapminder)


##EJERCICIO 1

gapminder_list <- split(gapminder, gapminder$year)

#Función sum_something
sum_something <- function(data, var, group_var){
  data %>%
    group_by({{group_var}}) %>%
    summarise(n = sum({{var}}))
}


#Función plot_table
plot_table <- function (table, x_var, y_var, input_title){
  p <- ggplot(table, aes(x = {{x_var}}, y = {{y_var}})) +
    geom_bar(stat = "identity") +
    labs(title = input_title)
  p
}

#Función plot_with_purr
plot_with_purrr <- gapminder_list %>%
  map(~sum_something(.x, var = pop, group_var = continent)) %>%
  imap(~plot_table(table = .x, continent, n,
                   glue::glue ("Población mundial, según continente, año {.y}")))
walk(plot_with_purrr, print)



# Ejercicio 2
# #Modifica la función plot_table para que el año del gráfico aparezca en el subtítulo y no en el título. La función modificada debería recibir un parámetro extra llamado subtitulo, que permita agregar el año al subtítulo del gráfico.
# #Una vez que hayas modificado tu función, utilízala dentro de plot_with_purrr. Cada gráfico debería tener el año correspondiente en el subtítulo.


plot_table_subtittle <- function (table, x_var, y_var, input_title, subtitulo){
  p <- ggplot(table, aes(x = {{x_var}}, y = {{y_var}})) +
    geom_bar(stat = "identity") +
    labs(title = input_title, subtitle = subtitulo)
  p
}

# Utilizando plot_table dentro de plot_with_purrr
plot_with_purrr_subs <- gapminder_list %>%
  map( ~sum_something(.x, var = pop, group_var = continent)) %>%
  imap(~plot_table_subtittle(table = .x, x_var = continent, y_var = n,
                             input_title = "Población mundial por continente",
                             subtitulo = glue::glue("Año {.y}")))
walk(plot_with_purrr_subs, print)



# Ejercicio 3
# 
# El ejercicio consiste en escribir una función llamada nested_map que utilice una sintaxis de purrr. La función debe recibir dos vectores numéricos (de igual o distinto largo) e imprimir pares de número.
# Es posible que la sintaxis llegue a ser un poco confusa. Reflexiona sobre la pertinencia de purrr para tareas de este tipo.

nested_map2 <- function(v1, v2) {
  map2_chr(v1, v2, ~ glue::glue("{.x} - {.y}"))
}

nested_map <- function(v1, v2) {
  if (length(v1) == length(v2)) {
    nested_map2(v1, v2)
  } else {
    print("Los vectores tienen diferente longitud")
  }
}


# Ejemplos de uso
nested_map(1:3, 5:8)
nested_map(1:3, 2:4)
