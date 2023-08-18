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





####   B O N U S  ####
#Lo intenté pero me faltó tiempo 


install.packages("haven")
library(haven)

#Intenté con archivos .csv pero no me dió resultado

trimestre_files <- c("D:/Descargas/ene-2022-02-efm.dta", "D:/Descargas/ene-2022-05-amj.dta","D:/Descargas/ene-2022-08-jas.dta", "D:/Descargas/ene-2022-11-ond.dta")

trimestre_list <- map(trimestre_files, ~ read_dta(.x))

##Cree la función solicitada

get_employment_sample <- function(dataframe) {
  result <- dataframe %>%
    summarise(
      ocupados = sum(activ == 1, na.rm = TRUE),
      no_ocupados = sum(activ != 1, na.rm = TRUE)
    )
  return(result)
}

resultados_por_trimestre <- map(trimestre_list, get_employment_sample)


print(resultados_por_trimestre)
