
# EJERCICIO 1 -------------------------------------------------------------

library(gapminder)
library(ggplot2)
library(dplyr)
library(glue)
library(purrr)

#Función 1 recibe un dataframe, agrupa por variable y suma otra
sum_something <- function(df, group_var, var) {
  x <- df %>% 
    group_by({{group_var}}) %>% 
    summarise(n = sum({{var}}))
  return(x)
}
#Función 2 para imprimir gráficos
plot_table <- function(table, x_var, y_var, input_title) {
  p <- ggplot(table, aes(x = {{x_var}}, y = {{y_var}})) +
    geom_bar(stat = 'identity') +
    labs(title = input_title)
  print(p)
}

#Dividir en tablas por año (columna year, debe existir)
gapminder_list <- gapminder %>% 
  split(.$year)

#Función recibir tablas y aplicar funciones 1 y 2
plot_with_purrr <- function(tablas, group_var, sum_var, title_var) {
  plots <- map(tablas, function(plot) {
    table <- sum_something(plot, {{group_var}}, {{sum_var}})
    plot_table(table, {{group_var}}, n, 
               glue::glue("{title_var} Año {plot$year[1]}"))
  })
  return(plots)
}


# Ejecutar plot_with_purrr en la lista de tablas gapminder_list
result_plots <- plot_with_purrr(gapminder_list, 
                                group_var = continent, 
                                sum_var = pop, 
                                title_var = "Población mundial, según continente. ")

# EJERCICIO 2 -------------------------------------------------------------

library(gapminder)
library(ggplot2)
library(dplyr)
library(glue)
library(purrr)

#Función 1 recibe un dataframe, agrupa por variable y suma otra
sum_something <- function(df, group_var, var) {
  x <- df %>% 
    group_by({{group_var}}) %>% 
    summarise(n = sum({{var}}))
  return(x)
}
#Función 2 para imprimir gráficos, con Subtítulos
plot_table <- function(table, x_var, y_var, input_title, subtitle) {
  p <- ggplot(table, aes(x = {{x_var}}, y = {{y_var}})) +
    geom_bar(stat = 'identity') +
    labs(title = input_title, subtitle = subtitle)
  print(p)
}

#Dividir en tablas por año (columna year, debe existir)
gapminder_list <- gapminder %>% 
  split(.$year)

#Función recibir tablas y aplicar funciones 1 y 2
plot_with_purrr <- function(tablas, group_var, sum_var, title_var) {
  plots <- map(tablas, function(plot) {
    table <- sum_something(plot, {{group_var}}, {{sum_var}})
    plot_table(table, {{group_var}}, n, {{title_var}}, 
               glue::glue("Año {plot$year[1]}"))
  })
  return(plots)
}


# Ejecutar plot_with_purrr en la lista de tablas gapminder_list
result_plots <- plot_with_purrr(gapminder_list, 
                                group_var = continent, 
                                sum_var = pop, 
                                title_var = "Población mundial, según continente. ")



# EJERCICIO 3 -------------------------------------------------------------

library(purrr)
library(tidyverse)

###Función que recibe dos vectores numéricos e imprime pares de números
nested_map <- function(x, y) {
  datalist <- expand.grid(x = x, y = y)           #Se generan todas las combinaciones
  datalist_res <- arrange(datalist, x) %>%        #Se ordena por x ascendente
    mutate(res = as.character(paste(x, !!y))) %>%     #Se crea columna con los pares
    as.list(select(res))                              # Se selecciona la nueva columna
  walk(datalist_res$res, ~ invisible(print(.)))     # Imprime igual que ""nested_for""
}


nested_map(1:3, 5:8)



# Bonus -------------------------------------------------------------------
library(tidyverse)
library(dplyr)
library(readxl)
library(janitor)
library(ggplot2)


#Carga tablas ocupados y desocupados 2022
tab_ocupados <- read_excel(path = "dataJL/ocupados.xlsx", sheet = "Hoja1")
tab_desocupados <- read_excel(path = "dataJL/desocupados.xlsx", sheet = "Hoja1")

#Renombra columnas
tab_ocupados <- tab_ocupados %>% 
  rename_all(~paste("ocu_", ., sep = ""))
tab_desocupados <- tab_desocupados %>% 
  rename_all(~paste("des_", ., sep = ""))

#Se unen los dataframes
tab_des_ocu <- cbind(tab_ocupados, tab_desocupados)

#se limpian los nombres
clean_tab_des_ocu <- tab_des_ocu %>% 
  clean_names()

#Crea columna de trimestres
build_trimestres <- function(ocu_region) {
  #cambios
  ocu_region <- str_replace(ocu_region, "2022 ene-mar", "T1 ene-mar")
  ocu_region <- str_replace(ocu_region, "2022 abr-jun", "T2 abr-jun")
  ocu_region <- str_replace(ocu_region, "2022 jul-sep", "T3 jul-sep")
  ocu_region <- str_replace(ocu_region, "2022 oct-dic", "T4 oct-dic")
  
}
clean_tab_des_ocu <- clean_tab_des_ocu %>%
  mutate(trimestres = build_trimestres(ocu_region))



# Transformar el dataframe de formato ancho a largo para ggplot
data_long <- pivot_longer(clean_tab_des_ocu, cols = c(ocu_total_pais, des_total_pais), names_to = "Empleo", values_to = "Total")

# Crear el gráfico utilizando ggplot2
p <- ggplot(data_long, aes(x = trimestres, y = Total, fill = Empleo)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total de Ocupados y Desocupados 2022", x = "Trimestres", y = "Total personas") +
  scale_fill_manual(values = c("ocu_total_pais" = "blue", "des_total_pais" = "pink"))

# Mostrar el gráfico
print(p)

