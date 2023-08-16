
# Tarea Sesion 2: Funcionales ---------------------------------------------

## Estefanía Pizarro Arias; eapizarroa@ine.gob.cl


# Limpiar ambiente y cargar paquetes --------------------------------------

rm(list = ls())
graphics.off()

pacman::p_load(tidyverse,
               gapminder)


# Ejercicio 1 -------------------------------------------------------------

## Se toma la funcion sum_something creada en la clase
sum_something <- function(df, var, group_var){
  
  x <- df %>% 
    group_by(pick({{group_var}})) %>% 
    summarise(n = sum({{var}}))
  
  return(x)
}

## Se toma la funcion plot_table creada en la clase
plot_table <- function(table, x_var, y_var,  input_title) {
  ggplot(table, aes(x = {{x_var}}, y ={{y_var}})) +
    geom_bar(stat = "identity") +
    labs(title = input_title)
}

## Ejemplo de funcion usando for
gapminder_list <- split(gapminder, gapminder$year)
plot_with_for <- function(tablas){
  plots <- list(vector(length = length(tablas) ))
  i <- 1
  for (plot in tablas) {
    table <- sum_something(plot, pop, continent)
    plots[[i]] <- plot_table(table, continent, n, paste("Población mundial, según continente. Año", plot$year[1] )  )
    i <-  i + 1
  }
  return(plots)
}

plots <- plot_with_for(gapminder_list)


## Funcion con estructura purrr (tarea)
plot_with_purrr <- function(tablas){
  
  plots <- tablas %>% 
    map(~ sum_something(. , pop, continent)) %>% ## usar virgulilla permite sintaxis .X
    imap(~plot_table(.,x_var = continent, y_var = n,
                     input_title = glue::glue("Población mundial, según continente, año {.y}") ))
  return(plots)
}

plots_purrr <- plot_with_purrr(gapminder_list)


# Ejercicio 2 -------------------------------------------------------------
## Modificar funcion plot_table
plot_table <- function(table, x_var, y_var,  input_title, input_subtitle) {
  ggplot(table, aes(x = {{x_var}}, y ={{y_var}}, fill = {{x_var}})) +
    geom_bar(stat = "identity") +
    
    labs(title = input_title,
         subtitle = input_subtitle)
}

## Aplicar nueva funcion a plot_with_purrr
plot_with_purrr <- function(tablas){
  
  plots <- tablas %>% 
    map(~ sum_something(. , pop, continent)) %>% ## usar virgulilla permite sintaxis .X
    imap(~plot_table(.,x_var = continent, y_var = n,
                     input_title = "Población mundial, según continente",
                     input_subtitle = glue::glue("Año {.y}")))
  return(plots)
}

plots_purrr_mod <- plot_with_purrr(gapminder_list)


# Ejercicio 3 --------------------------------------------------------------

nested_for <- function(v1, v2) {
  for (x in v1) {
    for (y in v2){
      print(paste(x, y))
    }
  }
}
nested_for(1:3, 5:8)

## Funcion con map
nested_map = function(v1,v2){
  
  map(v1, ~paste(.x, v2)) %>% 
    unlist() 
}

nested_map(1:3, 5:8)


# Bonus -------------------------------------------------------------------

# Listar archivos en la carpeta
files <- list.files("Data/In/", full.names = T)
# Rescatar el numero del trimestre
trimestres <- list.files("Data/In/") %>%
  str_extract(pattern = "-[[:digit:]]{2}-") %>% 
  str_remove_all("-")

# vector para nombrar cada trimestre al leer
names <-  paste0("trimestre_", trimestres)  
# Leer en conjunto los archivos y asignarle nombre
ene2022 <-  map(files, read_delim) %>% set_names(names)

# Funcion requerida (ocupados y no ocupados sin expandir)
get_employment_sample <- function(df, var, group_var){
  
  x <- df %>% 
    # Filtrar por ocupados y desocupados
    filter({{var}} %in% c(1,2)) %>% 
    # Agrupar por ocupacion
    group_by(pick({{var}})) %>% 
    # Realizar conteo para ocupados y desocupados
    summarise(Conteo = n()) %>% 
    mutate(Categoria = case_when({{var}} == 1 ~ "Ocupado",
                                 TRUE ~ "Desocupado"))
  
  
}

# Aplicar funcion
cant_ocup <- ene2022 %>% map(~get_employment_sample(.x,activ))

# Funcion para graficar ocupados y no ocupados 
plot_table_ene <- function(table, x_var, y_var,  input_title, input_subtitle) {
  ggplot(table, aes(x = {{x_var}}, y ={{y_var}}, fill = {{x_var}})) +
    geom_bar(stat = "identity") +
    ylim(c(0,40000))+
    labs(title = input_title,
         subtitle = input_subtitle)
}

grafica <- cant_ocup %>% imap(~plot_table_ene(.,x_var = Categoria, y_var = Conteo,
                        input_title = "Cantidad de ocupados y no ocupados (sin expandir)",
                        input_subtitle = glue::glue("{.y} año 2022")))


# Ruta de directorio donde se guardarán los gráficos exportados
out <- "Data/Out/"

# Usar map para exportar cada gráfico de la lista como archivo PNG
map2(grafica, seq_along(grafica), function(grafico, indice) {
  nombre <- paste0("grafico_ocupado_desocupado_trimestre_", indice,"_2022", ".png")
  ggsave(filename = file.path(out, nombre), plot = grafico, width = 8,height = 8)
})

saveRDS(object = grafica,file = "Data/Out/Graficas_trimestrales.rds")
