###Tarea Clase 2

#Ejercicio 1: 
#El siguiente código genera un resultado muy similar al del último ejercicio 
#revisado en la clase. La diferencia es que la implementación es mediante un ciclo 
#for. Adicionalmente, se agrega una funcionalidad que agrega al título el año 
#correspondiente.

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
plots

#La tarea consiste en llegar al mismo resultado, pero utilizando únicamente las 
#herramientas de purrr. Crea una función llamada plot_with_purrr que reciba una 
#lista de tablas y devuelva una lista de gráficos

#Pista: La función imap puede ser de gran ayuda

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

plot_with_purr <- gapminder_list %>% 
  map(~sum_something (.x, continent, pop)) %>% 
  imap(~plot_table (.x, continent, n, paste("Población mundial, según continente. Año", {.y})))
print(plot_with_purrr)



#Ejercicio 2
plot_table_sub <- function(table, group_var, var,  titulo, sub) { 
  ggplot(table, aes(x = {{group_var}}, y = {{var}} )) + 
    geom_bar(stat = "identity") +
    labs(title = titulo, sub = sub)
}

plot_with_purr <- gapminder_list %>% 
  map(~sum_something (.x, continent, pop)) %>% 
  imap(~plot_table (.x, continent, n,"Problación mundial, según continente", {{.y}}))
print(plot_with_purrr)

#Ejercicio 3
nested_for <- function(v1, v2) {
  for (x in v1) {
    for (y in v2){
      print(paste(x, y))
    }
  }
}
nested_for(1:3, 5:8)


nested_map <- function (v1, v2) {
  map2(v1, v2, ~ sample(v1, v2, size=1))
  print(paste(v1, v2))
}

#se intenta con expand_grid

nested_map <- function(v1, v2) {
  grid <- expand_grid(v1 = v1, v2 = v2)
  map2(grid$v1, grid$v2, ~ print(paste(.x, .y)))
}


nested_map(1:3, 5:8)


