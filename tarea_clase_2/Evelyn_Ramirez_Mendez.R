rm(list=ls())

library(gapminder)
gapminder_list <- split(gapminder, gapminder$year)

# Parte del enunciado -------------------------------------------------------------------------------------------------------


# plot_with_for <- function(tablas){
#   plots <- list(vector(length = length(tablas) ))
#   i <- 1
#   for (plot in tablas) {
#     table <- sum_something(plot, continent, pop)
#     plots[[i]] <- plot_table(table, continent, n, paste("Población mundial, según continente. Año", plot$year[1] )  )
#     i <-  i + 1
#   }
#   return(plots)
# }
# 
# plots <- plot_with_for(gapminder_list)
# plots


# Ejercicios 1 ---------------------------------------------------------------------------------------------------------------------------

sum_something <- function(data,agrupar, var,titulo){
  data %>% 
    group_by({{agrupar}}) %>% 
    summarise(n=sum({{var}}))
}

plot_table <- function(table,x,y,titulo){
  table %>% 
    ggplot(aes(x={{x}}, y={{y}})) + geom_bar(stat='identity')+ labs(title = titulo)
}


plot_with_purr <- gapminder_list %>% 
  map(~sum_something (.x, continent, pop)) %>% 
  imap(~plot_table (.x, continent, n, paste("Población mundial, según continente. Año", {.y})))

plot_with_purr

# Ejercicio 2 ---------------------------------------------------------------------------------------------------------------

# Modificamos plot_table para poner año en subtitulo

plot_table <- function(table,x,y,titulo,subtitulo){
  table %>% 
    ggplot(aes(x={{x}}, y={{y}})) + geom_bar(stat='identity')+ labs(title = titulo, subtitle = subtitulo)
}

# probamos
plot_with_purr <- gapminder_list %>% 
  map(~sum_something (.x, continent, pop)) %>% 
  imap(~plot_table (.x, continent, n, paste("Población mundial, según continente."), paste('Año',{.y})))

plot_with_purr

# Ejercicio 3 ---------------------------------------------------------------------------------------------------------------

nested_for <- function(v1, v2) {
  for (x in v1) {
    for (y in v2){
      print(paste(x, y))
    }
  }
}
nested_for(1:3, 5:8)

# creamos nested_map

nested_map <- function (v1, v2) {
  as.tibble(map(v1, ~paste(.x,v2)) %>% 
    unlist())
}
nested_map(1:3,5:8)










