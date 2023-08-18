
# dependencias ------------------------------------------------------------


library(gapminder)
library(tidyverse)
library(purrr)


# ejercicio 1 --------------------------------------------------------------

sum_something <- function(df, var, group_var){
  x <- df %>% 
    group_by(pick({{group_var}})) %>% 
    summarise(n = sum({{var}}))
  
  return(x)
}


plot_table <- function(table, x_var, y_var,  input_title) {
  ggplot(table, aes(x = {{x_var}}, y ={{y_var}})) +
    geom_bar(stat = "identity") +
    labs(title = input_title)
}


gapminder_list <- split(gapminder, gapminder$year)


plot_with_purrr <- function(tablas){
  plots <- tablas %>% 
    map(~sum_something(., pop, continent)) %>%
    imap(~plot_table(.,x_var = continent, y_var = n,
                     input_title = glue::glue("Población mundial, según continente. Año {.y}")))
  return(plots)
}

plotsitos <- plot_with_purrr(gapminder_list)

# ejercicio 2 -------------------------------------------------------------


plot_table_new <- function(table, x_var, y_var,  input_title, input_subtitle) {
  ggplot(table, aes(x = {{x_var}}, y ={{y_var}}, fill = {{x_var}})) +
    geom_bar(stat = "identity") +
      labs(title =  input_title) +
      labs(subtitle =  input_subtitle)
}

plot_with_purrr2 <- function(tablas){
  
  plots <- tablas %>% 
    map(~ sum_something(., pop, continent)) %>%
    imap(~plot_table_new(., x_var = continent, y_var = n,
                     input_title = "Población mundial, según continente.",
                     input_subtitle = glue::glue("Año {.y}")))
  return(plots)
}

plotsitos_2 <- plot_with_purrr2(gapminder_list)


# ejercicio 3 -------------------------------------------------------------

nested_for <- function(v1, v2) {
  for (x in v1) {
    for (y in v2){
      print(paste(x, y))
    }
  }
}

nested_for(1:3, 5:8)

#mapeo

nested_map = function(v1, v2){
  
  map(v1, ~paste(.x, v2)) %>% 
    unlist()
}

nested_map(1:3, 5:8)
}



