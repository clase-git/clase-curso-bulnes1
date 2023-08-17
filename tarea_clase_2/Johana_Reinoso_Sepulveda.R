if (!require("tidyverse")) install.packages("tidyverse")
if (!require("purrr")) install.packages("purrr")


library(gapminder)
library(tidyverse)
library(purrr)


### Ejercicio 1 ###


gapminder_list <- split(gapminder, gapminder$year)


plot_table<-function(table, x_var, y_var,  input_title ) {
  ggplot(table, aes(x = !!enexpr(x_var), y = !!enexpr(y_var) )) +
    geom_bar(stat = "identity") +
    labs(title = input_title)
}


sum_something<-function(data, group_var, var) {
  data %>% 
    group_by(!!enexpr(group_var)) %>% 
    summarise(n = sum(!!enexpr(var)))
}



plots_by_year <- gapminder_list %>% 
  map(~sum_something(.x, continent, pop)) %>% 
  imap(~plot_table(.x, continent, n,
                   glue::glue("Población mundial, según continente,{.y}" ))) 



plots_by_yea


### Ejercicio 2  ###  



plot_table2<-function(table, x_var, y_var,  input_title,input_subtitle ) {
  ggplot(table, aes(x = !!enexpr(x_var), y = !!enexpr(y_var) )) +
    geom_bar(stat = "identity") +
    labs(title =  input_title) +
    labs(subtitle =  input_subtitle)
}



plots_by_year <- gapminder_list %>% 
  map(~sum_something(.x, continent, pop)) %>% 
  imap(~plot_table2(.x, continent, n,
                    glue::glue("Población mundial, según continente" ),
                    glue::glue("{.y}" )
  ))



plots_by_year





### Ejercicio 3 ###


nested_map <- function(v1, v2) {
  combinacion<- expand_grid(v1 = v1, v2 = v2)
  map2(combinacion$v1, combinacion$v2, ~ print(paste(.x, .y)))
}


nested_map(1:3, 5:8)