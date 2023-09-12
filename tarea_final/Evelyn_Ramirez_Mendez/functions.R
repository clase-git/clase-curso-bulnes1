

extract_names <- function(url){
  url %>% 
    str_extract(pattern="[a-z]{3}-[[:digit:]]{4}-{3}[a-z]{8}.[a-z]{3}")
}


download_esi_data <- function(url,file_name=extract_names(url),directory='data'){
  download.file(url,paste0(directory,"/",file_name))
}

read_esi_data <- function(ruta){
  data.table::fread(ruta)
}

fn_media_dt <- function(df){
  
  dt <- as.data.table(df)
  dt[,.(media_ingresos = mean(ing_t_p, na.rm = T)), by = .(version)]
}


