# Importar librerías 
pacman::p_load(
  readxl, #importar archivos excel
  tidyverse, #manejo de datos
  here,       # directorios relativos
  janitor,    # limpiar dataframes
  lubridate,  # trabajar con fechas
  labelled,    # añadir 
  summarytools,
  rio
)

# Importar datos DEIS
ruta2 <- here('sesion_3',"data","deis2024.xlsx")
deis <- import(ruta2) %>%
  clean_names() %>%
  select(codigo_vigente, nombre_oficial)  %>% 
  mutate(
    codigo_vigente = as.character(codigo_vigente)
  )

#Importar datos de LE 
ruta <- here('sesion_3',"data","data_le_anonima.xlsx")
data <- import(ruta) %>%
  clean_names() %>% 
  left_join(deis, by=c('estab_orig'='codigo_vigente'))
  



