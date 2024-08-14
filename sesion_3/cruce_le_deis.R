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

# Importar datos DEIS establecimiento de destino
ruta2 <- here('sesion_3',"data","deis2024.xlsx")
deis <- import(ruta2) %>%
  clean_names() %>%
  select(codigo_vigente, nombre_oficial)  %>% 
  mutate(
    codigo_vigente = as.character(codigo_vigente)
  )

# Importar datos DEIS para establecimiento de origen

deis_origen <- import(ruta2) %>%
  clean_names() %>%
  select(codigo_vigente, nombre_oficial, nombre_comuna)  %>% 
  mutate(
    codigo_vigente = as.character(codigo_vigente)
  )


# Importar datos especialidades
ruta3 <- here('sesion_3',"data","especialidades.xlsx")
esp <- import(ruta3) %>%
  clean_names() %>%
  select(codigosigte, desc820)

#Importar datos de LE 
ruta <- here('sesion_3',"data","data_le_anonima.xlsx")

data <- import(ruta)
data2 <- data %>%
  clean_names() %>% 
  left_join(deis, by=c('estab_dest'='codigo_vigente')) %>%
  left_join(esp, by=c('presta_min'='codigosigte')) %>%
  left_join(deis_origen, by=c('estab_orig'='codigo_vigente')) %>% 
  filter(nombre_comuna == 'Calera de Tango')
  



