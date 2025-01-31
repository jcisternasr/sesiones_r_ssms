#install.packages('pacman')

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
# Definir la ruta del archivo
ruta <- here('sesion_3',"data","data_le_anonima.xlsx")
# Importar el archivo
data <- import(ruta) 
# Revisar columnas
names(data)
# Revisar los datos
dfSummary(data)

# Mismo formato de las columnas
data <- data %>%
  clean_names()

# importar base deis
ruta <- here('sesion_3',"data","deis2024.xlsx")

deis <- import(ruta) %>%
  clean_names() %>%
  select(codigo_vigente, nombre_oficial)  %>% 
  mutate(
    codigo_vigente = as.character(codigo_vigente)
  )

# cruzar los datos de LE y DEIS 
data <- data %>%
  left_join(deis, by=c('estab_orig'='codigo_vigente'))
  



