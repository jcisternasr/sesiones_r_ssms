

# https://www.fonasa.cl/sites/fonasa/datos-abiertos/bases-grd


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


data <- read.csv2(here('data','GRD_PUBLICO_EXTERNO_2022.txt'), sep = '|', fileEncoding = 'UTF-16') 
#el archivo tenía otra codificación, por lo que tuve que importarlo con read.csv2
cie10 <- import(here('data','CIE-10.xlsx'))
cie9 <- import(here('data','CIE-9.xlsx'))
diccionario <- import(here('data','Diccionario.xlsx'))
hospitales <- import(here('data','Tablas maestras bases GRD.xlsx'),which=1) 
# en import(data,which=1) which es para elegir el nombre o el número de la hoja de Excel.
# completar con el resto de las hojas del Excel si se necesita*


## Actividad ##

# 1. Explorar los datos.

#por ejemplo

names(data) #nombres de las columnas
a <- head(data)
unique(data$SERVICIO_SALUD) #valores únicos en columnas
skimr::skim(data) #reporte en consola
DataExplorer::create_report(data) #reporte html

# 2. Generar una pregunta de investigación de los datos.
# 3. Obtener información de los datos.
# 4. Generar gráficos informativos. 