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

ruta <- here('sesion_3',"data","data_le_anonima.xlsx")
dato <- import(ruta) 
