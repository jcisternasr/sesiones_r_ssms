pacman::p_load(
  tidyverse, #manejo de datos
  here,       # directorios relativos
  janitor,    # limpiar dataframes
  lubridate,  # trabajar con fechas
  summarytools,
  rio
)

# 2. Importar y sintetizar datos del DEIS ----

comunas_censo <- import(here("data_censo","Censo2017_Identificación_Geográfica","Microdato_Censo2017-Comunas.csv")) %>% 
  clean_names() %>%
  filter(
    str_detect(nom_comuna, 'BUIN|PAINE|CALERA DE TANGO|SAN BERNARDO|EL BOSQUE|LO ESPEJO|LA CISTERNA|SAN JOAQU|LA GRANJA|SAN MIGUEL|PEDRO AGUIRRE CERDA')
    )
         
data_censo <- import(here("data_censo","Censo2017_manzanas.csv")) %>% clean_names() %>%
  left_join(comunas_censo, by=comuna)
