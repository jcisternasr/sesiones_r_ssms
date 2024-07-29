pacman::p_load(
  rio, # importar varios tipos de archivo
  tidyverse, #manejo de datos
  here,       # directorios relativos
  janitor,    # limpiar dataframes
  lubridate,  # trabajar con fechas
  labelled,    # añadir 
  summarytools, # resumen del dataframe
  digest
)

#código para anonimizar ----
anonimizar_rut <- function(rut) {
  sapply(rut, function(x) digest(x, algo = "sha256"))
}
anonimizar_fecha <- function(fecha) {
  format(as.Date(fecha), "%Y-01-01")  # Mantener solo el año
}
data_le <- data_le %>%
  mutate(
    RUN = anonimizar_rut(RUN),
    FECHA_NAC = anonimizar_fecha(FECHA_NAC)
  )
rm(df_anonimizado)


#importo archivo ----
data_le <- import(here('sesion_3','data','data_le_anonima.xlsx')) %>%
  clean_names()

#miro columnas
names(data_le)
