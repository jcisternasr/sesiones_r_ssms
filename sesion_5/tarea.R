# 1. Importar librerías ----

pacman::p_load(
  tidyverse, #manejo de datos
  here,       # directorios relativos
  janitor,    # limpiar dataframes
  lubridate,  # trabajar con fechas
  summarytools,
  rio
)

# 2. Importar y sintetizar datos del DEIS ----

data_deis <- import(here("data","deis2024.xlsx")) %>% #importo datos
  clean_names() %>% #normalizo los nombres nombres de las columnas
  select(codigo_vigente, nombre_oficial, nombre_comuna, nivel_de_atencion, nombre_dependencia_jerarquica_seremi_servicio_de_salud)  %>% 
  mutate(codigo_vigente = as.character(codigo_vigente)) %>%
  filter(nombre_dependencia_jerarquica_seremi_servicio_de_salud == 'Servicio de Salud Metropolitano Sur')

# 3. Importar datos especialidades ----

especialidades <- import(here("data","especialidades.xlsx")) %>%
  clean_names() %>%
  select(codigosigte, desc820) %>%
  rename(
    codigo_sigte = codigosigte,
    desc_especialidad = desc820
  )

# 4. Importar datos de LE anonimizada ----

data_bruta <- import(here("data","data_le_anonima.xlsx"))

data_procesada <- data_bruta %>%
  clean_names() %>%  # limpio los nombres de las columnas
  left_join(data_deis %>%  #junto los dataframes para obtener el est dest
              select(codigo_vigente,
                     nombre_oficial), 
            by=c('estab_dest'='codigo_vigente')) %>%
  rename(establecimiento_destino = nombre_oficial) %>% # renombro para evitar duplicidad de nombres
  left_join(data_deis %>% #junto los dataframes para obtener el est orig, comuna, nivel de atención
              select(codigo_vigente,
                     nombre_oficial, 
                     nombre_comuna, 
                     nivel_de_atencion), 
            by=c('estab_orig'='codigo_vigente')) %>%
  rename(establecimiento_origen = nombre_oficial) %>% # renombro para evitar duplicidad de nombres
  left_join(especialidades, by=c('presta_min'='codigo_sigte')) %>% # combino con especialidades
  left_join( # combino con dataframe de abreviaciones
    import(here('data','abreviacion.xlsx')) %>% 
      clean_names() %>% 
      mutate(codigo_vigente = as.character(codigo_vigente)), 
    by=c('estab_dest'='codigo_vigente')) %>%
  rename('abrev_destino'='abreviacion') %>% 
  select(-presta_min,-comuna,-estab_orig,-estab_dest) %>%
  select(run, sexo, fecha_nac, 
         establecimiento_origen, nombre_comuna, nivel_de_atencion, f_entrada, sospecha_diag, 
         establecimiento_destino, abrev_destino, desc_especialidad, presta_est, 
         f_salida, c_salida ) 

data_le_hslbp <- data_procesada %>%
  filter(abrev_destino == 'HSLBP', is.na(c_salida)) %>%
  mutate(t_espera = round(interval(f_entrada,today()) / days(1), 1),
         edad_sic = round(interval(fecha_nac,f_entrada) / years(1), 1), #lubridate y redondear
         edad_hoy = round(interval(fecha_nac,today()) / years(1), 1)) #lubridate, today() y redondear)

rm(data_bruta, data_deis, especialidades) #elimino los datos que no usaré

