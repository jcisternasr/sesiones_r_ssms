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

# Columnas data_procesada
# [1] "run"                     [2]"sexo"                   
# [3] "fecha_nac"               [4]"presta_est"             
# [5] "f_entrada"               [6]"f_salida"               
# [7] "c_salida"                [8]"sospecha_diag"          
# [9] "establecimiento_destino" [10]"establecimiento_origen" 
# [11] "nombre_comuna"           [12]"nivel_de_atencion"      
# [13] "desc_especialidad"       [14]"abrev_destino"   
  

# 5. Agrupar y contar datos.----
# https://epirhandbook.com/es/new_pages/grouping.es.html 

# Agrupar por hospital y conocer los totales

group_hospital <- data_procesada %>% 
  group_by(abrev_destino,nivel_de_atencion) %>%
  tally()


# Agrupar por hospital y generar columnas nuevas

group_hospital <- data_procesada %>% 
  group_by(abrev_destino,
    estado = ifelse(is.na(c_salida),'abierta','cerrada')) %>%
  tally()

group_hospital <- data_procesada %>%
  count(nivel_de_atencion, estado = ifelse(is.na(c_salida),'abierta','cerrada'), f_entrada < '2022-01-01')

# Generar columnas para funciones específicas e intro al manejo del tiempo

data_procesada <-  data_procesada %>% 
  mutate(
    edad = round(interval(fecha_nac,f_entrada) / years(1), 1), #lubridate y redondear
    edad_hoy = round(interval(fecha_nac,today()) / years(1), 1),#lubridate, today() y redondear
  )

group_hospital <- data_procesada %>% 
  summarise(
    n_cases  = n(), #cuenta los totales del dataframe
    mean_age = mean(edad, na.rm=T),#obtengo la media
    max_age  = max(edad, na.rm=T),#máximos 
    min_age  = min(edad, na.rm=T),#mínimos
    n_pediatrico  = sum(edad < 15, na.rm=T))#suma según regla

revisar <- data_procesada %>%
  filter(edad < 0)


# 6. Ejercicios ----

#### a) Generar un dataframe con la LE abierta del HSLBP

data_hslbp <- data_procesada %>%
  filter(abrev_destino == 'HSLBP', is.na(c_salida))

#### b) Generar una columna con la edad en años y tiempo de espera en días
  
data_hslbp <- data_hslbp %>%
  mutate(
    edad = round(interval(fecha_nac,f_entrada) / years(1), 1),
    t_espera = interval(f_entrada,today())/days(1)
  )
  
  
#### d) Obtener el n de la LE según nivel de atención

group_nivel <- data_hslbp %>% 
  count(nivel_de_atencion)

#### e) Obtener el n de la LE según comuna de origen

group_comuna <- data_hslbp %>% 
  count(nivel_de_atencion, nombre_comuna)
#### f) Obtener los promedios de tiempo de espera por especialidad.



  
  