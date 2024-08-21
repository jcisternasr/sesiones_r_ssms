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



# 5. Intro a visualización ----


############# Scatterplot ----

##################################################### #

ggplot(
  data = data_le_hslbp, #datos
  mapping = 
      aes( #estética (valores columna)
         x = edad_hoy, 
         y = t_espera)
  ) +
geom_point()

##################################################### #
  
ggplot(data = data_le_hslbp, mapping = aes(x = edad_hoy, y = t_espera))+  # establecer datos y ejes de mapeo
geom_point(
  color = "darkgreen", 
  size = 0.5, 
  alpha = 0.2)         # establecer la estética de los puntos estáticos

####################################################### #

ggplot(data = data_le_hslbp,   # establecer los datos
       mapping = aes(     # asignar la estética a los valores de la columna
         x = edad_hoy,           # asigna el eje-x a la edad             
         y = t_espera,         # asignar el eje-y al peso
         color = edad_sic,
         size = t_espera)     # asignar el color a la edad
        )+  
  geom_point()

######################################################### #
ggplot(data = data_le_hslbp,
       mapping = aes(           # asignar la estética a las columnas
         x = edad_sic,
         y = t_espera,
         color = edad_sic)) + 
  geom_point(                   # añadir puntos para cada fila de datos
    size = 1,
    alpha = 0.5) +  
  geom_smooth(                  # añadir una línea de tendencia  
    method = "lm",             # con método lineal
    size = 1)

###################################################### #

ggplot(data = data_le_hslbp,
       mapping = aes(x = edad_sic, y = t_espera, color = nivel_de_atencion)) #color como variable categórica
  +
  geom_point(alpha = 0.5)


############# Histogram ----

ggplot(
  data = data_le_hslbp, 
  mapping = aes(x = t_espera)) +
  geom_histogram()

#####################################-


ggplot(data = data_le_hslbp, mapping = aes(x = t_espera))+       # establecer datos y ejes
  geom_histogram(              # mostrar histograma
    binwidth = 10,                # anchura de los bins (cuadrados)
    color = "red",               # color de la línea del bin
    fill = "blue",               # color del interior del bin
    alpha = 0.1)                 # transparencia del bin


#Incorporar facetas

#histograma
ggplot(data_le_hslbp, aes(x = t_espera)) +
  geom_histogram(              # mostrar histograma
    binwidth = 10,                # anchura de los bins (cuadrados)
    color = "red",               # color de la línea del bin
    fill = "blue",               # color del interior del bin
    alpha = 0.1) +                # transparencia del bin
  theme_minimal()+                              # simplificar los paneles de fondo
  labs(                                         # añadir al gráfico etiquetas, título, etc.
    x = "Tiempo de espera",
    y = "N° SIC",
    title = "Tiempo de espera por nivel de atención") +
  facet_wrap(~nivel_de_atencion)                       # se crean las facetas

#scatterplot
ggplot(data = data_procesada,
       mapping = aes(           # asignar la estética a las columnas
         x = edad_sic,
         y = t_espera,
         color = edad_sic)) + 
  geom_point(                   # añadir puntos para cada fila de datos
    size = 1,
    alpha = 0.5) +  
  geom_smooth(                  # añadir una línea de tendencia  
    method = "lm",             # con método lineal
    size = 1) +
  facet_wrap(~nivel_de_atencion)


# todos estos gráficos dan la misma información. 

ggplot(data = data_le_hslbp, mapping = aes(x = edad_sic))+
  geom_histogram()

ggplot(data = data_le_hslbp)+
  geom_histogram(mapping = aes(x = edad_sic))

ggplot()+
  geom_histogram(data = data_le_hslbp, mapping = aes(x = edad_sic))


# 6. Ejercicios ----

#### 1) Generar una pregunta de investigación respecto de los datos
          # hospital/es
          # especialidad/es
          # diagnósticos
          # edades
          # niveles de atención
          # etc
#### 2) Procesar los datos para obtenerla y generar las tablas necesarias para 
#### 3) Generar las tablas necesarias mediante agrupación
#### 4) Generar un gráfico que permita visualizar la respuesta a la pregunta de investigación

  