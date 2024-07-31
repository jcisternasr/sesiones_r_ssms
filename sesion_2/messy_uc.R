install.packages('pacman')

#### Importar librerías ----

pacman::p_load(
  readxl, #importar archivos excel
  tidyverse, #manejo de datos
  here,       # directorios relativos
  janitor,    # limpiar dataframes
  lubridate,  # trabajar con fechas
  labelled,    # añadir 
  summarytools
)

#### Importar archivos ----
data <- read_excel(
  path = here('sesion_2','data','messy_uc.xlsx'),
  sheet = "Data",
  skip = 5
  )

df_dictionary <- read_excel(
  path = here("sesion_2","data", "messy_uc.xlsx"),
  sheet = "Data_Dictionary"
)


#### Explorar dataframe ----

dfSummary(data)
names(data) # conocer los nombres de las columnas 

#### Crear diccionario de variables ----

vec_variables <- df_dictionary %>%  
  select(Variable, Details) %>% 
  deframe()

#### Limpiar data ----

clean_data <- data %>%
  #limpiar nombres de columnas
  janitor::clean_names() %>%
  #eliminar filas y columnas
  janitor::remove_empty(which = c('rows', 'cols')) 

unique(clean_data$ethnic)


clean_data <- data %>%
  #limpiar nombres de columnas
  janitor::clean_names() %>%
  #eliminar filas y columnas
  janitor::remove_empty(which = c('rows', 'cols')) %>%
  #crear columnas 
  mutate(
    #unificar valores
    ethnic_clean = case_when(
      ethnic %in%  c("hispanic", "Hispanic", "hispamnic") ~ "Hispanic",
      ethnic %in%  c("NOT hispanic", "not hispanic") ~ "Not hispanic",
      .default = ethnic), 
    #manejar valores erróneos
    end_na_clean = na_if(end_na,-99),
    #caracteres en columna numérica
    end_emo_clean = na_if(end_emo, 'not done') %>% as.numeric(),
    #convertir a fechas
    start_date_clean = janitor::convert_to_date(start_date),
    #extraer números
    start_na_clean = parse_number(start_na)) %>% 
  #separar según el delimitador y crear columnas
  separate_wider_delim(
    start_bp, 
    delim ="/", 
    names = c("bp_systolic", "bp_diastolic"), cols_remove = FALSE) %>% 
  mutate(across(c(bp_systolic, bp_diastolic), as.numeric)) %>%
  #agregar etiqueta a las columnas seleccionadas
  labelled::set_variable_labels(!!!vec_variables, .strict = FALSE) %>% 	 	
  labelled::set_variable_labels(
    ethnic_clean = "Ethnicity",
    start_na_clean = "Sodium level in serum at start",
    end_na_clean = "Sodium level in serum at end",
    end_emo_clean = "Emotional symptom score at end",
    bp_systolic = "Systolic blood pressure",
    bp_diastolic = "Diastolic blood pressure"
  )

# Mirar columnas con cambios
clean_data_corto <- clean_data %>% 
  select(pat_id, start_na_clean, start_na_clean, pre_post_wt_kg, start_emo)
