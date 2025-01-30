####################### Importo librerías ----

library('rio')
library('dplyr')
library('readxl')
library('writexl')
library('stringr')
library('lubridate')  
library('ggplot2') 
library('ggthemes')
library('ggsci')
library('scales')

####################### Importo los datos ########################################

data <- import('data/base_septiembre_2024.csv')
piv <- import('data/piv2024.xlsx')

####################### Manejo de la base de PIV 2024 ##################################

piv_comuna <- piv %>%
  group_by(Comuna) %>%
  summarize(n = sum(Inscritos))

names(piv_comuna) <- c('Comuna','PIV 2024')

rm(piv)


export(
  data %>% filter(`Tipo prestador` == 'Dental'),
  'telesalud_dental_junio2024.xlsx'
)



####################### Manejo de la base de telesalud 
columnas <- c('ID','Cesfam','Prioridad','Fecha solicitud','Dirección',
              'Tipo prestador','Motivo consulta','Especificidad',
              'Estado', 'Fecha cierre', 'Tipo cierre', 'Cargo',
              'Profesión', "Fecha agenda")

data1 <- data[columnas]
data1$`Fecha solicitud` <- as.Date(data1$`Fecha solicitud`, format='%d-%m-%Y %H:%M' )
data1$`Fecha cierre`  <- as.Date(data1$`Fecha cierre`, format='%d-%m-%Y %H:%M')
data1$`Fecha agenda`  <- as.Date(data1$`Fecha agenda`, format='%d-%m-%Y %H:%M')
rm(data,columnas)

data2024 <- data1 %>%
  filter(`Fecha solicitud` > '2023-12-31',`Fecha solicitud` < '2024-06-01')

####################### Agrego comuna por RE. ----
cond_list <- list(
  str_detect(data1$Cesfam, "(?i)Bajos|Inés|Tango"),
  str_detect(data1$Cesfam, "(?i)Sauces|Laura|Letelier|Cóndores|Tobar|Salcedo|Hay|Bosque"),
  str_detect(data1$Cesfam, "(?i)Montalva|Anselma"),
  str_detect(data1$Cesfam, "(?i)Gumucio"),
  str_detect(data1$Cesfam, "(?i)Estrella|Salgado|Acuña|Espejo"),
  str_detect(data1$Cesfam, "(?i)Moya|chada|Huelquén|Pintué|Rangue|Solar|Solar|Abrantes"),
  str_detect(data1$Cesfam, "(?i)Neghme|Miguel Enríquez|Valledor|Dubois|Villa|Pedro|Cerda|Fröedden|Feria"),
  str_detect(data1$Cesfam, "(?i)Peral"),
  str_detect(data1$Cesfam, "(?i)Hortensias|Herrera|Rapa|Ribera|Pino|Urzúa|Confraternidad|Manzano|Pablo|Joan|Brañes|Cuevas|Bernardo|eniste"),
  str_detect(data1$Cesfam, "(?i)Coñimo|Aravena|Martín|Reverendo|Sierra|Yalta|Goñi|San Joa|Ande"),
  str_detect(data1$Cesfam, "(?i)Barros Luco|Recreo|Atacama"),
  str_detect(data1$Cesfam, "(?i)Buin|García|Galindo|Jahuel|Maipo|Recurso|Morros|Valdivia|Viluco")
)

choice_list <- c('Calera De Tango', 'El Bosque', 'La Cisterna', 'La Granja', 'Lo Espejo', 'Paine',
                 'Pedro Aguirre Cerda', 'Puente alto', 'San Bernardo', 'San Joaquín', 'San Miguel','Buin')

data1 <- data1 %>%
  mutate(Comuna = case_when(
    cond_list[[1]] ~ choice_list[[1]],
    cond_list[[2]] ~ choice_list[[2]],
    cond_list[[3]] ~ choice_list[[3]],
    cond_list[[4]] ~ choice_list[[4]],
    cond_list[[5]] ~ choice_list[[5]],
    cond_list[[6]] ~ choice_list[[6]],
    cond_list[[7]] ~ choice_list[[7]],
    cond_list[[8]] ~ choice_list[[8]],
    cond_list[[9]] ~ choice_list[[9]],
    cond_list[[10]] ~ choice_list[[10]],
    cond_list[[11]] ~ choice_list[[11]],
    cond_list[[12]] ~ choice_list[[12]],
    TRUE ~ NA_character_
  )) 

rm(list = ls(pattern = "cond_list"))
rm(choice_list)
####################### Obtengo mes y año ----

data1 <- data1 %>%
  mutate(mes_sol = month(`Fecha solicitud`),
         ano_sol = year(`Fecha solicitud`),
         mes_cierre = month(`Fecha cierre`),
         ano_cierre = year(`Fecha cierre`),
         month_agenda = month(`Fecha agenda`),
         year_agenda = year(`Fecha agenda`)) 



data1$month_year_sol <- as.Date(paste(data1$ano_sol, data1$mes_sol, "1", sep = "-"))
data1$month_year_cierre <- as.Date(paste(data1$ano_cierre, data1$mes_cierre, "1", sep = "-"))
data1$month_year_agenda <- as.Date(paste(data1$year_agenda, data1$month_agenda, "1", sep = "-"))

last_sol_date <- max(data1$`Fecha solicitud`)
first_day_this_month <- floor_date(last_sol_date, unit = "month")
last_day_previous_month <- rollback(first_day_this_month)

data1 <- data1 %>%
  filter(`Fecha solicitud` < first_day_this_month) %>%
  select('ID','Comuna','Cesfam','Prioridad','Fecha solicitud',
         'Tipo prestador','Motivo consulta','Especificidad', 'Estado',
         'Fecha cierre', 'Tipo cierre','Cargo','Comuna', 'Profesión',
         'Fecha agenda', 'month_year_sol','month_year_cierre','month_year_agenda')


#######################  solicitudes mensuales  comuna y piv ##############

mensual_sol_comuna_piv <- data1 %>% 
  group_by(month_year_sol, Comuna) %>%
  summarize(n_sol = n()) %>%
  rename(date = month_year_sol) %>% 
  filter(date < as.Date(first_day_this_month)) %>%
  left_join(piv_comuna, by = "Comuna") %>%
  mutate(sol_per_capita = (n_sol / `PIV 2024`)*1000) 

######################  solicitudes mensuales totales y promedio - SSMS #########################

mensual_sol_ssms_2124 <- mensual_sol_comuna_piv %>%
  filter(date < as.Date(first_day_this_month)) %>%
  group_by(date) %>%
  summarize(total_sol = sum(n_sol),
            mean_sol = mean(n_sol, na.rm = TRUE),
            )

  
######################  cierres mensuales comuna #########################

mensual_cierre_comuna <- data1 %>%
  filter(!is.na(`Fecha cierre`)) %>% 
  group_by(month_year_cierre, Comuna) %>%
  summarize(n_cierre = n()) %>%
  rename(date = month_year_cierre) %>% 
  filter(date < as.Date(first_day_this_month))

mensual_proportion_comuna <- mensual_sol_comuna_piv %>%
  left_join(mensual_cierre_comuna, by = c("date", "Comuna")) %>%
  mutate(proportion = round((n_cierre / n_sol),2)) %>% 
  select(-`PIV 2024`,-`sol_per_capita`)  %>% 
  mutate(proportion = ifelse(is.na(proportion), 0, proportion)) %>%
  filter(date > as.Date('2022-12-31'))

mensual_proportion_ss <- mensual_sol_comuna_piv %>%
  left_join(mensual_cierre_comuna, by = c("date", "Comuna")) %>%
  mutate(proportion = round((n_cierre / n_sol*100),2)) %>% 
  select(-`PIV 2024`,-`sol_per_capita`)  %>% 
  mutate(proportion = ifelse(is.na(proportion), 0, proportion)) %>%
  filter(date > as.Date('2022-12-31'))

################# Proporción total año 2024 ##################

sol_ss_2024 <- data1 %>%
  filter((`Fecha solicitud` > as.Date('2023-12-31'))) %>%
  group_by(Comuna) %>%
  summarize(total = n())

cierre_ss_2024 <- data1 %>%
  filter((`Fecha cierre` > as.Date('2023-12-31')), !is.na(`Fecha cierre`)) %>%
  group_by(Comuna) %>%
  summarize(total = n())

proportion_ss_2024 <- sol_ss_2024 %>%
  left_join(cierre_ss_2024, by= 'Comuna') %>%
  rename(`Total solicitudes` = 'total.x',
         `Total cierres` = 'total.y') %>%
  mutate(`cie/sol` = round(`Total cierres`/`Total solicitudes`,2))


################# Proporción total por prestador  2024 ##################

### Médico

z_sol_medico_ss_2024 <- data1 %>%
  filter((`Fecha solicitud` > as.Date('2023-12-31')), `Tipo prestador` ==  'Medicina') %>%
  group_by(Comuna) %>%
  summarize(total = n())

z_cierre_medico_ss_2024 <- data1 %>%
  filter((`Fecha cierre` > as.Date('2023-12-31')), !is.na(`Fecha cierre`),
         `Tipo prestador` ==  'Medicina') %>%
  group_by(Comuna) %>%
  summarize(total = n())

z_proportion_medico_ss_2024 <- z_sol_medico_ss_2024 %>%
  left_join(z_cierre_medico_ss_2024, by= 'Comuna') %>%
  rename(`Total solicitudes` = 'total.x',
         `Total cierres` = 'total.y') %>%
  mutate(`cie/sol` = round(`Total cierres`/`Total solicitudes`,2))

## Matrona

z_sol_matrona_ss_2024 <- data1 %>%
  filter((`Fecha solicitud` > as.Date('2023-12-31')), `Tipo prestador` ==  'Matrona') %>%
  group_by(Comuna) %>%
  summarize(total = n())

z_cierre_matrona_ss_2024 <- data1 %>%
  filter((`Fecha cierre` > as.Date('2023-12-31')), !is.na(`Fecha cierre`),
         `Tipo prestador` ==  'Matrona') %>%
  group_by(Comuna) %>%
  summarize(total = n())

z_proportion_matrona_ss_2024 <- z_sol_matrona_ss_2024 %>%
  left_join(z_cierre_matrona_ss_2024, by= 'Comuna') %>%
  rename(`Total solicitudes` = 'total.x',
         `Total cierres` = 'total.y') %>%
  mutate(`cie/sol` = round(`Total cierres`/`Total solicitudes`,2))

## Dental

z_sol_dental_ss_2024 <- data1 %>%
  filter((`Fecha solicitud` > as.Date('2023-12-31')), `Tipo prestador` ==  'Dental') %>%
  group_by(Comuna) %>%
  summarize(total = n())

z_cierre_dental_ss_2024 <- data1 %>%
  filter((`Fecha cierre` > as.Date('2023-12-31')), !is.na(`Fecha cierre`),
         `Tipo prestador` ==  'Dental') %>%
  group_by(Comuna) %>%
  summarize(total = n())

z_proportion_dental_ss_2024 <- z_sol_dental_ss_2024 %>%
  left_join(z_cierre_dental_ss_2024, by= 'Comuna') %>%
  rename(`Total solicitudes` = 'total.x',
         `Total cierres` = 'total.y') %>%
  mutate(`cie/sol` = round(`Total cierres`/`Total solicitudes`,2))

## Juntar todo

z_proportion_prest_ss_2024 <- z_proportion_medico_ss_2024 %>%
  left_join(z_proportion_matrona_ss_2024, by='Comuna') %>%
  left_join(z_proportion_dental_ss_2024, by='Comuna') %>% 
  rename(`Cie/Sol Med.` = 'cie/sol.x',
         `Cie/Sol Mat.` = 'cie/sol.y',
         `Cie/Sol Dent.` = 'cie/sol',) %>%
  select('Comuna',`Cie/Sol Med.` , `Cie/Sol Mat.` , `Cie/Sol Dent.` )
  



#######################  solicitudes mensual por prestador #################

mensual_sol_prestador_comuna <- data1 %>% 
  group_by(month_year_sol, Comuna, `Tipo prestador`) %>%
  summarize(n_sol = n()) %>%
  rename(date = month_year_sol) %>% 
  filter(date < as.Date(first_day_this_month))

mensual_cierre_prestador_comuna <- data1 %>%
  filter(!is.na(`Fecha cierre`)) %>% 
  group_by(month_year_cierre, Comuna, `Tipo prestador`) %>%
  summarize(n_cierre = n()) %>%
  rename(date = month_year_cierre) %>% 
  filter(date < as.Date(first_day_this_month))

mensual_proportion_prestador_comuna <- mensual_sol_prestador_comuna %>%
  left_join(mensual_cierre_prestador_comuna, by = c("date", "Comuna", 'Tipo prestador')) %>%
  mutate(proportion = n_cierre / n_sol) %>%
  mutate(proportion = ifelse(is.na(proportion), 0, proportion)) %>%
  filter(date > as.Date('2022-12-31'))
  
#######################  proporcion por prestador ##########################################

mensual_proportion_prestador_comuna_medico <- mensual_proportion_prestador_comuna %>%
  mutate(proportion = ifelse(is.na(proportion), 0, proportion)) %>%
  filter(date > as.Date('2022-12-31')) %>% 
  filter(`Tipo prestador` == 'Medicina')

mensual_proportion_prestador_comuna_odonto <- mensual_proportion_prestador_comuna %>%
  mutate(proportion = ifelse(is.na(proportion), 0, proportion)) %>%
  filter(date > as.Date('2022-12-31')) %>% 
  filter(`Tipo prestador` == 'Dental')

mensual_proportion_prestador_comuna_matrona <- mensual_proportion_prestador_comuna %>%
  mutate(proportion = ifelse(is.na(proportion), 0, proportion)) %>%
  filter(date > as.Date('2022-12-31')) %>% 
  filter(`Tipo prestador` == 'Matrona')

#######################  tiempo de espera ######

comma <- function(x, big.mark = ".", decimal.mark = ",", ...) {
  format(as.numeric(x), big.mark = big.mark, decimal.mark = decimal.mark, ...)
}

data1$dias_al_cierre <- as.numeric(difftime(data1$`Fecha cierre`, data1$`Fecha solicitud`, units = "days"))
data1$dias_espera <- as.numeric(difftime(data1$`Fecha cierre`, last_day_previous_month, units = "days"))

promedio_espera_ssms <- data1 %>%
  filter(Estado == 'Cerrada', `Fecha cierre` > as.Date('2023-12-31') ) %>%
  group_by(Estado) %>%
  summarise(promedio = comma(round(mean(dias_al_cierre, na.rm = TRUE), 2)))

promedio_espera_comuna <- data1 %>%
  filter(Estado == 'Cerrada', `Fecha solicitud` > as.Date('2023-12-31')) %>%
  group_by(Comuna) %>%
  summarise(promedio = round(mean(dias_al_cierre, na.rm = TRUE), 2)) %>%
  arrange(desc(promedio)) %>% 
  mutate(promedio = comma(promedio)) %>% 
  select(c('Comuna','promedio')) %>%
  rename(`Días de espera` = promedio) 
  

mensual_diasalcierre <- data1 %>%
  filter(Estado == 'Cerrada') %>%
  group_by(month_year_sol, Comuna) %>%
  summarize(mean_days = round(mean(dias_al_cierre, na.rm = TRUE),2)) %>%
  rename(date = month_year_sol) %>%
  filter(date < as.Date(first_day_this_month)) %>%
  filter(date > as.Date('2023-12-31'))

mensual_diasalcierre_ssms <- data1 %>%
  filter(Estado == 'Cerrada') %>%
  group_by(month_year_sol) %>%
  summarize(mean_days = round(mean(dias_al_cierre, na.rm = TRUE),2)) %>%
  rename(date = month_year_sol) %>%
  filter(date < as.Date(first_day_this_month)) %>%
  filter(date > as.Date('2023-12-31'))


#######################  tiempo de espera por prioridad ######

library(dplyr)
library(tidyr)
library(ggplot2)

start_date <- '2023-12-31'
end_date <- as.Date(first_day_this_month)

# Function to calculate mean days to close by priority
calculate_mean_days <- function(data, priority, start_date, end_date) {
  data %>%
    filter(Estado == 'Cerrada',
           Prioridad == priority,
           month_year_cierre > as.Date(start_date),
           month_year_cierre < as.Date(end_date),
    ) %>%
    group_by(Comuna) %>%
    summarize(mean_days = round(mean(dias_al_cierre, na.rm = TRUE), 2)) %>%
    rename(!!paste0("mean_days_prior", priority) := mean_days)
}

# Calculate mean days for each priority
promedio_diasalcierre_prior1 <- calculate_mean_days(data1, 1, start_date, end_date)
promedio_diasalcierre_prior2 <- calculate_mean_days(data1, 2, start_date, end_date)
promedio_diasalcierre_prior3 <- calculate_mean_days(data1, 3, start_date, end_date)

# Combine the mean days data for each priority
promedio_diasalcierre_prior123 <- promedio_diasalcierre_prior1 %>%
  left_join(promedio_diasalcierre_prior2, by = 'Comuna') %>%
  left_join(promedio_diasalcierre_prior3, by = 'Comuna')

# Reshape the data to long format
promedio_diasalcierre_long <- promedio_diasalcierre_prior123 %>%
  pivot_longer(cols = starts_with("mean_days_prior"),
               names_to = "Priority",
               names_prefix = "mean_days_prior",
               values_to = "Mean_Days") %>%
  mutate(Priority = as.numeric(Priority)) # Convert Priority to numeric for proper sorting

# Print the reshaped data
promedio_diasalcierre_long



#######################  tiempo de espera por prestador ######
library(dplyr)
library(tidyr)
library(ggplot2)

start_date <- '2023-12-31'
end_date <- as.Date(first_day_this_month)

# Function to calculate mean days to close by priority
calculate_mean_days_prest <- function(data, prest, start_date, end_date) {
  data %>%
    filter(Estado == 'Cerrada',
           `Tipo prestador` == prest,
           month_year_cierre > as.Date(start_date),
           month_year_cierre < as.Date(end_date)) %>%
    group_by(Comuna) %>%
    summarize(mean_days = round(mean(dias_al_cierre, na.rm = TRUE), 2)) %>%
    rename(!!paste0(prest) := mean_days)
}

# Calculate mean days for each priority
promedio_diasalcierre_med <- calculate_mean_days_prest(data1, 'Medicina', start_date, end_date)
promedio_diasalcierre_mat <- calculate_mean_days_prest(data1, 'Matrona', start_date, end_date)
promedio_diasalcierre_dent <- calculate_mean_days_prest(data1, 'Dental', start_date, end_date)
promedio_diasalcierre_otros <- calculate_mean_days_prest(data1, 'Otros prestadores', start_date, end_date)

# Combine the mean days data for each priority
promedio_diasalcierre_todoprest <- promedio_diasalcierre_med %>%
  left_join(promedio_diasalcierre_mat, by = 'Comuna') %>%
  left_join(promedio_diasalcierre_dent, by = 'Comuna') %>% 
  left_join(promedio_diasalcierre_otros, by = 'Comuna') 

# Reshape the data to long format
promedio_diasalcierre_prest_long <- promedio_diasalcierre_todoprest %>%
  pivot_longer(cols = c('Medicina', 'Matrona', 'Dental', 'Otros prestadores'),
               names_to = "Prestador",
               values_to = "Mean_Days")

# Print the reshaped data
print(promedio_diasalcierre_prest_long)

promedio_espera_prestador_ssms <- data1 %>%
  filter(Estado == 'Cerrada', 
         `Fecha cierre` > as.Date(start_date),
         `Fecha cierre` < end_date,
         ) %>%
  group_by(`Tipo prestador`) %>%
  summarise(`Días de espera`= comma(round(mean(dias_al_cierre, na.rm = TRUE), 2)))

promedio_espera_prioridad_ssms <- data1 %>%
  filter(Estado == 'Cerrada',
         Prioridad != 4,
         `Fecha cierre` > as.Date(start_date),
         `Fecha cierre` < end_date,
  ) %>%
  group_by(`Prioridad`) %>%
  summarise(`Días de espera` = comma(round(mean(dias_al_cierre, na.rm = TRUE), 2)))


####################### Total de solicitudes por comuna #######
total_sol_comuna <- data1 %>%
  group_by(Comuna) %>%
  summarize(`Total de solicitudes` = n(),
            min_date = min(`Fecha solicitud`, na.rm = TRUE),
            max_date = max(`Fecha solicitud`, na.rm = TRUE),
            months_diff = interval(min_date, max_date) %/% months(1)) %>% 
  left_join(piv_comuna,by='Comuna') %>%
  mutate(`PIV 2024` = as.numeric(gsub("\\.", "", `PIV 2024`))) %>%
  mutate(sol_1000_month = round(`Total de solicitudes`/`PIV 2024`/months_diff *1000,2)) %>% 
  select(Comuna,`Total de solicitudes`, `PIV 2024`, sol_1000_month)

total_sol_comuna$`Total de solicitudes` <- comma(total_sol_comuna$`Total de solicitudes`)
total_sol_comuna$`PIV 2024` <- comma(total_sol_comuna$`PIV 2024`)
total_sol_comuna <- total_sol_comuna %>% arrange(desc(sol_1000_month))
total_sol_comuna$sol_1000_month <- comma(total_sol_comuna$sol_1000_month)




####################### Proporción de solicitudes por estado ####

##Pendientes##


proporcion_pendientes_comuna <- data1 %>%
  filter(Estado == 'Pendiente') %>% 
  group_by(Comuna, Estado) %>%
  summarize(n_sol = n()) %>%
  left_join(total_sol_comuna, by = 'Comuna')
proporcion_pendientes_comuna$prop_sol <- round((as.numeric(proporcion_pendientes_comuna$n_sol)/as.numeric(gsub("\\.", "", proporcion_pendientes_comuna$`Total de solicitudes`)))*100, 2)
proporcion_pendientes_comuna <- proporcion_pendientes_comuna %>%
  select('Comuna','n_sol','Total de solicitudes', 'prop_sol') %>%
  rename(
    Comuna = Comuna,
    `Solicitudes pendientes` = n_sol,
    `Total de solicitudes` = `Total de solicitudes`,
    `Proporción de solicitudes pendientes` = prop_sol) 
proporcion_pendientes_comuna$`Solicitudes pendientes` <- comma(as.numeric(proporcion_pendientes_comuna$`Solicitudes pendientes`))
proporcion_pendientes_comuna$`Proporción de solicitudes pendientes` <- round(proporcion_pendientes_comuna$`Proporción de solicitudes pendientes`,2)
proporcion_pendientes_comuna$`Proporción de solicitudes pendientes`<- comma(as.numeric(proporcion_pendientes_comuna$`Proporción de solicitudes pendientes`))
# proporcion_pendientes_comuna$`Total de solicitudes` <- as.numeric(gsub("\\,", "", proporcion_pendientes_comuna$`Total de solicitudes`))
# proporcion_pendientes_comuna$`Total de solicitudes` <- comma(as.numeric(proporcion_pendientes_comuna$`Total de solicitudes`))


total_sol <- nrow(data1)

# Group by 'Estado' and calculate the number of 'Pendiente' and total rows within each group
proporcion_pendientes_ssms <- data1 %>%
  filter(Estado == 'Pendiente') %>% 
  group_by(Estado) %>%
  summarize(
    n_sol = sum(Estado == 'Pendiente')  # Count the number of 'Pendiente' states
  ) %>%
  mutate(
    total_sol = total_sol,               # Add the total number of rows in the entire dataset
    proportion = n_sol / total_sol       # Calculate the proportion of 'Pendiente' states
  )


# Display the result
proporcion_pendientes_ssms

##Cerradas##

proporcion_cerrada_comuna <- data1 %>%
  filter(Estado == 'Cerrada') %>% 
  group_by(Comuna, Estado) %>%
  summarize(n_sol = n()) %>%
  left_join(total_sol_comuna, by = 'Comuna')
proporcion_cerrada_comuna$prop_sol <- as.numeric(proporcion_cerrada_comuna$n_sol)/as.numeric(gsub("\\,", "", proporcion_pendientes_comuna$`Total de solicitudes`)) *100

proporcion_cerrada_ssms <- data1 %>%
  filter(Estado == 'Cerrada') %>% 
  group_by(Estado) %>%
  summarize(n_sol = n(), 
            total_sol = sum(as.numeric(gsub("\\,", "", proporcion_cerrada_comuna$`Total de solicitudes`))))

proporcion_cerrada_ssms$prop_sol <- proporcion_cerrada_ssms$n_sol / proporcion_cerrada_ssms$total_sol * 100

##################################################### Tipos de cierre 2024#######

tipos_de_cierre <- data1 %>%
  filter((`Fecha cierre` > as.Date('2023-12-31')), !is.na(`Fecha cierre`)) %>%
  group_by(Comuna, `Tipo cierre`) %>%
  summarize(total = n()) %>%
  left_join(cierre_ss_2024, by='Comuna') %>% 
  mutate(`prop.` = round(`total.x`/`total.y`*100 , 2))

## Juntar todo

z_proportion_prest_ss_2024 <- z_proportion_medico_ss_2024 %>%
  left_join(z_proportion_matrona_ss_2024, by='Comuna') %>%
  left_join(z_proportion_dental_ss_2024, by='Comuna') %>% 
  rename(`Cie/Sol Med.` = 'cie/sol.x',
         `Cie/Sol Mat.` = 'cie/sol.y',
         `Cie/Sol Dent.` = 'cie/sol',) %>%
  select('Comuna',`Cie/Sol Med.` , `Cie/Sol Mat.` , `Cie/Sol Dent.` )

##################################################### Fecha agenda 2023 - 2024#######

data1$dias_sol_agenda <- as.numeric(difftime(data1$`Fecha agenda`, data1$`Fecha solicitud`, units = "days"))
data1$dias_cierre_agenda <- as.numeric(difftime(data1$`Fecha agenda`, data1$`Fecha cierre`, units = "days"))


## Tipo de cierre 

tabla_ss_dias_sol_cierre_agenda_tipocierre <- data1 %>% 
  filter(
    Estado == 'Cerrada',
    `Tipo cierre` %in% c('Agendado para atención presencial', 
                         'Agendado para atención por telemedicina', 
                         'Agendado para orden de examen'), 
    `Fecha cierre` > '2023-12-31',
    `Fecha agenda` < '2025-01-01',
    `Fecha agenda` > '2023-12-31',
    `dias_cierre_agenda` != 0, 
    )%>%
  group_by(`Tipo cierre`) %>%
  summarise(
    media_dias_sol_cierre = comma(round(mean(dias_al_cierre, na.rm = TRUE),2)),
    media_dias_sol_agenda = comma(round(mean(dias_sol_agenda, na.rm = TRUE),2)),
    media_dias_cierre_agenda = comma(round(mean(dias_cierre_agenda, na.rm = TRUE),2))
  ) %>% 
  rename(`Media sol. al cierre` = 'media_dias_sol_cierre',
         `Media sol. al agend.` = 'media_dias_sol_agenda',
         `Media cierre. al agend.` = 'media_dias_cierre_agenda',
  )

## Prioridad

tabla_ss_dias_sol_cierre_agenda_prioridad <- data1 %>% 
  filter(
    Estado == 'Cerrada',
    `Fecha cierre` > '2023-12-31',
    `Fecha agenda` < '2025-01-01',
    `Fecha agenda` > '2023-12-31',
    `dias_cierre_agenda` != 0,
    `Prioridad` != 4,
    `Tipo cierre` %in% c('Agendado para atención presencial', 
                         'Agendado para atención por telemedicina', 
                         'Agendado para orden de examen'), 
  )%>%
  group_by(`Prioridad`) %>%
  summarise(
    media_dias_sol_cierre = comma(round(mean(dias_al_cierre, na.rm = TRUE),2)),
    media_dias_sol_agenda = comma(round(mean(dias_sol_agenda, na.rm = TRUE),2)),
    media_dias_cierre_agenda = comma(round(mean(dias_cierre_agenda, na.rm = TRUE),2))
  ) %>% 
  rename(`Media sol. al cierre` = 'media_dias_sol_cierre',
         `Media sol. al agend.` = 'media_dias_sol_agenda',
         `Media cierre. al agend.` = 'media_dias_cierre_agenda',
  )

tabla_ss_dias_sol_cierre_agenda_prestador <- data1 %>% 
  filter(
    Estado == 'Cerrada',
    `Fecha cierre` > '2023-12-31',
    `Fecha agenda` < '2025-01-01',
    `Fecha agenda` > '2023-12-31',
    `dias_cierre_agenda` != 0,
    `Prioridad` != 4,
    `Tipo prestador` %in% c('Medicina', 'Matrona','Dental'),
    `Tipo cierre` %in% c('Agendado para atención presencial', 
                         'Agendado para atención por telemedicina', 
                         'Agendado para orden de examen'), 
  )%>%
  group_by(`Tipo prestador`) %>%
  summarise(
    media_dias_sol_cierre = comma(round(mean(dias_al_cierre, na.rm = TRUE),2)),
    media_dias_sol_agenda = comma(round(mean(dias_sol_agenda, na.rm = TRUE),2)),
    media_dias_cierre_agenda = comma(round(mean(dias_cierre_agenda, na.rm = TRUE),2))
  ) %>% 
  rename(`Media sol. al cierre` = 'media_dias_sol_cierre',
         `Media sol. al agend.` = 'media_dias_sol_agenda',
         `Media cierre. al agend.` = 'media_dias_cierre_agenda',
  )

tabla_ss_dias_sol_cierre_agenda_comuna <- data1 %>% 
  filter(
    Estado == 'Cerrada',
    `Fecha cierre` > '2023-12-31',
    `Fecha agenda` < '2025-01-01',
    `Fecha agenda` > '2023-12-31',
    `dias_cierre_agenda` != 0,
    `Prioridad` != 4,
    `Tipo cierre` %in% c('Agendado para atención presencial', 
                         'Agendado para atención por telemedicina', 
                         'Agendado para orden de examen'), 
  )%>%
  group_by(`Comuna`) %>%
  summarise(
    media_dias_sol_cierre = comma(round(mean(dias_al_cierre, na.rm = TRUE),2)),
    media_dias_sol_agenda = comma(round(mean(dias_sol_agenda, na.rm = TRUE),2)),
    media_dias_cierre_agenda = comma(round(mean(dias_cierre_agenda, na.rm = TRUE),2))
  ) %>% 
  rename(`Media sol. al cierre` = 'media_dias_sol_cierre',
         `Media sol. al agend.` = 'media_dias_sol_agenda',
         `Media cierre. al agend.` = 'media_dias_cierre_agenda',
  )

