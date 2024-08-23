pacman::p_load(
  tidyverse, #manejo de datos
  here,       # directorios relativos
  janitor,    # limpiar dataframes
  lubridate,  # trabajar con fechas
  summarytools,
  rio
)

# 2. Importar y sintetizar datos del DEIS ----

comunas_censo <- import(here("data_censo","proyeccion2017comunal.xlsx")) %>% 
  clean_names() %>%
  rename(sexo = sexo_1_hombre_2_mujer) %>% 
  select(nombre_comuna, edad, sexo ,poblacion_2024 ) %>%
  mutate(edad_categoria = cut(edad, 
                              breaks = c(0, 15, 18, 40, 65, Inf), 
                              labels = c("0-15", "15-18", "18-40","40-65", "65+"),
                              right = FALSE)) %>% 
  group_by(nombre_comuna, sexo, edad_categoria) %>%
  summarise(poblacion_2024 = sum(poblacion_2024,na.rm = TRUE))
  

ggplot(data = comunas_censo %>% filter(edad_categoria == '0-15'),
       mapping = aes(x = reorder(nombre_comuna, -poblacion_2024),
                     y = poblacion_2024)) + 
  geom_col(show.legend = FALSE) +
  labs(title = "Population in 2024 for Age Category 0-15 by Comuna",
       x = "Comuna",
       y = "Population 2024",
       caption = "Source: Data Censo") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(size = 10, hjust = 0.5))

serie_censo <- import(here("data_censo","proyeccion2017comunal.xlsx")) %>% 
  clean_names() %>%
  rename(sexo = sexo_1_hombre_2_mujer) %>%
  mutate(edad_categoria = cut(edad, 
                              breaks = c(0, 15, 18, 40, 65, Inf), 
                              labels = c("0-15", "15-18", "18-40", "40-65", "65+"),
                              right = FALSE)) %>%
  pivot_longer(cols = starts_with("poblacion_"), 
               names_to = "year", 
               names_prefix = "poblacion_", 
               values_to = "poblacion") %>%
  group_by(nombre_comuna, sexo, edad_categoria, year) %>%
  summarise(total_poblacion = sum(poblacion, na.rm = TRUE)) %>%
  ungroup()


ggplot(data = serie_censo %>% filter(edad_categoria == '0-15'),
       mapping = aes(x = date,
                     y = total_poblacion,
                     color = nombre_comuna)) +
  geom_line(show.legend = TRUE, size = 1) +
  labs(title = "Population Trend for Age Category 0-15 by Comuna (2017-2035)",
       x = "Year",
       y = "Total Population",
       color = "Comuna") +
  scale_x_continuous(breaks = seq(2017, 2035, by = 1)) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "right")
