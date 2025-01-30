####################### Importo librerías ----

library('rio')
library('dplyr')
library('readxl')
library('writexl')
library('stringr')
library('lubridate')  
library('ggplot2') # Para armar los gráficos
library('ggthemes')
library('ggsci')

#######################Importo código de análisis ----

source("analisis_telesalud.R")

####################### Custom Theme for plots   #######################

custom_theme <- theme_minimal() + 
                   theme(
  plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
  plot.subtitle = element_text(size = 8, face = "italic", hjust = 0.5),
  axis.title = element_text(size = 8),
  axis.text = element_text(size = 6), 
  legend.title = element_text(size = 7, face = "italic"),
  legend.text = element_text(size = 5),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(), 
  axis.line = element_line(linewidth = 0.3, color = "black"), 
  legend.background = element_rect(fill = "white"), 
  legend.key = element_rect(fill = "white", color = NA),
  legend.key.size = unit(0.5, "cm"),  # Adjust size of legend keys
  legend.spacing.y = unit(0.01, "cm"),
  legend.position = "right",
  plot.margin = margin(20, 20, 20, 20))

####################### Gráfico mensual solicitud ssms   #######################

graf_mensual_sol_ssms_historico <- ggplot(mensual_sol_ssms_2124, aes(x = date, y=mean_sol)) +
  geom_line(aes(y = mean_sol),linewidth=0.5, color='#4E79A7') +
  geom_smooth(method = "loess", se = FALSE, linewidth=0.5, color='#A0CBE8') +
  labs(x = "Fecha", y = "Cantidad de Solicitudes.", 
       title = "Media de cantidad de solicitudes por mes. SSMS.",
       subtitle = 'Media suavizada. Plataforma Telesalud. SSMS. 2021-2024',
       color = 'Comuna') +
  custom_theme + 
  scale_colour_tableau("Tableau 20")

graf_mensual_sol_ssms_historico

graf_mensual_sol_ssms_total <- ggplot(mensual_sol_ssms_2124, aes(x = date, y=total_sol)) +
  geom_line(aes(y = total_sol),linewidth=0.5, color='#4E79A7') +
  geom_smooth(method = "loess", se = FALSE, linewidth=0.5, color='#A0CBE8') +
  labs(x = "Fecha", y = "Cantidad de Solicitudes.", 
       title = "Evolución de la cantidad de solicitudes por mes. SSMS.",
       subtitle = 'Media suavizada. Plataforma Telesalud. SSMS. 2021-2024',
       color = 'Comuna') +
  custom_theme + 
  scale_colour_tableau("Tableau 20")

graf_mensual_sol_ssms_total

####################### Gráfico mensual solicitud histórico ssms   ##############
# 
# graf_mensual_sol_ssms <- ggplot(mensual_sol_ssms_2124, aes(x = date)) +
#   geom_line(aes(y = mean_sol)) +
#   labs(x = "Fecha", y = "Cantidad de Solicitudes.", 
#        title = "Cantidad de solicitudes promedio por mes. SSMS.",
#        subtitle = 'Plataforma Telesalud. SSMS. 2023-2024',
#        color = 'Comuna') +
#   custom_theme + 
#   scale_colour_tableau("Tableau 20")
# 
# graf_mensual_sol_ssms

####################### Gráfico mensual solicitud comuna  #######################

graf_mensual_sol_comuna <- ggplot(mensual_sol_comuna_piv, aes(x = date), color = Comuna) +
  geom_line(aes(y = n_sol, color = Comuna)) +
  labs(x = "Fecha", y = "Cantidad de Solicitudes.", 
       title = "Evolución de solicitudes de atención por Telesalud por comuna",
       subtitle = 'Plataforma Telesalud. SSMS. 2021-2024',
       color = 'Comuna') +
  custom_theme +
  scale_colour_tableau("Tableau 20")

graf_mensual_sol_comuna

####################### Gráfico mensual cierre comuna 

graf_mensual_cierre_comuna <- ggplot(mensual_cierre_comuna, aes(x = date), color = Comuna) +
  geom_line(aes(y = n_cierre, color = Comuna)) +
  labs(x = "Fecha", y = "Cantidad de cierre de solicitudes", 
       title = "Cierres de solicitudes de Telesalud por comuna",
       subtitle = 'Plataforma Telesalud.SSMS. 2021-2024',
       color = 'Comuna') +
  custom_theme + 
  scale_colour_tableau("Tableau 20")

graf_mensual_cierre_comuna

####################### Gráfico mensual proporción comuna ##########################

graf_mensual_prop_comuna <- ggplot(mensual_proportion_comuna, aes(x = date), color = Comuna) +
  geom_line(aes(y = proportion, color = Comuna)) +
  labs(x = "Fecha", y = "Proporción de cierre/solicitudes de telesalud", 
       title = "Proporción de cierre/solicitudes de Telesalud por comuna",
       subtitle = 'Plataforma Telesalud.SSMS. 2023-2024',
       color = 'Comuna') +
  custom_theme + 
  scale_colour_tableau("Tableau 20") +
  ylim(0,2)

graf_mensual_prop_comuna


####################### Gráfico mensual solicitudes suavizada ######################

graf_mensual_sol_comuna_suavizado <- ggplot(mensual_sol_comuna_piv, aes(x = date, y = n_sol, color = Comuna)) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(x = "Fecha", y = "Cantidad de Solicitudes", 
       title = "Solicitudes de Telesalud por comuna (suavizado)",
       subtitle = 'Plataforma Telesalud. SSMS. 2021-2024',
       color = 'Comuna') +
  custom_theme + 
  scale_colour_tableau("Tableau 20")

# Print the plot
graf_mensual_sol_comuna_suavizado

####################### Gráfico mensual solicitudes ssms suavizado #######

graf_mensual_sol_ssms_suavizado <- ggplot(mensual_sol_ssms_2124, aes(x = date, y = mean_sol)) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(x = "Fecha", y = "Cantidad de solicitudes", 
       title = "Cantidad de solicitudes promedio por mes. SSMS. (suavizado).",
       subtitle = 'Plataforma Telesalud. SSMS. 2021-2024.',
       color = 'Comuna') +
  custom_theme + 
  scale_colour_tableau("Tableau 20")

graf_mensual_sol_ssms_suavizado

####################### Gráfico mensual solicitudes comuna suavizado piv #######

graf_mensual_sol_comuna_suavizado_piv <- ggplot(mensual_sol_comuna_piv, aes(x = date, y = sol_per_capita, color = Comuna)) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(x = "Fecha", y = "N°/mil habs.", 
       title = "Cantidad solicitudes de Telesalud por comuna.",
       subtitle = 'Cantidad por cada 1000 habs. Gráfico suavizado. Plataforma Telesalud. SSMS. 2021-2024',
       color = 'Comuna') +
  custom_theme + 
  scale_colour_tableau("Tableau 20")

graf_mensual_sol_comuna_suavizado_piv

####################### Gráfico mensual proporción comuna suavizado ###############

graf_mensual_prop_comuna_suavizado <- ggplot(mensual_proportion_comuna, aes(x = date, y=proportion , color = Comuna)) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(x = "Fecha", y = "Proporción cierre/solicitudes", 
       title = 'Proporción de cierres por solicitud',
       subtitle = 'Plataforma Telesalud. Gráfico suavizado. SSMS. 2023-2024',
       color = 'Comuna') +
  custom_theme + 
  scale_colour_tableau("Tableau 20")

# Print the plot
graf_mensual_prop_comuna_suavizado

####################### Gráfico mensual proporción comuna suavizado medico ##########

graf_mensual_prop_comuna_tendencia_medico <- ggplot(mensual_proportion_prestador_comuna_medico, aes(x = date, y=proportion , color = Comuna)) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(x = "Fecha", y = "Proporción cierre/solicitudes", 
       title = 'Proporción de cierre/solicitudes de Telesalud - Medicina',
       subtitle = 'Plataforma Telesalud. Gráfico suavizado. SSMS. 2023-2024',
       color = 'Comuna') +
  custom_theme + 
  scale_colour_tableau("Tableau 20")

graf_mensual_prop_comuna_tendencia_medico

####################### Gráfico mensual proporción comuna suavizado odonto ########

graf_mensual_prop_comuna_tendencia_odonto <- ggplot(mensual_proportion_prestador_comuna_odonto, aes(x = date, y=proportion , color = Comuna)) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(x = "Fecha", y = "Proporción cierre/solicitudes", 
       title = 'Proporción de cierre/solicitudes de telesalud - Odontología',
       subtitle = 'Plataforma Telesalud. Gráfico suavizado. SSMS. 2023-2024',
       color = 'Comuna') +
  custom_theme + 
  scale_colour_tableau("Tableau 20")

graf_mensual_prop_comuna_tendencia_odonto

####################### Gráfico mensual proporción comuna suavizado matrona #######

graf_mensual_prop_comuna_tendencia_matrona <- ggplot(mensual_proportion_prestador_comuna_matrona, aes(x = date, y=proportion , color = Comuna)) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(x = "Fecha", y = "Proporción cierre/solicitudes", 
       title = 'Proporción de cierre/solicitudes de telesalud - Matrona',
       subtitle = 'Plataforma Telesalud. Gráfico suavizado.SSMS. 2023-2024',
       color = 'Comuna') +
  custom_theme + 
  scale_colour_tableau("Tableau 20")

graf_mensual_prop_comuna_tendencia_matrona


####################### Gráfico mensual espera al cierra comuna suavizado #######

graf_mensual_diasalcierre_comuna <- ggplot(mensual_diasalcierre, aes(x = date, y=mean_days , color = Comuna)) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(x = "Fecha", y = "Días de espera (promedio)", 
       title = 'Media de días de espera al cierre de solicitudes',
       subtitle = 'Plataforma Telesalud. Gráfico suavizado. SSMS. 2024',
       color = 'Comuna') +
  custom_theme +
  scale_colour_tableau("Tableau 20")

graf_mensual_diasalcierre_comuna

####################### Gráfico mensual espera al cierra comuna por prioridad suavizado #######

# graf_mensual_diasalcierre_comuna_prior1 <- ggplot(mensual_diasalcierre_prior1, aes(x = date, y=mean_days , color = Comuna)) +
#   geom_smooth(method = "loess", se = FALSE) +
#   labs(x = "Fecha", y = "Días de espera (promedio)", 
#        title = 'Promedio de días de espera al cierre de solicitudes - P1',
#        subtitle = 'Plataforma Telesalud. Gráfico suavizado. SSMS. 2023-2024',
#        color = 'Comuna') +
#   custom_theme + 
#   scale_colour_tableau("Tableau 20")
# 
# graf_mensual_diasalcierre_comuna_prior1
# 
# graf_mensual_diasalcierre_comuna_prior2 <- ggplot(mensual_diasalcierre_prior2, aes(x = date, y=mean_days , color = Comuna)) +
#   geom_smooth(method = "loess", se = FALSE) +
#   labs(x = "Fecha", y = "Días de espera (promedio)", 
#        title = 'Promedio de días de espera al cierre de solicitudes - P2 (suavizado)',
#        subtitle = 'Plataforma Telesalud. SSMS. 2023-2024',
#        color = 'Comuna') +
#   custom_theme + 
#   scale_colour_tableau("Tableau 20")
# 
# graf_mensual_diasalcierre_comuna_prior2
# 
# graf_mensual_diasalcierre_comuna_prior3 <- ggplot(mensual_diasalcierre_prior3, aes(x = date, y=mean_days , color = Comuna)) +
#   geom_smooth(method = "loess", se = FALSE) +
#   labs(x = "Fecha", y = "Días de espera (promedio)", 
#        title = 'Promedio de días de espera al cierre de solicitudes - P3 (suavizado)',
#        subtitle = 'Plataforma Telesalud. SSMS. 2023-2024',
#        color = 'Comuna') +
#   custom_theme + 
#   scale_colour_tableau("Tableau 20")
# 
# graf_mensual_diasalcierre_comuna_prior3


graf_promedio_por_prioridad <- ggplot(promedio_diasalcierre_long, 
                                      aes(x = Comuna, y = Mean_Days, 
                                          fill = factor(Priority))) +
  geom_col(position = position_dodge()) +
  labs(title = "Media de días de espera para el cierre, según prioridad",
       subtitle = 'Plataforma Telesalud. SSMS. 2024.',
       x = "Comuna",
       y = "Días",
       fill = "Prioridad") +
  custom_theme + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_colour_tableau("Tableau 20")

graf_promedio_por_prioridad

graf_promedio_por_prioridad <- ggplot(promedio_diasalcierre_long, 
                                      aes(x = Comuna, y = Mean_Days, 
                                          fill = factor(Priority))) +
  geom_col(position = position_dodge()) +
  labs(title = "Media de días de espera para el cierre, según prioridad",
       subtitle = 'Plataforma Telesalud. SSMS. 2024.',
       x = "Comuna",
       y = "Días",
       fill = "Prioridad") +
  custom_theme + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_colour_tableau("Tableau 20")

graf_promedio_por_prioridad

graf_promedio_por_prestador<- ggplot(promedio_diasalcierre_prest_long, 
                                      aes(x = Comuna, y = Mean_Days, 
                                          fill = factor(Prestador))) +
  geom_col(position = position_dodge()) +
  labs(title = "Media de días de espera para el cierre, según prestador",
       subtitle = 'Plataforma Telesalud. SSMS. 2024.',
       x = "Comuna",
       y = "Días",
       fill = "Prestador") +
  custom_theme + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_colour_tableau("Tableau 20")

graf_promedio_por_prestador



############## Mensual sol comuna ssms#####

graf_mensual_sol_ssms_comuna_cantidad <- ggplot(mensual_sol_ssms_2124, aes(x = date, y=total_sol)) +
  geom_line(aes(y = mean_sol),linewidth=0.5, color='#4E79A7') +
  labs(x = "Fecha", y = "Cantidad de Solicitudes.", 
       title = "Cantidad de solicitudes por mes. SSMS.",
       subtitle = 'Media suavizada. Plataforma Telesalud. SSMS. 2021-2024',
       color = 'Comuna') +
  custom_theme + 
  scale_colour_tableau("Tableau 20")

graf_mensual_sol_ssms_comuna_cantidad

graf_mensual_sol_ssms_comuna <- ggplot(mensual_sol_ssms_2124, aes(x = date, y=mean_sol)) +
  geom_line(aes(y = mean_sol),linewidth=0.5, color='#4E79A7') +
  geom_smooth(method = "loess", se = FALSE, linewidth=0.5, color='#A0CBE8') +
  labs(x = "Fecha", y = "Cantidad de Solicitudes.", 
       title = "Media cantidad de solicitudes por mes. SSMS.",
       subtitle = 'Media suavizada. Plataforma Telesalud. SSMS. 2021-2024',
       color = 'Comuna') +
  custom_theme + 
  scale_colour_tableau("Tableau 20")

graf_mensual_sol_ssms_historico


########### Tipo de cierre  ###########

top_values <- tipos_de_cierre %>%
  group_by(Comuna) %>%
  arrange(Comuna, desc(`prop.`)) %>%
  slice_head(n = 3) %>%
  ungroup()

tipos_de_cierre <- tipos_de_cierre %>%
  left_join(top_values %>% select(Comuna, `Tipo cierre`, `prop.`) %>% mutate(top = TRUE), 
            by = c("Comuna", "Tipo cierre", "prop.")) %>%
  mutate(top = ifelse(is.na(top), FALSE, top))

graf_tiposdecierre<- ggplot(tipos_de_cierre,aes(x = Comuna, y = `prop.`, 
                                         fill = factor(`Tipo cierre`))) +
  geom_col(position = 'stack') +
  geom_text(data = tipos_de_cierre %>% filter(top == TRUE), 
            aes(label = round(`prop.`, 2)), 
            position = position_stack(vjust = 0.5), 
            size = 2,  # Increase text size
            color = 'gray25') +
  labs(title = "Proporción de tipos de cierre, por comuna",
       subtitle = 'Plataforma Telesalud. SSMS. 2024.',
       x = "Comuna",
       y = "Proporción",
       fill = "Tipo de cierre") +
  custom_theme + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = 'Set2')

graf_tiposdecierre

