library(tidyverse)
library(ggrepel)
library(stringr)

datos <- readxl::read_excel("data/Consolidado cumplimiento EAR.xlsx")

## Revisión de datos ----
str(datos)
datos$servicio.salud <- as.factor(datos$servicio.salud)
datos$prestador <- as.factor(datos$prestador)
datos$porcentaje <- as.numeric(datos$porcentaje)
datos$estado <- as.factor(datos$estado)
datos$acreditacion <- as.factor(datos$acreditacion)


mediana <- datos %>% group_by(servicio.salud, año) %>%
  summarise(mediana = median(porcentaje)) %>% 
  ungroup()

mediana$mediana <- round(mediana$mediana, 1)

metropolitano <- c("Metropolitano Sur", "Metropolitano Sur Oriente", "Metropolitano Oriente",
                   "Metropolitano Occidente", "Metropolitano Norte", "Metropolitano Central")

servicio <- unique(datos$servicio.salud)

ss_RM <- datos %>% filter(servicio.salud %in% c(metropolitano))

datos %>% 
  filter(año == 2019) %>% 
  #arrange(desc(porcentaje)) %>% 
  ggplot(aes(x = reorder(prestador, porcentaje), y = porcentaje, fill = estado)) +
  geom_col() +
  scale_fill_manual(values = c("skyblue2", "grey80"), name = "Resultado")  +
  #geom_hline(yintercept = 75, color = "red", size = 3, alpha = 0.5) +
  coord_flip() +
  theme_bw()


mediana <- mutate(mediana, estado = ifelse(mediana >= 75, "Cumple", "No cumple"))
mediana$estado <- as.factor(mediana$estado)

mediana %>% 
  ggplot(aes(año, mediana)) +
  geom_line(size = 1, alpha = 0.5) +
  geom_point(size = 2.5) +
  #geom_smooth(se = FALSE, method = lm) +
  geom_label_repel(aes(label = mediana, fill = factor(estado)), size = 3.2, color = "white" ) +
  scale_fill_manual(values = c("limegreen", "red"), name = "Resultado")  +
  #geom_hline(yintercept = 75, color = "red") +
  facet_wrap(~servicio.salud) +
  theme_bw() +
  labs(title = "Resultados evaluación Establecimientos Autogestionados en Red (EAR) por servicios de salud, entre los años 2016 y 2019",
       subtitle = "1. Se muestra la mediana del cumplimiento en caso de que un servicio de salud tenga 2 ó más instituciones evaluadas\n2. Se incluyen tanto hospitales con calidad EAR, como candidatos e invitados.",
       caption = "Fuente: Departamento de eficiencia hospitalaria / Subsecretaria de Redes Asistenciales\n\nElaborado por Paulo Villarroel",
       x = "",
       y = "Porcentaje de cumplimiento") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


datos %>% 
  ggplot(aes(año, porcentaje)) +
  geom_line(size = 1, alpha = 0.5) +
  geom_point(size = 2) +
  #geom_label_repel(aes(label = porcentaje, fill = factor(estado)), size = 2, color = "white" ) +
  #scale_color_manual(values = c("limegreen", "red"), name = "Resultado")  +
  #geom_hline(yintercept = 75, color = "red") +
  facet_wrap(~prestador) +
  theme_bw() +
  labs(title = "Resultados evaluación Establecimientos Autogestionados en Red (EAR) por servicios de salud, entre los años 2016 y 2019",
       subtitle = "1. Se muestra la mediana del cumplimiento en caso de que un servicio de salud tenga 2 ó más instituciones evaluadas\n2. Se incluyen tanto hospitales con calidad EAR, como candidatos e invitados.",
       caption = "Fuente: Departamento de eficiencia hospitalaria / Subsecretaria de Redes Asistenciales\n\nElaborado por Paulo Villarroel",
       x = "",
       y = "Porcentaje de cumplimiento") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


## IF
datos %>% na.omit() %>% 
  filter(año == 2019) %>% 
  ggplot(aes(porcentaje, IF)) +
  geom_jitter(size = 2) +
  facet_wrap(~prestador) +
  coord_flip() +
  labs(title = "Relación entre índice funcional y nivel de cumplimiento EAR",
       x = "Porcentaje de cumplimiento EAR",
       y = "Índice funcional",
       caption = "MINSAL") +
  theme_bw()

datos %>% filter(año == 2019) %>% 
  ggplot(aes(IF, porcentaje, color = acreditacion)) +
  geom_point(size = 4) +
  geom_hline(yintercept = 75) +
  geom_vline(xintercept = 1) +
  scale_color_manual(values = c("green3", "red", "grey20"), name = "Acreditación") +
  labs(title = "Relación entre índice funcional y nivel de cumplimeinto EAR",
       subtitle = "Datos para el año 2019",
       x = "IF",
       y = "% Cumplimiento EAR",
       caption = "Fuente: MINSAL") +
  theme_bw()


## gasto equivalente ----

options(scipen=999)
datos %>% filter(año == 2019, prestador != "Complejo Hospitalario Dr. Sotero del Rio") %>%
  mutate(gasto = gasto.equivalente/1000000) %>% 
  ggplot(aes(gasto, complejidad)) +
  geom_point(size = 3) +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), color = "blue") +
  #stat_smooth(method = "lm", formula = y ~ log(x), color = "blue", se = FALSE) +
  #geom_label_repel(aes(label = prestador)) +
  scale_color_manual(values = c("green3", "red"), name = "EAR") +
  theme_bw() +
  labs(title = "Relación entre índice de complejidad y gasto hospitalario equivalente 2019",
       subtitle = "Basado en datos disponibles en informe Comisión de Productividad\nSe ha excluído al Hospital Sótero del Río por presentar un gasto muy elevado respecto al resto",
       caption = "Elaboración propia",
       x = "Gasto hospitalario (M$)",
       y = "Índice de complejidad")


##Acreditacion

datos %>% 
  filter(año == 2019) %>% 
  ggplot(aes(x = porcentaje)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  #scale_fill_manual(values=c("red", "limegreen"))
  facet_grid(~acreditacion) +
  theme_bw() +
  labs(title = "Cumplimiento del estándar Establecimientos Autogestionados en Red (EAR), segmentado por estado de Acreditación de Calidad en Salud",
       subtitle = "La asignación de acreditado es en base a encontrarse publicado como vigente a dicha condición al 1.8.2020",
       x = "Porcentaje de cumplimiento EAR (año 2019)",
       y = "N° instituciones",
       caption = "Fuentes: Departamento de eficiencia hospitalaria / Subsecretaria de Redes Asistenciales\nRegistro de prestadores institucionales / Superintendencia de Salud\n\nElaborado por Paulo Villarroel")


datos %>% filter(año == 2019) %>% 
  group_by(acreditacion) %>% 
  summarise(Mediana = median(porcentaje)) %>% 
  ungroup()

    
datos %>% 
  filter(año == 2019) %>% 
  ggplot(aes(porcentaje, porc.acred)) +
  geom_point()
    

datos %>% filter(año == 2019) %>% group_by(acreditacion) %>% summarise(median(porcentaje))

cor(datos2$porcentaje, datos2$porc.acred, method = "kendall")
  
datos2 <- na.omit(datos) 
  

datos %>% 
  filter(servicio.salud == "Metropolitano Oriente") %>% 
  ggplot(aes(año, porcentaje)) +
  geom_line(size = 1.5, alpha = 0.5) +
  geom_point(size = 3.5) +
  geom_label_repel(aes(label = porcentaje, fill = factor(estado)), size = 4, color = "white" ) +
  scale_fill_manual(values = c("green3", "red"), name = "Resultado")  +
  theme(legend.position = "top") +
  facet_wrap(~prestador) +
  labs(title = "Resultados evaluación Establecimientos Autogestionados en Red (EAR) / SS Metropolitano Oriente",
       subtitle = "Fuente: Departamento de eficiencia hospitalaria / Subsecretaria de Redes Asistenciales",
       caption = "Elaborado por Paulo Villarroel",
       x = "Año evaluación",
       y = "Porcentaje de cumplimiento")


## Correlaciones ----


install.packages("ggcorrplot")
library(ggcorrplot)

datos2 <- datos %>% select(2, 3, 10)
names(datos2)[2] <- "ear"
names(datos2)[3] <- "acred"

datos2 <- na.omit(datos2)

pairs(datos2$ear ~ datos2$acred)

install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

datos3 <- data.frame(datos2$ear, datos2$acred)
names(datos3)[1] <- "ear"
names(datos3)[2] <- "acred"

chart.Correlation(datos3, method = "spearman")

indfun <- datos %>% select(3, 8, 10) %>% na.omit()
indfun <- data.frame(indfun)
names(indfun)[1] <- "ear"
names(indfun)[2] <- "IF"
names(indfun)[3] <- "Acred"

chart.Correlation(indfun, method = "spearman")
