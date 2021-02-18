## para instalar paquete tidycovid19

library(devtools)

install_github("joachim-gassen/tidycovid19")

## activamos paquetes

library(tidyverse)
library(tidycovid19)

## leemos datos
datos <- download_merged_data(silent = T, cached = T)

## estructura de la tabla
glimpse(datos)

## visualizamos datos de Argentina
datos %>%  filter(country  == "Argentina") %>% View()

## gráfico de incidencia acumulada 
datos %>% 
arrange(date) %>% 
  filter(iso3c %in% c("ARG", "USA")) %>% 
  mutate(inc = confirmed/population*100000) %>% 
  ggplot(aes(x = date, y = inc, color = country)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("blue", "red"), name = "País")  +
  xlab("Fecha") + 
  ylab("Incidencia acumulada (x 100.000 hab.") +
  labs(title = "Incidencia acumulada Covid-19. Comparativo Argentina - USA (2020-2021)",
       caption = "Fuente: Dataset del Johns Hopkins University Center for Systems Science and Engineering") 


## grafico de mortalidad
datos %>% 
  arrange(date) %>% 
  filter(iso3c %in% c("ARG", "BRA")) %>% 
  mutate(mort = deaths/population*1000000) %>% 
  ggplot(aes(x = date, y = mort, color = country)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("blue", "green"), name = "País")  +
  xlab("Fecha") + 
  ylab("Mortalidad (x 1.000.000 hab.") +
  labs(title = "Tasa de mortalidad Covid-19. Comparativo Argentina - Brasil (2020-2021)",
       caption = "Fuente: Dataset del Johns Hopkins University Center for Systems Science and Engineering") 
