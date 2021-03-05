library(tidyverse)

datos_covid <- read_csv2("SISA_covid.csv")


## arrange()

# filtrar observaciones de Chaco de mayo de 2020
# ordenar fecha_inicio_sintomas
# seleccionar provincia, departamento, fis
# visualizar

datos_covid %>% 
  filter(residencia_provincia_nombre == "Chaco", anio == 2020, mes == 5) %>% 
  arrange(fecha_inicio_sintomas) %>% 
  select(residencia_provincia_nombre, residencia_departamento_nombre, fecha_inicio_sintomas) %>% 
  View()

## igual pero ordenado por departamento

datos_covid %>% 
  filter(residencia_provincia_nombre == "Chaco", anio == 2020, mes == 5) %>% 
  arrange(residencia_departamento_nombre) %>% 
  select(residencia_provincia_nombre, residencia_departamento_nombre, fecha_inicio_sintomas) %>% 
  View()

## igual pero ordenado por departamento y fis descendente

datos_covid %>% 
  filter(residencia_provincia_nombre == "Chaco", anio == 2020, mes == 5) %>% 
  arrange(residencia_departamento_nombre, desc(fecha_inicio_sintomas)) %>% 
  select(residencia_provincia_nombre, residencia_departamento_nombre, fecha_inicio_sintomas) %>% 
  View()


## count()


# sobre grafico barras comparativo mayo - septiembre 2020

datos_covid %>% 
  filter(anio == 2020, 
         mes %in% c(5,9)) %>% 
  count(mes,clasificacion_resumen, sort = TRUE) 

## orden necesario

datos_covid %>% 
  filter(anio == 2020, 
         mes %in% c(5,9)) %>% 
  count(mes,clasificacion_resumen) %>% 
  arrange(mes,desc(n))


# idem anterior para todo el 2020

datos_covid %>% 
  filter(anio == 2020,
         fecha_apertura < "2021-01-01") %>% 
  count(mes,clasificacion_resumen) %>% 
  arrange(mes,desc(n))


## agregamos nombre

datos_covid %>% 
  filter(anio == 2020,
         fecha_apertura < "2021-01-01") %>% 
  count(mes,clasificacion_resumen,name = "Frecuencia") %>% 
  arrange(mes,desc(Frecuencia))


## modificar gráfico con las otras dos posibilidades de posicion

# stack

datos_covid %>% 
  filter(anio == 2020,
         fecha_apertura < "2021-01-01") %>% 
  ggplot(aes(x = factor(mes), 
             fill = clasificacion_resumen)) + 
  geom_bar(position = "stack") 

# dodge

datos_covid %>% 
  filter(anio == 2020,
         fecha_apertura < "2021-01-01") %>% 
  ggplot(aes(x = factor(mes), 
             fill = clasificacion_resumen)) + 
  geom_bar(position = "dodge") 


#####

## sobre gráfico  de cantidad de confirmados por mes en Gral. Pueyrredon
## tipo tabla con count

datos_mdp %>% 
  filter(fecha_apertura < "2021-01-01",
         clasificacion_resumen == "Confirmado") %>% 
  count(mes, name = "Confirmados") 
