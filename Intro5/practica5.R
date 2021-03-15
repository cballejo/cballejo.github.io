library(tidyverse)

datos_covid <- read_csv2("SISA_covid.csv")


## vemos estructura de la tabla

glimpse(datos_covid)

# 11 variables
# 6.687.347 observaciones

### usamos select()

datos_covid  # tabla completa

# salidas a consola

datos_covid %>% select(fecha_inicio_sintomas)  # solo fecha inicio de síntomas

datos_covid %>% select(-fecha_inicio_sintomas) # todas menos fecha inicio de síntomas

datos_covid %>% select(sexo, edad) # solo sexo y edad

datos_covid %>% select(-c(sexo, edad)) # todas menos sexo y edad

datos_covid %>% select(-sexo, -edad) # todas menos sexo y edad

datos_covid %>% select(1,2,3) # usando posiciones numéricas (las primeras tres variables)

datos_covid %>% select(1,2,11) # posiciones numéricas 1,2 y 11

datos_covid %>% select(1:3) # rango de variable 1 a 3

datos_covid %>% select(sexo:residencia_pais_nombre) # rango con nombres de variable

datos_covid %>% select(starts_with("resi")) # variables comienzan con "resi"

datos_covid %>% select(ends_with("apertura")) # variables finalizan con "apertura"

## salida a nueva tabla

datos_resi <- datos_covid %>% select(starts_with("resi"))

datos_resi

rm(datos_resi) # eliminamos la tabla datos_resi de la memoria con rm()

## salida a visualización en tabla

datos_covid %>% select(starts_with("resi")) %>% View()


##################

# seleccione de la tabla datos_covid las variables que comienzan con "fecha"

# selecciones de la tabla datos_covid las variables que finalicen con "nombre"

##################

## volvemos a las diapositivas

##################

# usamos filter()

# selecciones simples

datos_covid %>% filter(sexo == "F") # filtramos solo a las mujeres

datos_covid %>% filter(edad > 64) # filtramos observaciones mayores a 64 años

datos_covid %>% filter(edad >= 64) # filtramos observaciones mayores o iguales a 64 años

datos_covid %>% filter(residencia_pais_nombre != "Argentina") # filtramos observaciones que no residan en Argentina

# selección  teniendo en cuenta los valores especiales NA

datos_covid %>% filter(is.na(fecha_inicio_sintomas)) # filtramos observaciones con NA en fecha_inicio_sintomas


# selecciones combinadas

datos_covid %>% filter(residencia_provincia_nombre %in% c("Jujuy", "Salta")) # filtramos observaciones de Jujuy o Salta

datos_covid %>% filter(between(sepi_apertura, 30, 40)) # filtramos observaciones con sepi_apertura entre 30 y 40 (se incluyen los extremos)

datos_covid %>% filter(edad >= 64 & sexo == "F") # filtramos observaciones de mujeres mayores o iguales a 64 años                      

datos_covid %>% filter(edad >= 64, sexo == "F") # filtramos observaciones de mujeres mayores o iguales a 64 años   

datos_covid %>% filter(residencia_provincia_nombre == "Jujuy" | residencia_provincia_nombre == "Salta")  # filtramos observaciones de Jujuy o Salta

##################

# filtre de la tabla datos_covid las observaciones que residan en el departamento de Pergamino y sean menores de 10 años

# filtre de la tabla datos_covid las observaciones de hombres que hayan fallecido

##################

## los tipos de datos fecha (Date) tienen las misma propiedades que las variables numéricas

datos_covid %>% filter(fecha_inicio_sintomas > "2020-12-01") # filtramos observaciones con inicio de sintomas mayores al 1-12-2020         

##################

## Usamos filtros combinados con gráficos ggplot

## Vamos a utilizar estas funciones que hemos visto como herramientas para explorar la tabla de datos y conectarlo con la visualización

# Previamente extraeremos información necesaria de la fechas de apertura

library(lubridate)  # paquete para manejo de fechas

datos_covid <- datos_covid %>%  
  mutate(anio = epiyear(fecha_apertura),  # calculo del año epidemiológico
         mes = month(fecha_apertura))     # extracción del mes

### Nos preguntan si la distribución de confirmados fue igual en mayo que
### en septiembre de 2020.

datos_covid %>% 
  filter(anio == 2020, 
         mes %in% c(5,9)) %>% 
  ggplot(aes(x = mes, 
             fill = clasificacion_resumen)) + 
  geom_bar(position = "fill") 

# podríamos verlo también para todo el año 2020

datos_covid %>% 
  filter(anio == 2020) %>% 
  ggplot(aes(x = factor(mes), 
             fill = clasificacion_resumen)) + 
  geom_bar(position = "fill") 

## se ven muchos casos confirmados en enero del 2020. Es eso posible?

## agregando filtro fecha_apertura < "2021-01-01" para eliminar observaciones de la sepi 53 2020 (enero 2021)

datos_covid %>% 
  filter(anio == 2020,
         fecha_apertura < "2021-01-01") %>% 
  ggplot(aes(x = factor(mes), 
             fill = clasificacion_resumen)) + 
  geom_bar(position = "fill") 


### Ahora nos interesa comparar la situación entre la provincia del Chaco y Misiones
## Cómo fue la evolución de casos confirmados diarios en la pandemia?

datos_covid %>%  
  filter(fecha_apertura < "2021-01-01",
         residencia_provincia_nombre %in% c("Chaco", "Misiones"),
         clasificacion_resumen == "Confirmado") %>% 
  ggplot(aes(x = fecha_apertura, fill = residencia_provincia_nombre)) + 
  geom_bar() + 
  facet_grid(. ~ residencia_provincia_nombre)

# y si lo queremos por semana epidemiológica?

datos_covid %>%  
  filter(fecha_apertura < "2021-01-01",
         residencia_provincia_nombre %in% c("Chaco", "Misiones"),
         clasificacion_resumen == "Confirmado") %>% 
  ggplot(aes(x = sepi_apertura, fill = residencia_provincia_nombre)) + 
  geom_bar() + 
  facet_grid(. ~ residencia_provincia_nombre)

# y para lo que va del 2021?

datos_covid %>%  
  filter(fecha_apertura > "2020-12-31",
         residencia_provincia_nombre %in% c("Chaco", "Misiones"),
         clasificacion_resumen == "Confirmado") %>% 
  ggplot(aes(x = sepi_apertura, fill = residencia_provincia_nombre)) + 
  geom_bar() + 
  facet_grid(. ~ residencia_provincia_nombre)

## aparecen observaciones en la sepi 53. Porqué?

## conviene manejarse con filtros de sepi cuando mostramos ejes con sepi

datos_covid %>%  
  filter(anio == 2021,
         sepi_apertura != 53,
         residencia_provincia_nombre %in% c("Chaco", "Misiones"),
         clasificacion_resumen == "Confirmado") %>% 
  ggplot(aes(x = sepi_apertura, fill = residencia_provincia_nombre)) + 
  geom_bar() + 
  facet_grid(. ~ residencia_provincia_nombre)


## Hagamos un recorte para trabajar con las observaciones de General Pueyrredón

datos_mdp <- datos_covid %>%  
  filter(residencia_departamento_nombre == "General Pueyrredón")


## como se distribuye la edad de los confirmados según sexo en cada mes del 2020

datos_mdp %>%  
  filter(fecha_apertura < "2021-01-01",
         clasificacion_resumen == "Confirmado") %>% 
  ggplot(aes(x = sexo, y = edad, fill = sexo)) +
  geom_boxplot() + 
  geom_jitter(size = 0.1, alpha = 0.2) +
  facet_wrap(~ mes, nrow = 2) + 
  scale_y_continuous(breaks = seq(0,125, by = 5))

# veamos las cantidades de confirmados por mes como complemento del gráfico anterior

datos_mdp %>% 
  filter(fecha_apertura < "2021-01-01",
         clasificacion_resumen == "Confirmado") %>% 
  count(mes, name = "Confirmados") %>% 
  ggplot(aes(x = factor(mes), y = Confirmados)) + 
  geom_bar(stat = "identity", fill = "sienna3")

# tomemos el mes 9 de mayor cantidad de confirmados y veamos la distribución de edad según sexo mediante un gráfico de densidad

datos_mdp %>% 
  filter(clasificacion_resumen == "Confirmado",
         mes == 9) %>% 
  ggplot(aes(x = edad, fill = sexo)) +
  geom_density()

# que característica podemos agregar al elemento densidad para visualizar todas las curvas?

datos_mdp %>% 
  filter(clasificacion_resumen == "Confirmado",
         mes == 9) %>% 
  ggplot(aes(x = edad, fill = sexo)) +
  geom_density(alpha = 0.4)

## que podríamos hacer para que no aparezcan las observaciones con NR en sexo?

datos_mdp %>% 
  filter(clasificacion_resumen == "Confirmado",
         mes == 9,
         sexo != "NR") %>% 
  ggplot(aes(x = edad, fill = sexo)) +
  geom_density(alpha = 0.4)


