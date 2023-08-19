library(tidyverse)

datos_covid <- read_csv2("SISA_covid.csv")


## recordamos la estructura de la tabla de datos

glimpse(datos_covid)

# 11 variables
# 6.687.347 observaciones

## En el script practica3.R ejecutamos sin analizar demasiado el siguiente
## código:

library(lubridate)  # paquete para manejo de fechas

datos_covid <- datos_covid %>%  
  mutate(anio = epiyear(fecha_apertura),  # cálculo del año epidemiológico
         mes = month(fecha_apertura))     # extracción del mes

# Construimos dos variables nuevas llamadas anio y mes con información 
# extraída de la fecha de apertura

## Analizando el código encontramos que aparece la función mutate() cumpliendo
## la tarea de "crear" estas nuevas variables

## Incluidas dentro del mutate hay dos funciones específicas (epiyear() y 
## month()) propias del paquete lubridate.

## Veremos más adelante funciones para el manejo de datos particulares como
## fechas, cadenas de caracteres y factores.

## Más allá del uso de funciones que aún no conocemos la estructura de mutate()
## se mantiene similar a los siguientes ejemplos que realizaremos.

## Usamos operadores aritméticos con variables numéricas

datos_covid %>% mutate(edad_centrada = edad - mean(edad, na.rm = T),
                       edad_normalizada = edad_centrada / sd(edad, na.rm = T)) %>% 
  select(edad, edad_centrada, edad_normalizada)

## notemos que se puede llamar a una nueva variable desde otra creación (edad_centrada)

## Usamos operadores aritméticos con variables fecha

datos_covid %>% 
  mutate(dif_fecha = fecha_apertura - fecha_inicio_sintomas) %>% 
  select(fecha_inicio_sintomas, fecha_apertura, dif_fecha)

# excluímos los NA

datos_covid %>% 
  mutate(dif_fecha = fecha_apertura - fecha_inicio_sintomas) %>% 
  filter(!is.na(dif_fecha)) %>% 
  select(fecha_inicio_sintomas, fecha_apertura, dif_fecha)


## aplicado sobre datos resumidos y agrupados

datos_covid %>% 
  group_by(residencia_provincia_nombre) %>% 
  count(clasificacion_resumen, name = "Frec")

# queremos calcular el porcentaje de Frec en cada provincia

datos_covid %>% 
  group_by(residencia_provincia_nombre) %>% 
  count(clasificacion_resumen, name = "Frec") %>% 
  mutate(Porc = Frec / sum(Frec) * 100)
  
# para visualizar el efecto del grupo podemos hacer

## sumatoria de los porcentajes (100 % para cada grupo (provincia))
datos_covid %>% 
  group_by(residencia_provincia_nombre) %>% 
  count(clasificacion_resumen, name = "Frec") %>% 
  mutate(Porc = Frec / sum(Frec) * 100) %>% 
  summarise(Porc = sum(Porc))

# aplicamos ungroup() y los porcentajes cambiam porque la sumatoria se
# realiza sobre todas las filas

datos_covid %>% 
  group_by(residencia_provincia_nombre) %>% 
  count(clasificacion_resumen, name = "Frec") %>% 
  ungroup() %>% 
  mutate(Porc = Frec / sum(Frec) * 100)

## Acumulativos

# En esta tabla de datos cada fila es una observación cargada en el SNVS

# podríamos construir una tabla de frecuencia con conteos de confirmados
# por semana epidemiológica, así:

conf_sepi <- datos_covid %>% 
  filter(clasificacion_resumen == "Confirmado") %>% 
  count(anio, sepi_apertura, name = "confirmados") 

# en esta nueva tabla tenemos la cantidad de confirmados cargados en SISA 
# por semana epidemiológica

# si quisieramos obtener los conteos acumulados deberíamos aplicar mutate()

conf_sepi %>% 
  mutate(acumulados = cumsum(confirmados))

## Ordenamiento

# podriamos también construir una tabla con los confirmados por provincia
# de residencia

conf_prov <- datos_covid %>% 
  filter(clasificacion_resumen == "Confirmado") %>% 
  count(residencia_provincia_nombre, name = "confirmados")

# una de las funciones de ranking es min_rank() y también se aplica con mutate()

conf_prov %>% 
  mutate(ranking = min_rank(confirmados)) 

# agregamos desc() para ranking descendente

conf_prov %>% 
  mutate(ranking = min_rank(desc(confirmados))) 

## ordenamos la salida para ver mejor los valores de la variable construida

conf_prov %>% 
  mutate(ranking = min_rank(desc(confirmados))) %>% 
  arrange(ranking)


### Volvemos a las diapositivas (nro. 7)

## if_else()

# tomemos una variable para ejemplificar el uso de if_else()

# veamos cuantas categorías tiene clasificacion_resumen

datos_covid %>% 
  distinct(clasificacion_resumen)

## son 4 categorías. Supongamos que deseamos unificar dos de ellas 
# (Sospechoso y Sin Clasificar) en una sola categoría denominada "Sin resultado"

datos_covid %>%  
  mutate(nueva_clasificacion = if_else(clasificacion_resumen %in% c("Sospechoso","Sin Clasificar"),"Sin resultado", clasificacion_resumen)) %>% 
  count(nueva_clasificacion)


## case_when()

# Una de las tareas habituales en el manejo de la variable edad es construir
# grupos etarios para categorizar la variable original cuantitativa

# por ejemplo necesitamos construir grupos de edad con los siguientes intervalos
# de clase:

# 0 a 17 años
# 18 a 24 años
# 25 a 34 años
# 35 a 49 años
# 50 a 64 años
# 65 y más años

## antes vamos a recortar un poco la tabla de datos para trabajar con menor
## cantidad de filas y evitar demora en la carga de trabajo

covid_SL <- datos_covid %>% 
  filter(residencia_provincia_nombre == "San Luis")



covid_SL %>% 
  mutate(grupo_edad = case_when(
    edad < 18 ~ "1. 0 a 17 años",
    edad > 17 & edad < 25 ~ "2. 18 a 24 años",
    edad > 24 & edad < 35 ~ "3. 25 a 34 años",
    edad > 34 & edad < 50 ~ "4. 35 a 49 años",
    edad > 49 & edad < 65 ~ "5. 50 a 64 años",
    edad > 64 ~ "6. 65 y más años"
  )
  ) %>% 
  count(grupo_edad)


# igual usando between()

covid_SL %>% 
  mutate(grupo_edad = case_when(
    edad < 18 ~ "1. 0 a 17 años",
    between(edad, 18, 24) ~ "2. 18 a 24 años",
    between(edad, 25, 34) ~ "3. 25 a 34 años",
    between(edad, 35, 49) ~ "4. 35 a 49 años",
    between(edad, 50, 64) ~ "5. 50 a 64 años",
    edad > 64 ~ "6. 65 y más años"
  )
  ) %>% 
  count(grupo_edad)
