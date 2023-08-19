library(tidyverse)

datos_covid <- read_csv2("SISA_covid.csv")


## recordamos la estructura de la tabla de datos

glimpse(datos_covid)

# 11 variables
# 6.687.347 observaciones

### nos detenemos un momento analizando las variables y sus tipos de datos

# solo hay dos de ellas que son numéricas: edad y sepi_apertura

## para aplicar la función summarise() en principio vamos a trabajar con edad

##########

# Resumimos las edades mediante una media

datos_covid %>% 
  summarise(media_edad = mean(edad))

## se acuerdan que dijimos que los valores especiales NA (datos no disponible) 
## eran "contagiosos". 

## si el resultado de un resumen nos devuelve NA es porque al menos una observación
## de la variable en cuestión (edad en este caso) tiene valor NA

datos_covid %>% 
  filter(is.na(edad)) %>% count(edad)  # hay 3785 edades como NA

## como hacemos para que las funciones eviten estos valores?

## usando el argumento na.rm = TRUE dentro de mean()

datos_covid %>% 
  summarise(media_edad = mean(edad, na.rm = TRUE))

## podemos calcular varios resúmenes dentro del mismo summarise

datos_covid %>% 
  summarise(media_edad = mean(edad, na.rm = TRUE),
            mediana_edad = median(edad, na.rm = TRUE))

# podemos integrar otras funciones conocidas, como filter()

datos_covid %>% 
  filter(residencia_provincia_nombre == "Catamarca") %>% 
  summarise(media_edad = mean(edad, na.rm = TRUE),
            mediana_edad = median(edad, na.rm = TRUE))

# resumen de media y mediana de la edad de los casos notificados residentes 
# de la provincia de Catamarca

###########

## veamos algunas otras funciones de resumen mencionadas en las diapositivas

datos_covid %>% 
  summarise(min_edad = min(edad, na.rm = TRUE),
            max_edad = max(edad, na.rm = TRUE),
            var_edad = var(edad, na.rm = TRUE),
            sd_edad = sd(edad, na.rm = TRUE),
            IQR_edad = IQR(edad, na.rm = TRUE))

## que pasa con el valor máximo de edad? Parece que se trata de un error

# veamos los valores máximos cercanos ordenando la variable

datos_covid %>% select(edad) %>% arrange(desc(edad)) 

# hay 5 observaciones con edades imposibles

# podriamos arreglar esto usando filter

datos_covid %>% 
  filter(edad < 132) %>% 
  summarise(min_edad = min(edad, na.rm = TRUE),
            max_edad = max(edad, na.rm = TRUE),
            var_edad = var(edad, na.rm = TRUE),
            sd_edad = sd(edad, na.rm = TRUE),
            IQR_edad = IQR(edad, na.rm = TRUE))

## con quantile se puede mostrar distintos valores de posición de la distribución
## de edad. Por ejemplo el primer y tercer cuartil 


datos_covid %>%   
  summarise(cuartil = quantile(edad, 
                               na.rm = TRUE, 
                               prob = c(0.25, 0.75)))

## percentilos incluidos minimo y maximo

datos_covid %>%   
  filter(edad < 132) %>%
  summarise(percentil = quantile(edad, 
                               na.rm = TRUE, 
                               prob = seq(from = 0, to = 1, by = 0.1)))

### otras funciones resumen básicas son los conteos

datos_covid %>%   
  summarise(cantidad = n())

## cuando se resume una variable suele acompañarse con el conteo de observaciones
## para conocer el tamaño de la población/muestra con la que estamos trabajando

## n_distinc() es útil para variables categóricas o cuantitativas discretas

datos_covid %>%   
  summarise(cantidad = n_distinct(sepi_apertura))  # 53 semanas epidemiológicas

datos_covid %>%   
  summarise(cantidad = n_distinct(sexo)) # 3 categorías diferentes de sexo

####### volvemos a las diapositivas

#### agrupamientos

## la función group_by() agrupa por los valores distintos de la variable que usemos
## como argumento. Hay que pensarla como la variable o variables que deseamos que
## estratifiquen el resumen aplicado por summarise()

## si usamos group_by() solo obtenemos

datos_covid %>% group_by(sexo)

## noten que en la salida de la consola el encabezado dice Groups: sexo [3],
## mientras que si llamamos a la tabla datos_covid solamente eso no aparece

datos_covid

## pero por más que nos muestre que la tabla se encuentra agrupada por sexo en
## sus tres categorías en definitiva no nos devuelve ningun resultado útil

## en cambio si luego aplicamos summarise() nos va a calcular el resumen pedido
## para cada uno de los grupos construidos

datos_covid %>% 
  group_by(sexo) %>% 
  summarise(media_edad = mean(edad, na.rm = T))

# este resultado sería media edad estratificado por sexo

# también es interesante agregar el conteo a este resultado para conocer 
# el n de cada grupo

datos_covid %>% 
  group_by(sexo) %>% 
  summarise(media_edad = mean(edad, na.rm = T),
            cantidad = n())

# podemos anidar variables de agrupamiento 

datos_covid %>% 
  group_by(clasificacion_resumen, sexo) %>% 
  summarise(media_edad = mean(edad, na.rm = T),
            cantidad = n())

## ahora agregando la provincia de residencia y visualizando con View()

datos_covid %>% 
  group_by(residencia_provincia_nombre, clasificacion_resumen, sexo) %>% 
  summarise(media_edad = mean(edad, na.rm = T),
            cantidad = n()) %>% 
  View()

######

## Con todo lo visto hasta ahora escriba el código para obtener los siguientes
## resultados:

# A)
# La media y el desvío estandar de la edad de los fallecidos de CABA
# estratificado por sexo



# B)
# La edad mínima (ordenada de menor a mayor) de los casos confirmados de la 
# provincia de Chaco estratificado por departamento de residencia y sexo 
# (muestre en el visualizador)



# C)
# La fecha menor de apertura de los casos confirmados estratificados por provincia
# y ordenados de la fecha mayor a la menor.




