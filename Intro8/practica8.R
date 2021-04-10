## Practica 8 

## Datos ordenados (paquete tidyr)

library(tidyverse) # tiene incluido el paquete tidyr

## Las siguientes tablas contienen la misma información pero ordenadas 
## en forma diferente

tabla1

tabla2

tabla3

tabla4a

tabla4b

## Cambiamos el formato usando pivot_longer()

tabla4a

tabla4a %>% 
  pivot_longer(cols = 2:3, names_to = "anio", values_to = "casos")

## Cambiamos el formato usando pivot_wider()

tabla2

tabla2 %>% 
  pivot_wider(names_from = tipo, values_from = cuenta)


## Ejercicio 1

## La siguiente tabla, que viene dentro del paquete tidyr, contiene información
## perteneciente al Banco Mundial de la cantidad de población de los países 
## del mundo 

glimpse(world_bank_pop)

## Posee 1056 observaciones y 20 columnas

## Observe la forma en que están dispuestos los datos y mediante las
## funciones pivot transfórmela en tidy data.

world_bank_pop

## Solución

world_bank_pop %>% pivot_longer(cols = 3:20, names_to = "Anio", values_to = "Poblacion")


## Volvemos a las diapositivas

############

## Usamos separate() y unite()

# Separamos valores de la columna tasa de la tabla3

tabla3

tabla3 %>% separate(col = tasa, into = c("casos", "poblacion"), sep = "/")

## en algunas situaciones como en este ejemplo podemos omitir el separador
## se debe a que es el único símbolo diferente a los demás (todo lo otro es número)

tabla3 %>% separate(col = tasa, into = c("casos", "poblacion"))

## Si la estructura de los valores es regular, es decir toda la estructura tiene
## el mismo formato (por ejemplo las patentes de los autos) en el argumento
## separador se puede utilizar un número que represente la primera parte de
## la estructura.

x <- tribble(
  ~Patente,
  "FHG427",
  "HUJ234",
  "LKU678"
)

x %>% separate(col = Patente, into = c("letras", "numeros"), sep = 3)

## Ahora aplicamos unite() en la siguiente tabla

tabla5

## Tenemos el anio dividido en dos partes (siglo y anio)

tabla5 %>% unite(col = nueva, 2:3, sep = "")

## Si no definimos el argumento sep en "" toma como valor predeterminado el "_"

tabla5 %>% unite(col = nueva, 2:3)

## Volvemos a las diapositivas

##############

## Trabajamos con valores faltantes

## Primero construiremos una pequeña tabla para poder jugar con ella

lesiones <- tribble(
  ~Anio, ~Semestre, ~Sexo, ~Edad, ~Accidente, ~Fallecido,
   2018,          1, "Mujer", 45, "Transito", "Si",
   2019,          1,    NA, 60, "Transito", "No",
   2019,          2, "Varon", 12, "Caida", NA,
   2020,         1,"Mujer", NA, "Quemadura", NA,
   2021,          1, "Mujer", 76, NA, "No"
)

## Usamos replace_na()

## Veamos algunas de las formas en que podemos aplicar la función para 
## etiquetar los valores NA

## en toda la tabla con formato list()

lesiones %>% replace_na(replace = list(Sexo = "s/d", Accidente = "Desconocido"))

## en una variable específica

lesiones %>% count(Accidente)

lesiones %>% count(Accidente = replace_na(Accidente, replace = "Sin dato"))

## por medio de mutate()

lesiones %>% mutate(Fallecido = replace_na(Fallecido,"Sin información"))

## Usamos drop_na()

## Similar al na.omit()

lesiones

lesiones %>%  drop_na() # con drop_na()

lesiones %>%  na.omit() # igual resultado con na.omit()

## si quisieramos eliminar solo las observaciones en función de los
## NA de una variable

lesiones

lesiones %>% drop_na(Sexo)

lesiones %>%  na.omit(Sexo) # no funciona!!

## podemos sumar variables

lesiones %>% drop_na(Sexo, Edad)

## Usamos complete()

lesiones

lesiones %>%  complete(Anio, Semestre)

lesiones %>%  complete(Anio, Semestre)

###########

## Ejercicio sobre datos reales

## La tabla de datos con la trabajaremos en esta practica contiene datos 
## de tuberculosis (TB) detallados por año, país, edad, sexo y método de 
## diagnóstico. 

## Los datos provienen del Informe de Tuberculosis de la Organización 
## Mundial de la Salud 2014, disponible en 
## http://www.who.int/tb/country/data/download/en/.

## Existe abundante información epidemiológica en este dataset, pero es 
## complicado trabajar con estos datos tal como son entregados:

## Inicialmente activamos el paquete necesario:

library(datos) # contiene la tabla oms con la que vamos a trabajar

glimpse(oms)

## La tabla contiene 7260 observaciones y 60 variables

## Este es un ejemplo muy típico de una base de datos de la vida real. 
## Contiene columnas redundantes, códigos extraños de variables y muchos 
## valores faltantes. 

## Analicemos su estructura:

## Pareciera ser que pais, iso2 e iso3 son variables redundantes que se 
## refieren al país (codificadas de diferentes formas).

## anio es claramente una variable

## Para entender el significado de las siguientes variables vamos a necesitar
## un diccionario de datos que acompañe la tabla

## La parte inicial del nombre de la columnas indica si los casos son nuevos o 
## antiguos (recaída)

## ep se refiere a tuberculosis extra pulmonar

## fpn refiere a casos de tbc frotis pulmonar negativo

## fpp refiere a casos de tbc frotis pulmonar positivo

## la lerta después del símbolo _ indica sexo. h para hombres y m para mujeres

## Los números siguientes están asociados al grupo etáreo con 7 categorías

# 014 = 0 a 14 años de edad
# 1524 = 15 – 24 años de edad
# 2534 = 25 – 34 años de edad
# 3544 = 35 – 44 años de edad
# 4554 = 45 – 54 años de edad
# 5564 = 55 – 64 años de edad
# 65 = 65 o más años de edad

# En principio esta claro que estas columnas no representan variables y
# que son conteos de una combinación de ellas. Por lo que vamos a 
# transformar la tabla de ancho a largo con pivot_longer()

oms1 <- oms %>%
  pivot_longer(
    cols = 5:60, 
    names_to = "clave", 
    values_to = "casos", 
    values_drop_na = TRUE
  )

## guargamos la tabla convertida en oms1

## usamos el argumento values_drop_na = TRUE para saltear las observaciones 
## con valores na en las columnas de la 5 a la 60

## Si hojeamos la tabla original oms visualizaremos que muchos países en
## distintos años tienen valores faltantes

View(oms)

## Si no utilizásemos el argumento en TRUE la operación consideraría estas
## observaciones con valores faltantes produciendo una tabla a lo largo
## mucho más grande

oms %>%
  pivot_longer(
    cols = 5:60, 
    names_to = "clave", 
    values_to = "casos"
  ) %>% glimpse()

## 405.440 observaciones contra 76.046 omitiendo los valores NA

## Ahora para dimensionar la cantidad de las distintas columnas que se encuentran en clave
## podemos hacer un conteo:

oms1 %>%
  count(clave)


## A la vista de esos nombres combinados en la columna clave vamos a necesitar
## separar las diferentes partes que luego se convertiran en variables

## Para utilizar separate() necesitamos definir un separador unívoco, es decir
## que se presente en todos los valores.

oms1 %>% arrange(desc(clave))

## Tenemos un problema con los valores nuevosrecaida porque no hay un caracter separador 
## entre las dos palabras

## Necesitamos hacer un pequeño cambio al formato de esos nombres.

## Veremos más adelante en cadenas de caracteres funciones que nos ayudan
## a llevar a cabo esta tarea, pero la idea básica es bastante simple: 
## reemplazar los caracteres “nuevosrecaida” por “nuevos_recaida”. 

## Esto genera nombres de variables consistentes.

oms2 <- oms1 %>%
  mutate(clave = str_replace(clave, "nuevosrecaida", "nuevos_recaida"))

oms2 %>% arrange(desc(clave))

## Ahora podemos separar los valores en cada código aplicando la función separate()
## Primero utilizando como separador el guión bajo "_"

oms3 <- oms2 %>%
  separate(col = clave, into = c("nuevos", "tipo", "sexo_edad"), sep = "_")

oms3

## Vemos que hay variables que son redundantes o constantes como el caso
## de "nuevos" y los código isoX. Para simplificar la tabla podemos eliminarlas

oms4 <- oms3 %>%
  select(-nuevos, -iso2, -iso3)

## Finalmente nos queda separar sexo_edad. Podemos utilizar un digito en el 
## argumento sep, dado que hay una sola letra que corresponde al sexo

oms5 <- oms4 %>%
  separate(col = sexo_edad, into = c("sexo", "edad"), sep = 1)

oms5

## Genial!! Conseguimos que la tabla de datos esté "ordenada".

## Vimos el códido paso a paso en forma separada y almacenando los resultados
## parciales en diferentes tablas llamadas oms1, oms2, etc

## En la práctica estas operaciones (oraciones) se realizan en un sólo bloque
## (parrafo) 

oms %>%
  pivot_longer(
    cols = 5:60,
    names_to = "clave", 
    values_to = "valor", 
    values_drop_na = TRUE) %>%
  mutate(clave = stringr::str_replace(clave, "nuevosrecaida", "nuevos_recaida")) %>%
  separate(clave, c("nuevos", "tipo", "sexo_edad")) %>%
  select(-nuevos, -iso2, -iso3) %>%
  separate(sexo_edad, c("sexo", "edad"), sep = 1)

## Con los datos ordenados en oms5 calcule para cada país, año y sexo el total 
## del número de casos de tuberculosis. 

oms5 %>% group_by(pais, anio, sexo) %>% 
  summarise(Casos = sum(casos))


