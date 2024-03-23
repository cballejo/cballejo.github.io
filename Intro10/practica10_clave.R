## Practica 10

## Lectura de datos

## Estando dentro de un proyecto nos vamos a desentender de la ubicación de
## los archivos que deseemos leer, dado que siempre las funciones de lectura
## buscaran en la carpeta asociada al proyecto.

## Por lo tanto los archivos de datos deberán guardarse dentro de esa carpeta

## En RStudio podemos visualizar el contenido desde el panel Files

#  o bien podemos ejecutar:

dir()  #devuelve el contenido del directorio de trabajo (carpeta del proyecto)

## Archivos texto plano

## Para determinar la forma de lectura de archivos tipo csv o de texto plano
## separados por algún caracter necesitamos que nos indiquen previamente el
## formato o debemos investigar por nuestra cuenta.

## Vamos a tratar de leer el archivo "Dieta.csv" 

## Para esto podemos utilizar el panel File y pulsando sobre el archivo 
## visualizar el contenido con View File

## Debemos prestar atención a la cabecera del archivo y al caracter que aparece 
## separando las columnas.

## En este caso vemos que tiene cabecera y el separador es la coma (,)

## Podemos utilizar read_csv() de readr, pero antes tenemos que activar tidyverse

library(tidyverse)

read_csv(file = "Dieta.csv")

## read_csv() efectivamente hizo la lectura pero solo lo muestra en consola

## Por qué?

## Bien, porque necesitamos asignar la lectura a un nombre que será el nombre
## del dataframe 

dieta <- read_csv(file = "Dieta.csv")

## Que pasa si me equivoco aplico mal la función, por ejemplo:

read_csv2(file = "Dieta.csv")

# Una primera revisión, cada vez que hacemos alguna lectura, es verificar
# la cantidad de variables

## Ahora háganlo con el archivo "CancerMama.csv"


# Qué sucede con el nombre de la tercer columna?


## Veamos si podemos solucionarlo importando desde el menú de RStudio (Import Dataset)

## INSERTE AQUÍ EL CÓDIGO GENERADO POR IMPORT DATASET


## A continuación revisemos el contenido del archivo farmaco.txt

## Que separador utiliza?

## Qué función podemos aplicar?



#############

## Archivos Excel

## Activamos el paquete para hacer lecturas de archivos Excel

library(readxl)

## Tenemos un archivo de Excel para practicar, se llama datos.xlsx

## Ábranlo y vean el contenido de las tres hojas

# con la función excel_sheets() también podemos ver cuantas hojas tiene
# y como se llaman

excel_sheets(path = "datos.xlsx")

## Vamos con la primer hoja

datos1 <- read_excel(path = "datos.xlsx", sheet = 1)

# que es lo mismo que hacer:

datos1 <- read_excel("datos.xlsx")

# asume 1 en sheet de forma predeterminada

## Con la hoja 2 tenemos un problema

datos2 <- read_excel(path = "datos.xlsx", sheet = 2)

datos2

## Cómo se soluciona?



## Vamos con la tercera llamada Poblacion

## Hay dos tablas dentro de la misma hoja, cómo podemos hacer la lectura?


pob2010 <- read_excel(path = "datos.xlsx", 
                      sheet = "Poblacion", 
                      range = "A4:D25")

## Hagan lo mismo para leer la otra tabla con range y guardenla como pob2021


## De qué forma podemos unirlas?



######################

## Práctico de integración

## antes de comenzar vaciamos el entorno pulsando sobre la escobita

## Activamos tidyverse
library(tidyverse)

## Vamos a leer el archivo "BsAs_Covid_2021.csv" bajo el nombre datos

datos <- read_csv2("BsAs_Covid_2021.csv")

## Exploramos el dataframe

glimpse(datos)

## La tabla contiene los casos SISA Covid de la provincia de BsAs del año 2021
## (fecha de apertura desde 1/1/2021 hasta 4/5/2021)

## Son 1854424 observaciones y hay 11 variables

## Queremos calcular la tasa de mortalidad por Covid por sexo y por partido (departamento)

## Vamos a necesitar datos de población, proyecciones para cada departamento 
## para el año 2021

## Podemos buscar en el INDEC y bajarla

# https://sitioanterior.indec.gob.ar/nivel4_default.asp?id_tema_1=2&id_tema_2=24&id_tema_3=119

## Antes de leerla, hay que explorar...

## Discutimos estrategias de lectura


## Activamos paquete para lectura de Excel
library(readxl)

datos <- read_csv2("BsAs_Covid_2021.csv")

## Lectura y organización de los datos

partidos1 <- read_excel("proy_1025_depto_buenos_aires.xls", 
                        range = "A11:A34", col_names = F) %>% 
  rename(Partido = "...1")

partidos2 <- read_excel("proy_1025_depto_buenos_aires.xls", 
                        range = "A37:A146", col_names = F) %>% 
  rename(Partido = "...1")

partidos <- partidos1 %>% bind_rows(partidos2)


varones1 <- read_excel("proy_1025_depto_buenos_aires.xls", 
                       range = "M157:M180", col_names = F) %>% 
  rename(Varon = "...1")

varones2 <- read_excel("proy_1025_depto_buenos_aires.xls", 
                       range = "M183:M292", col_names = F) %>% 
  rename(Varon = "...1")

varones <- varones1 %>%  bind_rows(varones2)

mujeres1 <- read_excel("proy_1025_depto_buenos_aires.xls", 
                       range = "M303:M326", col_names = F) %>% 
  rename(Mujer = "...1")

mujeres2 <- read_excel("proy_1025_depto_buenos_aires.xls", 
                       range = "M329:M438", col_names = F) %>% 
  rename(Mujer = "...1")

mujeres <- mujeres1 %>%  bind_rows(mujeres2)


poblacion <- bind_cols(partidos, varones, mujeres)

poblacion <- poblacion %>% 
  pivot_longer(cols = 2:3, names_to = "SEXO", values_to = "Poblacion") %>% 
  mutate(SEXO = if_else(SEXO == "Varon", "M", "F")) 


## Conteo de fallecidos por COVID por sexo y por departamento

datos %>% distinct(SEXO)

datos %>% distinct(CLASIF_RESUMEN)

datos %>% distinct(FALLECIDO)

datos %>% distinct(DEPARTAMENTO_RESIDENCIA) %>% 
  arrange(DEPARTAMENTO_RESIDENCIA)

fallecidos <- datos %>% filter(CLASIF_RESUMEN == "Confirmado", 
                               FALLECIDO == "SI", 
                               DEPARTAMENTO_RESIDENCIA!="*sin dato*",
                               SEXO != "A") %>% 
  group_by(DEPARTAMENTO_RESIDENCIA, SEXO) %>% 
  count(FALLECIDO, name = "Fallecidos") %>%  select(-FALLECIDO) %>% ungroup() %>% 
  rename(Partido = "DEPARTAMENTO_RESIDENCIA")


# Control

fallecidos %>% 
  left_join(poblacion) %>% 
  filter(is.na(Poblacion))

## arreglo

poblacion <- poblacion %>% 
  mutate(Partido = if_else(Partido == "Coronel de Marina Leonardo Rosales", "Coronel de Marina L. Rosales", Partido)) 

## El partido de Lezama fue creado en 2009 como escisión de Chascomús
## No hay información de su proyección poblacional por parte de INDEC

## Unión

mortalidad <- fallecidos %>% 
  left_join(poblacion) %>% 
  filter(Partido != "Lezama")

## Cálculo de las tasas x 100000

mortalidad %>% group_by(Partido,SEXO) %>% 
  summarise(Tasa = 100000*Fallecidos/Poblacion)


mortalidad %>% group_by(Partido,SEXO) %>% 
  summarise(Tasa = 100000*Fallecidos/Poblacion) %>% 
  pivot_wider(names_from = SEXO, values_from = Tasa)


mortalidad %>% group_by(Partido) %>%
  summarise(Fallecidos = sum(Fallecidos),
            Poblacion = sum(Poblacion)) %>% 
  group_by(Partido) %>%
  summarise(Tasa = 100000*Fallecidos/Poblacion) 


#####

## Parte 2

# Comparaciones - ajuste de tasas

## Tasas por sexo

tasas <- mortalidad %>% group_by(Partido) %>%
  summarise(Fallecidos = sum(Fallecidos),
            Poblacion = sum(Poblacion)) %>% 
  group_by(Partido) %>%
  summarise(Tasa = 100000*Fallecidos/Poblacion) 


tasas_sexo <- mortalidad %>% group_by(Partido, SEXO) %>%
  summarise(Fallecidos = sum(Fallecidos),
            Poblacion = sum(Poblacion)) %>% 
  group_by(Partido, SEXO) %>%
  summarise(Tasa = 100000*Fallecidos/Poblacion) 

# Tomamos dos partidos con tasas diferentes (Rauch e Hipólito Yrigoyen)

## Vamos a comparar previo ajuste


# Revisamos categorías de variables SISA

datos %>% distinct(GRUPO_ETARIO_MORTALIDAD) 

datos %>% distinct(SEXO)


# Filtramos los códigos de los dos partidos

deptos <- datos %>%  filter(ID_DEPTO_INDEC_RESIDENCIA %in% c("06406", "06672"))

## Controlamos que categorias quedan
deptos %>% distinct(DEPARTAMENTO_RESIDENCIA)

deptos %>% distinct(GRUPO_ETARIO_MORTALIDAD)

deptos %>% distinct(SEXO)

## contamos fallecidos en Rauch

falleRauch <- deptos %>% filter(CLASIF_RESUMEN == "Confirmado", 
                               FALLECIDO == "SI", 
                               DEPARTAMENTO_RESIDENCIA =="Rauch",
                               SEXO != "A") %>% 
  group_by(SEXO, GRUPO_ETARIO_MORTALIDAD) %>% 
  count(FALLECIDO, name = "Fallecidos") %>%  select(-FALLECIDO) %>% 
  pivot_wider(names_from = SEXO, values_from = Fallecidos) %>% 
  mutate(Total = F+M) %>% 
  pivot_longer(2:4, names_to = "SEXO", values_to = "Fallecidos")

## contamos fallecidos en Yrigoyen

falleYrigoyen <- deptos %>% filter(CLASIF_RESUMEN == "Confirmado", 
                                FALLECIDO == "SI", 
                                DEPARTAMENTO_RESIDENCIA =="Hipólito Yrigoyen",
                                SEXO != "A") %>% 
  group_by(SEXO, GRUPO_ETARIO_MORTALIDAD) %>% 
  count(FALLECIDO, name = "Fallecidos") %>%  select(-FALLECIDO) %>% 
  pivot_wider(names_from = SEXO, values_from = Fallecidos) %>% 
  mutate(Total = F+M) %>% 
  pivot_longer(2:4, names_to = "SEXO", values_to = "Fallecidos")

###  Tablas INDEC x Partido, Sexo y Edad

# Estas tablas o cuadros estadísticos se descargan del INDEC del siguiente sitio:

# https://sitioanterior.indec.gob.ar/censos_provinciales.asp?id_tema_1=2&id_tema_2=41&id_tema_3=135&p=06&d=000&t=3&s=6&c=2010


# Lectura y procesamiento Rauch

Rauch <- read_excel("P2-D_6_672.xls", 
                        range = "A9:D129", col_names = F) %>% 
  rename(Edad = "...1", Total = "...2", Varones = "...3", Mujeres = "...4") %>% 
  filter(nchar(Edad) < 3) %>%
  mutate(Varones = as.numeric(Varones),
         Mujeres = as.numeric(Mujeres),
         Total = as.numeric(Total),
         Varones = replace_na(Varones, 0),
         Mujeres = replace_na(Mujeres, 0),
         Total = replace_na(Total, 0),
         GRUPO_ETARIO_MORTALIDAD = case_when(    # grupo etario igual a SISA
           Edad < 20 ~ "0 a 20 Años",
           Edad > 19 & Edad < 40 ~ "20 a 39 Años",
           Edad > 39 & Edad < 60 ~ "40 a 59 Años",
           Edad > 59 ~ "60+"
         )) %>% 
  group_by(GRUPO_ETARIO_MORTALIDAD) %>% 
  summarise(Total = sum(Total),  
            M = sum(Varones),
            F = sum(Mujeres)) %>% 
  pivot_longer(cols = 2:4, names_to = "SEXO", values_to = "Poblacion")
  
## Lectura y procesamiento Yrigoyen

Yrigoyen <- read_excel("P2-D_6_406.xls", 
                             range = "A9:D129", col_names = F) %>% 
  rename(Edad = "...1", Total = "...2", Varones = "...3", Mujeres = "...4") %>% 
  filter(nchar(Edad) < 3) %>%
  mutate(Varones = as.numeric(Varones),
         Mujeres = as.numeric(Mujeres),
         Total = as.numeric(Total),
         Varones = replace_na(Varones, 0),
         Mujeres = replace_na(Mujeres, 0),
         Total = replace_na(Total, 0),
         GRUPO_ETARIO_MORTALIDAD = case_when(
           Edad < 20 ~ "0 a 20 Años",
           Edad > 19 & Edad < 40 ~ "20 a 39 Años",
           Edad > 39 & Edad < 60 ~ "40 a 59 Años",
           Edad > 59 ~ "60+"
         )) %>% 
  group_by(GRUPO_ETARIO_MORTALIDAD) %>% 
  summarise(Total = sum(Total),
            M = sum(Varones),
            F = sum(Mujeres)) %>% 
  pivot_longer(cols = 2:4, names_to = "SEXO", values_to = "Poblacion")

## Tasa específicas por sexo y edad para cada partido

tasa_esp_Rauch <- falleRauch %>% 
  inner_join(Rauch, by = c("SEXO", "GRUPO_ETARIO_MORTALIDAD")) %>% 
  group_by(SEXO, GRUPO_ETARIO_MORTALIDAD) %>% 
  summarise(Tasa = 100000*Fallecidos/Poblacion) %>% ungroup()

tasa_esp_Yrigoyen <- falleYrigoyen %>% 
  inner_join(Yrigoyen, by = c("SEXO", "GRUPO_ETARIO_MORTALIDAD")) %>% 
  group_by(SEXO, GRUPO_ETARIO_MORTALIDAD) %>% 
  summarise(Tasa = 100000*Fallecidos/Poblacion) %>% ungroup()

## Ajuste con población estándar OMS

pob_OMS <- read_csv2("Poblacion_std.csv")  # lectura de población estandar OMS 2000-2025

## Ajuste Rauch

tasa_est_Rauch <- tasa_esp_Rauch %>% 
  inner_join(pob_OMS, by = c("GRUPO_ETARIO_MORTALIDAD" = "Gedad")) %>% 
  mutate(Tasa_estandarizada = Tasa*`Pob-std`) 

## Por sexo y total

tasa_est_Rauch %>%  
  group_by(SEXO) %>% 
  summarise(Tasa_std = sum(Tasa_estandarizada))

## comparamos con las anteriores

tasas %>%  filter(Partido == "Rauch") 

tasas_sexo %>%  filter(Partido == "Rauch")

### Ajuste Yrigoyen

tasa_est_Yrigoyen <- tasa_esp_Yrigoyen %>% 
  inner_join(pob_OMS, by = c("GRUPO_ETARIO_MORTALIDAD" = "Gedad")) %>% 
  mutate(Tasa_estandarizada = Tasa*`Pob-std`) 

## Por sexo y total

tasa_est_Yrigoyen %>%  
  group_by(SEXO) %>% 
  summarise(Tasa_std = sum(Tasa_estandarizada))

## comparamos con las anteriores

tasas %>%  filter(Partido == "Hipólito Yrigoyen")

tasas_sexo %>%  filter(Partido == "Hipólito Yrigoyen")


### Ahora hagan lo mismo con General Pueyrredón

# El archivo de población se llama P2-D_6_357.xls

Pueyrredon <- read_excel("P2-D_6_357.xls", 
                       range = "A9:D129", col_names = F) %>% 
  rename(Edad = "...1", Total = "...2", Varones = "...3", Mujeres = "...4") %>% 
  filter(nchar(Edad) < 3) %>%
  mutate(Varones = as.numeric(Varones),
         Mujeres = as.numeric(Mujeres),
         Total = as.numeric(Total),
         Varones = replace_na(Varones, 0),
         Mujeres = replace_na(Mujeres, 0),
         Total = replace_na(Total, 0),
         GRUPO_ETARIO_MORTALIDAD = case_when(
           Edad < 20 ~ "0 a 20 Años",
           Edad > 19 & Edad < 40 ~ "20 a 39 Años",
           Edad > 39 & Edad < 60 ~ "40 a 59 Años",
           Edad > 59 ~ "60+"
         )) %>% 
  group_by(GRUPO_ETARIO_MORTALIDAD) %>% 
  summarise(Total = sum(Total),
            M = sum(Varones),
            F = sum(Mujeres)) %>% 
  pivot_longer(cols = 2:4, names_to = "SEXO", values_to = "Poblacion")



## General Pueyrredón

Puey <- datos %>%  filter(ID_DEPTO_INDEC_RESIDENCIA == "06357")

fallePuey <- Puey %>% filter(CLASIF_RESUMEN == "Confirmado", 
                             FALLECIDO == "SI",
                             SEXO != "A") %>% 
  group_by(SEXO, GRUPO_ETARIO_MORTALIDAD) %>% 
  count(FALLECIDO, name = "Fallecidos") %>%  select(-FALLECIDO) %>% 
  pivot_wider(names_from = SEXO, values_from = Fallecidos) %>% 
  mutate(Total = F+M) %>% 
  pivot_longer(2:4, names_to = "SEXO", values_to = "Fallecidos")

tasa_esp_Puey <- fallePuey %>% inner_join(Pueyrredon, by = c("SEXO", "GRUPO_ETARIO_MORTALIDAD")) %>% 
  group_by(SEXO, GRUPO_ETARIO_MORTALIDAD) %>% 
  summarise(Tasa = 100000*Fallecidos/Poblacion) %>% ungroup()

tasa_est_Puey <- tasa_esp_Puey %>% 
  inner_join(pob_OMS, by = c("GRUPO_ETARIO_MORTALIDAD" = "Gedad")) %>% 
  mutate(Tasa_estandarizada = Tasa*`Pob-std`) 

tasa_est_Puey %>%  
  group_by(SEXO) %>% 
  summarise(Tasa_std = sum(Tasa_estandarizada))

tasas %>%  filter(Partido == "General Pueyrredón")

tasas_sexo %>%  filter(Partido == "General Pueyrredón")
