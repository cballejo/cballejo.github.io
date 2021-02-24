library(tidyverse)
library(datos)

encuesta

encuesta %>% distinct(anio)

enc2014 <- encuesta %>% filter(anio == 2014)

## barras de estado civil

enc2014 %>% 
  ggplot(aes(x = estado_civil)) +
  geom_bar()

## agregamos relleno seteado

enc2014 %>% 
  ggplot(aes(x = estado_civil)) +
  geom_bar(fill = "skyblue4")


## agregamos relleno y color seteado

enc2014 %>% 
  ggplot(aes(x = estado_civil)) +
  geom_bar(fill = "skyblue4", color = "blue")


## boxplot con edad según estado civil

enc2014 %>% 
  ggplot(aes(x= estado_civil, y = edad)) +
  geom_boxplot()



## agregamos relleno mapeado

enc2014 %>% 
  ggplot(aes(x= estado_civil, y = edad, fill = estado_civil)) +
  geom_boxplot()

## cambiamos escala de color

enc2014 %>% 
  ggplot(aes(x= estado_civil, y = edad, fill = estado_civil)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set1")

# agregamos capa de puntos con argumentos shape, size y alpha

enc2014 %>% 
  ggplot(aes(x= estado_civil, y = edad, fill = estado_civil)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set1") +
  geom_jitter(shape = 20, size = 1, alpha = 0.5)


# histograma de edad 

enc2014 %>% 
  ggplot(aes(x= edad)) +
  geom_histogram() 


# cambiamos tamaño de intervalo de clase a 5

enc2014 %>% 
  ggplot(aes(x= edad)) +
  geom_histogram(binwidth = 5) 

# agregamos relleno seteado y color de contorno para visualizar barras

enc2014 %>% 
  ggplot(aes(x= edad)) +
  geom_histogram(binwidth = 5, fill = "maroon", color = "white") 

# cambiamos escala eje x (5 en 5)

enc2014 %>% 
  ggplot(aes(x= edad)) +
  geom_histogram(binwidth = 5, fill = "maroon", color = "white") +
  scale_x_continuous(breaks = seq(0,90, by = 5))


# incorporamos una variable que estratifique los valores de edad

# histograma de edad según estado_civil

enc2014 %>% 
  ggplot(aes(x= edad, fill = estado_civil)) +
  geom_histogram() +
  facet_grid(estado_civil ~ .)

