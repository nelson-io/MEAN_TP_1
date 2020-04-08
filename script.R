#Activamos librerias
library(tidyverse)
library(lubridate)
library(skimr)
library(rio)
library(units)
library(naniar)
library(jsonlite)
library(hrbrthemes)
library(ggthemes)



# Descargamos los datos de Buenos Aires Data
# bicis_df <- read_csv(
#   "http://cdn.buenosaires.gob.ar/datosabiertos/datasets/bicicletas-publicas/recorridos-realizados-2018.csv"
# )


# bicis_df <- import("bicis_df.RDS")

#Guardamos archivo localmente
  # export(bicis_df, "bicis_df.RDS")


# 1) EDA



#a observar evolución de cantidad de operaciones por día

bicis_df$fecha_origen_ymd <- date(bicis_df$fecha_origen_recorrido)

operaciones_dia <- bicis_df %>%
  group_by(fecha_origen_ymd) %>%
  summarise(registros = n())


ggplot(operaciones_dia)+
  geom_line(aes(x = fecha_origen_ymd, y = registros))+
  ggtitle("Cantidad de operaciones por día")+
  xlab("Operaciones")+
  ylab("# de Operaciones")+
  theme_bw()

#b operaciones por mes

operaciones_mes <- bicis_df %>%
  group_by(mes = month(fecha_origen_ymd, label = T, abbr =T)) %>%
  summarise(registros = n())

ggplot(operaciones_mes)+
  geom_col(aes(x = mes, y = registros), fill = "blue", alpha=.8)+
  ggtitle("Cantidad de operaciones por mes")+
  xlab("Meses")+
  ylab("# de Operaciones")+
  theme_bw()

#c duracion de recorridos 

# problema, pasar a minutos la duracion del recorrido

# Primero usamos expresiones regulares para aislar la componente hms

bicis_df$duracion_recorrido <- bicis_df$duracion_recorrido %>%
  str_extract("(\\d{2}:){2}\\d{2}")

#coercemos la variable a numerico para obtener segundos y transformamos a minutos
bicis_df$duracion_recorrido_minutos <- (hms(bicis_df$duracion_recorrido) %>% 
                                          as.numeric())/60


#hacemos un histograma del tiempo de uso
ggplot(bicis_df)+
  geom_histogram(aes(x = duracion_recorrido_minutos),
                 col = "white", fill = "blue", alpha = .8, bins = 50)+
  theme_bw()+
  ggtitle("Tiempo de uso de ECOBICI")+
  xlab("Minutos")+
  ylab("Frecuencia")  
## notamos fuerte asimetría hacia la derecha y la presencia de outliers

#d duración de recorrido por día de semana 

#generamos variable dia de semana 




bicis_df$dia_semana <-  bicis_df$fecha_origen_ymd %>% wday(label = T, abbr = F)

ggplot(bicis_df %>% sample_n(2e4),
       aes(x = dia_semana, y =duracion_recorrido_minutos))+
  geom_boxplot(outlier.colour = NA)+
  geom_jitter(col= "blue", alpha = .05)+
  coord_cartesian(ylim=c(0,75))+
  theme_bw()+
  ggtitle("Boxplots duracion de viaje por dia de semana")+
  xlab("dia de semana")+
  ylab("duracion del recorrido en minutos")
  
 

#e identificamos presencia de outliers en duracion_recorrido_minutos a través del criterio del rango intercuartil

outliers <- bicis_df %>% 
  filter(duracion_recorrido_minutos > 1.5*IQR(duracion_recorrido_minutos, na.rm = T)+
           quantile(duracion_recorrido_minutos,.75, na.rm = T))

# Hacemos un histograma de los outliers para ver cómo se distribuyen

ggplot(outliers)+
  geom_histogram(aes(x =duracion_recorrido_minutos), col = " White", binwidth = 3)+
  theme_bw()+
  scale_x_continuous(breaks = seq(60,180,20))+
  ggtitle("Histograma de Outliers")
  

# Se puede observar una notable asimetría hacia la derecha en los outliers estando los valores más extremos
# rondando las 3 horas
  
# f Evaluamos cantidad de misings por variable
map_df(bicis_df, ~ sum(is.na(.))) %>% 
  gather(key = "Variable",value = "Missings") %>% 
  arrange(desc(Missings))

#  Evaluamos el porcentaje de completitud de cada variable

map_df(bicis_df, ~ 1- sum(is.na(.))/sum(!(is.na(.)))) %>% 
  gather(key = "Variable",value = "Completitud" ) %>% 
  arrange(Completitud)

# Evaluamos la existencia de patrones de missingness


gg_miss_upset(bicis_df %>% select(-duracion_recorrido_minutos), nsets = n_var_miss(bicis_df))

# Se observa que existen 7 tipos de combinaciones de missing data, el caso más reiterado es en el que 
# no hay datos de  fecha_destino_recorrido ni de duración_recorrido
# seguido por los casos en que no hay datos del domicilio de la estación de destino ni de sus coordenadas geográficas
# El caso análogo para los datos de origen
# Otras combinaciones que aparecen con menor frecuencia

#g por último ofrecemos algunas estadísticas descriptivas

bicis_df$genero_usuario <- as.factor(bicis_df$genero_usuario)
summary(bicis_df)


# 2) #partimos de la cantidad total de usuarios por día de semana

operaciones_dia <- operaciones_dia %>% 
  mutate(dia_semana = wday(fecha_origen_ymd,label = T,abbr = F))

# generamos intervalos de confianza por día de semana

t.test(operaciones_dia$registros,conf.level = .95)$conf.int[2]

operaciones_dia_summ <- operaciones_dia %>% 
  group_by(dia_semana) %>% 
  summarise(media = mean(registros),
            int_min = t.test(registros,conf.level = .95)$conf.int[1],
            int_max = t.test(registros,conf.level = .95)$conf.int[2])

#hacemos Cleveland point plot

ggplot(operaciones_dia_summ, aes(x = fct_rev(dia_semana))) +
  geom_segment( aes(xend=dia_semana, y=int_min, yend=int_max), color="grey") +
  geom_point( aes(y=int_min), color=rgb(0.7,0.2,0.1,0.5), size=3 ) +
  geom_point( aes( y=int_max), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
  geom_point(aes(y= media), size = 2)+
  coord_flip()+
  theme_tufte() +
  theme(
    legend.position = "none",
  ) +
  xlab("Día de la semana") +
  ylab("Cantidad de Registros")

# A partir de la tabla y tal como se ha constatado en la visual, para un nivel de significancia del 5%
# ünicamente podemos encontrar diferencias significativas en las medias al comparar el
# grupo comprendido por los días lunes, martes, mércoles, jueves y viernes, con el comprendido por los 
# días sábado y domingo.
# Es decir, existe superposición de los intevalos de confianza entre los días de semana entre sí, como así
# también para los días de los fines de semana entre sí pero no al comparar días de semana con días del
# fin de semana.



# 3)

# 4) Identificamos los registros del usuario 606320 con origen distinto a destino y
#omitimos los que tienen destino u origen NA
df_606320 <- bicis_df %>% 
  filter(id_usuario == 606320,
         nombre_estacion_origen != nombre_estacion_destino,
         !is.na(id_estacion_destino),
         !is.na(id_estacion_origen))

#Calculamos distancia entre estaciones 
#para esto usamos la API de google maps.

# seteamos la API KEY
apiKEY <- read_lines(choose.files()) #elegimos el documento donde hayamos guardado nuestra apiKEY

# Dado que las consultas son limitadas, conservamos las combinaciones únicas de origen destino
# para minimizar la cantidad total de consultas a la API

df_606320_pairs <-  df_606320 %>%
  select(
    id_estacion_origen,
    long_estacion_origen,
    lat_estacion_origen,
    id_estacion_destino,
    long_estacion_destino,
    lat_estacion_destino
  ) %>% 
  unique()

#Definimos la consulta base de la url de consulta para la api de google

url_base <- "https://maps.googleapis.com/maps/api/distancematrix/json?"

# Hacemos una función que extraiga la distancia en metros de cada recorrido
distance_extractor <- function(x){
  url <- paste0(url_base, 
                "origins=",
                x$lat_estacion_origen,",", 
                x$long_estacion_origen, "&destinations=",
                x$lat_estacion_destino, ",",
                x$long_estacion_destino,"&key=",
                apiKEY)
                
  
  
  json <- fromJSON(txt = url)
  
  distance <- json$rows$elements[[1]]$distance$value
   
  return(distance)
}

#inicializamos df
df_distancias <- data.frame()
# hacemos for loop

for(i in 1:nrow(df_606320_pairs)){
  
  df <- df_606320_pairs %>% slice(i)
  distancia <- distance_extractor(df)
  
  df_distancias <- rbind(df_distancias, cbind(df,distancia = distancia))
  
}


#hacemos un left join para integrar las distancias a los registros del usuario

df_606320 <- left_join(df_606320, df_distancias)

#removemos del global env lo que no precisemos
rm(list = setdiff(ls(),c("df_606320", "bicis_df")))

# calculamos km/h 

df_606320 <-  df_606320 %>% 
  mutate(km_h = (distancia/1e3)/(duracion_recorrido_minutos/60))


#por último calculamos la velocidad media del usuario a través de la media harmónica

harmonic_mean <- function(x){
  (mean(x^(-1)))^(-1)
}

harmonic_weighted_mean <- function(x, weights){
  (sum((weights * x^(-1)))/sum(weights))^(-1)
}



harmonic_mean(df_606320$km_h) 
harmonic_weighted_mean(x = df_606320$km_h, weights = df_606320$distancia/1e3 )

# Observamos que la velocidad media es de 8,1329 KM/H empleando la media harmónica
# No obstante, no son equidistantes los recorridos por lo que resulta conveniente
# emplear una media harmónica ponderada por las distancias.
# Al usar la media Harmónica ponderada, la velocidad media es de 8.8152 km/h

# El problema de emplear la media aritmética, es que este estadístico
# se calcula de manera aditiva y no contempla el hecho de que cuando vamos a una velocidad mayor,
# es menor el tiempo que se 
# transcurre viajando. Consecuentemente, el estadístico suele sobreestimar la velocidad.
# En cambio, la media harmónica, al emplear recíprocos,permite anular este efecto,
# ya que implícitamente opera con el ratio de horas por km.
