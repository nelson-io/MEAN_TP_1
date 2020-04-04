#Activamos librerias
library(tidyverse)
library(lubridate)
library(skimr)
library(rio)
library(units)
library(naniar)




#Descargamos los datos de Buenos Aires Data
 # bicis_df <- read_csv(
 #   "http://cdn.buenosaires.gob.ar/datosabiertos/datasets/bicicletas-publicas/recorridos-realizados-2018.csv"
 # )


bicis_df <- import("bicis_df.RDS")

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
gg_miss_upset(bicis_df %>% select(-duracion_recorrido_minutos))

# Se observa que existen 3 tipos de combinaciones de missing data, el caso más reiterado es en el que 
# no hay datos de  fecha_destino_recorrido ni de duración_recorrido
# seguido por los casos en que no hay datos del domicilio de la estación de destino ni de sus coordenadas geográficas
# por último hay ungrupo muy reducido en el que ninguna de las 5 variables descritas anteriormente están presentes

#g por último ofrecemos algunas estadísticas descriptivas

bicis_df$genero_usuario <- as.factor(bicis_df$genero_usuario)
summary(bicis_df)


# 2)








