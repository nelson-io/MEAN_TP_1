#Activamos librerias
library(tidyverse)
library(lubridate)
library(skimr)
library(rio)
library(units)




#Descargamos los datos de Buenos Aires Data
# bicis_df <- read_csv(
#   "http://cdn.buenosaires.gob.ar/datosabiertos/datasets/bicicletas-publicas/recorridos-realizados-2018.csv"
# )


bicis_df <- import("bicis_df.RDS")

#Guardamos archivo localmente
# export(bicis_df, "bicis_df.RDS")


# EDA

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

#duracion de recorridos e identificacion de outliers

# problema, pasar a minutos la duracion del recorrido

#Primero usamos expresiones regulares para aislar la componente hms

bicis_df$duracion_recorrido <- bicis_df$duracion_recorrido %>%
  str_extract("(\\d{2}:){2}\\d{2}")

#coercemos la variable a numerico para obtener segundos y transformamos a minutos
bicis_df$duracion_recorrido_minutos <- (hms(bicis_df$duracion_recorrido) %>% 
                                          as.numeric())/60

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
  
 

#Dado que hay outliers




