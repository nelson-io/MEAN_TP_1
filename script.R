#Activamos librerias
library(tidyverse)
library(lubridate)
library(skimr)
library(rio)
library(naniar)
library(jsonlite)
library(ggthemes)
library(pwr)
library(parsedate)
library(boot)
library(knitr)

# Descargamos los datos de Buenos Aires Data
bicis_df <- read_csv(
  "http://cdn.buenosaires.gob.ar/datosabiertos/datasets/bicicletas-publicas/recorridos-realizados-2018.csv"
)

usuarios_df <- map_df(2015:2018,~ read_csv(paste0(
  "http://cdn.buenosaires.gob.ar/datosabiertos/datasets/bicicletas-publicas/usuarios-ecobici-",
  .x, ".csv")) %>%
    mutate(fecha_alta = parse_date(fecha_alta)))


# 1) EDA



#a observar evolución de cantidad de operaciones por día

bicis_df$fecha_origen_ymd <- date(bicis_df$fecha_origen_recorrido)

operaciones_dia <- bicis_df %>%
  group_by(fecha_origen_ymd) %>%
  summarise(registros = n())


ggplot(operaciones_dia)+
  geom_line(aes(x = fecha_origen_ymd, y = registros))+
  ggtitle("Número de operaciones por día")+
  xlab("Mes")+
  ylab("Número de Operaciones")+
  theme_bw()

#b operaciones por mes

operaciones_mes <- bicis_df %>%
  group_by(mes = month(fecha_origen_ymd, label = T, abbr =F)) %>%
  summarise(registros = n())

ggplot(operaciones_mes)+
  geom_col(aes(x = mes, y = registros), alpha=.8)+
  ggtitle("Número de operaciones por mes")+
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
                 col = "white", alpha = .8, bins = 80)+
  theme_bw()+
  ggtitle("Tiempo de uso de ECOBICI")+
  xlab("Minutos")+
  ylab("Número de operaciones")  
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
  ggtitle("Duracion de viaje por dia de la semana")+
  xlab("Día")+
  ylab("Duración del recorrido (Minutos)")
  
 

#e identificamos presencia de outliers en duracion_recorrido_minutos a través del criterio del rango intercuartil

outliers <- bicis_df %>% 
  filter(duracion_recorrido_minutos > 1.5*IQR(duracion_recorrido_minutos, na.rm = T)+
           quantile(duracion_recorrido_minutos,.75, na.rm = T))

# Hacemos un histograma de los outliers para ver cómo se distribuyen

ggplot(outliers)+
  geom_histogram(aes(x =duracion_recorrido_minutos), col = " White", binwidth = 3)+
  theme_bw()+
  scale_x_continuous(breaks = seq(60,180,20))+
  ggtitle("Histograma de Outliers")+
  xlab("Duración del recorrido (Minutos)")+
  ylab("Número de operaciones")
  

# Se puede observar una notable asimetría hacia la derecha en los outliers estando los valores más extremos
# rondando las 3 horas
  
# f Evaluamos cantidad de misings por variable
map_df(bicis_df, ~ sum(is.na(.))) %>% 
  gather(key = "Variable",value = "Missings") %>% 
  arrange(desc(Missings)) %>% 
  kable()

#  Evaluamos el porcentaje de completitud de cada variable

map_df(bicis_df, ~ 1- sum(is.na(.))/sum(!(is.na(.)))) %>% 
  gather(key = "Variable",value = "Completitud" ) %>% 
  arrange(Completitud) %>% 
  kable()

# Evaluamos la existencia de patrones de missingness


gg_miss_upset(bicis_df %>% select(-duracion_recorrido_minutos), nsets = n_var_miss(bicis_df))

# Se observa que existen 7 tipos de combinaciones de missing data, el caso más reiterado es en el que 
# no hay datos de  fecha_destino_recorrido ni de duración_recorrido
# seguido por los casos en que no hay datos del domicilio de la estación de destino ni de sus coordenadas geográficas
# El caso análogo para los datos de origen
# Otras combinaciones que aparecen con menor frecuencia

#lo que llama poderosamente la atención en esta visual, es que las observaciones que no disponen
#datos de id de estación, cordenadas geográficas y domicilio, sí suelen disponer de nombre 
#de estación. 

#Dado este hallazgo, identificaremos los nombres de esas estaciones y en caso de que sea 
#posible inferir el resto de los datos, procederemos a imputarlos.

bicis_df %>% 
  filter(is.na(id_estacion_origen)) %>% 
  pull(nombre_estacion_origen) %>% table() %>% kable()

bicis_df %>% 
  filter(is.na(id_estacion_destino)) %>% 
  pull(nombre_estacion_destino) %>% table() %>% kable()

#observamos que las estaciones con datos faltantes corresponden a las de Ecoparque
#y Fitzroy y Gorriti

# De una pequeña búsqueda cursada en google, surge que esas estaciones poseen los siguientes
#atributos

recovered_data <-  data.frame(nombre_de_estacion = c("Ecoparque", "Fitz Roy y Gorriti"),
           id_estacion = c(44, 159),
           latitud = c(-34.575327, -34.584879),
           longitud = c(-58.414603, -58.437229),
           domicilio = c("Av. del Libertador 3260","Fitz Roy y Gorriti"))

# Ahora imputamos en nuestro data frame
bicis_df[bicis_df$nombre_estacion_origen == "Ecoparque",
         c("nombre_estacion_origen",
           "id_estacion_origen",
           "lat_estacion_origen",
           "long_estacion_origen",
           "domicilio_estacion_origen")] <- recovered_data %>% slice(1) 

bicis_df[bicis_df$nombre_estacion_origen == "Fitz Roy y Gorriti",
         c("nombre_estacion_origen",
           "id_estacion_origen",
           "lat_estacion_origen",
           "long_estacion_origen",
           "domicilio_estacion_origen")] <- recovered_data %>% slice(2) 


bicis_df[bicis_df$nombre_estacion_destino == "Ecoparque",
         c("nombre_estacion_destino",
           "id_estacion_destino",
           "lat_estacion_destino",
           "long_estacion_destino",
           "domicilio_estacion_destino")] <- recovered_data %>% slice(1)

bicis_df[bicis_df$nombre_estacion_destino == "Fitz Roy y Gorriti",
         c("nombre_estacion_destino",
           "id_estacion_destino",
           "lat_estacion_destino",
           "long_estacion_destino",
           "domicilio_estacion_destino")] <- recovered_data %>% slice(2)

# Si ahora volvemos a evaluar la estructura de los NA observamos lo siguiente

gg_miss_upset(bicis_df %>% select(-duracion_recorrido_minutos), nsets = n_var_miss(bicis_df))

# en todos los NA restantes se omiten tanto la duración del recorrido como la fecha de destino
# y esta no es información que a priori corresponda imputar


#g Ofrecemos algunas estadísticas descriptivas

bicis_df$genero_usuario <- as.factor(bicis_df$genero_usuario)
summary(bicis_df)


# Por último hacemos una pirámide poblacional de los usuarios inscriptos
usuarios_plot <- usuarios_df %>% 
  filter(usuario_sexo %in% c("M","F")) %>% 
  mutate(rango_etario = cut(usuario_edad,breaks = seq(15,100,5))) %>% 
  filter(!is.na(rango_etario)) %>% 
  group_by(usuario_sexo, rango_etario) %>% 
  summarise(total = n())


usuarios_plot$total[usuarios_plot$usuario_sexo == "M"] <-
  -1 * usuarios_plot$total[usuarios_plot$usuario_sexo == "M"]



ggplot(usuarios_plot, aes(x = rango_etario, y = total, fill = usuario_sexo))+
  geom_bar(stat = "identity")+
  scale_y_continuous(breaks = seq(-24e3,20e3,4e3),
                     labels =c(seq(24e3,0,-4e3), seq(4e3,20e3,4e3)) )+
  coord_flip()+
  theme_bw()+
  ggtitle("Piramide poblacional de usuarios de ECOBICI")+
  ylab("Número de usuarios")+
  xlab("Rango etario (años)")+
  labs(fill = "Sexo")




# 2) #partimos de la cantidad total de usuarios por día de semana

operaciones_dia <- operaciones_dia %>% 
  mutate(dia_semana = wday(fecha_origen_ymd,label = T,abbr = F))

# generamos intervalos de confianza por día de semana


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
  geom_point(aes(y= media), size = 2, shape = 1 )+
  coord_flip()+
  theme_tufte() +
  theme(
    legend.position = "none",
  ) +
  xlab("Día de la semana") +
  ylab("Cantidad de Registros")+
  ggtitle("Intervalos de confianza de registros medios por día de semana")

# A partir de la tabla y tal como se ha constatado en la visual, para un nivel de significancia del 5%
# ünicamente podemos encontrar diferencias significativas en las medias al comparar el
# grupo comprendido por los días lunes, martes, mércoles, jueves y viernes, con el comprendido por los 
# días sábado y domingo.
# Es decir, existe superposición de los intevalos de confianza entre los días de semana entre sí, como así
# también para los días de los fines de semana entre sí pero no al comparar días de semana con días del
# fin de semana.



# 3) identificamos días feriados de API de feriados


feriados <- fromJSON("http://nolaborables.com.ar/api/v2/feriados/2018") %>% 
  transmute(feriado = ymd(paste0("2018-",mes,"-",dia))) %>% 
  pull()

#filtramos omitiendo estos días y corremos lo mismo que en 2)

bicis_df_sf <- bicis_df %>% 
  filter(!(fecha_origen_ymd %in% feriados))


# generamos intervalos de confianza por día de semana


operaciones_dia_summ_sf <- operaciones_dia %>% 
  filter(!(fecha_origen_ymd %in% feriados)) %>% 
  group_by(dia_semana) %>% 
  summarise(media = mean(registros),
            int_min = t.test(registros,conf.level = .95)$conf.int[1],
            int_max = t.test(registros,conf.level = .95)$conf.int[2])

#hacemos Cleveland point plot

ggplot(operaciones_dia_summ_sf, aes(x = fct_rev(dia_semana))) +
  geom_segment( aes(xend=dia_semana, y=int_min, yend=int_max), color="grey") +
  geom_point( aes(y=int_min), color=rgb(0.7,0.2,0.1,0.5), size=3 ) +
  geom_point( aes( y=int_max), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
  geom_point(aes(y= media), size = 2, shape = 1 )+
  coord_flip()+
  theme_tufte() +
  theme(
    legend.position = "none",
  ) +
  xlab("Día de la semana") +
  ylab("Cantidad de Registros")+
  ggtitle("Intervalos de confianza de registros medios por día de semana (sin feriados)")

#luego de comparar los resultados y hacer las visualizaciones, encontramos variaciones
# que denotan una mayor cantidad de operaciones para todos los días de la semana (excepto jueves
# dado que no hubo feriados). En este sentido, las mayores diferencias absolutas se aprecian en los días 
# lunes y martes. No obstante, dados los nuevos intervalos de confianza, no hay evidencia estadística de
# que al remover los feriados haya diferencias en las medias vs contemplando los feriados con un nivel
# de significatividad del 5%. Adicionalmente, se siguen presentando diferencias estadísticamente
# significativas entre los valores medios de los días de semana y los correspondientes a los fines de semana.
 

# 4) Identificamos los registros del usuario 606320 con origen distinto a destino y
#omitimos los que tienen destino u origen NA
df_606320 <- bicis_df %>% 
  filter(id_usuario == 606320,
         nombre_estacion_origen != nombre_estacion_destino)

#Calculamos distancia entre estaciones 
#para esto usamos la API de google maps.

# seteamos la API KEY
apiKEY <- read_lines(tcltk::tk_choose.files()) #elegimos el documento donde hayamos guardado nuestra apiKEY

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
rm(list = setdiff(ls(),c("df_606320", "bicis_df", "usuarios_df")))

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

# Observamos que la velocidad media es de 9 KM/H empleando la media harmónica
# No obstante, no son equidistantes los recorridos por lo que resulta conveniente
# emplear una media harmónica ponderada por las distancias.
# Al usar la media Harmónica ponderada, la velocidad media es de 9.9093 km/h

# El problema de emplear la media aritmética, es que este estadístico
# se calcula de manera aditiva y no contempla el hecho de que cuando vamos a una velocidad mayor,
# es menor el tiempo que se 
# transcurre viajando. Consecuentemente, el estadístico suele sobreestimar la velocidad.
# En cambio, la media harmónica, al emplear recíprocos,permite anular este efecto,
# ya que implícitamente opera con el ratio de horas por km.

# 5)
bicis_9_66 <-  bicis_df %>% 
  filter(id_estacion_origen == 9,
         id_estacion_destino == 66,
         !is.na(duracion_recorrido_minutos))


#hacemos binom test para diferencias en proporciones
x <-  vector(mode = "integer", length = 2L)
x[1] <- sum(bicis_9_66$duracion_recorrido_minutos > 15)
x[2] <- sum(!bicis_9_66$duracion_recorrido_minutos > 15)

binom.test(x = x, p = .2, alternative = "less")

# 6
# Construimos curva de potencia del test
x <- seq(.05,.2,.001)
x_pwr <- pwr.p.test(h = ES.h(x ,.2),n = 1073, alternative = "less" ,sig.level = .05)$power 
plot(x, x_pwr, type = "l",lwd = 2)


# 7 Consultando wikipedia, encontramos que la inauguración de la estación "Facultad de Derecho"
# de la línea H, ha sido inaugurada el 17 de mayo de 2018.

#identificamos los registros en que el origen y/o destino haya sido la estación de ecobici
# Facultad de Derecho, agrupamos las observaciones por día e identificamos las observaciones previas
# con las anteriores 


bicis_fd <- bicis_df %>% 
  filter(id_estacion_origen == 1 | id_estacion_destino == 1) %>% 
  group_by(fecha_origen_ymd) %>% 
  summarise(total = n()) %>% 
  mutate(inaugurado = ifelse(fecha_origen_ymd >= ymd("2018-05-17"),1,0))
  
  

#graficamos
ggplot(bicis_fd)+
  geom_line(aes(x=fecha_origen_ymd, y = total))+
  geom_vline(xintercept = ymd("2018-05-17"), col = "red")+
  theme_bw()+
  annotate("text",ymd("2018-06-20"),300, 
           label = "Inauguración Línea H \n Facultad de Derecho", size = 3, col= "red")+
  ggtitle("Número de operaciones diarias involucrando estación Facultad de Derecho")+
  xlab("Tiempo")+
  ylab("Número de operaciones")

t.test(x = bicis_fd[bicis_fd$inaugurado ==1,"total"],
       y = bicis_fd[bicis_fd$inaugurado ==0,"total"],
       alternative = "greater")

#sí, hay diferencias estadísticamente significativas



#8 #evaluamos si hay diferencias estadísticamente significativas en uso por género


t.test(x = bicis_df[bicis_df$genero_usuario == "M","duracion_recorrido_minutos"],
       y = bicis_df[bicis_df$genero_usuario == "F","duracion_recorrido_minutos"],
       alternative = "two.sided",
       conf.level = .95)

# No tiene mucho sentido comparar por cantidad de usos porque ya sabemos que tanto en el padrón como en el uso
# hay muchos más usuarios de sexo masculino que femenino


#9 observamos lo mismo por grupo etario
#  hacemos un left join de registros con usuarios para saber la edad de cada uno
## primero hacemos un anti-join para saber si quedarían registro exceptuados
anti_join(bicis_df, usuarios_df %>% select(usuario_id,usuario_edad) %>% unique(),
          by = c("id_usuario" = "usuario_id")) %>% 
  select(id_usuario) %>% 
  unique() %>% 
  pull() %>% 
  length() #hay 31.105 ids de los cuales no hay información


# Ahora observamos si algun usuario aparece con más de una edad en la tabla de usuarios
usuarios_df %>% 
  select(usuario_id, usuario_edad) %>% 
  unique() %>% 
  group_by(usuario_id) %>% 
  summarise(total = n()) %>% 
  filter(total > 1)


# observamos que esto acontece con dos usuarios. Procedemos a quedarnos con la última observación
# asociada a cada registro

usuarios_df <- usuarios_df %>% 
  select(usuario_id, usuario_edad) %>% 
  group_by(usuario_id) %>% 
  summarise(usuario_edad = last(usuario_edad)) %>% 
  ungroup()

# realizamos el left join y omitimos registros sin edad o con una edad irracionalmente alta     

bicis_df_joined <-  left_join(bicis_df, usuarios_df,by = c("id_usuario" = "usuario_id")) %>% 
  filter(!is.na(usuario_edad),
         !usuario_edad > 100)

# ahora vamos a definir el rango etario. Vamos a  priorizar un output de grupos 
# aproximadamente balanceados por lo que emplearemos los deciles de edad para armar 10 grupos

cuts <- c(as.integer(quantile(bicis_df_joined$usuario_edad, seq(0,1,.1))))
bicis_df_joined$rango_etario <- cut(bicis_df_joined$usuario_edad, breaks = cuts)
  
#análisis de varianza
aov_rango_etario <- aov(duracion_recorrido_minutos ~ rango_etario , data = bicis_df_joined)

# test honesto de Tukey pra visualizar relaciones
TukeyHSD(aov_rango_etario)



#10
#definimos subset de datos
bicis_joined_9_66 <-  bicis_df_joined %>% 
  filter(id_estacion_origen == 9,
         id_estacion_destino == 66,
         !is.na(duracion_recorrido_minutos))

#definimos función
cor_boot <- function(d,i){
 data <- d[i,]
return(cor(data$usuario_edad,data$duracion_recorrido_minutos))
}

#hacemos bootstrap
set.seed(0)
boot_out <- boot(data = bicis_joined_9_66,statistic = cor_boot,R = 1000)

# Hacemos histograma

ggplot(boot_out$t %>% data.frame(replicates = .), aes(x = replicates))+
  geom_histogram(aes(y =..density..), col = "white", binwidth = .008)+
  geom_density(col = "blue", size = 1, fill = "blue", alpha = .2)+
  theme_bw()+
  ggtitle("Distribución de réplicas bootstrap\n Coef. de Correlación entre edad y minutos de uso")+
  xlab("réplicas")+
  ylab("densidad")

boot.ci(boot.out = boot_out, type = "perc",conf = .95) 

#hay correlación positiva y estadísticamente significativa entre las variables. 
  
