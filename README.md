---
title: "Ecobicis"
author: "Germán Martinez, Sol Rodríguez Giralt, Nelson Shilman & Ezequiel Vannoni"
date: "2020-04-18"
output:
  html_document:
    keep_md: true
---

# Trabajo práctico n°1 - ECOBICIS -

#### 1) Descargue los datos de 2018 y a través de gráficos/tabulaciones describa brevemente\n las principales estadísticas relacionadas al uso de la EcoBici.


Primero procedemos a activar los paquetes que vamos a emplear y descargamos los datos a usar
a lo largo del trabajo práctico.


```r
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
```


```r
bicis_df <- read_csv(
  "http://cdn.buenosaires.gob.ar/datosabiertos/datasets/bicicletas-publicas/recorridos-realizados-2018.csv"
)

usuarios_df <- map_df(2015:2018,~ read_csv(paste0(
  "http://cdn.buenosaires.gob.ar/datosabiertos/datasets/bicicletas-publicas/usuarios-ecobici-",
  .x, ".csv")) %>%
    mutate(fecha_alta = parse_date(fecha_alta)))
```

##### Análisis exploratorio de datos

La primer pregunta que nos surgió al descargar estos datasets, fue la de cómo fue evolucionando
la cantidad de operaciones realizadas por día a lo largo del año 2018. Para ello, hemos tenido en cuenta como operaciones cada una de las observaciones.

para ello, primero generamos la variable *fecha_origen_ymd* extrayendo la componente de fecha de la variable
*fecha_origen_ymd*, agrupamos los datos por *fecha_origen_ymd* para calcular los registros diarios y graficamos:


```r
bicis_df$fecha_origen_ymd <- date(bicis_df$fecha_origen_recorrido)

operaciones_dia <- bicis_df %>%
  group_by(fecha_origen_ymd) %>%
  summarise(registros = n())
```



```r
ggplot(operaciones_dia)+
  geom_line(aes(x = fecha_origen_ymd, y = registros))+
  ggtitle("Número de operaciones por día")+
  xlab("Mes")+
  ylab("Número de Operaciones")+
  theme_bw()
```

![](README_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Ahora queremos ver el comportamiento de la cantidad de registros por mes para el mismo período,
En este caso, primero extraemos la componente mes de la variable *fecha_origen_ymd* con la función month del paquete **Lubridate**, la cual configuramos para no sólo extraer el mes, sino también para ofrecer el nombre de cada mes como un factor ordenado, lo que resulta de gran utilidad a la hora de graficar.

Después graficamos con un bar plot


```r
operaciones_mes <- bicis_df %>%
  group_by(mes = month(fecha_origen_ymd, label = T, abbr =F)) %>%
  summarise(registros = n())

ggplot(operaciones_mes)+
  geom_col(aes(x = mes, y = registros), alpha=.8)+
  ggtitle("Número de operaciones por mes")+
  xlab("Meses")+
  ylab("# de Operaciones")+
  theme_bw()
```

![](README_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

Aquí notamos que agosto, septiembre y octubre son los meses que registran más operaciones , mientras que diciembre ,
enero febrero y marzo registran la menor cantidad de operaciones.


Habiéndo observado el comportamiento de la serie y la cantidad de los registros por mes, ahora queremos mostrar la distribución de la variable *duracion_recorrido*. 

lamentablemente, al observar los primeros registros de esta variable, encontramos que su formato no resulta del todo amigable y que debemos parsearla para retirar el componente de horas, minutos y segundos, a continuación se muestra el formato original de la variable:


```r
bicis_df$duracion_recorrido[1:5] %>% kable()
```



|x                         |
|:-------------------------|
|0 days 00:19:53.000000000 |
|0 days 00:26:19.000000000 |
|0 days 00:27:39.000000000 |
|0 days 00:48:51.000000000 |
|0 days 00:49:27.000000000 |

Dentro de las posibles estrategias de parseo, decidimos la de extraer la componente indicada a partir de la detección de patrones en la variable, empleando expresiones regulares. Luego cohercemos la variable para obtener el resultado
en horas, minutos y segundos, y por último realizamos las operaciones necesarias para conservar el resultado expresado en minutos.


```r
bicis_df$duracion_recorrido <- bicis_df$duracion_recorrido %>%
  str_extract("(\\d{2}:){2}\\d{2}")

bicis_df$duracion_recorrido_minutos <- (hms(bicis_df$duracion_recorrido) %>% 
                                          as.numeric())/60
```

```
## Warning in .parse_hms(..., order = "HMS", quiet = quiet): Some strings failed to
## parse, or all strings are NAs
```


Ya con la variable parseada, procedemos a mostrar el histograma:


```r
ggplot(bicis_df)+
  geom_histogram(aes(x = duracion_recorrido_minutos),
                 col = "white", alpha = .8, bins = 80)+
  theme_bw()+
  ggtitle("Tiempo de uso de ECOBICI")+
  xlab("Minutos")+
  ylab("Número de operaciones")
```

![](README_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

El histograma nos evidencia una clara asimetría positiva en los datos como así también la presencia de outliers.

Con motivo de visualizar la distribución de los datos segmentada por día de semana, procedemos a extraer la componente del día de semana de la variable *fecha_origen_ymd* empleando la función wday del paquete **Lubridate**.

Luegos mostramos un boxplot identificando los días de semana en el eje de abscisas y para mostrar el comportamiento
de los datos, aplicamos una capa de puntos con jitter que posee una muestra aleatoria de los registros.


```r
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
```

![](README_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

En el gráfico se observa que el percentil 50 de las variables es muy parecido para todos los días de semana pero no lo es para los días Domingo y Sábado. Asimismo notamos mayor dispersión en el fin de semana, como así también la superposición de la capa de puntos con jitter nos muestra que estos días hay menos registros que en el resto de la semana.


Posicionándonos sobre las observaciones extremas, procedemos a identificar los outliers a partir del criterio del rango intercuartil, esto es, consideramos como outlier toda observación cuyo valor se encuentra por encima de 1.5 veces el rango intercuartil y observamos cómo se distribuyen


```r
outliers <- bicis_df %>% 
  filter(duracion_recorrido_minutos > 1.5*IQR(duracion_recorrido_minutos, na.rm = T)+
           quantile(duracion_recorrido_minutos,.75, na.rm = T))
```


```r
ggplot(outliers)+
  geom_histogram(aes(x =duracion_recorrido_minutos), col = " White", binwidth = 3)+
  theme_bw()+
  scale_x_continuous(breaks = seq(60,180,20))+
  ggtitle("Histograma de Outliers")+
  xlab("Duración del recorrido (Minutos)")+
  ylab("Número de operaciones")
```

![](README_files/figure-html/unnamed-chunk-10-1.png)<!-- -->


Aquí también se evidencia una fuerte asimetría positiva con valores máximos cercanos a las 3 horas.


Ahora nos enfocamos en uno de los aspectos que consideramos más importantes dentro del análisis exploratorio que es 
la identificación de missing data. para ello, como paso preliminar, computamos la cantidad de observaciones faltantes por variable


```r
map_df(bicis_df, ~ sum(is.na(.))) %>% 
  gather(key = "Variable",value = "Missings") %>% 
  arrange(desc(Missings)) %>% 
  kable()
```



Variable                      Missings
---------------------------  ---------
duracion_recorrido               43723
fecha_destino_recorrido          43723
duracion_recorrido_minutos       43723
id_estacion_destino              30486
long_estacion_destino            30486
lat_estacion_destino             30486
domicilio_estacion_destino       30486
id_estacion_origen               30224
long_estacion_origen             30224
lat_estacion_origen              30224
domicilio_estacion_origen        30224
id_usuario                           0
genero_usuario                       0
fecha_origen_recorrido               0
nombre_estacion_origen               0
nombre_estacion_destino              0
fecha_origen_ymd                     0
dia_semana                           0

Encontramos que los registros faltantes se ubican en las decenas de miles para múltiples variables!
Esto en realidad no resulta tan asombrante dado que contamos con millones de observaciones. Por ello, a continuación mostramos los mismos datos expresados como el porcentaje de completitud de cada variable


```r
map_df(bicis_df, ~ 1- sum(is.na(.))/sum(!(is.na(.)))) %>% 
  gather(key = "Variable",value = "Completitud" ) %>% 
  arrange(Completitud) %>% 
  kable()
```



Variable                      Completitud
---------------------------  ------------
duracion_recorrido              0.9830284
fecha_destino_recorrido         0.9830284
duracion_recorrido_minutos      0.9830284
id_estacion_destino             0.9882270
long_estacion_destino           0.9882270
lat_estacion_destino            0.9882270
domicilio_estacion_destino      0.9882270
id_estacion_origen              0.9883293
long_estacion_origen            0.9883293
lat_estacion_origen             0.9883293
domicilio_estacion_origen       0.9883293
id_usuario                      1.0000000
genero_usuario                  1.0000000
fecha_origen_recorrido          1.0000000
nombre_estacion_origen          1.0000000
nombre_estacion_destino         1.0000000
fecha_origen_ymd                1.0000000
dia_semana                      1.0000000

Ya vimos que la missing data supone menos del 1.2% de las observaciones, pero aún así resulta de la máxima importancia entender qué es lo que ocurre con esos datos faltantes.\n \n

Nos surge la pregunta de si estos datos ausentes no se encuentran disponibles de manera aleatroria o si es que existe algún tipo de patrón en la ausencia de información.

Para testear esto, tratamos de entender las interacciones de los datos faltantes, esto es, evaluar las intersecciones en que distintas de las variables se ausentan de manera simultánea y observar si existe algún patrón en particular o no. Para ello realizamos el siguiente gráfico:


```r
gg_miss_upset(bicis_df %>% select(-duracion_recorrido_minutos), nsets = n_var_miss(bicis_df))
```

![](README_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

Se observa que  el caso más reiterado es en el que no hay datos de  **fecha_destino_recorrido** ni de **duración_recorrido**,seguido por los casos en que no hay datos del domicilio de la estación de destino ni de sus coordenadas geográficas y ell caso análogo para los datos de origen. \n Otras combinaciones aparecen con menor frecuencia

lo que llama poderosamente la atención en esta visual, es que las observaciones que no disponen
datos de id de estación, cordenadas geográficas y domicilio, sí suelen disponer de nombre
de estación.

Dado este hallazgo, identificaremos los nombres de esas estaciones y en caso de que sea
posible inferir el resto de los datos, procederemos a imputarlos.


```r
bicis_df %>% 
  filter(is.na(id_estacion_origen)) %>% 
  pull(nombre_estacion_origen) %>% table() %>% kable()
```



.                      Freq
-------------------  ------
Ecoparque             15715
Fitz Roy y Gorriti    14509

```r
bicis_df %>% 
  filter(is.na(id_estacion_destino)) %>% 
  pull(nombre_estacion_destino) %>% table() %>% kable()
```



.                      Freq
-------------------  ------
Ecoparque             15946
Fitz Roy y Gorriti    14540

observamos que las estaciones con datos faltantes corresponden a las de Ecoparque
y Fitzroy y Gorriti

De una pequeña búsqueda cursada en google, surge que esas estaciones poseen los siguientes
atributos:


```r
recovered_data <-  data.frame(nombre_de_estacion = c("Ecoparque", "Fitz Roy y Gorriti"),
           id_estacion = c(44, 159),
           latitud = c(-34.575327, -34.584879),
           longitud = c(-58.414603, -58.437229),
           domicilio = c("Av. del Libertador 3260","Fitz Roy y Gorriti"))
```

Ahora imputamos estos datos en nuestro Data Frame


```r
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
```


Si ahora volvemos a evaluar la estructura de los NA observamos lo siguiente:


```r
gg_miss_upset(bicis_df %>% select(-duracion_recorrido_minutos), nsets = n_var_miss(bicis_df))
```

![](README_files/figure-html/unnamed-chunk-17-1.png)<!-- -->
 
 
