#En este proyecto haremos un análisis de datos, basandonos en dos data sets
#Ambos data sets muestran un registro de los datos relacionados al Covid-19
#Empezaremos con la importación, luego con el análisis y KPI´s y luego visualización.
#Cada sección será partida en dos, el año 2020 y 2024, viendo un antes y después

#Primero reunamos las herramientas necesarias
install.packages("sf")
library(sf)
install.packages("ggplot2")
library(ggplot2)
install.packages("tidyverse")
library(tidyverse)
library(lubridate)
install.packages("dplyr")
library(dplyr)
#Carga de Datos: 
#2020
Covid19_20=read.csv("C:\\Users\\USUARIO\\OneDrive\\Documentos\\ciencia de datos\\proyectos de ciencia de datos\\ADconR\\casos_covid19_2020.csv",sep=",",header=TRUE)
str(Covid19_20)

#2024
Covid19_24 = read.csv("C:\\Users\\USUARIO\\OneDrive\\Documentos\\ciencia de datos\\proyectos de ciencia de datos\\ADconR\\casos_covid19_2024.csv", sep = ";", stringsAsFactors = FALSE)
str(Covid19_24)
#los convertimos a dataframe para cuidar la fuente principal de datos
C19_20=data.frame(Covid19_20)
C19_24=data.frame(Covid19_24)

#Tratar con los Nulos de cada DataFrame

C19_20[is.na(C19_20)]= "Sin dato"
C19_20[C19_20 == ""] = "Sin dato"
#2024
C19_24[is.na(C19_24)]= "Sin dato"

#Hagamos un par de cambios y adaptaciones
#2020
# Haremos una conversión, para poder convertir la cadena de texto en fecha
#Como R solo reconoce el inglés, y el formato original está en español, haremos traducción
convertor_fecha = function(fecha_char) {
  fecha_char = toupper(fecha_char)
  meses_esp = c("ENE", "FEB", "MAR", "ABR", "MAY", "JUN",
                 "JUL", "AGO", "SEP", "OCT", "NOV", "DIC")
  meses_eng = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN",
                 "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")
  #bucle for para recrres los vectores del bloque anterior
  #seq_along recorre el vector meses_esp para iterarlos
  #gsub reemplaza sus equivalentes del español al inglés
  for (i in seq_along(meses_esp)) {
    fecha_char = gsub(meses_esp[i], meses_eng[i], fecha_char)
  }
  return(fecha_char)
}
#el modelo de código
#df$fecha <- as.Date(df$fecha_char, format = "%d%b%Y")
#aplicaremos lo mismo para las otras columnas


C19_20$fecha_muestra = as.Date(C19_20$fecha_toma_muestra, format = "%d%b%Y")

C19_20$fecha_de_clasificacion = as.Date(C19_20$fecha_clasificacion, format = "%d%b%Y")

#Ahora borraremos las columnas inecesarias y corregiremos la posición

C19_20$fecha_apertura_snvs= NULL
C19_20$fecha_toma_muestra= NULL
C19_20$fecha_clasificacion=NULL

C19_20 = C19_20[, c("numero_de_caso", "fecha_apertura", "fecha_muestra", "fecha_de_clasificacion", "provincia","barrio","comuna","genero","edad","clasificacion","fecha_fallecimiento","fallecido","fecha_alta","tipo_contagio")]


#2024
#Ningún cambio por ahora

#Empecemos con el análisis de cada dataframe

#Empecemos con:

#Fechas:
#2020
#Hagamos un conteo por año
C19_20 %>%
  mutate(anio = year(fecha_apertura)) %>%
  count(anio)
#Al hacer un conteo por año, nos sale que hay un total de 573332 de casos en el 2020 de Covid_19
#Analicemos por mes
C19_20 %>%
  mutate(mes = month(fecha_apertura)) %>%
  count(mes)

# Mes/Nro de casos:   2/9-- 3/1227--4  6095--5/17588--6 39942--7 72286--8 98855
# 9 93041--10 83568--11 64988--12 95733
#Siendo 9 casos con los que inició en Febrero del 2020, y llegando a un pico de 98855 en el mes de Agosto

#Veamos la fecha mas común de registros de pacientes con Covid_19

C19_20%>%
  count(fecha_apertura) %>%
  arrange(desc(n)) %>%
  slice(1)
#la Fecha mas común de registro de Covid_19 fue el 30-12-2020, con 5853 pacientes

#Fecha de toma de muestra con mas pacientes:

C19_20%>%
  count(fecha_muestra) %>%
  arrange(desc(n)) %>%
  slice(1)
#En la fecha 28-12-2020 se tomaron mas muestras de pacientes con Covid_19, unas 5260 muestras

#Fecha de clasificación:

C19_20%>%
  count(fecha_de_clasificacion) %>%
  arrange(desc(n)) %>%
  slice(1)

# En la Fecha 28-12-2020 se clasificaron alrededor de 4959 pacientes de Covid_19



#2024
#Hagamos el mismo análisis


C19_24 %>%
  mutate(anio = year(fecha_apertura_snvs)) %>%
  count(anio)
#Al hacer un conteo por año, nos sale que hay un total de 57230 de casos en el 2024 de Covid_19

C19_24 %>%
  mutate(mes = month(fecha_apertura_snvs)) %>%
  count(mes)

# Mes/Nro de casos:  1/15175 2/11031-- 3/5246--4/2954--5/4441--6/4668--7/3683--8/2677
# 9/2401--10/2175--11/1573--12/1206
#Siendo 15175 casos con los que inició en Enero del 2024, y llegando a 1206 en el mes de Diciembre, finalizando así el año

#Veamos la fecha mas común de registros de pacientes con Covid_19

C19_24%>%
  count(fecha_apertura_snvs) %>%
  arrange(desc(n)) %>%
  slice(1)
#la Fecha mas común de registro de Covid_19 fue el 12-01-2024, con 3330 pacientes

#Fecha de toma de muestra con mas pacientes:

C19_24%>%
  count(fecha_toma_muestra) %>%
  arrange(desc(n)) %>%
  slice(1)
#En la fecha 29-01-2024 se tomaron mas muestras de pacientes con Covid_19, unas 887 muestras

#Fecha de clasificación:

C19_24%>%
  count(fecha_clasificacion) %>%
  arrange(desc(n)) %>%
  slice(1)

# En la Fecha 29-01-2024 se clasificaron alrededor de 885 pacientes de Covid_19


#Analicemos ahora a nivel geográfico a estos data sets

#2020
#Provincia
C19_20 %>%
  count(provincia) %>%
  arrange(desc(n))
#Provincia con mas casos de Covid_19 es CABA con 276012 casos
#La provincia con menos casos registrados es Catamarca con 192 casos

#Barrio
C19_20 %>%
  count(barrio) %>%
  arrange(desc(n))
#lamentablemente no se cuentan con datos de barrios de al menos 307062 casos
# pero si nos centramos en los que si, el primer puesto lo tiene Palermo
#con 19445 casos registrados, siendo Agronomia el mas bajo con 797 casos


#Por comuna en CABA

C19_20 %>%
  count(comuna) %>%
  arrange(desc(n))

#La Comuna 4 es la mas grande en casos de Covid_19, con 27876 casos registrados
#Dejando a la menor afectada, la Comuna 10 con 12126 casos registrados

#2024
#Provincia
C19_24 %>%
  count(provincia) %>%
  arrange(desc(n))
#Provincia con mas casos de Covid_19 es CABA con 33503 casos
#La provincia con menos casos registrados es La Rioja con 17 casos

#Barrio
C19_24 %>%
  count(barrio) %>%
  arrange(desc(n))
#lamentablemente no se cuentan con datos de barrios de al menos 36106 casos
# pero si nos centramos en los que si, el primer puesto lo tiene Palermo
#con 1474 casos registrados, siendo Puerto Madero el mas bajo con 52 casos


#Por comuna en CABA

C19_24 %>%
  count(comuna) %>%
  arrange(desc(n))

#La Comuna 4 es la mas grande en casos de Covid_19, con 2218 casos registrados
#Dejando a la menor afectada, la Comuna 9 con 985 casos registrados

#Clasificación del paciente:
#2020
#Por Género:
C19_20 %>%
  count(genero) %>%
  arrange(desc(n))

#Podemos contabilizar que, en el año 2020, de los 573332 de casos de Covid 19
#Unos 295697 se trataron de personas del género masculino, 277594 del género femenino
#Unos 31 casos sin registrar su género, y unos 10 casos de personas consideradas con otro género

#Veamos como fue avanzando por mes afectando a los distintos géneros:
C19_20 %>%
  mutate(mes = month(fecha_apertura)) %>%
  count(mes, genero) %>%
  arrange(mes, genero)
#En el mes de Febrero empezó con 4 casos del género femenino, 5 del caso masculino
#llegando a picos como en diciembre, finalizando el año con 47570 de casos femeninos y 48159 del caso masculino

#Analicemos por edades

C19_20 %>%
  count(edad)%>%
  arrange(desc(edad))
#En vista que tenemos edades como -8 y arriba de 190, haremos un filtro
C19_20 %>%
  filter(edad > 0, edad < 115) %>%
  count(edad)%>%
  arrange(desc(n))
#Esto nos devuelve que, la edad mas afectada fueron las personas con: 33 años con 13184 casos
# y la menos afectada fue de arriba de los 100 años
#Para ver en un rango de entre 0 y 100 años, corramos el siguiente código
C19_20 %>%
  filter(edad > 0, edad < 101) %>%
  count(edad)%>%
  arrange(desc(n))
#La edad menos afectada fue 100, con 268 afectados

#Veamos la clasificación que se les dió:
C19_20%>%
  count(clasificacion)
#Casos confirmados:276517
#Casos descartados: 283878
#Casos con sospecha: 12937

#Omitiremos la fecha de fallecido, falecido, y fecha de alta, pues se cuenta con muy pcoos datos al respecto
#Por último Veamos el tipo de contagio

C19_20 %>%
  count(tipo_contagio)%>%
  arrange(desc(n))
#Contamos con 297429 casos sin dato
#Contamos con 188438 casos contagiados por comunitario(por contagio en la comunidad)
#Contamos con 46861 casos por contacto con otro contagiado
#Contamos con 24433 casos en área de investigación
#Contamos con 15856 casos por trabajadores de salud
#Por último, con 315 casos de contagiados por impotarlo, de vuelta de algún viaje

#Clasificación del paciente:
#2024
#Por Género:
C19_24 %>%
  count(genero) %>%
  arrange(desc(n))

#Podemos contabilizar que, en el año 2024, de los 57230 de casos de Covid 19
#Unos 31671 se trataron de personas del género femenino, 25481 del género masculino
#Unos 57 casos de personas consideradas con otro género y unos 21 sin regsitrar

#Veamos como fue avanzando por mes afectando a los distintos géneros:
C19_24 %>%
  mutate(mes = month(fecha_apertura_snvs)) %>%
  count(mes, genero) %>%
  arrange(mes, genero)
#En el mes de Enero empezó con 8898 casos del género femenino, 6264 del caso masculino y 8 casos de otro
#finalizando el año con 602 de casos femeninos, 602 del caso masculino y 2 casso de otro

#Analicemos por edades
#Tenemos un par de errores de tipeo en las edades, pues al correr el siguiente código
#Nos retorna edades de mas de 110 años, limpiemos esto y pongamosolo como "sin dato"
C19_24 = C19_24 %>%
  mutate(edad = case_when(
    as.numeric(edad) > 110 ~ "Sin dato",
    TRUE ~ as.character(edad)
  ))
#Ahora si
C19_24 %>%
  count(edad)%>%
  arrange(desc(n))
#Esto nos devuelve que, la edad mas afectada fueron las personas con: 1 años con 2113 casos
# y la menos afectada fue de arriba de los 100 años,con 21 casos sin datos específicos

#Veamos la clasificación que se les dió:
C19_24%>%
  count(clasificacion)
#Casos confirmados:14727
#Casos descartados: 42498
#Casos con sospecha: 5

#Omitiremos las columnas de fecha de fallecido, fallecido, y fecha de alta, pues se cuenta con muy pcoos datos al respecto
#Por último Veamos el tipo de contagio

C19_24 %>%
  count(tipo_contagio)%>%
  arrange(desc(n))
#Contamos con 57230 casos sin dato
#Básicamente todos los casos no fueron registrados de forma correcta

#Pasemos a la visualización para entenderlo mejor:

#Tiempo:
#2020
 
# Contar casos por fecha de apertura
#Hay que simplicarlo y hacerlo por mes, mas fácil de analizar
C19_20 = C19_20 %>%
  mutate(
    mes_apertura = month(fecha_apertura),
    mes_muestra = month(fecha_muestra),
    mes_clasificacion= month(fecha_de_clasificacion)
  )
#Ya teniendo nuestras variables, hay que visualizarlas
#Mes de inicio
C19_20 %>%
  count(mes_apertura) %>%
  ggplot(aes(x = factor(mes_apertura), y = n)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  labs(
    title = "Ingresos por Mes",
    x = "Mes (1=Enero, 12=Diciembre)",
    y = "Número de Casos"
  )
#Mayor mes con casos Ingresados: Agosto

#Veamos la Toma de muestra
C19_20 %>%
  count(mes_muestra) %>%
  ggplot(aes(x = factor(mes_muestra), y = n)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  labs(
    title = "Toma de Muestras de Pacientes",
    x = "Mes (1=Enero, 12=Diciembre)",
    y = "Número de Casos"
  )
#Mes con mayor toma de muestra: Agosto

#Veamos los meses de clasificacion
C19_20 %>%
  count(mes_clasificacion) %>%
  ggplot(aes(x = factor(mes_clasificacion), y = n)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  labs(
    title = "Clasificación de Pacientes",
    x = "Mes (1=Enero, 12=Diciembre)",
    y = "Número de Casos"
  )
#Mayor mes: Agosto

#Veamos por Trimestre:
C19_20 <- C19_20 %>%
  mutate(
    trimestre = case_when(
      mes_apertura %in% c(1, 2, 3) ~ "Q1",
      mes_apertura %in% c(4, 5, 6) ~ "Q2", 
      mes_apertura %in% c(7, 8, 9) ~ "Q3",
      mes_apertura %in% c(10, 11, 12) ~ "Q4"
    )
  )
#Visualizamos los trimestres, que serán llamados por Q1(1-3),2(4-6),3(7-9),4(10-12)
C19_20 %>%
  count(trimestre) %>%
  ggplot(aes(x = trimestre, y = n)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  scale_y_continuous(labels = scales::comma)+
  labs(
    title = "Casos COVID-19 por Trimestre",
    x = "Trimestre",
    y = "Número de Casos"
  )
#El mayor Trimestre de contagios fue: el 3er Trimestre(Meses de Julio, Agosto y Septiembre)



#Ahora lo mismo pero con el 2024:
# Contar casos por fecha de apertura
#Hay que simplicarlo y hacerlo por mes, mas fácil de analizar
C19_24 = C19_24 %>%
  mutate(
    mes_apertura2 = month(fecha_apertura_snvs),
    mes_muestra2 = month(fecha_toma_muestra),
    mes_clasificacion2= month(fecha_clasificacion)
  )
#Ya teniendo nuestras variables, hay que visualizarlas
#Mes de inicio
C19_24 %>%
  count(mes_apertura2) %>%
  ggplot(aes(x = factor(mes_apertura2), y = n)) +
  geom_col(fill = "red", alpha = 0.7) +
  labs(
    title = "Ingresos por Mes",
    x = "Mes (1=Enero, 12=Diciembre)",
    y = "Número de Casos"
  )
#Mayor mes con casos Ingresados: Enero


#Veamos la Toma de muestra
C19_24 %>%
  count(mes_muestra2) %>%
  ggplot(aes(x = factor(mes_muestra2), y = n)) +
  geom_col(fill = "red", alpha = 0.7) +
  labs(
    title = "Toma de Muestras de Pacientes",
    x = "Mes (1=Enero, 12=Diciembre)",
    y = "Número de Casos"
  )
#Mes con mayor toma de muestra: Enero

#Veamos los meses de clasificacion
C19_24 %>%
  count(mes_clasificacion2) %>%
  ggplot(aes(x = factor(mes_clasificacion2), y = n)) +
  geom_col(fill = "red", alpha = 0.7) +
  labs(
    title = "Clasificación de Pacientes",
    x = "Mes (1=Enero, 12=Diciembre)",
    y = "Número de Casos"
  )
#Mayor mes: Enero

#Veamos por Trimestre:
C19_24 = C19_24 %>%
  mutate(
    trimestre2 = case_when(
      mes_apertura2 %in% c(1, 2, 3) ~ "Q1",
      mes_apertura2 %in% c(4, 5, 6) ~ "Q2", 
      mes_apertura2 %in% c(7, 8, 9) ~ "Q3",
      mes_apertura2 %in% c(10, 11, 12) ~ "Q4"
    )
  )
#Visualizamos los trimestres, que serán llamados por Q1(1-3),2(4-6),3(7-9),4(10-12)
C19_24 %>%
  count(trimestre) %>%
  ggplot(aes(x = trimestre, y = n)) +
  geom_col(fill = "red", alpha = 0.7) +
  scale_y_continuous(labels = scales::comma)+
  labs(
    title = "Casos COVID-19 por Trimestre",
    x = "Trimestre",
    y = "Número de Casos"
  )
#El mayor Trimestre de contagios fue: el 1ero Trimestre(Meses de Enero, Febrero y Marzo)






#Visualicemos Ahora por Zona Geográfica
#2020
# Por Provincia
C19_20 %>%
  count(provincia, sort = TRUE) %>%
  ggplot(aes(x = reorder(provincia,n), y = n)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  coord_flip() +
  scale_y_continuous(labels = scales::comma)+
  labs(
    title = "Por Provincia",
    x = "Provincia",
    y = "Número de Casos"
  )+
  theme_minimal()
#Provincia mas afectada es CABA, la menos afectada, Catamarca


#Veamos por barrio
C19_20 %>%
  count(barrio, sort = TRUE) %>%
  slice_head(n = 20) %>%
  ggplot(aes(x = reorder(barrio,n), y = n)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  coord_flip() +
  scale_y_continuous(labels = scales::comma)+
  labs(
    title = "Por Barrio",
    x = "Barrios",
    y = "Número de Casos"
  )+
  theme_minimal()
#Barrio mas afectado: Palermo, de entre los 20 mas afectadas

#Veamos por Comuna
C19_20 %>%
  count(comuna, sort = TRUE) %>%
  ggplot(aes(x = reorder(comuna,n), y = n)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  coord_flip() +
  scale_y_continuous(labels = scales::comma)+
  labs(
    title = "Por Comuna",
    x = "Comuna",
    y = "Número de Casos"
  )+
  theme_minimal()
#Comuna mas afectada: 4

#2024
# Por Provincia
C19_24 %>%
  count(provincia, sort = TRUE) %>%
  ggplot(aes(x = reorder(provincia,n), y = n)) +
  geom_col(fill = "red", alpha = 0.7) +
  coord_flip() +
  scale_y_continuous(labels = scales::comma)+
  labs(
    title = "Por Provincia",
    x = "Provincia",
    y = "Número de Casos"
  )+
  theme_minimal()
#Provincia mas afectada es CABA, la menos afectada, La Rioja
#Veamos por barrio
C19_24 %>%
  count(barrio, sort = TRUE) %>%
  slice_head(n = 20) %>%
  ggplot(aes(x = reorder(barrio,n), y = n)) +
  geom_col(fill = "red", alpha = 0.7) +
  coord_flip() +
  scale_y_continuous(labels = scales::comma)+
  labs(
    title = "Por Barrio",
    x = "Barrios",
    y = "Número de Casos"
  )+
  theme_minimal()
#Barrio mas afectado: Palermo, de entre los 20 mas afectadas

#Veamos por Comuna
C19_24 %>%
  count(comuna, sort = TRUE) %>%
  ggplot(aes(x = reorder(comuna,n), y = n)) +
  geom_col(fill = "red", alpha = 0.7) +
  coord_flip() +
  scale_y_continuous(labels = scales::comma)+
  labs(
    title = "Por Comuna",
    x = "Comuna",
    y = "Número de Casos"
  )+
  theme_minimal()
#Comuna mas afectada: 4




#Por Categoría
#2020
# Por Género
C19_20 %>%
  filter(!is.na(genero)) %>%
  count(genero) %>%
  mutate(porc = n / sum(n) * 100,
         etiqueta = paste0(genero, ": ", round(porc, 1), "%")) %>%
  ggplot(aes(x = "", y = porc, fill = genero)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  geom_text(aes(label = etiqueta), position = position_stack(vjust = 0.5)) +
  labs(title = "Género mas afectado")
#El Género mas afectado fue: el masculino(51.6%)
#Femenino(48.4%)


#Por Edad:
C19_20 %>%
  filter(!is.na(edad), edad >= 0 & edad <= 120) %>%
  count(edad) %>%
  arrange(desc(n)) %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = reorder(as.character(edad), n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Edades mas afectadas",
       x = "Edad",
       y = "Cantidad de casos")

#Edad mas afectada: 33. Pero teneindo en cuenta algún rango de edad
#Podría establecerse de entre 30-40 fueron las edades mas afectadas


#Clasificación:

C19_20 %>%
  filter(!is.na(clasificacion)) %>%
  count(clasificacion) %>%
  mutate(porc = n / sum(n) * 100,
         etiqueta = paste0(clasificacion, ": ", round(porc, 1), "%")) %>%
  ggplot(aes(x = "", y = porc, fill = clasificacion)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  geom_text(aes(label = etiqueta), position = position_stack(vjust = 0.5)) +
  labs(title = "Clasificación de los pacientes ")
#Clasificación mas común: descartado(49.5%)
#Confirmado(48.2%) y sopechoso(2.3%)


#Por tipo de Contagio:
C19_20 %>%
  filter(!is.na(tipo_contagio)) %>%
  count(tipo_contagio) %>%
  arrange(desc(n)) %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = reorder(as.character(tipo_contagio), n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Tipos de contagio mas comunes",
       x = "Tipos de contagio",
       y = "Cantidad de casos")

#Tenemos una gran cantidad de "Sin Dato", pero un tipo de contagio mas común
#Fue el comunitario, siendo seguido por contacto con algún paciente o portador







#2024

C19_24 %>%
  filter(!is.na(genero)) %>%
  count(genero) %>%
  mutate(porc = n / sum(n) * 100,
         etiqueta = paste0(genero, ": ", round(porc, 1), "%")) %>%
  ggplot(aes(x = "", y = porc, fill = genero)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  geom_text(aes(label = etiqueta), position = position_stack(vjust = 0.5)) +
  labs(title = "Género mas afectado")
#El Género mas afectado fue: el femenino(55.3%)
#Masculino(44.5%)


#Por Edad:
C19_24 %>%
  filter(!is.na(edad), edad >= 0 & edad <= 120) %>%
  count(edad) %>%
  slice_head(n = 10)%>%
  arrange(desc(n)) %>%
  ggplot(aes(x = reorder(as.character(edad), n), y = n)) +
  geom_col(fill = "red") +
  coord_flip() +
  labs(title = "Edades mas afectadas",
       x = "Edad",
       y = "Cantidad de casos")

#Edad mas afectada: 1. Pero teniendo en cuenta algún rango de edad
#Podría establecerse de entre 0-10 fueron las edades mas afectadas


#Clasificación:

C19_24 %>%
  filter(!is.na(clasificacion)) %>%
  count(clasificacion) %>%
  mutate(porc = n / sum(n) * 100,
         etiqueta = paste0(clasificacion, ": ", round(porc, 1), "%")) %>%
  ggplot(aes(x = "", y = porc, fill = clasificacion)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  geom_text(aes(label = etiqueta), position = position_stack(vjust = 0.5)) +
  labs(title = "Clasificación de los pacientes ")
#Clasificación mas común: descartado(74.3%)
#Confirmado(25.7%) y sopechoso(0%)


#Por tipo de Contagio:
C19_24 %>%
  filter(!is.na(tipo_contagio)) %>%
  count(tipo_contagio) %>%
  arrange(desc(n)) %>%
  ggplot(aes(x = reorder(as.character(tipo_contagio), n), y = n)) +
  geom_col(fill = "red") +
  coord_flip() +
  labs(title = "Tipos de contagio mas comunes",
       x = "Tipos de contagio",
       y = "Cantidad de casos")

#Tenemos una gran cantidad de "Sin Dato", dejandonos saber que no se llevó un registro de los contagios en este año 2024


