# Instalar y cargar paquetes
# install.packages(c("dplyr", "ggplot2"))
#library(dplyr)
library(ggplot2)

# Cargar el archivo CSV original
datos <- read.csv("./data/dataset_integrado.csv")

# Imprimir los datos
head(datos)

# Estadísticas descriptivas para la variable "Nota media"
nota_media_summary  <- summary(datos$`Nota media`)

# Frecuencia de cada comunidad autónoma
frecuencias_autonoma <- table(datos$`Comunidad Autónoma`)

# Gráfico de barras para visualizar la frecuencia de cada comunidad autónoma
'
ggplot(datos, aes(x = `Comunidad.Autónoma`)) +
  geom_bar() +
  labs(title = "Frecuencia de estudiantes por Comunidad Autónoma")
'


# Verificar valores perdidos
sum(is.na(datos$Nota.media))
sum(is.na(datos$Número.de.estudiantes))

# Los resultados de sum son 0, por lo que no hay valores perdidos en estas variables
# Imputar con la mediana (puedes ajustar según tus necesidades)
datos$Nota.media[is.na(datos$Nota.media)] <- median(datos$Nota.media, na.rm = TRUE)

# Matriz de correlación para variables numéricas
cor(datos[, c("Nota.media", "Número.de.estudiantes")])

# Los resultados muestran que la correlación entre Nota media y Número de estudiantes es 0.01, muy baja, por lo que no hay una relación lineal entre estas variables
# Diagrama de dispersión para visualizar la relación entre Nota media y Número de estudiantes
ggplot(datos, aes(x = `Nota.media`, y = `Número.de.estudiantes`)) +
  geom_point() +
  labs(title = "Relación entre Nota media y Número de estudiantes")

'
# Estadiísticas descriptivas para la variable "Nota media" por comunidad autónoma
nota_media_por_autonoma <- datos %>% 
  group_by(`Comunidad Autónoma`) %>% 
  summarise(Mediana = median(`Nota media`), 
            Media = mean(`Nota media`), 
            Desviación_estándar = sd(`Nota media`), 
            Mínimo = min(`Nota media`), 
            Máximo = max(`Nota media`))

# Estadiísticas descriptivas para la variable "Número de estudiantes" por comunidad autónoma
numero_estudiantes_por_autonoma <- datos %>% 
  group_by(`Comunidad Autónoma`) %>% 
  summarise(Mediana = median(`Número de estudiantes`), 
            Media = mean(`Número de estudiantes`), 
            Desviación_estándar = sd(`Número de estudiantes`), 
            Mínimo = min(`Número de estudiantes`), 
            Máximo = max(`Número de estudiantes`))

# Estadiísticas descriptivas para la variable "Nota media" por universidad
nota_media_por_universidad <- datos %>% 
  group_by(Universidad) %>% 
  summarise(Mediana = median(`Nota media`), 
            Media = mean(`Nota media`), 
            Desviación_estándar = sd(`Nota media`), 
            Mínimo = min(`Nota media`), 
            Máximo = max(`Nota media`))

# Estadiísticas descriptivas para la variable "Número de estudiantes" por universidad
numero_estudiantes_por_universidad <- datos %>% 
  group_by(Universidad) %>% 
  summarise(Mediana = median(`Número de estudiantes`), 
            Media = mean(`Número de estudiantes`), 
            Desviación_estándar = sd(`Número de estudiantes`), 
            Mínimo = min(`Número de estudiantes`), 
            Máximo = max(`Número de estudiantes`))

# Estadiísticas descriptivas para la variable "Nota media" por sexo
nota_media_por_sexo <- datos %>% 
  group_by(Sexo) %>% 
  summarise(Mediana = median(`Nota media`), 
            Media = mean(`Nota media`), 
            Desviación_estándar = sd(`Nota media`), 
            Mínimo = min(`Nota media`), 
            Máximo = max(`Nota media`))

# Estadiísticas descriptivas para la variable "Número de estudiantes" por sexo
numero_estudiantes_por_sexo <- datos %>% 
  group_by(Sexo) %>% 
  summarise(Mediana = median(`Número de estudiantes`), 
            Media = mean(`Número de estudiantes`), 
            Desviación_estándar = sd(`Número de estudiantes`), 
            Mínimo = min(`Número de estudiantes`), 
            Máximo = max(`Número de estudiantes`))

# Estadiísticas descriptivas para la variable "Nota media" por comunidad autónoma y universidad
nota_media_por_autonoma_universidad <- datos %>% 
  group_by(`Comunidad Autónoma`, Universidad) %>% 
  summarise(Mediana = median(`Nota media`), 
            Media = mean(`Nota media`), 
            Desviación_estándar = sd(`Nota media`), 
            Mínimo = min(`Nota media`), 
            Máximo = max(`Nota media`))

# Estadiísticas descriptivas para la variable "Número de estudiantes" por comunidad autónoma y universidad
numero_estudiantes_por_autonoma_universidad <- datos %>% 
  group_by(`Comunidad Autónoma`, Universidad) %>% 
  summarise(Mediana = median(`Número de estudiantes`), 
            Media = mean(`Número de estudiantes`), 
            Desviación_estándar = sd(`Número de estudiantes`), 
            Mínimo = min(`Número de estudiantes`), 
            Máximo = max(`Número de estudiantes`))

# Estadiísticas descriptivas para la variable "Nota media" por comunidad autónoma y sexo
nota_media_por_autonoma_sexo <- datos %>% 
  group_by(`Comunidad Autónoma`, Sexo) %>% 
  summarise(Mediana = median(`Nota media`), 
            Media = mean(`Nota media`), 
            Desviación_estándar = sd(`Nota media`), 
            Mínimo = min(`Nota media`), 
            Máximo = max(`Nota media`))

# Estadiísticas descriptivas para la variable "Número de estudiantes" por comunidad autónoma y sexo
numero_estudiantes_por_autonoma_sexo <- datos %>% 
  group_by(`Comunidad Autónoma`, Sexo) %>% 
  summarise(Mediana = median(`Número de estudiantes`), 
            Media = mean(`Número de estudiantes`), 
            Desviación_estándar = sd(`Número de estudiantes`), 
            Mínimo = min(`Número de estudiantes`), 
            Máximo = max(`Número de estudiantes`))

# Estadiísticas descriptivas para la variable "Nota media" por universidad y sexo
nota_media_por_universidad_sexo <- datos %>% 
  group_by(Universidad, Sexo) %>% 
  summarise(Mediana = median(`Nota media`), 
            Media = mean(`Nota media`), 
            Desviación_estándar = sd(`Nota media`), 
            Mínimo = min(`Nota media`), 
            Máximo = max(`Nota media`))

# Estadiísticas descriptivas para la variable "Número de estudiantes" por universidad y sexo
numero_estudiantes_por_universidad_sexo <- datos %>% 
  group_by(Universidad, Sexo) %>% 
  summarise(Mediana = median(`Número de estudiantes`), 
            Media = mean(`Número de estudiantes`), 
            Desviación_estándar = sd(`Número de estudiantes`), 
            Mínimo = min(`Número de estudiantes`), 
            Máximo = max(`Número de estudiantes`))
'
#Graficas para las estadisticas descriptivas
# Graficar la distribución de la variable "Nota media"
'
ggplot(datos, aes(x = `Nota.media`)) +
  geom_histogram(binwidth = 0.5) +
  labs(title = "Distribución de la variable Nota media")
'
# Graficar la distribución de la variable "Número de estudiantes"
ggplot(datos, aes(x = `Número.de.estudiantes`)) +
  geom_histogram(binwidth = 100) +
  labs(title = "Distribución de la variable Número de estudiantes")

# Graficar la distribución de la variable "Nota media" por comunidad autónoma
ggplot(datos, aes(x = `Nota.media`, fill = `Comunidad.Autónoma`)) +
  geom_histogram(binwidth = 0.5) +
  labs(title = "Distribución de la variable Nota media por comunidad autónoma")

#Gráficas posibles para diagramas de pastel
'
#Proporción de estudiantes por comunidad autónoma
ggplot(datos, aes(x = "", fill = `Comunidad Autónoma`)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Distribución de Estudiantes por Comunidad Autónoma", x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.title = element_blank())
'
'
# Graficar la distribución de la variable "Número de estudiantes" por comunidad autónoma
ggplot(datos, aes(x = `Número.de.estudiantes`, fill = `Comunidad.Autónoma`)) +
  geom_histogram(binwidth = 100) +
  labs(title = "Distribución de la variable Número de estudiantes por comunidad autónoma")

# Graficar la distribución de la variable "Nota media" por universidad
ggplot(datos, aes(x = `Nota.media`, fill = Universidad)) +
  geom_histogram(binwidth = 0.5) +
  labs(title = "Distribución de la variable Nota media por universidad")

# Graficar la distribución de la variable "Número de estudiantes" por universidad
ggplot(datos, aes(x = `Número.de.estudiantes`, fill = Universidad)) +
  geom_histogram(binwidth = 100) +
  labs(title = "Distribución de la variable Número de estudiantes por universidad")

# Graficar la distribución de la variable "Nota media" por sexo
ggplot(datos, aes(x = `Nota.media`, fill = Sexo)) +
  geom_histogram(binwidth = 0.5) +
  labs(title = "Distribución de la variable Nota media por sexo")

# Graficar la distribución de la variable "Número de estudiantes" por sexo
ggplot(datos, aes(x = `Número.de.estudiantes`, fill = Sexo)) +
  geom_histogram(binwidth = 100) +
  labs(title = "Distribución de la variable Número de estudiantes por sexo")

# Graficar la distribución de la variable "Nota media" por comunidad autónoma y universidad
ggplot(datos, aes(x = `Nota.media`, fill = `Comunidad.Autónoma`)) +
  geom_histogram(binwidth = 0.5) +
  facet_wrap(~Universidad) +
  labs(title = "Distribución de la variable Nota media por comunidad autónoma y universidad")

# Graficar la distribución de la variable "Número de estudiantes" por comunidad autónoma y universidad
ggplot(datos, aes(x = `Número.de.estudiantes`, fill = `Comunidad.Autónoma`)) +
  geom_histogram(binwidth = 100) +
  facet_wrap(~Universidad) +
  labs(title = "Distribución de la variable Número de estudiantes por comunidad autónoma y universidad")

# Graficar la distribución de la variable "Nota media" por comunidad autónoma y sexo
ggplot(datos, aes(x = `Nota.media`, fill = `Comunidad.Autónoma`)) +
  geom_histogram(binwidth = 0.5) +
  facet_wrap(~Sexo) +
  labs(title = "Distribución de la variable Nota media por comunidad autónoma y sexo")


# Número total de estudiantes por comunidad autónoma
total_estudiantes_por_comunidad <- datos %>%
  group_by(`Comunidad Autónoma`) %>%
  summarise(Total_estudiantes = sum(`Número de estudiantes`))
'