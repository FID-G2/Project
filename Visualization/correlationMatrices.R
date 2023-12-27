library(ggplot2)
library(corrplot)

# Cargar los datos 
datos <- read.csv("./data/dataset_integrado.csv")

# Verificar la estructura de los datos
str(datos)

#Limpieza de NA de la variable 'Número de estudiantes'
datos <- datos[complete.cases(datos$`Número.de.estudiantes`), ]

#Limpieza de NA de la variable 'Nota media'
datos <- datos[complete.cases(datos$Nota.media), ]

################ ESTUDIO SOBRE ÁMBITOS DE ESTUDIO ################


############ ECONOMÍA ############
# Filtrar datos por categoría (por ejemplo, "Economía")
datos_economia <- subset(datos, datos$Ámbito.de.estudio == "Economía")

matriz_correlacion_economia <- cor(datos_economia[, c("Nota.media", "Número.de.estudiantes")])
print("Matriz de correlación para Economía: \n")
print(matriz_correlacion_economia)

# Visualización de la matriz de correlación
corrplot(matriz_correlacion_economia, method = "color")

# Calculo de la matriz de correlación para la variable "Comunidad Autónoma" y "Nota media"

# Convertir la variable categórica "Comunidad Autónoma" en un formato numérico
datos_economia$Comunidad.Autónoma <- as.numeric(as.factor(datos_economia$Comunidad.Autónoma ))

# Calcular la matriz de correlación
matriz_correlacion_economia_comunidad <- cor(datos_economia[, c("Comunidad.Autónoma", "Nota.media")])

print("Matriz de correlación para Economía (Comunidad Autónoma y nota media): \n")
print(matriz_correlacion_economia_comunidad)

# Visualización de la matriz de correlación
corrplot(matriz_correlacion_economia_comunidad, method = "color")

#Visualización de la distribución de la variable 'Nota media' para Economía
ggplot(datos_economia, aes(x = Nota.media)) +
  geom_histogram(binwidth = 0.5, fill = "steelblue", color = "white") +
  labs(title = "Distribución de la variable 'Nota media' para Economía", x = "Nota media", y = "Frecuencia") +
  theme_minimal()


############ DERECHO ############
# Filtrar datos por categoría 
datos_derecho <- subset(datos, datos$Ámbito.de.estudio == "Derecho")

matriz_correlacion_derecho <- cor(datos_derecho[, c("Nota.media", "Número.de.estudiantes")])

print("Matriz de correlación para Derecho: \n")
print(matriz_correlacion_derecho)

# Visualización de la matriz de correlación
corrplot(matriz_correlacion_derecho, method = "color")

# Calculo de la matriz de correlación para la variable Universidad y "Nota media"

# Convertir la variable categórica "Universidad" en un formato numérico
datos_derecho$Universidad <- as.numeric(as.factor(datos_derecho$Universidad))

# Calcular la matriz de correlación
matriz_correlacion_derecho_universidad <- cor(datos_derecho[, c("Universidad", "Nota.media")])

print("Matriz de correlación para Derecho (Universidad y nota media): \n")
print(matriz_correlacion_derecho_universidad)

# Visualización de la matriz de correlación
corrplot(matriz_correlacion_derecho_universidad, method = "color")

#Visualización de la distribución de la variable Nota media para Derecho
ggplot(datos_derecho, aes(x = Nota.media)) +
  geom_histogram(binwidth = 0.5, fill = "steelblue", color = "white") +
  labs(title = "Distribución de la variable Nota media para Derecho", x = "Nota media", y = "Frecuencia") +
  theme_minimal()



##################### ESTUDIO SOBRE FORMA DE ADMISIÓN #####################

# Filtrar datos por forma de admisión  "PAU"

datos_pau <- subset(datos, datos$`Forma.de.admisión` == "PAU")

matriz_correlacion_pau <- cor(datos_pau[, c("Nota.media", "Número.de.estudiantes")])

print("Matriz de correlación para PAU: \n")
print(matriz_correlacion_pau)

# Visualización de la matriz de correlación
corrplot(matriz_correlacion_pau, method = "color")

# Calculo de la matriz de correlación para la variable "Rama de enseñanza" y "Zona de nacionalidad"

# Convertir la variable categórica "Rama de enseñanza" en un formato numérico
datos_pau$`Rama.de.enseñanza` <- as.numeric(as.factor(datos_pau$`Rama.de.enseñanza`))

# Convertir la variable categórica "Zona de nacionalidad" en un formato numérico
datos_pau$`Zona.de.nacionalidad` <- as.numeric(as.factor(datos_pau$`Zona.de.nacionalidad`))

# Calcular la matriz de correlación
matriz_correlacion_pau_rama_zona <- cor(datos_pau[, c("Rama.de.enseñanza", "Zona.de.nacionalidad")])

print("Matriz de correlación para PAU (Rama de enseñanza y Zona de nacionalidad): \n")
print(matriz_correlacion_pau_rama_zona)

# Visualización de la matriz de correlación
corrplot(matriz_correlacion_pau_rama_zona, method = "color")

' Resultado negativo: 

                     Rama.de.enseñanza Zona.de.nacionalidad
Rama.de.enseñanza         1.000000e+00        -3.420738e-05
Zona.de.nacionalidad     -3.420738e-05         1.000000e+00

'

#Visualización de la distribución de la variable Nota media para PAU
ggplot(datos_pau, aes(x = Nota.media)) +
  geom_histogram(binwidth = 0.5, fill = "steelblue", color = "white") +
  labs(title = "Distribución de la variable Nota media para PAU", x = "Nota media", y = "Frecuencia") +
  theme_minimal()


# Filtrar datos por forma de admisión  "FP"

datos_fp <- subset(datos, datos$`Forma.de.admisión` == "FP")

matriz_correlacion_fp <- cor(datos_fp[, c("Nota.media", "Número.de.estudiantes")])

print("Matriz de correlación para FP: \n")
print(matriz_correlacion_fp)

# Visualización de la matriz de correlación
corrplot(matriz_correlacion_fp, method = "color")

# Calculo de la matriz de correlación para la variable "Sexo" y "Nota media"

# Convertir la variable categórica "Sexo"en un formato numérico
datos_fp$Sexo <- as.numeric(as.factor(datos_fp$Sexo))

# Calcular la matriz de correlación
matriz_correlacion_fp_sexo <- cor(datos_fp[, c("Sexo", "Nota.media")])

print("Matriz de correlación para FP (Sexo y nota media): \n")
print(matriz_correlacion_fp_sexo)

# Visualización de la matriz de correlación
corrplot(matriz_correlacion_fp_sexo, method = "color")

#Visualización de la distribución de la variable Nota media para FP
ggplot(datos_fp, aes(x = Nota.media)) +
  geom_histogram(binwidth = 0.5, fill = "steelblue", color = "white") +
  labs(title = "Distribución de la variable Nota media para FP", x = "Nota media", y = "Frecuencia") +
  theme_minimal()

