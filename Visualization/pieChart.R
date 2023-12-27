
library(ggplot2)

# Cargar el archivo CSV original
datos <- read.csv("./data/dataset_integrado.csv")

# Hay muchos numeros de estudiantes que son 0, estas gráficas no son muy útiles
datos_limpios <- datos[complete.cases(datos$`Número.de.estudiantes`), ]


#Eliminar filas con numeros de estudiantes 0
datos <- datos[complete.cases(datos$`Número.de.estudiantes`), ]

# Verificar si hay algún 0 en la variable 'Número de estudiantes'
hay_ceros <- any(datos$`Número de estudiantes` == 0)

# Asegurar de que no hay filas con 0s
print(hay_ceros)


######################################## Proporción de estudiantes por comunidad autónoma ###########################################
#Estudiantes por comunidad autónoma
ggplot(datos, aes(x = "", fill = `Comunidad.Autónoma`)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Distribución de Estudiantes por Comunidad Autónoma", x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.title = element_blank())



#################################### Forma de admisión ###########################################

# Calcular el número de estudiantes con forma de admisión "PAU" y "FP"


ggplot(datos, aes(x = "", fill = `Forma.de.admisión`)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Distribución de Estudiantes por Forma de Admisión", x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.title = element_blank())

#Justificación del gráfico anterior 50/50

num_estudiantes_PAU <- sum(datos$`Forma.de.admisión` == "PAU", na.rm = TRUE)
num_estudiantes_FP <- sum(datos$`Forma.de.admisión` == "FP", na.rm = TRUE)

# Mostrar los resultados
cat("Número de estudiantes con forma de admisión PAU:", num_estudiantes_PAU, "\n")
cat("Número de estudiantes con forma de admisión FP:", num_estudiantes_FP, "\n")


######################### Proporción de hombres y mujeres #######################################


ggplot(datos, aes(x = "", fill = Sexo)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Distribución de Estudiantes por Sexo", x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.title = element_blank())

# Calcular el número de estudiantes hombres y mujeres
num_estudiantes_hombres <- sum(datos$Sexo == "Hombres", na.rm = TRUE)
num_estudiantes_mujeres <- sum(datos$Sexo == "Mujeres", na.rm = TRUE)

# Justificación del gráfico anterior 50/50
cat("Número de estudiantes hombres:", num_estudiantes_hombres, "\n")
cat("Número de estudiantes mujeres:", num_estudiantes_mujeres, "\n")


#################################### Rama de enseñanza ###########################################
ggplot(datos, aes(x = "", fill = `Rama.de.enseñanza`)) +
  geom_bar(stat = "count", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Frecuencia de Ramas de Enseñanza", x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.title = element_blank())

#################################### Ámbito de estudio ###########################################
ggplot(datos, aes(x = "", fill = `Ámbito.de.estudio`)) +
  geom_bar(stat = "count", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Frecuencia de Ámbitos de Estudio", x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.title = element_blank())

#################################### Universidad ###########################################
ggplot(datos, aes(x = "", fill = `Universidad`)) +
  geom_bar(stat = "count", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Frecuencia de Universidades", x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.title = element_blank())

#################################### Zona de nacionalidad ###########################################
ggplot(datos, aes(x = "", fill = `Zona.de.nacionalidad`)) +
  geom_bar(stat = "count", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Frecuencia de Zonas de Nacionalidad", x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.title = element_blank())