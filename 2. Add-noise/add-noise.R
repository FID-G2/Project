#setwd("C:/Users/David/Desktop/WD FID/Project/Add-noise")
#EJECUTAR ESTO PRIMERO
#######################################################################################
# Cargar el archivo CSV original
datos <- read.csv("../data/CleanData/dataset_integrado.csv")
n <- nrow(datos)
seed <- 375
# Porcentaje de filas que serán cambiadas
porcentaje <- readline(prompt = "Ingrese un porcentaje: ") 
#######################################################################################

porcentaje <- as.numeric(porcentaje)
set.seed(seed)
nestudiantesMissingRows <- sample(1:n, round(porcentaje/100 * n, 0))

set.seed(seed+seed*seed)
cautonomaMissingRows <- sample(1:n, round(porcentaje/100 * n, 0))

set.seed(seed+seed)
universidadMissingRows <- sample(1:n, round(porcentaje/100 * n, 0))

set.seed(seed*seed)
sexoMissingRows <- sample(1:n, round(porcentaje/100 * n, 0))


conjuntoPalabras <- c("rojo", "azul", "verde", "amarillo", "naranja")
datos_ruido <- datos  # Crear una copia de los datos originales

datos_ruido[nestudiantesMissingRows, "Número.de.estudiantes"] <- NA
datos_ruido[cautonomaMissingRows, "Comunidad.Autónoma"] <- NA
datos_ruido[universidadMissingRows, "Universidad"] <- NA
datos_ruido[sexoMissingRows, "Sexo"] <- sample(conjuntoPalabras, size = length(sexoMissingRows), replace = TRUE)

cantidad_na_por_columna <- colSums(is.na(datos_ruido)) # Comprobar cuántos NA hay por columna
print(cantidad_na_por_columna)

# Guardar el nuevo conjunto de datos con ruido en un archivo CSV

# install.packages("readr")
library(readr)

extraction_path <- paste0("../2. Add-noise/dataset_ruido.csv")

readr::write_excel_csv(datos_ruido, file = extraction_path)

cat("Dataset exported to", extraction_path, "\n")