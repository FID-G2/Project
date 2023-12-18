#setwd("C:/Users/Windows/Documents/GitHub/Project/FP-Growth")

datos_ruido <- read.csv("../Add-noise/dataset_ruido_5.csv")

# Instalar y cargar el paquete dplyr si aún no lo has hecho
#install.packages("dplyr")
library(dplyr)


# Contar el número de NAs antes del preprocesamiento
nas_antes <- sum(is.na(datos_ruido))
cat("Número de NAs antes del preprocesamiento:", nas_antes, "\n")
#Quitar NAs por moda dentro de la columna Comunidad Autónoma
preprocesamiento <- datos_ruido %>%
  mutate(`Comunidad.Autónoma` = ifelse(is.na(`Comunidad.Autónoma`), names(which.max(table(na.omit(`Comunidad.Autónoma`)))), `Comunidad.Autónoma`))
nas_preproCA <- sum(is.na(preprocesamiento))
cat("Número de NAs después de sustituir por MODA en columna COMUNIDAD AUTÓNOMA:", nas_preproCA, "\n")

# Calcular la moda por "Comunidad Autónoma" para la columna "Universidad"
modas_por_comunidad <- preprocesamiento %>%
  group_by(`Comunidad.Autónoma`) %>%
  summarize(Moda_Universidad = names(which.max(table(na.omit(`Universidad`)))))


# Fusionar las modas con el conjunto de datos original
preprocesamiento <- left_join(preprocesamiento, modas_por_comunidad, by = "Comunidad.Autónoma")

# Reemplazar los NA en "Universidad" por las modas correspondientes
preprocesamiento$Universidad <- ifelse(is.na(preprocesamiento$Universidad), preprocesamiento$Moda_Universidad, preprocesamiento$Universidad)

# Eliminar la columna temporal "Moda_Universidad"
preprocesamiento <- select(preprocesamiento, -Moda_Universidad)
nas_preproUNIV <- sum(is.na(preprocesamiento))
cat("Número de NAs despues de sustituir por MODA en la columna UNIVERSIDAD según la COMUNIDAD AUTÓNOMA:", nas_preproUNIV, "\n")






