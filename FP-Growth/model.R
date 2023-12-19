setwd("C:/Users/Windows/Documents/GitHub/Project/FP-Growth")

datos_ruido <- read.csv("../Add-noise/dataset_ruido_5.csv")

# Instalar y cargar el paquete dplyr si aún no lo has hecho
#install.packages("dplyr")
library(dplyr)

#####################################################################
#                                                                   #
#               PREPROCESAMIENTO DEL DATASET                        #
#                                                                   #
#####################################################################
###############QUITAR RUIDO C.AUTONOMA################################

# Contar el número de NAs antes del preprocesamiento
nas_antes <- sum(is.na(datos_ruido))
cat("Número de NAs antes del preprocesamiento:", nas_antes, "\n")
#Quitar NAs por moda dentro de la columna Comunidad Autónoma
preprocesamiento <- datos_ruido %>%
  mutate(`Comunidad.Autónoma` = ifelse(is.na(`Comunidad.Autónoma`), names(which.max(table(na.omit(`Comunidad.Autónoma`)))), `Comunidad.Autónoma`))
nas_preproCA <- sum(is.na(preprocesamiento))
cat("Número de NAs después de sustituir por MODA en columna COMUNIDAD AUTÓNOMA:", nas_preproCA, "\n")

######################################################################

##############QUITAR RUIDO UNIVERSIDAD################################

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

#####################################################################

############QUITAR RUIDO SEXO########################################

# Contar el número de elementos que NO son "Hombre" ni "Mujer" (manejando valores nulos)
conteo_no_hombre_mujer <- sum(!is.na(preprocesamiento$Sexo) & preprocesamiento$Sexo != "Hombres" & preprocesamiento$Sexo != "Mujeres")

# Mostrar el resultado
cat("Cantidad de elementos que no son ni Hombre ni Mujer en la columna sexo antes del preprocesamiento:", conteo_no_hombre_mujer, "\n")

# Calcular la moda por "Universidad" para la columna "Sexo"

modas_por_universidad <- preprocesamiento %>%
  group_by(Universidad) %>%
  summarise(ValorComun = names(which.max(table(Sexo))))

# Reemplazar los valores no comunes

preprocesamiento <- preprocesamiento %>%
  left_join(modas_por_universidad, by = "Universidad") %>%
  mutate(Sexo = ifelse(Sexo %in% c("Hombres", "Mujeres"), Sexo, ValorComun)) %>%
  select(-ValorComun)

conteo_no_hombre_mujer <- sum(preprocesamiento$Sexo != "Hombres" & preprocesamiento$Sexo != "Mujeres")
cat("Cantidad de elementos que no son ni Hombre ni Mujer en la columna sexo despues del preprocesamiento:", conteo_no_hombre_mujer, "\n")

#####################################################################

####################QUITAR RUIDO NOTA MEDIA##########################
nas_preproNOTAMEDIA <- sum(is.na(preprocesamiento))
cat("Cantidad de elementos NA antes de realizar preprocesamiento a columna Nota Media: ", nas_preproNOTAMEDIA, "\n")


# Calcular la moda por grupos en la columna "Nota.media"
modas_por_grupo <- preprocesamiento %>%
  group_by(Universidad, Curso, Forma.de.admisión, Rama.de.enseñanza) %>%
  summarise(ValorComun = if(length(unique(Nota.media[!is.na(Nota.media)])) > 0)
    names(which.max(table(Nota.media[!is.na(Nota.media)])))
    else NA_character_)

# Reemplazar los valores NA en la columna "Nota.media"
preprocesamiento <- preprocesamiento %>%
  left_join(modas_por_grupo, by = c("Universidad", "Curso", "Forma.de.admisión", "Rama.de.enseñanza")) %>%
  mutate(Nota.media = ifelse(is.na(Nota.media), ValorComun, Nota.media)) %>%
  select(-ValorComun)

nas_preproNOTAMEDIA <- sum(is.na(preprocesamiento))
cat("Cantidad de elementos NA despues de realizar preprocesamiento a columna Nota Media: ", nas_preproNOTAMEDIA, "\n")




