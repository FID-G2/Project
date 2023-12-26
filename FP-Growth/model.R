#setwd("C:/Users/Windows/Documents/GitHub/Project/FP-Growth")

datos_ruido <- read.csv("../Add-noise/dataset_ruido_5.csv")

# Instalar y cargar el paquete dplyr si aún no lo has hecho
#install.packages("dplyr")
library(dplyr)

#####################################################################
#                                                                   #
#               PREPROCESAMIENTO DEL DATASET                        #
#                                                                   #
#####################################################################
###############QUITAR RUIDO C.AUTONOMA###############################

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


imputar_recursivo <- function(datos, columnas_grupo, columna_imputar) {
  nas_columna <- sum(is.na(datos[[columna_imputar]]))
  
  cat("Cantidad de elementos NA antes de realizar preprocesamiento a columna", columna_imputar, ":", nas_columna, "\n")
  
  while (nas_columna > 0) {
    modas_por_grupo <- datos %>%
      group_by(across(all_of(columnas_grupo))) %>%
      summarise(ValorComun = if(length(unique(.data[[columna_imputar]][!is.na(.data[[columna_imputar]])])) > 0)
        names(which.max(table(.data[[columna_imputar]][!is.na(.data[[columna_imputar]])])))
        else NA_character_)
    
    datos <- datos %>%
      left_join(modas_por_grupo, by = setNames(columnas_grupo, c("Universidad", "Curso", "Forma.de.admisión", "Rama.de.enseñanza","Ámbito.de.estudio")[seq_along(columnas_grupo)])) %>%
      mutate(across(all_of(columna_imputar), ~ ifelse(is.na(.), ValorComun, .))) %>%
      select(-ValorComun)
    
    nas_columna <- sum(is.na(datos[[columna_imputar]]))
    
    cat("Cantidad de elementos NA después de realizar preprocesamiento a columna", columna_imputar, ":", nas_columna, "\n")
    
    # Reducir las columnas de agrupación para la próxima iteración
    if (length(columnas_grupo) > 1) {
      columnas_grupo <- columnas_grupo[-length(columnas_grupo)]
    } else {
      break
    }
  }
  
  return(datos)
}

# Uso de la función
preprocesamiento <- imputar_recursivo(preprocesamiento, c("Universidad", "Curso", "Forma.de.admisión", "Rama.de.enseñanza","Ámbito.de.estudio"), "Nota.media")

#####################################################################

####################SELECCIÓN DE DATOS RELEVANTES####################

# install.packages("fpgrowth")
library(arules)
library(data.table)
# Ver los primeros registros de tu conjunto de datos
head(preprocesamiento)

# Convierte el conjunto de datos a data.table
dt <- as.data.table(preprocesamiento)

# Convierte la columna "Nota.media" a tipo numérico y agrupa
transacciones <- dt[, .(Media_Nota = mean(as.numeric(Nota.media), na.rm = TRUE)), 
                    by = .(Comunidad.Autónoma, Universidad, Curso, Sexo, 
                           Forma.de.admisión, Rama.de.enseñanza, Ámbito.de.estudio, Zona.de.nacionalidad)]

# Convierte la columna "Media_Nota" a un formato adecuado para el algoritmo arules
transacciones <- as(transacciones$Media_Nota, "transactions")

# Aplica el algoritmo FP-Growth
reglas_fp <- arules::fpgrowth(transacciones, support = 0.1, confidence = 0.5)

# Muestra las reglas generadas
inspect(reglas_fp)




