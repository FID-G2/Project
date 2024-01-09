datos_ruido <- read.csv("../2. Add-noise/dataset_ruido.csv")

# Instalar y cargar el paquete dplyr si aún no lo has hecho
#install.packages("dplyr")
#install.packages("fastDummies")
#install.packages("cluster")
#install.packages("plotly")
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

modas_por_nacionalidad_curso <- preprocesamiento %>%
  group_by(Zona.de.nacionalidad,Curso) %>%
  summarise(ValorComun = names(which.max(table(Sexo))))

# Reemplazar los valores no comunes

preprocesamiento <- preprocesamiento %>%
  left_join(modas_por_nacionalidad_curso, by = c("Zona.de.nacionalidad", "Curso")) %>%
  mutate(Sexo = ifelse(Sexo %in% c("Hombres", "Mujeres"), Sexo, ValorComun)) %>%
  select(-ValorComun)


conteo_no_hombre_mujer <- sum(preprocesamiento$Sexo != "Hombres" & preprocesamiento$Sexo != "Mujeres")
cat("Cantidad de elementos que no son ni Hombre ni Mujer en la columna sexo despues del preprocesamiento:", conteo_no_hombre_mujer, "\n")

#####################################################################

####################QUITAR RUIDO NOTA MEDIA Y NUMERO DE ESTUDIANTES##########################


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
      left_join(modas_por_grupo, by = setNames(columnas_grupo, c(columnas_grupo)[seq_along(columnas_grupo)])) %>%
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

preprocesamiento <- imputar_recursivo(preprocesamiento, c("Universidad", "Curso", "Forma.de.admisión", "Rama.de.enseñanza","Ámbito.de.estudio","Sexo"), "Número.de.estudiantes")
# Convertir la columna Nota.media a numérica (asegurarse de que no haya valores no numéricos)
preprocesamiento <- preprocesamiento %>%
  mutate(Nota.media = as.numeric(as.character(Nota.media)))

preprocesamiento <- preprocesamiento %>%
  mutate(Número.de.estudiantes = as.numeric(as.character(Número.de.estudiantes)))
preprocesamiento <- preprocesamiento %>%
  mutate(Número.de.estudiantes = ifelse(Número.de.estudiantes == 0, 1, Número.de.estudiantes))

preprocesamiento$Numero.estudiantes_norm <- scale(preprocesamiento$Número.de.estudiantes)

preprocesamiento$Nota.media_norm <- scale(preprocesamiento$Nota.media)

set.seed(123)

# Obtener índices de las filas con nacionalidad España
indices_espana <- which(preprocesamiento$Zona.de.nacionalidad == "España")

indices_ue <- which(preprocesamiento$Zona.de.nacionalidad == "Unión Europea")

# Seleccionar un subconjunto aleatorio de esos índices (por ejemplo, el 80%)
indices_a_eliminar <- sample(indices_espana, size = round(0.7 * length(indices_espana)))

# Eliminar las filas seleccionadas
preprocesamiento <- preprocesamiento[-indices_a_eliminar, ]




#####################################################################
#                                                                   #
#               CREACIÓN MODELO CLUSTERING (K-MEANS)                #
#                                                                   #
#####################################################################

library(dplyr)
library(fastDummies)
library(cluster)
library(plotly)


# Seleccionar las variables relevantes para clustering
variables_cluster <- preprocesamiento %>%
  select(Ámbito.de.estudio, Sexo,Zona.de.nacionalidad , Nota.media_norm)

# Convertir variables categóricas a numéricas (codificación one-hot)
variables_cluster <- dummy_cols(variables_cluster, select_columns = NULL, remove_selected_columns = TRUE)

# Seleccionar todas las columnas necesarias
variables_cluster <- variables_cluster %>%
  select(matches("Ámbito.de.estudio_|Sexo_|Zona.de.nacionalidad_|Nota.media_norm"))

# Calcular la inercia para diferentes valores de k
inertia_values <- c()
for (k in 1:15) {
  kmeans_result <- kmeans(variables_cluster, centers = k)
  inertia_values <- c(inertia_values, kmeans_result$tot.withinss)
}

# Graficar la inercia
plot(1:15, inertia_values, type = "b", pch = 19, frame = FALSE, 
     xlab = "Número de Clusters (k)", ylab = "Inercia",
     main = "Método del Codo (Elbow Method)")

# Realizar k-means clustering usando como número de centros el método de elbow
kmeans_result <- kmeans(variables_cluster, centers = 4)

# Añadir el resultado del clustering al conjunto de datos original
preprocesamiento$cluster_rama <- as.factor(kmeans_result$cluster)



# Añadir el índice de las filas como una columna en preprocesamiento
preprocesamiento$indice_fila <- seq_len(nrow(preprocesamiento))

# Gráfico de dispersión con el índice de las filas en el eje x
ggplot(preprocesamiento, aes(x = indice_fila, y = Nota.media, color = as.factor(cluster_rama), size = Número.de.estudiantes)) +
  geom_point(alpha = 0.5) +
  labs(title = "Distribución de Estudiantes por Clusters",
       x = "Índice de fila", y = "Nota media", color = "Cluster") +
  scale_size_continuous(range = c(0.5, 5)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(angle = 0, hjust = 1)
  )
#####################################################################
#                                                                   #
#               EVALUACIÓN DE LA APLICACIÓN DEL MODELO              #
#                                                                   #
#####################################################################


# Calcular el WCSS y el BCSS
wcss <- kmeans_result$tot.withinss
bcss <- kmeans_result$betweenss

# Calcular el total de sumas de cuadrados
total_ss <- wcss + bcss

# Calcular la proporción explicada por el clustering (R^2 ajustado)
r_squared <- bcss / total_ss

# Mostrar resultados
cat("WCSS (Within-Cluster Sum of Squares):", wcss, "\n")
cat("BCSS (Between-Cluster Sum of Squares):", bcss, "\n")
cat("R^2 ajustado (Proporción explicada por el clustering):", r_squared, "\n")

# Estadísticas del resultado de kmeans
summary(kmeans_result)
