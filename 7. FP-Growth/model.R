#setwd("C:/Users/Windows/Documents/GitHub/Project/FP-Growth")

datos_ruido <- read.csv("../2. Add-noise/dataset_ruido.csv")

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
# Crear un dataframe de ejemplo
df <- data.frame(
  "Comunidad.Autónoma" = c("Cataluña", "Madrid", "Cataluña", "Andalucía", "Galicia", "Valencia"),
  "Universidad" = c("UPC", "UAM", "UPC", "US", "UVigo", "UPV"),
  "Curso" = c("2021-2022", "2021-2022", "2020-2021", "2020-2021", "2021-2022", "2021-2022"),
  "Sexo" = c("Hombre", "Mujer", "Hombre", "Mujer", "Hombre", "Mujer"),
  "Forma.de.admisión" = c("PAU", "FP", "PAU", "FP", "PAU", "FP"),
  "Rama.de.enseñanza" = c("Ingeniería", "Ciencias", "Ingeniería", "Ciencias", "Ingeniería", "Ciencias"),
  "Nota.media" = c(8.5, 9.2, 7.8, 8.9, 8.0, 9.5),
  "Ámbito.de.estudio" = c("Tecnología", "Ciencias", "Tecnología", "Ciencias", "Tecnología", "Ciencias"),
  "Zona.de.nacionalidad" = c("España", "Extranjero", "España", "Extranjero", "España", "Extranjero"),
  "Número.de.estudiantes" = c(1500, 2000, 1200, 1800, 800, 1200)
)
# Seleccionar las columnas relevantes para las transacciones
transactions <- df[, c("Comunidad.Autónoma", "Universidad", "Curso", "Sexo", "Forma.de.admisión", "Rama.de.enseñanza", "Nota.media", "Ámbito.de.estudio", "Zona.de.nacionalidad", "Número.de.estudiantes")]

# Crear una lista de transacciones
transaction_list <- apply(transactions, 1, function(x) as.character(na.omit(x)))

# Función para construir el árbol FP con impresiones de depuración
build_fp_tree_debug_insensitive_corrected_v7 <- function(transactions) {
  
  root <- list(name = "root", count = 0, children = list())
  current_node <- root
  
  for (i in 1:length(transactions)) {
    transaction <- transactions[[i]]
    
    cat("Transacción:", i, "\n")
    
    for (item in transaction) {
      cat("El item es:", item, "\n")
      cat("El current_node es:", current_node$name, "\n")
      
      child_node <- NULL
      found_child <- FALSE
      
      # Buscar el nodo existente con el mismo nombre (insensible a mayúsculas/minúsculas)
      for (j in seq_along(current_node$children)) {
        child <- current_node$children[[j]]
        cat("Comienza el bucle")
        cat("Comparando:", tolower(child$name), "con", tolower(item), "\n")
        if (tolower(child$name) == tolower(item)) {
          child_node <- child
          found_child <- TRUE
          break
        }
      }
      cat("Lo has encontrado:", found_child, "\n")
      if (!found_child) {
        # Si no existe, crear uno nuevo y agregarlo a la lista de hijos
        new_node <- list(name = item, count = 1, parent = current_node, children = list())
        cat("Nuevo nodo:", new_node$name, "\n")
        node_nuevo = list(new_node)
        root$children <- c(root$children, list(new_node))
        # Ahora, establecer el nuevo nodo como el nodo actual
        current_node <- new_node
      } else {
        # Si existe, simplemente incrementar el contador
        cat("Nodo existente. Incrementando contador de", child_node$name, "\n")
        child_node$count <- child_node$count + 1
        # Y establecer el nodo existente como el nodo actual
        current_node <- child_node
      }
    }
    root$count <- root$count + 1
    cat("\n--- Fin de la transacción ---\n\n")
  }
  
  return(root)
}

# Construir el árbol FP con impresiones de depuración
fp_tree_debug_insensitive_corrected_v7 <- build_fp_tree_debug_insensitive_corrected_v7(transaction_list)




# Función para extraer patrones de árbol FP
mine_patterns <- function(fp_tree, min_support) {
  patterns <- list()
  
  for (i in length(fp_tree$children):1) {
    child <- fp_tree$children[[i]]
    
    if (child$count >= min_support) {
      pattern <- list(items = character(), count = child$count)
      
      while (!is.null(child$parent)) {
        pattern$items <- c(pattern$items, child$name)
        child <- child$parent
      }
      
      patterns <- c(patterns, list(pattern))
    }
  }
  
  return(patterns)
}

# Extraer patrones con un soporte mínimo del 1%
min_support <- length(transaction_list) * 0.01
patterns <- mine_patterns(fp_tree_debug_insensitive_corrected_v7, min_support)

# Mostrar patrones
for (pattern in patterns) {
  cat("Items:", paste(pattern$items, collapse = ", "), "Count:", pattern$count, "\n")
}
