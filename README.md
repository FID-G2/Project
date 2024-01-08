# Proyecto FID - G2

Este repositorio contiene el código y los recursos necesarios para abordar la manipulación y análisis de un conjunto de datos, aplicado a las técnicas vistas durante el curso, cuya temática está relacionada con los resultados de la [selectividad en España](https://estadisticas.universidades.gob.es/dynPx/inebase/index.htm?type=pcaxis&path=/Universitaria/Alumnado/EEU_2023/GradoCiclo/NuevoIngreso/&file=pcaxis&l=s0).

## Estructura del Proyecto

El proyecto está organizado en varias carpetas, cada una con un propósito específico. A continuación, se describen brevemente cada una de ellas:

### 1. Add-noise

Contiene funciones esenciales para llevar a cabo la adición de ruido al dataset original. Estas funciones son fundamentales para aplicar diversas técnicas de generación de ruido y evaluar su impacto en el análisis de datos.

### 2. Apriori

Incluye los archivos necesarios para realizar el preprocesamiento y la construcción del modelo *Apriori*.

### 3. Data

- FirstData: Contiene los datos iniciales descargados desde la plataforma del Ministerio de Universidades.
- CleanData: Almacena datasets que han sido generados después de diversas operaciones como limpieza, transformación y selección. Estos conjuntos de datos preparados son utilizados en fases posteriores del análisis.

### 4. Integration

Incluye archivos necesarios para llevar a cabo la integración de los datasets seleccionados.

### 5. Regression

Contiene archivos asociados al problema de regresión, incluyendo:

- Módulos para la limpieza, transformación y selección de datos antes de entrenar modelos de regresión.
- Model: Archivos relacionados con el entrenamiento y la comparación de modelos de regresión.

### 6. Visualization

Contiene un archivo Rmd que presenta 9 técnicas diferentes de visualización aplicadas al dataset.
