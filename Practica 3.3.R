library(ggplot2)  # Para la visualización de datos
library(dplyr)    # Para la manipulación de datos
library(readr)    # Para la lectura de archivos CSV
library(corrplot) # Para visualizar matrices de correlación

# 2. Cargar el conjunto de datos

# Supongamos que el conjunto de datos se encuentra en un archivo CSV.
# Cambia "ruta/del/archivo.csv" por la ruta correcta en tu equipo.
datos <- read_csv("C:/Users/felic/OneDrive/Escritorio/housing.csv")

# 3. Análisis Descriptivo de los Datos

# Visualizamos las primeras filas del dataset para entender su estructura
head(datos)

# Obtenemos un resumen estadístico de las variables numéricas
summary(datos)

# Revisamos la estructura de los datos (tipos de variables, número de observaciones, etc.)
str(datos)

# 4. Visualización de algunas variables

# Ejemplo: Distribución del ingreso medio (MedInc)
ggplot(datos, aes(x = MedInc)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black") +
  labs(title = "Distribución del Ingreso Medio",
       x = "Ingreso Medio",
       y = "Frecuencia")

# Ejemplo: Relación entre ingreso medio y precio medio de la vivienda (MedHouseVal)
ggplot(datos, aes(x = MedInc, y = MedHouseVal)) +
  geom_point(alpha = 0.5, color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relación entre Ingreso Medio y Precio de Vivienda",
       x = "Ingreso Medio",
       y = "Precio Medio de la Vivienda")

# 5. Análisis de Correlación

# Seleccionamos únicamente las variables numéricas para calcular la matriz de correlación
datos_numeric <- datos %>% select_if(is.numeric)
correlaciones <- cor(datos_numeric, use = "complete.obs")

# Visualizamos la matriz de correlación
corrplot(correlaciones, method = "color", type = "upper", tl.cex = 0.8)

# 6. Creación del Modelo de Regresión Lineal

# Se construye un modelo de regresión lineal para predecir el precio medio de la vivienda (MedHouseVal)
# usando las variables predictoras disponibles.
modelo <- lm(MedHouseVal ~ MedInc + HouseAge + AveRooms + AveBedrms + Population + AveOccup + Latitude + Longitude, 
             data = datos)

# Resumen del modelo: muestra los coeficientes, error estándar, valores t y p, y estadísticas de ajuste.
summary(modelo)

# 7. Evaluación y Diagnóstico del Modelo

# Diagnóstico: Graficamos los residuos para verificar supuestos del modelo.
par(mfrow = c(2, 2))  # Dividimos la ventana gráfica en 4 partes
plot(modelo)
par(mfrow = c(1, 1))  # Restauramos la configuración gráfica original

