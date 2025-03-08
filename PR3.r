# Cargamos las librerías necesarias para manipulación de datos, visualización y análisis
library(tidyverse)  # Incluye dplyr, ggplot2, entre otros
library(corrplot)   # Para visualizar la matriz de correlaciones

# Leemos el dataset. Suponemos que el archivo "housing.csv" se encuentra en el directorio de trabajo.
data <- read.csv("housing.csv", header = TRUE, sep = ",")

# Revisamos las primeras filas del dataset para entender su estructura
head(data)

# Resumen estadístico de las variables
summary(data)

# Estructura del dataset
str(data)

# Histograma de la variable 'MedInc' (Ingreso Medio)
ggplot(data, aes(x = MedInc)) +
  geom_histogram(bins = 30, fill = "blue", color = "white", alpha = 0.7) +
  labs(title = "Distribución del Ingreso Medio", x = "Ingreso Medio", y = "Frecuencia")

# Seleccionamos solo las variables numéricas para calcular la matriz de correlación
numericas <- data[sapply(data, is.numeric)]
cor_matrix <- cor(numericas, use = "complete.obs")

# Visualizamos la matriz de correlación
corrplot(cor_matrix, method = "number")

# Ajustamos el modelo de regresión lineal
# Se usa la fórmula: Precio ~ Ingreso Medio + Edad de la vivienda + Promedio de cuartos + 
# Promedio de dormitorios + Población + Promedio de ocupación + Latitud + Longitud
modelo <- lm(MedianHouseValue ~ MedInc + HouseAge + AveRooms + AveBedrms +
             Population + AveOccup + Latitude + Longitude, data = data)

# Resumen del modelo para evaluar coeficientes, significancia y ajuste general
summary(modelo)

# Extraemos los residuos del modelo
residuos <- residuals(modelo)

# Graficamos los residuos vs. los valores ajustados
plot(modelo$fitted.values, residuos,
     xlab = "Valores Ajustados", ylab = "Residuos",
     main = "Residuos vs Valores Ajustados", pch = 20, col = "darkgreen")
abline(h = 0, col = "red", lwd = 2)
