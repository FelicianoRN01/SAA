# Cargar las librerías necesarias
library(randomForest)
library(caret)

# Cargar los datos (cambia la ruta si es necesario)
housing <- read.csv("C:/Users/felic/OneDrive/Escritorio/housing.csv", header = TRUE)

# Convertir 'ocean_proximity' a variable categórica (factor)
housing$ocean_proximity <- as.factor(housing$ocean_proximity)

# Eliminar filas con valores faltantes
housing <- na.omit(housing)

# Dividir los datos en entrenamiento (80%) y prueba (20%)
set.seed(123)  # Para reproducibilidad
trainIndex <- createDataPartition(housing$median_house_value, p = 0.8, list = FALSE)
trainData <- housing[trainIndex, ]
testData <- housing[-trainIndex, ]

# --- Modelo de Regresión Lineal ---
lm_model <- lm(median_house_value ~ ., data = trainData)
summary(lm_model)  # Muestra detalles del modelo

# Predicciones y evaluación del modelo de regresión lineal
pred_lm <- predict(lm_model, newdata = testData)
rmse_lm <- sqrt(mean((pred_lm - testData$median_house_value)^2))
r2_lm <- cor(pred_lm, testData$median_house_value)^2

# --- Modelo de Random Forest ---
rf_model <- randomForest(median_house_value ~ ., data = trainData, ntree = 100)
print(rf_model)

# Predicciones y evaluación del modelo de random forest
pred_rf <- predict(rf_model, newdata = testData)
rmse_rf <- sqrt(mean((pred_rf - testData$median_house_value)^2))
r2_rf <- cor(pred_rf, testData$median_house_value)^2

# --- Comparación de resultados ---
cat("Regresión Lineal: RMSE =", rmse_lm, ", R² =", r2_lm, "\n")
cat("Random Forest:    RMSE =", rmse_rf, ", R² =", r2_rf, "\n")

