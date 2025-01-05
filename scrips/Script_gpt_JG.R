```{r}
# Paso 1: Cargar librerías necesarias
library(tidyverse)
library(reshape2)

# Paso 2: Cargar los datos
setwd ("C:\\Users\\JGORDILLO\\OneDrive - Celsa Group\\MasterI40\\practica en equipo")
data <- read.csv("Sleep_Efficiency.csv")

# Paso 3: Explorar los datos
str(data)
summary(data)

# Paso 4: Preprocesar los datos
# Convertir las columnas Bedtime y Wakeup time a formato datetime
data$Bedtime <- as.POSIXct(data$Bedtime, format = "%Y-%m-%d %H:%M:%S")
data$Wakeup.time <- as.POSIXct(data$Wakeup.time, format = "%Y-%m-%d %H:%M:%S")

# Crear una nueva columna para la duración del sueño calculada (por si hay inconsistencias)
data$Calculated_Sleep_Duration <- as.numeric(difftime(data$Wakeup.time, data$Bedtime, units = "hours"))

# Manejar valores nulos rellenando con la media (puedes ajustar según el análisis)
data$Awakenings[is.na(data$Awakenings)] <- mean(data$Awakenings, na.rm = TRUE)
data$Caffeine.consumption[is.na(data$Caffeine.consumption)] <- mean(data$Caffeine.consumption, na.rm = TRUE)
data$Alcohol.consumption[is.na(data$Alcohol.consumption)] <- mean(data$Alcohol.consumption, na.rm = TRUE)
data$Exercise.frequency[is.na(data$Exercise.frequency)] <- mean(data$Exercise.frequency, na.rm = TRUE)

# Codificar variables categóricas
# Convertir Gender y Smoking status a factores
data$Gender <- as.factor(data$Gender)
data$Smoking.status <- as.factor(data$Smoking.status)

# Paso 5: Análisis Exploratorio
# Correlación entre variables numéricas (con coeficientes y p-valores)
calc_correlation <- function(df) {
  numeric_vars <- df %>% select_if(is.numeric)
  results <- expand.grid(var1 = names(numeric_vars), var2 = names(numeric_vars)) %>%
    filter(var1 != var2) %>%
    rowwise() %>%
    mutate(cor = cor.test(numeric_vars[[var1]], numeric_vars[[var2]], use = "complete.obs")$estimate,
           p_value = cor.test(numeric_vars[[var1]], numeric_vars[[var2]], use = "complete.obs")$p.value)
  return(results)
}

correlation_results <- calc_correlation(data)
print(head(correlation_results))  # Ver las primeras correlaciones y p-valores

# Visualizar correlaciones significativas
significant_correlations <- correlation_results %>%
  filter(p_value < 0.05)
print(significant_correlations)

# Mapa de calor de correlaciones
corr_matrix <- cor(data %>% select_if(is.numeric), use = "complete.obs")
corr_melted <- melt(corr_matrix)

ggplot(corr_melted, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  labs(title = "Correlation Matrix", x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Relación entre Sleep efficiency y otras variables
pairs(data %>% select(Sleep.efficiency, Age, Caffeine.consumption, 
                      Alcohol.consumption, Exercise.frequency, Awakenings))

# Boxplot de Sleep efficiency por género
ggplot(data, aes(x = Gender, y = Sleep.efficiency)) +
  geom_boxplot() +
  labs(title = "Sleep Efficiency by Gender", x = "Gender", y = "Sleep Efficiency")

# Histograma de la distribución de Sleep efficiency
ggplot(data, aes(x = Sleep.efficiency)) +
  geom_histogram(binwidth = 0.05, fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of Sleep Efficiency", x = "Sleep Efficiency", y = "Count")

# Identificar valores atípicos
boxplot(data$Sleep.efficiency, main = "Boxplot of Sleep Efficiency")

# Paso 6: Modelado predictivo
# Dividir los datos en entrenamiento y prueba
set.seed(123)
sample_indices <- sample(1:nrow(data), 0.8 * nrow(data))
train_data <- data[sample_indices, ]
test_data <- data[-sample_indices, ]

# Ajustar un modelo de regresión lineal
model <- lm(Sleep.efficiency ~ Age + Gender + Caffeine.consumption + 
              Alcohol.consumption + Smoking.status + Exercise.frequency + 
              Awakenings, data = train_data)

# Resumen del modelo
summary(model)

# Evaluar el modelo en el conjunto de prueba
predictions <- predict(model, newdata = test_data)
actuals <- test_data$Sleep.efficiency
rmse <- sqrt(mean((predictions - actuals)^2))
cat("RMSE del modelo: ", rmse, "\n")
```
