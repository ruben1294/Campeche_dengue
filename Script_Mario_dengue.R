# =========================================
# ANALISIS DE DENGUE vs CLIMA EN CAMPECHE
# =========================================

# Cargar librerías
library(tidyverse)
library(ggplot2)
library(MASS)       # Para modelos robustos
library(mgcv)       # Para GAM
library(GGally)     # Para correlogramas

# ----------------------------
# DATOS EJEMPLO (CAMPECHE)
# ----------------------------
df <- data.frame(
  anio = 2013:2024,
  temp_max = c(32.4,33,33.6,33.7,33.6,32.9,33.6,33.5,33.4,33.3,34.2,34),
  temp_med = c(26.8,27.2,27.6,27.8,27.7,27,27.6,27.8,27.5,27.3,28.3,28.3),
  temp_min = c(21.1,21.3,21.7,22,21.8,21.2,21.7,22.1,21.7,21.7,22.5,22.7),
  precip = c(1698.5,1390.1,1326.1,1177,1407.3,1389.8,1303.2,1779.3,1218.7,1423.2,1132.5,1348.8),
  casos = c(1685,977,7475,4109,588,194,760,302,90,604,13397,5996)
)

# ----------------------------
# 1️⃣ Visualización básica
# ----------------------------
ggplot(df, aes(x=temp_max, y=casos)) +
  geom_point(color="red") +
  geom_smooth(method="lm", se=TRUE, color="blue") +
  labs(title="Relación Temperatura vs Casos de Dengue",
       x="Temperatura Máxima (°C)",
       y="Casos de Dengue") +
  theme_minimal()

# ----------------------------
# 2️⃣ Modelo lineal simple
# ----------------------------
modelo_simple <- lm(casos ~ temp_max, data=df)
summary(modelo_simple)

# ----------------------------
# 3️⃣ Agregar lags de temperatura (1–3 años, ya que son datos anuales)
# ----------------------------
df <- df %>%
  arrange(anio) %>%
  mutate(
    temp_lag1 = lag(temp_max, 1),
    temp_lag2 = lag(temp_max, 2),
    temp_lag3 = lag(temp_max, 3)
  )

# Modelo con lags
modelo_lag <- lm(casos ~ temp_lag1 + temp_lag2 + temp_lag3, data=df)
summary(modelo_lag)

# ----------------------------
# 4️⃣ Modelo múltiple con variables climáticas
# ----------------------------
modelo_multi <- lm(casos ~ temp_max + precip + humedad, data=df)
summary(modelo_multi)

# ----------------------------
# 5️⃣ Modelo robusto para controlar outliers
# ----------------------------
modelo_robusto <- rlm(casos ~ temp_max + precip + humedad, data=df)
summary(modelo_robusto)

# ----------------------------
# 6️⃣ Modelo GAM (relaciones no lineales)
# ----------------------------
modelo_gam <- gam(casos ~ s(temp_max) + s(precip) + s(humedad), data=df)
summary(modelo_gam)
plot(modelo_gam, se=TRUE, col="blue")

# ----------------------------
# 7️⃣ Correlaciones y diagnósticos
# ----------------------------
# Correlograma de variables
ggpairs(df %>% select(casos, temp_max, precip, humedad))

# Diagnóstico de residuos del modelo múltiple
par(mfrow=c(2,2))
plot(modelo_multi)
par(mfrow=c(1,1))

# ----------------------------
# 8️⃣ Visualización de lags
# ----------------------------
df %>%
  pivot_longer(cols = starts_with("temp_lag"), names_to="lag", values_to="temp") %>%
  ggplot(aes(x=temp, y=casos, color=lag)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE) +
  labs(title="Casos de Dengue vs Temperatura con Lags",
       x="Temperatura (°C)", y="Casos de Dengue") +
  theme_minimal()

# ----------------------------
# 9️⃣ Predicción con modelo múltiple
# ----------------------------
df$predicted <- predict(modelo_multi, newdata=df)

ggplot(df, aes(x=anio)) +
  geom_line(aes(y=casos), color="red", size=1) +
  geom_line(aes(y=predicted), color="blue", linetype="dashed") +
  labs(title="Predicción de Casos de Dengue vs Observados",
       x="Año", y="Casos de Dengue") +
  theme_minimal()