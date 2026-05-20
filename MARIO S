# Datos
anio <- 2013:2024

temp_max <- c(32.4, 33.0, 33.6, 33.7, 33.6, 32.9,
              33.6, 33.5, 33.4, 33.0, 34.2, 34.0)

temp_media <- c(26.8, 27.2, 27.6, 27.8, 27.7, 27.0,
                27.6, 27.8, 27.5, 27.3, 28.3, 28.3)

temp_min <- c(21.1, 21.3, 21.7, 22.0, 21.8, 21.2,
              21.7, 22.1, 21.7, 21.7, 22.5, 22.7)

dengue <- c(1685, 977, 7475, 4109, 588, 194,
            760, 302, 90, 604, 13397, 5996)

# Crear dataframe
datos <- data.frame(anio, temp_max, temp_media, temp_min, dengue)

# Ver datos
print(datos)

# =========================
# CORRELACIONES
# =========================

# Temperatura máxima
cor_max <- cor.test(datos$temp_max, datos$dengue, method = "pearson")

# Temperatura media
cor_media <- cor.test(datos$temp_media, datos$dengue, method = "pearson")

# Temperatura mínima
cor_min <- cor.test(datos$temp_min, datos$dengue, method = "pearson")

# Mostrar resultados
cat("\n--- Correlación Temperatura Máxima vs Dengue ---\n")
print(cor_max)

cat("\n--- Correlación Temperatura Media vs Dengue ---\n")
print(cor_media)

cat("\n--- Correlación Temperatura Mínima vs Dengue ---\n")
print(cor_min)

# =========================
# MATRIZ DE CORRELACIÓN
# =========================

matriz <- cor(datos[, c("temp_max", "temp_media", "temp_min", "dengue")])

cat("\n--- Matriz de correlación ---\n")
print(round(matriz, 3))
