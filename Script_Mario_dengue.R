# ============================================================
# ANÁLISIS DE DENGUE vs CLIMA EN CAMPECHE (2013–2024)
# ============================================================
# Datos anuales (n = 12). Conteos sobredispersos (rango 90-13 397).
# Por tamaño muestral y distribución, el análisis se basa en:
#   - Estadística descriptiva y prueba de normalidad (Shapiro–Wilk)
#   - Correlación no paramétrica (Spearman)
#   - Correlación cruzada (CCF) para evaluar rezagos
#   - Regresión Binomial Negativa (apropiada para conteos sobredispersos)
#
# Significancia representada como:
#   *** p < 0.001   ** p < 0.01   * p < 0.05   ns no significativo
#
# Limitaciones: n = 12 es pequeño; resultados exploratorios. Las
# temperaturas (max, med, min) son colineales; se evalúan en
# modelos univariados separados.
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(MASS)        # glm.nb
  library(patchwork)   # composición de paneles
})

# Resolver conflictos de namespace (MASS::select vs dplyr::select)
select <- dplyr::select

# --- Directorio de salida ------------------------------------
fig_dir <- "Figuras"
if (!dir.exists(fig_dir)) dir.create(fig_dir)

# --- Datos ---------------------------------------------------
df <- data.frame(
  anio     = 2013:2024,
  temp_max = c(32.4,33.0,33.6,33.7,33.6,32.9,33.6,33.5,33.4,33.3,34.2,34.0),
  temp_med = c(26.8,27.2,27.6,27.8,27.7,27.0,27.6,27.8,27.5,27.3,28.3,28.3),
  temp_min = c(21.1,21.3,21.7,22.0,21.8,21.2,21.7,22.1,21.7,21.7,22.5,22.7),
  precip   = c(1698.5,1390.1,1326.1,1177.0,1407.3,1389.8,1303.2,1779.3,
               1218.7,1423.2,1132.5,1348.8),
  casos    = c(1685,977,7475,4109,588,194,760,302,90,604,13397,5996)
)

# --- Estilo y utilidades -------------------------------------
theme_sci <- function(base_size = 11) {
  theme_classic(base_size = base_size) +
    theme(
      axis.line        = element_line(linewidth = 0.4, colour = "grey20"),
      axis.ticks       = element_line(linewidth = 0.4, colour = "grey20"),
      axis.text        = element_text(colour = "grey20"),
      plot.title       = element_text(size = base_size + 1, face = "bold"),
      plot.subtitle    = element_text(size = base_size - 1, colour = "grey30"),
      plot.caption     = element_text(size = base_size - 2, colour = "grey40",
                                      hjust = 0),
      strip.background = element_blank(),
      strip.text       = element_text(face = "bold"),
      legend.position  = "right",
      panel.grid.major = element_line(colour = "grey85", linewidth = 0.25,
                                      linetype = "dotted"),
      panel.grid.minor = element_blank()
    )
}

sig_stars <- function(p) {
  dplyr::case_when(
    is.na(p)   ~ "",
    p < 0.001  ~ "***",
    p < 0.01   ~ "**",
    p < 0.05   ~ "*",
    TRUE       ~ "ns"
  )
}

save_fig <- function(plot, n, name, w = 7, h = 5) {
  fname <- sprintf("Figura_%02d_%s.png", n, name)
  ggsave(file.path(fig_dir, fname), plot,
         width = w, height = h, dpi = 300, bg = "white")
  message("  -> ", fname)
  invisible(fname)
}

cap_sig <- "Significancia: *** p<0.001, ** p<0.01, * p<0.05, ns no significativo."

preds        <- c("temp_max", "temp_med", "temp_min", "precip")
labels_preds <- c("Temp. máxima (°C)", "Temp. media (°C)",
                  "Temp. mínima (°C)", "Precipitación (mm)")
vars_all     <- c("casos", preds)
labels_all   <- c("Casos de dengue", labels_preds)

df_long <- df %>%
  pivot_longer(-anio, names_to = "variable", values_to = "valor") %>%
  mutate(variable = factor(variable, levels = vars_all, labels = labels_all))

# =============================================================
# FIGURA 01 — Serie temporal
# =============================================================
p1 <- ggplot(df_long, aes(anio, valor)) +
  geom_line(colour = "grey40", linewidth = 0.5) +
  geom_point(colour = "black", size = 1.6) +
  facet_wrap(~variable, scales = "free_y", ncol = 1, strip.position = "left") +
  scale_x_continuous(breaks = df$anio) +
  labs(x = "Año", y = NULL,
       title = "Serie temporal de dengue y variables climáticas",
       subtitle = "Campeche, 2013–2024") +
  theme_sci() +
  theme(strip.placement  = "outside",
        strip.text.y.left = element_text(angle = 90, face = "bold"),
        axis.text.x       = element_text(angle = 0))
save_fig(p1, 1, "serie_temporal", w = 7, h = 9)

# =============================================================
# FIGURA 02 — Distribución + Shapiro–Wilk
# =============================================================
shapiro_res <- df_long %>%
  group_by(variable) %>%
  summarise(W = shapiro.test(valor)$statistic,
            p = shapiro.test(valor)$p.value, .groups = "drop") %>%
  mutate(stars = sig_stars(p),
         label = sprintf("W = %.2f   p = %.3f %s", W, p, stars))

p2 <- ggplot(df_long, aes(valor)) +
  geom_histogram(bins = 6, fill = "grey85", colour = "grey20",
                 linewidth = 0.3) +
  geom_text(data = shapiro_res,
            aes(x = -Inf, y = Inf, label = label),
            hjust = -0.05, vjust = 1.4, size = 3, inherit.aes = FALSE) +
  facet_wrap(~variable, scales = "free", ncol = 2) +
  labs(x = "Valor", y = "Frecuencia",
       title = "Distribución de variables y prueba de normalidad") +
  theme_sci()
save_fig(p2, 2, "distribucion_normalidad", w = 8, h = 6.5)

# =============================================================
# FIGURA 03 — Matriz de correlación de Spearman
# =============================================================
cor_mat <- cor(df[, vars_all], method = "spearman")
p_mat   <- matrix(NA, length(vars_all), length(vars_all),
                  dimnames = list(vars_all, vars_all))
for (i in seq_along(vars_all)) for (j in seq_along(vars_all)) {
  p_mat[i, j] <- suppressWarnings(
    cor.test(df[[vars_all[i]]], df[[vars_all[j]]],
             method = "spearman")$p.value
  )
}

cor_long <- as.data.frame(as.table(cor_mat)) %>%
  rename(var1 = Var1, var2 = Var2, rho = Freq) %>%
  mutate(p     = as.vector(p_mat),
         stars = sig_stars(p),
         label = ifelse(var1 == var2, "",
                        sprintf("%.2f\n%s", rho, stars)),
         var1  = factor(var1, levels = vars_all, labels = labels_all),
         var2  = factor(var2, levels = rev(vars_all),
                        labels = rev(labels_all)))

p3 <- ggplot(cor_long, aes(var1, var2, fill = rho)) +
  geom_tile(colour = "white", linewidth = 0.4) +
  geom_text(aes(label = label), size = 3, lineheight = 0.9) +
  scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b",
                       midpoint = 0, limits = c(-1, 1),
                       name = expression(rho)) +
  coord_fixed() +
  labs(title = "Correlaciones de Spearman: dengue y clima",
       x = NULL, y = NULL) +
  theme_sci() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        panel.grid  = element_blank())
save_fig(p3, 3, "correlacion_spearman", w = 7, h = 6)

# =============================================================
# FIGURA 04 — Bivariadas casos vs predictores
# =============================================================
bivar <- df %>%
  select(anio, casos, all_of(preds)) %>%
  pivot_longer(-c(anio, casos), names_to = "variable", values_to = "x") %>%
  mutate(variable = factor(variable, levels = preds, labels = labels_preds))

annot <- map_dfr(preds, function(v) {
  ct <- suppressWarnings(cor.test(df$casos, df[[v]], method = "spearman"))
  tibble(variable = v,
         rho      = unname(ct$estimate),
         p        = ct$p.value,
         stars    = sig_stars(ct$p.value))
}) %>%
  mutate(variable = factor(variable, levels = preds, labels = labels_preds),
         label    = sprintf("ρ = %.2f   p = %.3f %s", rho, p, stars))

p4 <- ggplot(bivar, aes(x, casos)) +
  geom_smooth(method = "loess", se = TRUE, colour = "grey45",
              fill = "grey85", linewidth = 0.5,
              formula = y ~ x, span = 1) +
  geom_point(size = 2, colour = "black") +
  geom_text(data = annot,
            aes(x = -Inf, y = Inf, label = label),
            hjust = -0.05, vjust = 1.4, size = 3, inherit.aes = FALSE) +
  facet_wrap(~variable, scales = "free_x", ncol = 2) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = NULL, y = "Casos de dengue",
       title = "Relación bivariada: dengue vs clima") +
  theme_sci()
save_fig(p4, 4, "bivariado_spearman", w = 8.5, h = 6.5)

# =============================================================
# FIGURA 05 — Correlación cruzada (CCF) con rezagos
# =============================================================
# ccf(casos, climate): lag k = cor(casos_{t+k}, climate_t).
#   k > 0  → el clima precede a los casos (efecto rezagado del clima)
#   k < 0  → los casos preceden al clima
ccf_data <- map_dfr(preds, function(v) {
  obj <- ccf(df$casos, df[[v]], lag.max = 4, plot = FALSE)
  tibble(lag      = as.vector(obj$lag),
         acf      = as.vector(obj$acf),
         variable = v)
}) %>%
  mutate(variable = factor(variable, levels = preds, labels = labels_preds))

ci_ccf <- qnorm(0.975) / sqrt(nrow(df))

p5 <- ggplot(ccf_data, aes(lag, acf)) +
  geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
  geom_hline(yintercept = c(-ci_ccf, ci_ccf), colour = "grey50",
             linetype = "dashed", linewidth = 0.3) +
  geom_segment(aes(xend = lag, yend = 0), linewidth = 0.6) +
  geom_point(size = 1.6) +
  facet_wrap(~variable, ncol = 2) +
  scale_x_continuous(breaks = -4:4) +
  labs(x = "Rezago (años)",
       y = "Correlación cruzada",
       title = "Correlación cruzada clima–dengue") +
  theme_sci()
save_fig(p5, 5, "ccf_lags", w = 8.5, h = 6)

# =============================================================
# FIGURA 06 — Forest plot: regresión Binomial Negativa univariada
# =============================================================
nb_fit <- function(predictor) {
  f <- as.formula(paste("casos ~", predictor))
  glm.nb(f, data = df)
}
models <- setNames(lapply(preds, nb_fit), labels_preds)

coef_df <- imap_dfr(models, function(m, name) {
  s   <- summary(m)$coefficients
  est <- s[2, 1]; se <- s[2, 2]; p <- s[2, 4]
  tibble(modelo = name,
         IRR    = exp(est),
         lo     = exp(est - qnorm(0.975) * se),
         hi     = exp(est + qnorm(0.975) * se),
         p      = p,
         stars  = sig_stars(p),
         AIC    = AIC(m),
         theta  = m$theta)
}) %>%
  mutate(modelo = factor(modelo, levels = rev(labels_preds)))

p6 <- ggplot(coef_df, aes(IRR, modelo)) +
  geom_vline(xintercept = 1, linetype = "dashed",
             colour = "grey50", linewidth = 0.3) +
  geom_errorbarh(aes(xmin = lo, xmax = hi),
                 height = 0.15, colour = "grey30", linewidth = 0.5) +
  geom_point(size = 2.6) +
  geom_text(aes(x = hi, label = stars),
            hjust = -0.4, vjust = 0.5, size = 4) +
  scale_x_log10() +
  labs(x = "Razón de tasas (IRR, escala log)",
       y = NULL,
       title = "Asociación clima–dengue: Binomial Negativa univariada") +
  theme_sci()
save_fig(p6, 6, "forest_NB", w = 7.5, h = 4.5)

# =============================================================
# FIGURA 07 — Observado vs predicho (mejor modelo por AIC)
# =============================================================
best_name  <- coef_df$modelo[which.min(coef_df$AIC)] %>% as.character()
best_idx   <- match(best_name, labels_preds)
best_model <- models[[best_name]]
df$pred    <- predict(best_model, type = "response")

p7 <- ggplot(df, aes(anio)) +
  geom_line(aes(y = casos, colour = "Observado"), linewidth = 0.6) +
  geom_point(aes(y = casos, colour = "Observado"), size = 2.2) +
  geom_line(aes(y = pred, colour = "Predicho (NB)"),
            linewidth = 0.6, linetype = "dashed") +
  geom_point(aes(y = pred, colour = "Predicho (NB)"),
             size = 2.2, shape = 17) +
  scale_x_continuous(breaks = df$anio) +
  scale_y_continuous(labels = scales::comma) +
  scale_colour_manual(values = c("Observado"     = "black",
                                 "Predicho (NB)" = "#b2182b"),
                      name = NULL) +
  labs(x = "Año", y = "Casos de dengue",
       title    = sprintf("Casos observados vs predichos · NB ~ %s", best_name),
       subtitle = sprintf("AIC = %.1f   θ = %.2f   IRR = %.3f %s",
                          AIC(best_model), best_model$theta,
                          coef_df$IRR[best_idx], coef_df$stars[best_idx])) +
  theme_sci()
save_fig(p7, 7, "obs_vs_pred_NB", w = 8.5, h = 4.5)

# =============================================================
# FIGURA 08 — Diagnóstico del modelo NB
# =============================================================
diag_df <- tibble(
  fitted        = fitted(best_model),
  resid_pearson = residuals(best_model, type = "pearson"),
  resid_dev     = residuals(best_model, type = "deviance"),
  anio          = df$anio
)

g1 <- ggplot(diag_df, aes(fitted, resid_pearson)) +
  geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
  geom_point(size = 2) +
  labs(x = "Valores ajustados", y = "Residuos de Pearson",
       title = "(a) Residuos vs ajustados") +
  theme_sci()

g2 <- ggplot(diag_df, aes(sample = resid_dev)) +
  stat_qq(size = 2) + stat_qq_line(linewidth = 0.4) +
  labs(x = "Cuantil teórico", y = "Residuos de devianza",
       title = "(b) Q–Q de residuos") +
  theme_sci()

g3 <- ggplot(diag_df, aes(anio, resid_pearson)) +
  geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
  geom_line(colour = "grey40", linewidth = 0.4) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = df$anio) +
  labs(x = "Año", y = "Residuos de Pearson",
       title = "(c) Residuos a lo largo del tiempo") +
  theme_sci()

acf_obj <- acf(diag_df$resid_pearson, plot = FALSE, lag.max = 5)
acf_df  <- tibble(lag = as.vector(acf_obj$lag),
                  acf = as.vector(acf_obj$acf))
ci_acf  <- qnorm(0.975) / sqrt(nrow(diag_df))

# Prueba Box–Ljung (autocorrelación de residuos)
bl <- Box.test(diag_df$resid_pearson, lag = 3, type = "Ljung-Box")
bl_label <- sprintf("Box–Ljung (lag 3): χ² = %.2f, p = %.3f %s",
                    bl$statistic, bl$p.value, sig_stars(bl$p.value))

g4 <- ggplot(acf_df, aes(lag, acf)) +
  geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
  geom_hline(yintercept = c(-ci_acf, ci_acf), colour = "grey50",
             linetype = "dashed", linewidth = 0.3) +
  geom_segment(aes(xend = lag, yend = 0), linewidth = 0.6) +
  geom_point(size = 1.6) +
  annotate("text", x = Inf, y = Inf, label = bl_label,
           hjust = 1.05, vjust = 1.4, size = 3) +
  labs(x = "Rezago", y = "ACF",
       title = "(d) Autocorrelación de residuos") +
  theme_sci()

p8 <- (g1 | g2) / (g3 | g4) +
  plot_annotation(
    title = sprintf("Diagnóstico del modelo Binomial Negativa: casos ~ %s",
                    best_name),
    theme = theme(plot.title = element_text(face = "bold", size = 12))
  )
save_fig(p8, 8, "diagnostico_NB", w = 10, h = 7.5)

# =============================================================
# Resumen al log
# =============================================================
cat("\n================ RESUMEN ESTADÍSTICO ================\n")
cat("\nShapiro–Wilk (normalidad por variable):\n")
print(shapiro_res %>% select(variable, W, p, stars), n = Inf)

cat("\nCorrelaciones de Spearman vs casos:\n")
print(annot %>% select(variable, rho, p, stars), n = Inf)

cat("\nModelos Binomial Negativa univariados:\n")
print(coef_df %>% select(modelo, IRR, lo, hi, p, stars, AIC, theta), n = Inf)

cat(sprintf("\nMejor modelo por AIC: NB ~ %s   (AIC = %.2f)\n",
            best_name, AIC(best_model)))
cat(sprintf("Box–Ljung sobre residuos (lag 3): p = %.3f %s\n",
            bl$p.value, sig_stars(bl$p.value)))
cat("\nFiguras guardadas en:", normalizePath(fig_dir), "\n")
