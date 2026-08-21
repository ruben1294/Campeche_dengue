# ==============================================================
# ANÁLISIS DENGUE–CLIMA EN CAMPECHE
# ==============================================================
# Ejecuta:  Rscript Script_Mario_dengue.R
#
# Fuentes:
#   - Clima diario:   NASA POWER API  (lat -90.53, lon 19.84)
#                     https://power.larc.nasa.gov
#   - ENSO mensual:   ONI · NOAA PSL
#                     https://psl.noaa.gov/data/correlation/oni.data
#   - Dengue anual:   Hardcoded (Anuarios DGE / SSA, 2013–2024).
#                     La descarga automatizada de los Anuarios DGE
#                     requiere parseo de PDFs y no es robusta; se
#                     mantienen como tabla embebida.
#
# Resolución temporal:
#   - Clima: diario (NASA POWER) → mensual y anual
#   - ENSO:  mensual (ONI NOAA) → expuesto como serie mensual y
#            agregado anualmente en múltiples features (media,
#            estacional DJF/MAM/JJA/SON, máx/mín, conteo de meses
#            Niño/Niña, rezago de 1 año)
#   - Dengue: anual (próxima iteración: mensual, con CONAGUA y
#            Panorama Epidemiológico DGE)
#
# Salida `data/processed/monthly_dataset.csv` es la plantilla
# lista para análisis mensual una vez se incorporen casos
# mensuales y/o variables CONAGUA puntuales.
#
# Salidas:
#   data/raw/         insumos crudos
#   data/processed/   tablas limpias (CSV)
#   Figuras/          PNG 300 DPI numeradas
#   outputs/          resumen estadístico (txt)
# ==============================================================

# --------------------------------------------------------------
# 0. Bootstrap: paquetes
# --------------------------------------------------------------
required <- c("tidyverse", "MASS", "patchwork", "scales")
to_install <- setdiff(required, rownames(installed.packages()))
if (length(to_install)) {
  install.packages(to_install, repos = "https://cloud.r-project.org")
}
suppressPackageStartupMessages(
  invisible(lapply(required, library, character.only = TRUE))
)
select <- dplyr::select # evita choque con MASS::select

# --------------------------------------------------------------
# 1. Configuración global
# --------------------------------------------------------------
CONFIG <- list(
  # Coordenadas: ciudad de Campeche
  lat = 19.84,
  lon = -90.53,
  date_start = "20130101",
  date_end = "20241231",
  # URLs
  url_power = "https://power.larc.nasa.gov/api/temporal/daily/point",
  url_oni = "https://psl.noaa.gov/data/correlation/oni.data",
  # Forzar nueva descarga aunque exista caché
  force_download = FALSE
)

ROOT <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)),
  error = function(e) getwd()
)
DIRS <- list(
  raw  = file.path(ROOT, "data", "raw"),
  proc = file.path(ROOT, "data", "processed"),
  fig  = file.path(ROOT, "Figuras"),
  out  = file.path(ROOT, "outputs")
)
invisible(lapply(DIRS, dir.create, showWarnings = FALSE, recursive = TRUE))

# --------------------------------------------------------------
# 2. Utilidades (estilo, helpers, logging)
# --------------------------------------------------------------
log_msg <- function(...) {
  cat(format(Sys.time(), "[%H:%M:%S] "), ..., "\n", sep = "")
}

sig_stars <- function(p) {
  dplyr::case_when(
    is.na(p) ~ "",
    p < 0.001 ~ "***",
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    TRUE ~ "ns"
  )
}

theme_sci <- function(base_size = 11) {
  theme_classic(base_size = base_size) +
    theme(
      axis.line = element_line(linewidth = 0.4, colour = "grey20"),
      axis.ticks = element_line(linewidth = 0.4, colour = "grey20"),
      axis.text = element_text(colour = "grey20"),
      plot.title = element_text(size = base_size + 1, face = "bold"),
      plot.subtitle = element_text(size = base_size - 1, colour = "grey30"),
      plot.caption = element_text(
        size = base_size - 2, colour = "grey40",
        hjust = 0
      ),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold"),
      legend.position = "right",
      panel.grid.major = element_line(
        colour = "grey85", linewidth = 0.25,
        linetype = "dotted"
      ),
      panel.grid.minor = element_blank()
    )
}

save_fig <- function(plot, n, name, w = 7, h = 5) {
  fname <- sprintf("Figura_%02d_%s.png", n, name)
  ggsave(file.path(DIRS$fig, fname), plot,
    width = w, height = h, dpi = 300, bg = "white"
  )
  log_msg("  -> ", fname)
  invisible(fname)
}

# --------------------------------------------------------------
# 3. Descargas (con caché local)
# --------------------------------------------------------------
download_cached <- function(url, dest, label) {
  if (file.exists(dest) && !CONFIG$force_download) {
    log_msg("[", label, "] caché: ", basename(dest))
    return(invisible(dest))
  }
  log_msg("[", label, "] descargando ", url)
  tryCatch(
    {
      utils::download.file(url, dest,
        mode = "wb", quiet = TRUE,
        method = "libcurl"
      )
      if (file.size(dest) < 1024L) {
        stop("Archivo descargado demasiado pequeño (<1 KB).")
      }
      log_msg("[", label, "] OK (", file.size(dest), " bytes)")
    },
    error = function(e) {
      stop(sprintf(
        "Falló la descarga de %s: %s\nVerifica tu conexión.",
        label, conditionMessage(e)
      ))
    }
  )
  invisible(dest)
}

fetch_power <- function() {
  params <- "T2M,T2M_MAX,T2M_MIN,RH2M,PRECTOTCORR,WS2M"
  url <- sprintf(
    "%s?parameters=%s&community=AG&latitude=%s&longitude=%s&start=%s&end=%s&format=CSV",
    CONFIG$url_power, params, CONFIG$lat, CONFIG$lon,
    CONFIG$date_start, CONFIG$date_end
  )
  dest <- file.path(DIRS$raw, "nasa_power_daily.csv")
  download_cached(url, dest, "NASA POWER")
}

fetch_oni <- function() {
  dest <- file.path(DIRS$raw, "oni_monthly.txt")
  download_cached(CONFIG$url_oni, dest, "ONI NOAA")
}

# --------------------------------------------------------------
# 4. Parseo
# --------------------------------------------------------------
# El CSV de NASA POWER trae un preámbulo terminado con "-END HEADER-"
parse_power <- function(path) {
  raw <- readLines(path)
  end_idx <- grep("-END HEADER-", raw, fixed = TRUE)
  skip_n <- if (length(end_idx)) end_idx else 0L
  read.csv(path,
    skip = skip_n,
    na.strings = c("-999", "-999.0", "")
  ) |>
    as_tibble() |>
    rename_with(tolower)
}

# Formato ONI: cabecera "yr_start yr_end" y luego una fila por año
# con 12 valores mensuales. Valor faltante: -99.9.
parse_oni <- function(path) {
  lines <- readLines(path)
  rng <- as.integer(strsplit(trimws(lines[1]), "\\s+")[[1]])
  yrs <- rng[1]:rng[2]
  data_rows <- lines[2:(1 + length(yrs))]
  tab <- read.table(
    text = data_rows, header = FALSE,
    col.names = c("year", month.abb)
  )
  tab[tab == -99.9] <- NA
  tab |>
    as_tibble() |>
    pivot_longer(-year, names_to = "month_abb", values_to = "oni") |>
    mutate(
      month = match(month_abb, month.abb),
      date = as.Date(sprintf("%04d-%02d-01", year, month))
    ) |>
    select(year, month, date, oni)
}

# --------------------------------------------------------------
# 5. Datos de dengue (tabla embebida)
# --------------------------------------------------------------
# Fuente: Anuarios de Morbilidad DGE / SSA y conteos del análisis
# original.
dengue_annual_table <- function() {
  tibble(
    anio = 2013:2024,
    casos = c(
      1685, 977, 7475, 4109, 588, 194,
      760, 302, 90, 604, 13397, 5996
    )
  )
}

# --------------------------------------------------------------
# 6. Procesamiento: series mensual y anual
# --------------------------------------------------------------
process_data <- function(power, oni, dengue) {
  power_daily <- power |>
    mutate(
      date = as.Date(doy - 1, origin = sprintf("%04d-01-01", year)),
      mo = as.integer(format(date, "%m")),
      dy = as.integer(format(date, "%d")),
      t_max = t2m_max, t_med = t2m, t_min = t2m_min,
      hr = rh2m, precip = prectotcorr
    ) |>
    select(date, year, mo, dy, t_max, t_med, t_min, hr, precip)

  # Clasificación ENSO mensual a partir del ONI
  oni_classified <- oni |>
    mutate(enso_state = case_when(
      is.na(oni) ~ NA_character_,
      oni >= 0.5 ~ "Niño",
      oni <= -0.5 ~ "Niña",
      TRUE ~ "Neutral"
    ))

  # Dataset mensual integrado: clima NASA POWER y ONI mensual.
  # `casos` se inicializa NA, lo que sirve como plantilla para
  # incorporar la serie mensual de dengue (Panorama DGE) y/o
  # variables de CONAGUA estacionales en futuras iteraciones.
  monthly <- power_daily |>
    group_by(year, mo) |>
    summarise(
      t_max = mean(t_max, na.rm = TRUE),
      t_med = mean(t_med, na.rm = TRUE),
      t_min = mean(t_min, na.rm = TRUE),
      hr = mean(hr, na.rm = TRUE),
      precip = sum(precip, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(date = as.Date(sprintf("%04d-%02d-01", year, mo))) |>
    left_join(
      oni_classified |> select(year, mo = month, oni, enso_state),
      by = c("year", "mo")
    ) |>
    mutate(casos = NA_real_) |>
    select(
      date, year, mo, casos,
      t_max, t_med, t_min, hr, precip, oni, enso_state
    )

  annual <- power_daily |>
    group_by(year) |>
    summarise(
      t_max = mean(t_max, na.rm = TRUE),
      t_med = mean(t_med, na.rm = TRUE),
      t_min = mean(t_min, na.rm = TRUE),
      hr = mean(hr, na.rm = TRUE),
      precip = sum(precip, na.rm = TRUE),
      .groups = "drop"
    ) |>
    rename(anio = year)

  # Features ENSO anuales derivados de la serie mensual de ONI:
  # múltiples agregados estacionales + conteos + rezago anual.
  oni_features <- oni |>
    group_by(year) |>
    summarise(
      oni_mean = mean(oni, na.rm = TRUE),
      oni_djf = mean(oni[month %in% c(12, 1, 2)], na.rm = TRUE),
      oni_mam = mean(oni[month %in% c(3, 4, 5)], na.rm = TRUE),
      oni_jja = mean(oni[month %in% c(6, 7, 8)], na.rm = TRUE),
      oni_son = mean(oni[month %in% c(9, 10, 11)], na.rm = TRUE),
      oni_max = suppressWarnings(max(oni, na.rm = TRUE)),
      oni_min = suppressWarnings(min(oni, na.rm = TRUE)),
      n_nino_months = sum(oni >= 0.5, na.rm = TRUE),
      n_nina_months = sum(oni <= -0.5, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(year) |>
    mutate(oni_lag1 = lag(oni_mean, 1)) |>
    rename(anio = year)

  annual_full <- dengue |>
    left_join(annual, by = "anio") |>
    left_join(oni_features, by = "anio")

  list(
    daily   = power_daily,
    monthly = monthly,
    annual  = annual_full,
    oni     = oni_classified
  )
}

# --------------------------------------------------------------
# 7. Análisis estadístico
# --------------------------------------------------------------
PREDICTORS <- c(
  "t_max", "t_med", "t_min", "hr", "precip",
  "oni_mean", "oni_djf", "oni_jja", "oni_max",
  "n_nino_months", "oni_lag1"
)
LABELS <- c(
  t_max         = "Temp. máxima (°C)",
  t_med         = "Temp. media (°C)",
  t_min         = "Temp. mínima (°C)",
  hr            = "Humedad relativa (%)",
  precip        = "Precipitación anual (mm)",
  oni_mean      = "ONI medio anual",
  oni_djf       = "ONI invierno (DJF)",
  oni_jja       = "ONI verano (JJA)",
  oni_max       = "ONI máximo anual",
  n_nino_months = "Meses Niño (ONI ≥ 0.5)",
  oni_lag1      = "ONI año previo (lag 1)",
  casos         = "Casos de dengue"
)

run_stats <- function(df_annual) {
  vars <- c("casos", PREDICTORS)

  # Shapiro–Wilk (omite NA, lo que es relevante para oni_lag1)
  shapiro <- map_dfr(vars, function(v) {
    x <- na.omit(df_annual[[v]])
    if (length(x) < 3) {
      return(tibble(variable = v, W = NA_real_, p = NA_real_))
    }
    s <- shapiro.test(x)
    tibble(variable = v, W = unname(s$statistic), p = s$p.value)
  }) |> mutate(stars = sig_stars(p))

  # Spearman vs casos (descarta pares incompletos)
  spearman <- map_dfr(PREDICTORS, function(v) {
    xy <- df_annual[, c("casos", v)]
    xy <- xy[stats::complete.cases(xy), ]
    ct <- suppressWarnings(
      cor.test(xy$casos, xy[[v]], method = "spearman")
    )
    tibble(
      variable = v, rho = unname(ct$estimate),
      p = ct$p.value, stars = sig_stars(ct$p.value)
    )
  })

  # Matriz Spearman (par a par, con manejo de NA)
  cor_mat <- cor(df_annual[, vars],
    method = "spearman",
    use = "pairwise.complete.obs"
  )
  p_mat <- matrix(NA_real_, length(vars), length(vars),
    dimnames = list(vars, vars)
  )
  for (i in seq_along(vars)) {
    for (j in seq_along(vars)) {
      xy <- df_annual[, c(vars[i], vars[j])]
      xy <- xy[stats::complete.cases(xy), ]
      if (nrow(xy) < 3) next
      p_mat[i, j] <- suppressWarnings(
        cor.test(xy[[1]], xy[[2]], method = "spearman")$p.value
      )
    }
  }

  # Regresión Binomial Negativa univariada (na.omit por modelo)
  nb_models <- setNames(
    lapply(PREDICTORS, function(v) {
      glm.nb(
        as.formula(paste("casos ~", v)),
        data = df_annual,
        na.action = na.omit
      )
    }),
    PREDICTORS
  )
  nb_summary <- imap_dfr(nb_models, function(m, name) {
    s <- summary(m)$coefficients
    est <- s[2, 1]
    se <- s[2, 2]
    p <- s[2, 4]
    tibble(
      variable = name,
      IRR = exp(est),
      lo = exp(est - qnorm(0.975) * se),
      hi = exp(est + qnorm(0.975) * se),
      p = p,
      stars = sig_stars(p),
      AIC = AIC(m),
      theta = m$theta
    )
  })

  best <- nb_summary$variable[which.min(nb_summary$AIC)]

  list(
    shapiro    = shapiro,
    spearman   = spearman,
    cor_mat    = cor_mat,
    p_mat      = p_mat,
    nb_models  = nb_models,
    nb_summary = nb_summary,
    best       = best
  )
}

# --------------------------------------------------------------
# 8. Figuras
# --------------------------------------------------------------
make_figures <- function(data, stats) {
  da <- data$annual
  dm <- data$monthly
  oni <- data$oni

  vars_all <- c("casos", PREDICTORS)
  labels_all <- LABELS[vars_all]
  preds_lab <- LABELS[PREDICTORS]

  # ---- Figura 01: Climatología mensual (NASA POWER) -----------
  climatology <- dm |>
    group_by(mo) |>
    summarise(
      t_med = mean(t_med),
      hr = mean(hr),
      precip = mean(precip),
      .groups = "drop"
    ) |>
    pivot_longer(-mo, names_to = "var", values_to = "val") |>
    mutate(var = factor(var,
      levels = c("t_med", "hr", "precip"),
      labels = c(
        "Temp. media (°C)", "Humedad relativa (%)",
        "Precipitación (mm/mes)"
      )
    ))

  p1 <- ggplot(climatology, aes(mo, val)) +
    geom_col(
      data = ~ filter(., var == "Precipitación (mm/mes)"),
      fill = "grey75", colour = "grey30", linewidth = 0.3
    ) +
    geom_line(
      data = ~ filter(., var != "Precipitación (mm/mes)"),
      colour = "grey25", linewidth = 0.6
    ) +
    geom_point(
      data = ~ filter(., var != "Precipitación (mm/mes)"),
      colour = "black", size = 1.6
    ) +
    facet_wrap(~var, scales = "free_y", ncol = 1, strip.position = "left") +
    scale_x_continuous(breaks = 1:12, labels = month.abb) +
    labs(
      x = "Mes", y = NULL,
      title = "Climatología mensual de Campeche (2013–2024)"
    ) +
    theme_sci() +
    theme(
      strip.placement = "outside",
      strip.text.y.left = element_text(angle = 90, face = "bold")
    )
  save_fig(p1, 1, "climatologia_mensual", w = 8, h = 7)

  # ---- Figura 02: Anomalía de 2023 vs climatología ------------
  clim_baseline <- dm |>
    group_by(mo) |>
    summarise(
      t_med_clim = mean(t_med),
      hr_clim = mean(hr),
      precip_clim = mean(precip),
      .groups = "drop"
    )

  anom_2023 <- dm |>
    filter(year == 2023) |>
    left_join(clim_baseline, by = "mo") |>
    transmute(mo,
      `Temp. media (°C)` = t_med - t_med_clim,
      `Humedad relativa (%)` = hr - hr_clim,
      `Precipitación (mm)` = precip - precip_clim
    ) |>
    pivot_longer(-mo, names_to = "var", values_to = "anom")

  p2 <- ggplot(anom_2023, aes(mo, anom, fill = anom > 0)) +
    geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.4) +
    geom_col(colour = "grey20", linewidth = 0.3) +
    facet_wrap(~var, scales = "free_y", ncol = 1, strip.position = "left") +
    scale_x_continuous(breaks = 1:12, labels = month.abb) +
    scale_fill_manual(
      values = c(`TRUE` = "#b2182b", `FALSE` = "#2166ac"),
      guide = "none"
    ) +
    labs(
      x = "Mes", y = "Anomalía vs media 2013–2024",
      title = "Anomalías climáticas de 2023 (año del brote)"
    ) +
    theme_sci() +
    theme(
      strip.placement = "outside",
      strip.text.y.left = element_text(angle = 90, face = "bold")
    )
  save_fig(p2, 2, "anomalias_2023", w = 8, h = 7)

  # ---- Figura 03: Serie temporal anual ------------------------
  df_long <- da |>
    select(anio, all_of(vars_all)) |>
    pivot_longer(-anio, names_to = "variable", values_to = "valor") |>
    mutate(variable = factor(variable,
      levels = vars_all,
      labels = unname(labels_all)
    ))

  p3 <- ggplot(df_long, aes(anio, valor)) +
    geom_line(colour = "grey40", linewidth = 0.5) +
    geom_point(colour = "black", size = 1.6) +
    facet_wrap(~variable,
      scales = "free_y", ncol = 2,
      strip.position = "top"
    ) +
    scale_x_continuous(breaks = da$anio) +
    labs(
      x = "Año", y = NULL,
      title = "Serie temporal anual de dengue y variables ambientales"
    ) +
    theme_sci() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  save_fig(p3, 3, "serie_temporal_anual", w = 9, h = 7)

  # ---- Figura 04: Distribuciones + Shapiro --------------------
  sh <- stats$shapiro |>
    mutate(
      variable = factor(variable,
        levels = vars_all,
        labels = unname(labels_all)
      ),
      label = sprintf("W = %.2f   p = %.3f %s", W, p, stars)
    )

  p4 <- df_long |>
    ggplot(aes(valor)) +
    geom_histogram(
      bins = 6, fill = "grey85", colour = "grey20",
      linewidth = 0.3
    ) +
    geom_text(
      data = sh, aes(x = -Inf, y = Inf, label = label),
      hjust = -0.05, vjust = 1.4, size = 3, inherit.aes = FALSE
    ) +
    facet_wrap(~variable, scales = "free", ncol = 3) +
    labs(
      x = "Valor", y = "Frecuencia",
      title = "Distribución de variables y prueba de normalidad"
    ) +
    theme_sci()
  save_fig(p4, 4, "distribucion_normalidad", w = 9, h = 6)

  # ---- Figura 05: Matriz de correlación Spearman --------------
  cor_long <- as.data.frame(as.table(stats$cor_mat)) |>
    rename(var1 = Var1, var2 = Var2, rho = Freq) |>
    mutate(
      p = as.vector(stats$p_mat),
      stars = sig_stars(p),
      label = ifelse(var1 == var2, "",
        sprintf("%.2f\n%s", rho, stars)
      ),
      var1 = factor(var1,
        levels = vars_all,
        labels = unname(labels_all)
      ),
      var2 = factor(var2,
        levels = rev(vars_all),
        labels = rev(unname(labels_all))
      )
    )

  p5 <- ggplot(cor_long, aes(var1, var2, fill = rho)) +
    geom_tile(colour = "white", linewidth = 0.4) +
    geom_text(aes(label = label), size = 2.8, lineheight = 0.9) +
    scale_fill_gradient2(
      low = "#2166ac", mid = "white", high = "#b2182b",
      midpoint = 0, limits = c(-1, 1),
      name = expression(rho)
    ) +
    coord_fixed() +
    labs(
      title = "Correlaciones de Spearman entre variables",
      x = NULL, y = NULL
    ) +
    theme_sci() +
    theme(
      axis.text.x = element_text(angle = 30, hjust = 1),
      panel.grid = element_blank()
    )
  save_fig(p5, 5, "correlacion_spearman", w = 7.5, h = 6.5)

  # ---- Figura 06: Bivariadas casos vs cada predictor ---------
  bivar <- da |>
    select(anio, casos, all_of(PREDICTORS)) |>
    pivot_longer(-c(anio, casos), names_to = "variable", values_to = "x") |>
    mutate(variable = factor(variable,
      levels = PREDICTORS,
      labels = unname(preds_lab)
    ))

  annot <- stats$spearman |>
    mutate(
      variable = factor(variable,
        levels = PREDICTORS,
        labels = unname(preds_lab)
      ),
      label = sprintf("ρ = %.2f   p = %.3f %s", rho, p, stars)
    )

  p6 <- ggplot(bivar, aes(x, casos)) +
    geom_smooth(
      method = "loess", se = TRUE, colour = "grey45",
      fill = "grey85", linewidth = 0.5,
      formula = y ~ x, span = 1
    ) +
    geom_point(size = 2, colour = "black") +
    geom_text(
      data = annot, aes(x = -Inf, y = Inf, label = label),
      hjust = -0.05, vjust = 1.4, size = 3, inherit.aes = FALSE
    ) +
    facet_wrap(~variable, scales = "free_x", ncol = 3) +
    scale_y_continuous(labels = scales::comma) +
    labs(
      x = NULL, y = "Casos de dengue",
      title = "Relación bivariada: dengue vs cada predictor"
    ) +
    theme_sci()
  save_fig(p6, 6, "bivariado_spearman", w = 9.5, h = 6.5)

  # ---- Figura 07: ONI mensual y casos anuales (paneles) ------
  yr_range <- range(da$anio)
  xlims <- as.Date(c(
    sprintf("%d-01-01", yr_range[1]),
    sprintf("%d-12-31", yr_range[2])
  ))
  oni_sub <- oni |> filter(year >= yr_range[1], year <= yr_range[2])
  cases_dated <- da |>
    mutate(date = as.Date(sprintf("%04d-07-01", anio)))

  state_colours <- c(
    "Niño"    = "#b2182b",
    "Neutral" = "grey55",
    "Niña"    = "#2166ac"
  )

  p7_top <- ggplot(oni_sub, aes(date, oni)) +
    annotate("rect",
      xmin = xlims[1], xmax = xlims[2],
      ymin = 0.5, ymax = Inf, fill = "#fee0d2", alpha = 0.5
    ) +
    annotate("rect",
      xmin = xlims[1], xmax = xlims[2],
      ymin = -Inf, ymax = -0.5, fill = "#deebf7", alpha = 0.5
    ) +
    geom_hline(
      yintercept = c(-0.5, 0, 0.5), colour = "grey60",
      linewidth = 0.25, linetype = "dotted"
    ) +
    geom_line(colour = "grey25", linewidth = 0.5) +
    geom_point(aes(colour = enso_state), size = 1.2) +
    scale_colour_manual(values = state_colours, name = "Estado") +
    scale_x_date(
      limits = xlims, date_breaks = "1 year",
      date_labels = "%Y"
    ) +
    labs(
      x = NULL, y = "ONI (anomalía SST, °C)",
      title = "ENSO (ONI mensual) y casos anuales de dengue"
    ) +
    theme_sci() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      plot.margin = margin(5, 5, 0, 5)
    )

  p7_bot <- ggplot(cases_dated, aes(date, casos)) +
    geom_col(fill = "grey25", width = 200) +
    scale_x_date(
      limits = xlims, date_breaks = "1 year",
      date_labels = "%Y"
    ) +
    scale_y_continuous(labels = scales::comma) +
    labs(x = NULL, y = "Casos de dengue") +
    theme_sci() +
    theme(plot.margin = margin(0, 5, 5, 5))

  p7 <- p7_top / p7_bot + plot_layout(heights = c(1.4, 1))
  save_fig(p7, 7, "ENSO_dengue", w = 9, h = 6)

  # ---- Figura 08: Forest plot IRR (NB univariada) ------------
  fp <- stats$nb_summary |>
    mutate(variable = factor(variable,
      levels = rev(PREDICTORS),
      labels = rev(unname(preds_lab))
    ))

  p8 <- ggplot(fp, aes(IRR, variable)) +
    geom_vline(
      xintercept = 1, linetype = "dashed",
      colour = "grey50", linewidth = 0.3
    ) +
    geom_errorbar(aes(xmin = lo, xmax = hi),
      width = 0.15, colour = "grey30", linewidth = 0.5,
      orientation = "y"
    ) +
    geom_point(size = 2.6) +
    geom_text(aes(x = hi, label = stars),
      hjust = -0.4, vjust = 0.5, size = 4
    ) +
    scale_x_log10() +
    labs(
      x = "Razón de tasas (IRR, escala log)", y = NULL,
      title = "Asociación clima/ENSO–dengue: Binomial Negativa univariada"
    ) +
    theme_sci()
  save_fig(p8, 8, "forest_NB", w = 8, h = 5)

  # ---- Figura 09: Observado vs predicho del mejor modelo -----
  best_name <- stats$best
  best_model <- stats$nb_models[[best_name]]
  da$pred <- predict(best_model, type = "response")
  best_lab <- LABELS[best_name]
  best_row <- stats$nb_summary |> filter(variable == best_name)

  p9 <- ggplot(da, aes(anio)) +
    geom_line(aes(y = casos, colour = "Observado"), linewidth = 0.6) +
    geom_point(aes(y = casos, colour = "Observado"), size = 2.2) +
    geom_line(aes(y = pred, colour = "Predicho (NB)"),
      linewidth = 0.6, linetype = "dashed"
    ) +
    geom_point(aes(y = pred, colour = "Predicho (NB)"),
      size = 2.2, shape = 17
    ) +
    scale_x_continuous(breaks = da$anio) +
    scale_y_continuous(labels = scales::comma) +
    scale_colour_manual(
      values = c(
        "Observado" = "black",
        "Predicho (NB)" = "#b2182b"
      ),
      name = NULL
    ) +
    labs(
      x = "Año", y = "Casos de dengue",
      title = sprintf(
        "Casos observados vs predichos · NB ~ %s",
        best_lab
      )
    ) +
    theme_sci()
  save_fig(p9, 9, "obs_vs_pred_NB", w = 9, h = 4.5)

  # ---- Figura 10: Diagnóstico NB ------------------------------
  diag_df <- tibble(
    fitted   = fitted(best_model),
    rp       = residuals(best_model, type = "pearson"),
    rd       = residuals(best_model, type = "deviance"),
    anio     = da$anio
  )
  g1 <- ggplot(diag_df, aes(fitted, rp)) +
    geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
    geom_point(size = 2) +
    labs(
      x = "Ajustados", y = "Residuos Pearson",
      title = "(a) Residuos vs ajustados"
    ) +
    theme_sci()
  g2 <- ggplot(diag_df, aes(sample = rd)) +
    stat_qq(size = 2) +
    stat_qq_line(linewidth = 0.4) +
    labs(
      x = "Cuantil teórico", y = "Residuos devianza",
      title = "(b) Q–Q de residuos"
    ) +
    theme_sci()
  g3 <- ggplot(diag_df, aes(anio, rp)) +
    geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
    geom_line(colour = "grey40", linewidth = 0.4) +
    geom_point(size = 2) +
    scale_x_continuous(breaks = da$anio) +
    labs(
      x = "Año", y = "Residuos Pearson",
      title = "(c) Residuos en el tiempo"
    ) +
    theme_sci()
  acf_obj <- acf(diag_df$rp, plot = FALSE, lag.max = 5)
  acf_df <- tibble(
    lag = as.vector(acf_obj$lag),
    acf = as.vector(acf_obj$acf)
  )
  ci_acf <- qnorm(0.975) / sqrt(nrow(diag_df))
  bl <- Box.test(diag_df$rp, lag = 3, type = "Ljung-Box")
  bl_lab <- sprintf(
    "Box–Ljung (lag 3): χ² = %.2f, p = %.3f %s",
    bl$statistic, bl$p.value, sig_stars(bl$p.value)
  )
  g4 <- ggplot(acf_df, aes(lag, acf)) +
    geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
    geom_hline(
      yintercept = c(-ci_acf, ci_acf), colour = "grey50",
      linetype = "dashed", linewidth = 0.3
    ) +
    geom_segment(aes(xend = lag, yend = 0), linewidth = 0.6) +
    geom_point(size = 1.6) +
    annotate("text",
      x = Inf, y = Inf, label = bl_lab,
      hjust = 1.05, vjust = 1.4, size = 3
    ) +
    labs(
      x = "Rezago", y = "ACF",
      title = "(d) Autocorrelación de residuos"
    ) +
    theme_sci()

  p10 <- (g1 | g2) / (g3 | g4) +
    plot_annotation(
      title = sprintf("Diagnóstico del modelo NB: casos ~ %s", best_lab),
      theme = theme(plot.title = element_text(face = "bold", size = 12))
    )
  save_fig(p10, 10, "diagnostico_NB", w = 10, h = 7.5)

  # ---- Figura 11: Validación NASA POWER vs valores originales -
  conagua_ref <- tibble(
    anio = 2013:2024,
    t_max_cn = c(32.4, 33.0, 33.6, 33.7, 33.6, 32.9, 33.6, 33.5, 33.4, 33.3, 34.2, 34.0),
    t_med_cn = c(26.8, 27.2, 27.6, 27.8, 27.7, 27.0, 27.6, 27.8, 27.5, 27.3, 28.3, 28.3),
    t_min_cn = c(21.1, 21.3, 21.7, 22.0, 21.8, 21.2, 21.7, 22.1, 21.7, 21.7, 22.5, 22.7),
    precip_cn = c(
      1698.5, 1390.1, 1326.1, 1177.0, 1407.3, 1389.8,
      1303.2, 1779.3, 1218.7, 1423.2, 1132.5, 1348.8
    )
  )

  comp <- da |>
    select(anio, t_max, t_med, t_min, precip) |>
    left_join(conagua_ref, by = "anio") |>
    pivot_longer(-anio) |>
    mutate(
      fuente = ifelse(str_ends(name, "_cn"), "CONAGUA", "NASA POWER"),
      var = str_remove(name, "_cn$"),
      var = factor(var,
        levels = c("t_max", "t_med", "t_min", "precip"),
        labels = c(
          "Temp. máxima (°C)", "Temp. media (°C)",
          "Temp. mínima (°C)", "Precipitación anual (mm)"
        )
      )
    )

  p11 <- ggplot(comp, aes(anio, value, colour = fuente, shape = fuente)) +
    geom_line(linewidth = 0.5) +
    geom_point(size = 2) +
    scale_colour_manual(
      values = c(
        "CONAGUA" = "#1b7837",
        "NASA POWER" = "#762a83"
      ),
      name = NULL
    ) +
    scale_shape_manual(
      values = c("CONAGUA" = 16, "NASA POWER" = 17),
      name = NULL
    ) +
    scale_x_continuous(breaks = da$anio) +
    facet_wrap(~var, scales = "free_y", ncol = 2) +
    labs(
      x = "Año", y = NULL,
      title = "Validación cruzada: NASA POWER vs CONAGUA"
    ) +
    theme_sci() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
  save_fig(p11, 11, "validacion_NASA_vs_CONAGUA", w = 9, h = 6)

  # ---- Figura 12: Calendario ENSO (año × mes) ----------------
  oni_cal <- oni |>
    filter(year >= yr_range[1], year <= yr_range[2]) |>
    mutate(
      month_lab = factor(month.abb[month], levels = month.abb),
      year_lab  = factor(year, levels = yr_range[1]:yr_range[2])
    )

  p12 <- ggplot(oni_cal, aes(month_lab, year_lab, fill = oni)) +
    geom_tile(colour = "white", linewidth = 0.4) +
    geom_text(aes(label = sprintf("%.1f", oni)),
      size = 2.6, colour = "grey15"
    ) +
    scale_fill_gradient2(
      low = "#2166ac", mid = "white", high = "#b2182b",
      midpoint = 0, limits = c(-3, 3),
      name = "ONI"
    ) +
    coord_fixed() +
    labs(
      x = NULL, y = "Año",
      title = "Calendario ENSO: ONI mensual (2013–2024)"
    ) +
    theme_sci() +
    theme(panel.grid = element_blank())
  save_fig(p12, 12, "calendario_ENSO", w = 9, h = 5.5)
}

# --------------------------------------------------------------
# 9. Resumen estadístico con texto
# --------------------------------------------------------------
write_summary <- function(data, stats) {
  da <- data$annual
  path <- file.path(DIRS$out, "resumen_estadistico.txt")
  con <- file(path, "w", encoding = "UTF-8")
  on.exit(close(con))

  w <- function(...) cat(..., "\n", sep = "", file = con)

  w("================================================================")
  w("RESUMEN ESTADÍSTICO · DENGUE–CLIMA EN CAMPECHE (2013–2024)")
  w("Generado: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  w(
    "Fuente clima: NASA POWER (lat ", CONFIG$lat,
    ", lon ", CONFIG$lon, ")"
  )
  w("Fuente ENSO:  NOAA PSL · ONI mensual")
  w("Fuente dengue: Anuarios DGE / SSA")
  w("================================================================\n")

  w("--- Tabla anual integrada ---")
  capture.output(print(da, n = Inf, width = 120), file = con)

  w("\n--- Shapiro–Wilk (normalidad por variable) ---")
  capture.output(print(stats$shapiro, n = Inf), file = con)

  w("\n--- Correlaciones de Spearman vs casos ---")
  capture.output(print(stats$spearman, n = Inf), file = con)

  w("\n--- Regresión Binomial Negativa univariada ---")
  capture.output(
    print(
      stats$nb_summary |>
        select(variable, IRR, lo, hi, p, stars, AIC, theta),
      n = Inf, width = 120
    ),
    file = con
  )

  w("\n--- Mejor modelo por AIC ---")
  w(
    "Predictor: ", stats$best,
    "   AIC = ", round(AIC(stats$nb_models[[stats$best]]), 2)
  )

  w("\nFiguras: ", DIRS$fig)
  w("Datos procesados: ", DIRS$proc)
  log_msg("Resumen guardado: ", path)
}

# --------------------------------------------------------------
# 10. Pipeline principal
# --------------------------------------------------------------
main <- function() {
  log_msg("== Inicio pipeline dengue–clima Campeche ==")

  power_path <- fetch_power()
  oni_path <- fetch_oni()

  log_msg("Parseando insumos…")
  power <- parse_power(power_path)
  oni <- parse_oni(oni_path)
  dengue <- dengue_annual_table()

  log_msg("Procesando series…")
  data <- process_data(power, oni, dengue)

  # Dataset anual integrado (clima + ENSO + casos)
  write.csv(data$annual, file.path(DIRS$proc, "annual_dataset.csv"),
    row.names = FALSE
  )
  # Dataset mensual plantilla — `casos` queda NA hasta que se
  # incorporen casos mensuales (DGE / Panorama Epidemiológico)
  # y, opcionalmente, variables CONAGUA por estación.
  write.csv(data$monthly, file.path(DIRS$proc, "monthly_dataset.csv"),
    row.names = FALSE
  )
  # Series de soporte
  write.csv(data$oni, file.path(DIRS$proc, "oni_monthly.csv"),
    row.names = FALSE
  )

  log_msg("Ejecutando análisis estadístico…")
  stats <- run_stats(data$annual)

  log_msg("Generando figuras…")
  make_figures(data, stats)

  log_msg("Escribiendo resumen…")
  write_summary(data, stats)

  log_msg("== Pipeline completado ==")
  invisible(list(data = data, stats = stats))
}

if (!interactive()) {
  main()
}
