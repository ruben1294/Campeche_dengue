[![DOI](https://zenodo.org/badge/1215439624.svg)](https://doi.org/10.5281/zenodo.22040615)

# Análisis de dengue y clima en Campeche, México

Este repositorio contiene un estudio reproducible de la relación entre variables climáticas y la incidencia de casos de dengue en Campeche, México.

## ¿Qué hace este proyecto?

- Descarga y procesa datos climáticos diarios de NASA POWER.
- Integra series mensuales y anuales de temperatura, humedad relativa, precipitación y ENSO (ONI).
- Combina esos datos con registros anuales de casos de dengue.
- Evalúa asociaciones mediante correlaciones y modelos de regresión binomial negativa.
- Genera figuras, tablas resumidas y resultados estadísticos para apoyar un manuscrito científico.

## Estructura del repositorio

- `Script_Mario_dengue.R`: pipeline principal en R para obtener, limpiar y analizar los datos.
- `data/raw/`: datos crudos de entrada.
- `data/processed/`: bases procesadas listas para análisis.
- `Figuras/`: gráficos generados en formato PNG.
- `outputs/`: resúmenes estadísticos y resultados exportados.

## Cómo ejecutarlo

```bash
Rscript Script_Mario_dengue.R
```
