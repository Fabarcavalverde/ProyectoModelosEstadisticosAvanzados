

library(tidyverse)
library(GGally)
library(car)
library(jtools)
library(gvlma)
library(sandwich)
library(lmtest)

# ============================================================
#  ANÁLISIS DE REGRESIÓN
# ============================================================

titulo("11. ANÁLISIS DE REGRESIÓN")


# ============================================================
#  PREPARACIÓN DE LOS DATOS
# ============================================================

subtitulo("Preparación de datos para regresión")

datos_regresion <- datos %>%
  select(profit, sales, quantity, discount, ship_days) %>%
  drop_na()

cat("Observaciones utilizadas: ",
    format(nrow(datos_regresion), big.mark = ","), "\n", sep = "")

cat("Variables utilizadas: ",
    paste(names(datos_regresion), collapse = ", "), "\n", sep = "")

ok("Datos preparados para el análisis")


# ============================================================
# EXPLORACIÓN Y CORRELACIONES
# ============================================================

subtitulo("Estructura de los datos")

str(datos_regresion)

subtitulo("Resumen estadístico")

print(summary(datos_regresion))


subtitulo("Matriz de correlaciones")

matriz_cor <- round(cor(datos_regresion), 3)

print(
  kable(
    matriz_cor
  )
)


subtitulo("Relaciones entre variables")

ggpairs(datos_regresion)


# ============================================================
#  MODELO COMPLETO
# ============================================================

titulo("11.3 MODELO DE REGRESIÓN COMPLETO")

modelo_completo <- lm(
  profit ~ sales + quantity + discount + ship_days,
  data = datos_regresion
)

subtitulo("Resumen del modelo completo")

print(summary(modelo_completo))


# ============================================================
#  MULTICOLINEALIDAD
# ============================================================

subtitulo("Evaluación de multicolinealidad - VIF")

vif_resultado <- vif(modelo_completo)

print(
  round(vif_resultado, 3)
)


# ============================================================
#  SELECCIÓN DEL MODELO FINAL
# ============================================================

titulo("11.5 SELECCIÓN DEL MODELO")

modelo_final <- step(
  modelo_completo,
  direction = "backward"
)

subtitulo("Resumen del modelo final")

print(summary(modelo_final))


# ============================================================
#  MODELO CON INTERACCIÓN
# ============================================================

subtitulo("Modelo con interacción Sales × Discount")

modelo_interaccion <- lm(
  profit ~ sales * discount + quantity,
  data = datos_regresion
)

print(summary(modelo_interaccion))


# ============================================================
#  COMPARACIÓN DE MODELOS
# ============================================================

titulo("11.7 COMPARACIÓN DE MODELOS")

cat("Modelos evaluados:\n")
cat("  • Modelo completo\n")
cat("  • Modelo final\n")
cat("  • Modelo con interacción\n\n")


subtitulo("AIC")

aic_modelos <- AIC(
  modelo_completo,
  modelo_final,
  modelo_interaccion
)

print(
  kable(
    aic_modelos,
    digits = 2,
    col.names = c(
      "Modelo",
      "Grados de libertad",
      "AIC"
    )
  )
)


subtitulo("BIC")

bic_modelos <- BIC(
  modelo_completo,
  modelo_final,
  modelo_interaccion
)

print(
  kable(
    bic_modelos,
    digits = 2,
    col.names = c(
      "Modelo",
      "Grados de libertad",
      "BIC"
    )
  )
)


subtitulo("Comparación visual de coeficientes")

plot_summs(
  modelo_completo,
  modelo_final,
  modelo_interaccion,
  model.names = c(
    "Completo",
    "Final",
    "Interacción"
  )
)


# ============================================================
#  EVALUACIÓN DEL MODELO
# ============================================================

titulo("11.8 EVALUACIÓN DEL MODELO")

subtitulo("Diagnóstico GVLMA")

print(
  summary(gvlma(modelo_interaccion))
)


# ============================================================
# GRÁFICOS DE DIAGNÓSTICO
# ============================================================

subtitulo("Gráficos de diagnóstico")

par(mfrow = c(2, 2))

plot(modelo_interaccion)

par(mfrow = c(1, 1))


# ============================================================
# RESIDUOS CONTRA VALORES AJUSTADOS
# ============================================================

subtitulo("Residuos contra valores ajustados")

datos_residuos <- data.frame(
  ajustados = fitted(modelo_interaccion),
  residuos = residuals(modelo_interaccion)
)

ggplot(
  datos_residuos,
  aes(x = ajustados, y = residuos)
) +
  geom_point(
    alpha = 0.4,
    color = "steelblue"
  ) +
  geom_hline(
    yintercept = 0,
    color = "red",
    linewidth = 1
  ) +
  labs(
    title = "Residuos contra valores ajustados",
    x = "Valores ajustados",
    y = "Residuos"
  ) +
  theme_minimal()


# ============================================================
#  DISTRIBUCIÓN DE LOS RESIDUOS
# ============================================================

subtitulo("Distribución de los residuos")

ggplot(
  data.frame(
    residuos = residuals(modelo_interaccion)
  ),
  aes(x = residuos)
) +
  geom_histogram(
    bins = 60,
    fill = "steelblue",
    color = "white"
  ) +
  coord_cartesian(
    xlim = c(-500, 500)
  ) +
  labs(
    title = "Distribución de los residuos",
    x = "Residuos",
    y = "Frecuencia"
  ) +
  theme_minimal()


# ============================================================
#  ERRORES ESTÁNDAR ROBUSTOS
# ============================================================

titulo("11.12 ERRORES ESTÁNDAR ROBUSTOS")

subtitulo("Prueba con errores robustos HC3")

resultado_hc3 <- coeftest(
  modelo_interaccion,
  vcov = vcovHC(
    modelo_interaccion,
    type = "HC3"
  )
)

print(resultado_hc3)


# ============================================================
# PREDICCIÓN
# ============================================================

titulo("11.13 PREDICCIÓN")

subtitulo("Nueva venta")

nueva_venta <- data.frame(
  sales = 500,
  quantity = 3,
  discount = 0.10
)

print(
  kable(
    nueva_venta,
    digits = 2,
    col.names = c(
      "Ventas",
      "Cantidad",
      "Descuento"
    )
  )
)


subtitulo("Ganancia predicha")

prediccion <- predict(
  modelo_interaccion,
  newdata = nueva_venta
)

cat(
  "Ganancia estimada: $",
  format(
    round(prediccion, 2),
    big.mark = ","
  ),
  "\n",
  sep = ""
)


# ============================================================
# INTERVALO DE PREDICCIÓN
# ============================================================

subtitulo("Intervalo de predicción al 95%")

prediccion_intervalo <- predict(
  modelo_interaccion,
  newdata = nueva_venta,
  interval = "prediction",
  level = 0.95
)

print(
  kable(
    round(
      as.data.frame(prediccion_intervalo),
      2
    ),
    col.names = c(
      "Predicción",
      "Límite inferior",
      "Límite superior"
    )
  )
)


