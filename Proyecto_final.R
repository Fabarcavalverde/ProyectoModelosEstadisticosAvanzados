# ============================================================
# ANÁLISIS DE DATOS - SUPERSTORE
# ============================================================

# Cargar librerías
library(tidyverse)
library(lubridate)
library(janitor)
library(skimr)
library(GGally)
library(corrplot)
library(scales)
library(viridis)
library(knitr)


# ============================================================
# FUNCIONES PARA MEJORAR LA SALIDA EN CONSOLA
# ============================================================

titulo <- function(texto) {
  cat("\n")
  cat("══════════════════════════════════════════════════════\n")
  cat(" ", texto, "\n")
  cat("══════════════════════════════════════════════════════\n\n")
}

subtitulo <- function(texto) {
  cat("\n")
  cat("──────────────────────────────────────────────────────\n")
  cat(" ", texto, "\n")
  cat("──────────────────────────────────────────────────────\n\n")
}

ok <- function(texto) {
  cat("✓ ", texto, "\n", sep = "")
}


# ============================================================
# CARGAR DATOS
# ============================================================

titulo("ANÁLISIS DE DATOS - SUPERSTORE")

datos <- Sample_Superstore

ok("Datos cargados correctamente")


# ============================================================
# REVISIÓN INICIAL DE LOS DATOS
# ============================================================

titulo("1. REVISIÓN INICIAL DE LOS DATOS")

cat("Dimensiones del dataset:\n")
cat("  Filas:    ", nrow(datos), "\n", sep = "")
cat("  Columnas: ", ncol(datos), "\n\n", sep = "")

subtitulo("Nombres de las variables")
print(names(datos))

subtitulo("Primeras filas")
print(head(datos))

subtitulo("Últimas filas")
print(tail(datos))

subtitulo("Estructura de los datos")
str(datos)

subtitulo("Resumen estadístico")
print(summary(datos))


# ============================================================
# VALORES FALTANTES
# ============================================================

titulo("2. CALIDAD DE LOS DATOS")

subtitulo("Valores faltantes por variable")

faltantes <- colSums(is.na(datos))

if (sum(faltantes) == 0) {
  ok("No se encontraron valores faltantes")
} else {
  print(faltantes[faltantes > 0])
}


# ============================================================
# LIMPIAR NOMBRES
# ============================================================

subtitulo("Limpieza de nombres de variables")

datos <- datos %>%
  clean_names()

ok("Nombres de variables limpiados")

cat("\nNuevos nombres:\n")
print(names(datos))


# ============================================================
# CAMBIAR FECHAS
# ============================================================

subtitulo("Conversión de fechas")

datos <- datos %>%
  mutate(
    order_date = mdy(order_date),
    ship_date = mdy(ship_date)
  )

ok("Fechas convertidas correctamente")

cat("\nEstructura de las fechas:\n")
cat("  order_date: ")
print(class(datos$order_date))

cat("  ship_date:  ")
print(class(datos$ship_date))


# ============================================================
# REVISAR FALTANTES Y ELIMINAR FILAS
# ============================================================

subtitulo("Revisión de valores faltantes")

faltantes <- colSums(is.na(datos))

if (sum(faltantes) == 0) {
  ok("No hay valores faltantes")
} else {
  print(faltantes[faltantes > 0])
}

filas_antes <- nrow(datos)

datos <- datos %>%
  drop_na(sales, quantity, discount, profit)

filas_despues <- nrow(datos)

cat("\nFilas antes de eliminar faltantes: ", 
    format(filas_antes, big.mark = ","), "\n", sep = "")

cat("Filas después:                    ", 
    format(filas_despues, big.mark = ","), "\n", sep = "")

cat("Filas eliminadas:                 ", 
    format(filas_antes - filas_despues, big.mark = ","), "\n", sep = "")


# ============================================================
# DUPLICADOS
# ============================================================

subtitulo("Registros duplicados")

duplicados <- sum(duplicated(datos))

cat("Registros duplicados: ", 
    format(duplicados, big.mark = ","), "\n", sep = "")


# ============================================================
# VARIABLES CATEGÓRICAS
# ============================================================

titulo("3. VARIABLES CATEGÓRICAS")

subtitulo("Ship Mode")
print(table(datos$ship_mode))

subtitulo("Segment")
print(table(datos$segment))

subtitulo("Region")
print(table(datos$region))

subtitulo("Category")
print(table(datos$category))


# ============================================================
# CREAR VARIABLES DE FECHA
# ============================================================

titulo("4. CREACIÓN DE VARIABLES DE FECHA")

datos <- datos %>%
  mutate(
    year = year(order_date),
    month = month(order_date),
    month_name = month(order_date, label = TRUE),
    ship_days = as.numeric(ship_date - order_date)
  )

ok("Variables de fecha creadas")

cat("\nNuevas variables:\n")
cat("  • year\n")
cat("  • month\n")
cat("  • month_name\n")
cat("  • ship_days\n")


# ============================================================
# INFORMACIÓN GENERAL DE VENTAS
# ============================================================

titulo("5. INFORMACIÓN GENERAL DE VENTAS")

resumen_general <- datos %>%
  summarise(
    ventas_totales = sum(sales),
    ganancia_total = sum(profit),
    unidades = sum(quantity),
    venta_promedio = mean(sales),
    ganancia_promedio = mean(profit)
  )

cat("Ventas totales:    $", 
    format(round(resumen_general$ventas_totales, 2), 
           big.mark = ","), "\n", sep = "")

cat("Ganancia total:    $", 
    format(round(resumen_general$ganancia_total, 2), 
           big.mark = ","), "\n", sep = "")

cat("Unidades vendidas: ", 
    format(resumen_general$unidades, big.mark = ","), "\n", sep = "")

cat("Venta promedio:    $", 
    format(round(resumen_general$venta_promedio, 2), 
           big.mark = ","), "\n", sep = "")

cat("Ganancia promedio: $", 
    format(round(resumen_general$ganancia_promedio, 2), 
           big.mark = ","), "\n", sep = "")


# ============================================================
# VENTAS Y GANANCIAS POR CATEGORÍA
# ============================================================

titulo("6. VENTAS Y GANANCIAS POR CATEGORÍA")

ventas_categoria <- datos %>%
  group_by(category) %>%
  summarise(
    ventas = sum(sales),
    ganancia = sum(profit),
    cantidad = sum(quantity),
    .groups = "drop"
  ) %>%
  arrange(desc(ventas))

print(
  kable(
    ventas_categoria,
    digits = 2,
    col.names = c("Categoría", "Ventas", "Ganancia", "Unidades")
  )
)


# ============================================================
# GRÁFICO DE VENTAS POR CATEGORÍA
# ============================================================

subtitulo("Gráfico: Ventas por categoría")

ggplot(datos, aes(x = category, y = sales, fill = category)) +
  stat_summary(fun = sum, geom = "bar") +
  labs(
    title = "Ventas por categoría",
    x = "Categoría",
    y = "Ventas"
  ) +
  theme_minimal()


# ============================================================
# GRÁFICO DE GANANCIAS POR CATEGORÍA
# ============================================================

subtitulo("Gráfico: Ganancias por categoría")

ggplot(datos, aes(x = category, y = profit, fill = category)) +
  stat_summary(fun = sum, geom = "bar") +
  labs(
    title = "Ganancia por categoría",
    x = "Categoría",
    y = "Ganancia"
  ) +
  theme_minimal()


# ============================================================
# VENTAS POR REGIÓN
# ============================================================

titulo("7. ANÁLISIS POR REGIÓN")

ventas_region <- datos %>%
  group_by(region) %>%
  summarise(
    ventas = sum(sales),
    ganancia = sum(profit),
    .groups = "drop"
  ) %>%
  arrange(desc(ventas))

print(
  kable(
    ventas_region,
    digits = 2,
    col.names = c("Región", "Ventas", "Ganancia")
  )
)


# ============================================================
# GRÁFICO DE VENTAS POR REGIÓN
# ============================================================

subtitulo("Gráfico: Ventas por región")

ggplot(datos, aes(x = region, y = sales, fill = region)) +
  stat_summary(fun = sum, geom = "bar") +
  labs(
    title = "Ventas por región",
    x = "Región",
    y = "Ventas"
  ) +
  theme_minimal()


# ============================================================
# VENTAS POR AÑO
# ============================================================

titulo("8. ANÁLISIS TEMPORAL")

ventas_anuales <- datos %>%
  group_by(year) %>%
  summarise(
    ventas = sum(sales),
    ganancia = sum(profit),
    .groups = "drop"
  )

print(
  kable(
    ventas_anuales,
    digits = 2,
    col.names = c("Año", "Ventas", "Ganancia")
  )
)


# ============================================================
# GRÁFICO DE VENTAS POR AÑO
# ============================================================

subtitulo("Gráfico: Ventas por año")

ggplot(datos, aes(x = year, y = sales)) +
  stat_summary(fun = sum, geom = "line", linewidth = 1.2) +
  geom_point() +
  labs(
    title = "Ventas por año",
    x = "Año",
    y = "Ventas"
  ) +
  theme_minimal()


# ============================================================
# VENTAS POR MES
# ============================================================

subtitulo("Ventas mensuales")

ventas_mes <- datos %>%
  group_by(year, month) %>%
  summarise(
    ventas = sum(sales),
    ganancia = sum(profit),
    .groups = "drop"
  )

print(
  kable(
    ventas_mes,
    digits = 2,
    col.names = c("Año", "Mes", "Ventas", "Ganancia")
  )
)


# ============================================================
# GRÁFICO DE VENTAS MENSUALES
# ============================================================

subtitulo("Gráfico: Ventas mensuales")

ggplot(ventas_mes, aes(x = month, y = ventas, color = factor(year))) +
  geom_line() +
  geom_point() +
  labs(
    title = "Ventas mensuales",
    x = "Mes",
    y = "Ventas",
    color = "Año"
  ) +
  theme_minimal()


# ============================================================
# TOP 10 PRODUCTOS CON MAYORES VENTAS
# ============================================================

titulo("9. PRODUCTOS DESTACADOS")

subtitulo("Top 10 productos con mayores ventas")

top_ventas <- datos %>%
  group_by(product_name) %>%
  summarise(
    ventas = sum(sales, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(ventas)) %>%
  head(10)

print(
  kable(
    top_ventas,
    digits = 2,
    col.names = c("Producto", "Ventas")
  )
)


# ============================================================
# GRÁFICO TOP 10 VENTAS
# ============================================================

ggplot(top_ventas, aes(x = reorder(product_name, ventas), y = ventas)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 10 productos con mayores ventas",
    x = "Producto",
    y = "Ventas"
  ) +
  theme_minimal()


# ============================================================
# TOP 10 PRODUCTOS CON MAYORES GANANCIAS
# ============================================================

subtitulo("Top 10 productos con mayores ganancias")

top_ganancias <- datos %>%
  group_by(product_name) %>%
  summarise(
    ganancia = sum(profit, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(ganancia)) %>%
  head(10)

print(
  kable(
    top_ganancias,
    digits = 2,
    col.names = c("Producto", "Ganancia")
  )
)


# ============================================================
# GRÁFICO TOP 10 GANANCIAS
# ============================================================

ggplot(top_ganancias, aes(x = reorder(product_name, ganancia), y = ganancia)) +
  geom_col(fill = "seagreen") +
  coord_flip() +
  labs(
    title = "Top 10 productos con mayores ganancias",
    x = "Producto",
    y = "Ganancia"
  ) +
  theme_minimal()


# ============================================================
# TOP 10 PRODUCTOS CON MAYORES PÉRDIDAS
# ============================================================

subtitulo("Top 10 productos con mayores pérdidas")

top_perdidas <- datos %>%
  group_by(product_name) %>%
  summarise(
    ganancia = sum(profit, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(ganancia) %>%
  head(10)

print(
  kable(
    top_perdidas,
    digits = 2,
    col.names = c("Producto", "Ganancia / Pérdida")
  )
)


# ============================================================
# GRÁFICO TOP 10 PÉRDIDAS
# ============================================================

ggplot(top_perdidas, aes(x = reorder(product_name, ganancia), y = ganancia)) +
  geom_col(fill = "firebrick") +
  coord_flip() +
  labs(
    title = "Top 10 productos con mayores pérdidas",
    x = "Producto",
    y = "Pérdida"
  ) +
  theme_minimal()


# ============================================================
# CORRELACIÓN
# ============================================================

titulo("10. ANÁLISIS DE CORRELACIÓN")

correlacion <- cor(
  datos[, c("sales", "quantity", "discount", "profit")]
)

subtitulo("Matriz de correlación")

print(
  round(correlacion, 2)
)

subtitulo("Gráfico de correlación")

corrplot(
  correlacion,
  method = "color",
  addCoef.col = "black"
)

