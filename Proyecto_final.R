# Cargar librerías
library(tidyverse)
library(lubridate)
library(janitor)
library(skimr)
library(GGally)
library(corrplot)
library(scales)
library(viridis)

# Cargar datos
datos <- Sample...Superstore

# Revisar los datos
dim(datos)
names(datos)
head(datos)
tail(datos)
str(datos)
summary(datos)

# Valores faltantes
colSums(is.na(datos))

# Limpiar nombres
datos <- datos %>%
  clean_names()

names(datos)

# Cambiar fechas
datos <- datos %>%
  mutate(
    order_date = mdy(order_date),
    ship_date = mdy(ship_date)
  )

str(datos)

# Revisar los faltantes
colSums(is.na(datos))

# Eliminar filas con datos faltantes 
datos <- datos %>%
  drop_na(sales, quantity, discount, profit)

dim(datos)

# Revisar duplicados
sum(duplicated(datos))

# Variables categóricas
table(datos$ship_mode)
table(datos$segment)
table(datos$region)
table(datos$category)

# Crear variables de fecha
datos <- datos %>%
  mutate(
    year = year(order_date),
    month = month(order_date),
    month_name = month(order_date, label = TRUE),
    ship_days = as.numeric(ship_date - order_date)
  )

str(datos)

#Informacion general de ventas

datos %>%
  summarise(
    ventas_totales = sum(sales),
    ganancia_total = sum(profit),
    unidades = sum(quantity),
    venta_promedio = mean(sales),
    ganancia_promedio = mean(profit)
  )


# Ventas y ganancias por categoria

datos %>%
  group_by(category) %>%
  summarise(
    ventas = sum(sales),
    ganancia = sum(profit),
    cantidad = sum(quantity)
  )


# Grafico de ventas por categoria

ggplot(datos, aes(x = category, y = sales, fill = category)) +
  stat_summary(fun = sum, geom = "bar") +
  labs(
    title = "Ventas por categoría",
    x = "Categoría",
    y = "Ventas"
  ) +
  theme_minimal()


# Grafico de ganancias por categoria

ggplot(datos, aes(x = category, y = profit, fill = category)) +
  stat_summary(fun = sum, geom = "bar") +
  labs(
    title = "Ganancia por categoría",
    x = "Categoría",
    y = "Ganancia"
  ) +
  theme_minimal()


# Ventas por region

datos %>%
  group_by(region) %>%
  summarise(
    ventas = sum(sales),
    ganancia = sum(profit)
  )


# Grafico de ventas por region

ggplot(datos, aes(x = region, y = sales, fill = region)) +
  stat_summary(fun = sum, geom = "bar") +
  labs(
    title = "Ventas por región",
    x = "Región",
    y = "Ventas"
  ) +
  theme_minimal()


# Ventas por año

datos %>%
  group_by(year) %>%
  summarise(
    ventas = sum(sales),
    ganancia = sum(profit)
  )


ggplot(datos, aes(x = year, y = sales)) +
  stat_summary(fun = sum, geom = "line", linewidth = 1.2) +
  geom_point() +
  labs(
    title = "Ventas por año",
    x = "Año",
    y = "Ventas"
  ) +
  theme_minimal()


# Ventas por mes

ventas_mes <- datos %>%
  group_by(year, month) %>%
  summarise(
    ventas = sum(sales),
    ganancia = sum(profit),
    .groups = "drop"
  )

ventas_mes


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


# Productos con mayores ventas

top_ventas <- datos %>%
  group_by(product_name) %>%
  summarise(
    ventas = sum(sales, na.rm = TRUE)
  ) %>%
  arrange(desc(ventas)) %>%
  head(10)

top_ventas


ggplot(top_ventas, aes(x = reorder(product_name, ventas), y = ventas)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 10 productos con mayores ventas",
    x = "Producto",
    y = "Ventas"
  ) +
  theme_minimal()


# Productos con mayores ganancias

top_ganancias <- datos %>%
  group_by(product_name) %>%
  summarise(
    ganancia = sum(profit, na.rm = TRUE)
  ) %>%
  arrange(desc(ganancia)) %>%
  head(10)

top_ganancias


ggplot(top_ganancias, aes(x = reorder(product_name, ganancia), y = ganancia)) +
  geom_col(fill = "seagreen") +
  coord_flip() +
  labs(
    title = "Top 10 productos con mayores ganancias",
    x = "Producto",
    y = "Ganancia"
  ) +
  theme_minimal()


# Productos con mayores perdidas

top_perdidas <- datos %>%
  group_by(product_name) %>%
  summarise(
    ganancia = sum(profit, na.rm = TRUE)
  ) %>%
  arrange(ganancia) %>%
  head(10)

top_perdidas


ggplot(top_perdidas, aes(x = reorder(product_name, ganancia), y = ganancia)) +
  geom_col(fill = "firebrick") +
  coord_flip() +
  labs(
    title = "Top 10 productos con mayores pérdidas",
    x = "Producto",
    y = "Pérdida"
  ) +
  theme_minimal()



# Correlacion

correlacion <- cor(
  datos[, c("sales", "quantity", "discount", "profit")]
)

corrplot(
  correlacion,
  method = "color",
  addCoef.col = "black"
)



