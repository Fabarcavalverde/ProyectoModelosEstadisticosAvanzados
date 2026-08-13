# 📊 Análisis de Ventas y Rentabilidad — Sample Superstore

> Proyecto universitario de análisis exploratorio y regresión lineal múltiple en **R** para identificar los factores asociados con la ganancia de una empresa minorista.

![R](https://img.shields.io/badge/R-4.5+-276DC3?logo=r&logoColor=white)
![Análisis de datos](https://img.shields.io/badge/Enfoque-Análisis%20de%20datos-16A085)
![Estado](https://img.shields.io/badge/Estado-Completado-2ECC71)

---

## 🎯 Objetivo

Analizar los datos de **Sample Superstore** para comprender el comportamiento de las ventas y ganancias, detectar oportunidades de mejora y construir un modelo que estime la ganancia a partir de:

- Monto de ventas
- Cantidad de unidades
- Descuento aplicado
- Días de envío

El proyecto combina análisis exploratorio de datos (EDA), visualización, correlación, regresión lineal múltiple y diagnóstico estadístico del modelo.

## 📁 Dataset

La base contiene información de una tienda minorista ficticia de Estados Unidos entre 2014 y 2017.

| Elemento | Detalle |
|---|---:|
| Registros | 9.994 |
| Variables originales | 21 |
| Periodo | 2014–2017 |
| Variable objetivo | `profit` (ganancia) |
| Variables numéricas clave | `sales`, `quantity`, `discount`, `profit` |

También incluye información de pedidos, clientes, segmentos, regiones, categorías, productos y fechas de envío.

## 🧰 Tecnologías y librerías

- **R**
- `tidyverse`: transformación de datos y gráficos.
- `lubridate`: conversión y manipulación de fechas.
- `janitor`: normalización de nombres de columnas.
- `GGally` y `corrplot`: exploración de relaciones y correlaciones.
- `car`: diagnóstico de multicolinealidad mediante VIF.
- `gvlma`: evaluación de supuestos del modelo lineal.
- `sandwich` y `lmtest`: errores estándar robustos HC3.
- `jtools`: comparación visual de coeficientes de modelos.

## 🔎 Metodología

### 1. Revisión y preparación de datos

1. Se revisaron dimensiones, tipos de datos, valores descriptivos y estructura general.
2. Se estandarizaron los nombres de variables con `clean_names()`.
3. Las fechas de pedido y envío se convirtieron de texto a formato fecha usando `mdy()`.
4. Se verificaron valores faltantes y duplicados.
5. Se crearon variables derivadas:
   - `year`: año del pedido.
   - `month`: mes del pedido.
   - `month_name`: nombre ordenado del mes.
   - `ship_days`: diferencia, en días, entre pedido y envío.

**Calidad de datos:** no se encontraron valores faltantes en las variables críticas ni filas duplicadas. Por ello, se mantuvieron los 9.994 registros en el análisis.

### 2. Análisis exploratorio

Se estudió el desempeño por categoría, región, año, mes y producto. Además, se elaboraron gráficos de barras y líneas para comunicar los resultados de forma visual.

### 3. Correlación y regresión

Se exploraron las relaciones entre ganancia, ventas, cantidad, descuento y días de envío. Luego se evaluaron tres modelos:

```r
# Modelo completo
profit ~ sales + quantity + discount + ship_days

# Modelo reducido por selección backward
profit ~ sales + quantity + discount

# Modelo con interacción
profit ~ sales * discount + quantity
```

El término `sales * discount` incluye los efectos individuales de ventas y descuento, junto con su interacción. Esto permite evaluar si el efecto de las ventas sobre la ganancia cambia al aplicar distintos descuentos.

## 📈 Hallazgos principales

### Resumen general

| Indicador | Resultado aproximado |
|---|---:|
| Ventas totales | 2.297.201 |
| Ganancia total | 286.397 |
| Unidades vendidas | 37.873 |
| Venta promedio por registro | 230,00 |
| Ganancia promedio por registro | 28,70 |

### Categorías

| Categoría | Ventas | Ganancia | Unidades |
|---|---:|---:|---:|
| Furniture | 742.000 | 18.451 | 8.028 |
| Office Supplies | 719.047 | 122.491 | 22.906 |
| Technology | 836.154 | 145.455 | 6.939 |

- **Technology** lidera tanto en ventas como en ganancia.
- **Office Supplies** destaca por su alta ganancia y volumen de unidades.
- **Furniture** presenta una alerta: genera ventas considerables, pero una utilidad significativamente menor.

### Regiones

| Región | Ventas | Ganancia |
|---|---:|---:|
| Central | 501.240 | 39.706 |
| East | 678.781 | 91.523 |
| South | 391.722 | 46.749 |
| West | 725.458 | 108.418 |

**West** es la región con mayor desempeño total, seguida de **East**.

### Evolución anual

| Año | Ventas | Ganancia |
|---|---:|---:|
| 2014 | 484.247 | 49.544 |
| 2015 | 470.533 | 61.619 |
| 2016 | 609.206 | 81.795 |
| 2017 | 733.215 | 93.439 |

La empresa muestra una tendencia positiva de ventas y ganancias a partir de 2015, con su mejor desempeño en 2017.

### Productos relevantes

- **Mayor venta y mayor ganancia:** `Canon imageCLASS 2200 Advanced Copier`, con aproximadamente 61.600 en ventas y 25.200 en ganancia.
- **Mayor pérdida:** `Cubify CubeX 3D Printer Double Head Print`, con una pérdida aproximada de 8.880.

Este contraste evidencia que facturar más no siempre equivale a ser más rentable; es necesario evaluar el margen de cada producto.

## 📐 Resultados del modelo

El modelo con interacción fue el de mejor desempeño entre los modelos evaluados.

```r
profit ~ sales * discount + quantity
```

| Métrica | Modelo completo | Modelo final | Modelo con interacción |
|---|---:|---:|---:|
| R² ajustado | 0,2724 | 0,2725 | **0,7176** |
| AIC | 134.253,3 | 134.251,3 | **124.795,1** |
| BIC | 134.296,6 | 134.287,4 | **124.838,4** |

Principales interpretaciones:

- Las **ventas** se asocian positivamente con la ganancia.
- Los **descuentos** reducen la rentabilidad esperada.
- La interacción `sales:discount` es negativa y significativa: a mayor descuento, menor es el efecto positivo de una venta sobre la ganancia.
- La variable `ship_days` no fue significativa y se eliminó del modelo reducido.
- Los VIF fueron cercanos a 1, por lo que no se detectó multicolinealidad problemática.

### Predicción de ejemplo

Para una venta de 500, con 3 unidades y 10% de descuento:

| Resultado | Valor |
|---|---:|
| Ganancia estimada | 116,51 |
| Límite inferior del intervalo de predicción al 95% | -127,53 |
| Límite superior del intervalo de predicción al 95% | 360,55 |

El intervalo amplio muestra que las ganancias individuales presentan variabilidad considerable; la predicción debe utilizarse como apoyo a la decisión, no como garantía exacta.

## ⚠️ Limitaciones del modelo

La prueba global `gvlma` indicó incumplimiento de algunos supuestos de regresión lineal, incluyendo normalidad de residuos y homocedasticidad. Por esta razón:

- Los resultados se interpretan como asociaciones, no como causalidad.
- Se calcularon errores estándar robustos HC3 para fortalecer la inferencia frente a heterocedasticidad.
- Se recomienda complementar el modelo con variables de costo, margen, subcategoría, cliente y logística si estuvieran disponibles.

## 💡 Recomendaciones de negocio

1. Revisar la estrategia de descuentos, especialmente en pedidos de alto valor.
2. Analizar costos, precios y condiciones comerciales de **Furniture**.
3. Auditar productos con pérdidas recurrentes antes de continuar impulsando sus ventas.
4. Identificar prácticas replicables de las regiones **West** y **East**.
5. Priorizar indicadores de ganancia y margen, no únicamente volumen de ventas.

## ▶️ Ejecución

1. Descargar o clonar este repositorio.
2. Abrir `Proyecto_final.R` en RStudio.
3. Instalar las librerías necesarias si no están disponibles:

```r
install.packages(c(
  "tidyverse", "lubridate", "janitor", "skimr", "GGally",
  "corrplot", "scales", "viridis", "car", "jtools", "gvlma",
  "sandwich", "lmtest"
))
```

4. Cargar el archivo `Sample - Superstore.csv` y asignarlo al objeto `Sample_Superstore`.
5. Ejecutar el script de arriba hacia abajo.

> Nota: en el script, la primera asignación `datos <- Sample...Superstore` debe sustituirse por `datos <- Sample_Superstore`, ya que ese es el objeto con el que se importó correctamente el archivo CSV.

## 👤 Integrantes

**Roberto Montoya Leiva**  
**Sharon Obando Gómez**  
**Fiorella Abarca Valverde**  
**Monica Mendoza Morales**  
**Roberto Coto Guevara**  
Proyecto académico — Análisis de Datos en R

---

Si este análisis te resultó útil, puedes dejar una ⭐ en el repositorio.
