#----------------------------------ANOVA---------------------------------------#
#1. Librerias.

library(tidyverse)
library(car)

#2. Cargar y preparar los datos ------------------------------------------------

datos <- Sample_Superstore
head(datos) # Vista previa
str(datos) # Estructura

#2.1. Cambiar la variable 'Region' por factor.

# Se cambia 'Region' a factor ya que es una variable categorica que nos esta definiendo
# los lugares de compras (Central, East, South, West).

datos$Region <- as.factor(datos$Region)
class(datos$Region) # Saber si se realizo el cambio

#3. Analisis Exploratorio ------------------------------------------------------

summary(datos$Profit) # Resumen estadistico
table(datos$Region) # Cuantas observaciones tiene la variable

#3.1. Calcular la media de ganancia - Profit por Region.

datos %>% 
  group_by(Region) %>% 
  summarise(
    media = mean(Profit, na.rm = TRUE),
  )

# Podemos observar que la region West presenta la mayor ganacia promedio, seguida por
# East, mientras que South mantiene un nivel intermedio y la region Central nos muestra
# la menor ganancia promedio, se podria decir que si existen diferencias entre las regiones

#4. Verificacion de los supuestos ----------------------------------------------

# QQ-plot por Region
ggplot(datos, aes(sample = Profit)) +
  stat_qq() + stat_qq_line() +
  facet_wrap(~ Region) +
  theme_minimal()

#Shapiro
set.seed(123)
datos %>%
  group_by(Region) %>%
  summarise(
    p_value = shapiro.test(sample(Profit, min(500, n())))$p.value
  )

# Prueba de Levene (homogeneidad de varianzas)
leveneTest(Profit ~ Region, data = datos)

#5. Plantamiento de la hipotesis -----------------------------------------------

# H0: Las medias de Profit son iguales entre las 4 regiones.
# H1: Al menos una media difiere de las demás.
# Por convención científica significa que acepta hasta un 5% de riesgo de rechazar H0 siendo verdadera.

#6. Prueba ANOVA ---------------------------------------------------------------

anova_resultado <- aov(Profit ~ Region, data = datos)
summary(anova_resultado)

# Como el valor p es menor que 0.05, se rechaza la hipotesis nula.

#7. Comparacion multiples ------------------------------------------------------

TukeyHSD(anova_resultado)

#East-Central: p = 0.0986433 -> no hay diferencia significativa.
#South-Central: p = 0.4064931 -> no hay diferencia significativa.
#West-Central: p = 0.0430362 -> sí hay diferencia significativa.
#South-East: p = 0.9696845 -> no hay diferencia significativa.
#West-East: p = 0.9920144 -> no hay diferencia significativa.
#West-South: p = 0.8974796 -> no hay diferencia significativa.

#8. Visualizacion --------------------------------------------------------------

ggplot(datos, aes(x = Region, y = Profit, fill = Region)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +  # oculta los outliers extremos
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black") +
  coord_cartesian(ylim = c(-50, 100)) +  # hace "zoom" sin eliminar datos reales
  labs(
    title = "ANOVA de un factor: ganancia (Profit) promedio entre regiones",
    subtitle = "Vista ampliada (zoom) — el rombo negro marca la media",
    x = "Región", y = "Profit"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

#''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''#
#''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''#

#---------------------------------MANOVA---------------------------------------#

#1. Librerias.

library(tidyverse)
library(biotools)
library(car)

#2. Cargar los datos -----------------------------------------------------------

datos2 <- Sample_Superstore

#2.1 Pasamos las variables tipo_producto y turno en factores.

datos2$Category <- factor(datos2$Category)
datos2$Segment  <- factor(datos2$Segment)

str(datos2)
head(datos2)

table(datos2$Category, datos2$Segment)

#3. Analisis exploratorio ------------------------------------------------------

datos2 %>%
  group_by(Category, Segment) %>%
  summarise(media_sales = mean(Sales),
            media_profit = mean(Profit),
            .groups = "drop")

cor(datos2$Sales, datos2$Profit) #Correlacion

#4. Supuestos ------------------------------------------------------------------

#4.1 Vamos a evaluar la normalidad

#Usamos Henze-Zirkler

henze_zirkler <- function(X) {
  X <- as.matrix(X)
  n <- nrow(X); p <- ncol(X)
  S <- cov(X); Sinv <- solve(S)
  Xc <- scale(X, center = TRUE, scale = FALSE)
  
  G  <- Xc %*% Sinv %*% t(Xc)
  Di <- diag(G)
  Dij <- outer(Di, Di, "+") - 2 * G
  Dij[Dij < 0] <- 0
  
  beta <- (1 / sqrt(2)) * ((n * (2 * p + 1) / 4)^(1 / (p + 4)))
  b2   <- beta^2
  t1 <- sum(exp(-b2 / 2 * Dij)) / n^2
  t2 <- -2 * (1 + b2)^(-p/2) * sum(exp(-b2 / (2 * (1 + b2)) * Di)) / n
  t3 <- (1 + 2 * b2)^(-p/2)
  HZ <- n * (t1 + t2 + t3)
  wb  <- (1 + b2) * (1 + 3 * b2); a <- 1 + 2 * b2
  mu  <- 1 - a^(-p/2) * (1 + p * b2 / a + p * (p + 2) * b2^2 / (2 * a^2))
  si2 <- 2 * (1 + 4 * b2)^(-p/2) +
    2 * a^(-p) * (1 + 2 * p * b2^2 / a^2 + 3 * p * (p + 2) * b2^4 / (4 * a^4)) -
    4 * wb^(-p/2) * (1 + 3 * p * b2^2 / (2 * wb) + p * (p + 2) * b2^4 / (2 * wb^2))
  pmu <- log(sqrt(mu^4 / (si2 + mu^2)))
  psi <- sqrt(log((si2 + mu^2) / mu^2))
  pval <- 1 - pnorm((log(HZ) - pmu) / psi)
  cat("Prueba de Henze-Zirkler (normalidad multivariada)\n")
  cat(sprintf("  Estadistico HZ : %.4f\n", HZ))
  cat(sprintf("  Valor p        : %.4f\n", pval))
  cat(sprintf("  Normalidad MVN : %s\n", ifelse(pval > 0.05, "SI (p > 0.05)", "NO (p <= 0.05)")))
  invisible(list(HZ = HZ, p.value = pval, MVN = pval > 0.05))
}

vd <- datos2[, c("Sales", "Profit")]
henze_zirkler(vd)

#4.2 Normalidad univariada Shapiro-Wilk
mod_lm <- lm(cbind(Sales, Profit) ~ Category * Segment,
             data = datos2)
res <- residuals(mod_lm) #Residuales del modelo

set.seed(123)
shapiro.test(sample(res[, "Sales"], 5000))    #espero p>0.05
shapiro.test(sample(res[, "Profit"], 5000))   #espero p>0.05

#4.3 Homogeneidad de matrices de covarianza
biotools::boxM(datos2[, c("Sales", "Profit")],
               interaction(datos2$Category, datos2$Segment))

#4.4 Homogeneidad de varianzas univariadas
leveneTest(Sales ~ Category * Segment, data = datos2)
leveneTest(Profit ~ Category * Segment, data = datos2)

#5. Planteo de Hipotesis --------------------------------------------------------

# Sea mu = (mu_Sales, mu_Profit) el vector de medias.
#
#   Efecto CATEGORY:
#     H0: mu_Furniture = mu_OfficeSupplies = mu_Technology
#     H1: al menos un vector de medias difiere entre categorías de producto
#
#   Efecto SEGMENT:
#     H0: mu_Consumer = mu_Corporate = mu_HomeOffice
#     H1: al menos un vector de medias difiere entre segmentos de cliente
#
#   INTERACCION Category x Segment:
#     H0: el efecto del segmento es el mismo para las tres categorías de
#         producto (sin interacción)
#     H1: el efecto del segmento depende de la categoría de producto

#6. Ajuste de modelo MANOVA -----------------------------------------------------
modelo <- manova(cbind(Sales, Profit) ~ Category * Segment,
                 data = datos2)
summary(modelo, test = "Pillai")

#7. ANOVA univariado de seguimiento ---------------------------------------------
summary.aov(modelo)

#8. Visualizacion --------------------------------------------------
medias <- datos2 %>%
  group_by(Category, Segment) %>%
  summarise(media_sales = mean(Sales), .groups = "drop")

ggplot(medias, aes(x = Segment, y = media_sales,
                   color = Category, group = Category)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 3) +
  labs(title = "Interaccion Category x Segment en las ventas (Sales)",
       x = "Segmento de cliente", y = "Venta media (Sales)",
       color = "Categoria de producto") +
  scale_color_manual(values = c("#153664", "#C55A11", "#2E7D32")) +
  theme_minimal(base_size = 12)
