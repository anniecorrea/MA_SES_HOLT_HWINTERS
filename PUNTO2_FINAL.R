#install.packages("imputeTS")
# Paquetes
library(tseries)
library(forecast)
library(ggfortify)
library(readxl)
library(lubridate)
library(ggplot2)
library(imputeTS)

# ------------------------- EDA ---------------------------------

# Cargar los datos
df_2 <- read.csv("C:/Users/fabia/Downloads/Viviendas.csv",
                 header = TRUE, sep = ";")

# Revisar estructura básica
str(df_2)
head(df_2)
summary(df_2)

# Número de filas (observaciones)
nrow(df_2)

# Número de valores faltantes en Viviendas
sum(is.na(df_2$Viviendas))

# Convertir Fecha a tipo Date
df_2$Fecha <- as.Date(df_2$Fecha, format = "%d/%m/%Y")

# Verificar fechas mínima y máxima ya como Date
range(df_2$Fecha)

# Crear objeto ts (inicia en 1959, mes 1, frecuencia mensual)
viviendas_ts <- ts(df_2$Viviendas,
                   start = c(1959, 1),
                   frequency = 12)

# usar un nombre corto para la serie
y <- viviendas_ts

# Gráfica de la serie en el tiempo
autoplot(y, ts.colour = "black") +
  ggtitle("Nuevas unidades de vivienda privada") +
  ylab("Viviendas") + xlab("Año")

# Filtrar las observaciones con NA
na_rows <- df_2[is.na(df_2$Viviendas), ]
na_rows

# Grafico de la serie con los nulos

df_2$NA_flag <- is.na(df_2$Viviendas)

ggplot(df_2, aes(x = Fecha, y = Viviendas)) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  geom_vline(data = df_2[df_2$NA_flag == TRUE, ],
             aes(xintercept = Fecha),
             color = "red", alpha = 0.8) +
  labs(title = "Serie de tiempo con nulos resaltados",
       x = "Fecha",
       y = "Viviendas") +
  theme_minimal()

# Gráfico de estacionalidad (de momento lo dejamos preparado)
ggseasonplot(y,
             year.labels = FALSE,
             continuous = TRUE) +
  ggtitle("Patrón estacional de la construcción de vivienda") +
  ylab("Viviendas") + xlab("Mes")

# ---------------------------- IMPUTAR DATOS NA -------------------------------

# IMPUTACIÓN KALMAN

# Usamos un modelo estructural (nivel + estacionalidad + ruido)
y_kalman <- na.kalman(y, model = "StructTS")

# Verificar que ya no hay NA
sum(is.na(y_kalman))   

# Guardar en el data.frame (por si quieres usarlo luego)
df_2$Viviendas_kalman <- as.numeric(y_kalman)

# Comparar visualmente original vs Kalman
autoplot(cbind(Original = y,
               Kalman   = y_kalman)) +
  ggtitle("Serie de viviendas: original vs imputada con filtro de Kalman") +
  ylab("Viviendas") + xlab("Año")

# Gráfico de estacionalidad con serie imputada
ggseasonplot(y_kalman,
             year.labels = FALSE,
             continuous = TRUE) +
  ggtitle("Patrón estacional con imputación Kalman") +
  ylab("Viviendas") + xlab("Mes")

# Grafico de descomposición 
decomp_add <- decompose(y_kalman, type = "additive")
plot(decomp_add)

# FAC y FACP de la serie original
par(mfrow = c(1,2))
acf(y_kalman,
    main = "FAC - serie original",
    ylim = c(-1,1))

pacf(y_kalman,
     main = "FACP - serie original",
     ylim = c(-1,1))
par(mfrow = c(1,1))

# Prueba ADF (Dickey-Fuller) 
print(adf.test(y_kalman)) # la serie NO es estacionaria según la ADF

# Para una serie original, si los p-valores son bajos, indica que hay autocorrelación,
# lo cual es consistente con la no estacionariedad.
# p-value < 2.2e-16: La serie está fuertemente autocorrelada. NO es estacionaria.
Box.test(y_kalman, lag = 12, type = "Ljung")
Box.test(y_kalman, lag = 1, type = "Ljung-Box")



# ----------------------- Estacionarización ---------------------------------

# Quitar estacionalidad determinística
Serie_Ajustada <- y_kalman - comp$seasonal

autoplot(Serie_Ajustada) +
  ggtitle("Serie sin estacionalidad") +
  ylab("Viviendas ajustadas") + xlab("Año")

# Diferencia ordinaria (d = 1)
#Serie_Ajustada_d1 <- diff(Serie_Ajustada, differences = 12)
Serie_Ajustada_d1 <- diff(Serie_Ajustada, differences = 1)

autoplot(Serie_Ajustada_d1) +
  ggtitle("Serie Ajustada + 1 Diferencia (estacionaria)") +
  ylab("Δ viviendas ajustadas") + xlab("Año")

# FAC / FACP
par(mfrow = c(1,2))
acf(Serie_Ajustada_d1, main="FAC - Serie Ajustada d=1", ylim = c(-1,1))
pacf(Serie_Ajustada_d1, main="FACP - Serie Ajustada d=1", ylim = c(-1,1))
par(mfrow = c(1,1))

# Prueba ADF y boxtest (debe ser estacionaria)
library(tseries)
adf.test(Serie_Ajustada_d1)

Box.test(Serie_Ajustada_d1, lag = 12, type = "Ljung")

# Esta es la serie estacionaria de trabajo
y_work <- Serie_Ajustada_d1


# ------------------ SPLIT 70% TRAIN - 30% TEST ------------------------

n <- length(y_work)

n_train <- round(0.7 * n)
n_test  <- n - n_train

y_train <- y_work[1:n_train]
y_test  <- y_work[(n_train + 1):n]

cat("Observaciones totales:", n, "\n")
cat("Train:", n_train, " - Test:", n_test, "\n")


# ---------- Comparación de modelos ARMA (70% TRAIN) ----------------

# Ajuste de modelos candidatos
fit_ar1      <- Arima(y_train, order=c(1,0,0))
fit_ar2      <- Arima(y_train, order=c(2,0,0))

fit_ma1      <- Arima(y_train, order=c(0,0,1))
fit_ma2      <- Arima(y_train, order=c(0,0,2))

fit_arma11   <- Arima(y_train, order=c(1,0,1))
fit_arma21   <- Arima(y_train, order=c(2,0,1))
fit_arma12   <- Arima(y_train, order=c(1,0,2))
fit_arma22   <- Arima(y_train, order=c(2,0,2))

# Tabla comparativa
tabla_modelos <- data.frame(
  Modelo = c(
    "AR(1)", "AR(2)",
    "MA(1)", "MA(2)",
    "ARMA(1,1)", "ARMA(2,1)", "ARMA(1,2)", "ARMA(2,2)"
  ),
  AIC = c(
    AIC(fit_ar1), AIC(fit_ar2),
    AIC(fit_ma1), AIC(fit_ma2),
    AIC(fit_arma11), AIC(fit_arma21), AIC(fit_arma12), AIC(fit_arma22)
  ),
  BIC = c(
    BIC(fit_ar1), BIC(fit_ar2),
    BIC(fit_ma1), BIC(fit_ma2),
    BIC(fit_arma11), BIC(fit_arma21), BIC(fit_arma12), BIC(fit_arma22)
  )
)

# Ordenar por AIC
cat("\nModelos ordenados por AIC:\n")
print(tabla_modelos[order(tabla_modelos$AIC), ])

# Ordenar por BIC
cat("\nModelos ordenados por BIC:\n")
print(tabla_modelos[order(tabla_modelos$BIC), ])


# ------ Probando los ARIMA sobre Serie_Ajustada <- y_kalman - comp$seasonal ------

# Train/Test usando esta serie
yA <- Serie_Ajustada
nA <- length(yA)
n_trainA <- round(0.7 * nA)

Serie_Ajustada_train <- yA[1:n_trainA]
Serie_Ajustada_test  <- yA[(n_trainA+1):nA]

mod_arima011 <- Arima(Serie_Ajustada_train, order = c(0, 1, 1),
                      include.mean = TRUE)

mod_arima012 <- Arima(Serie_Ajustada_train, order = c(0, 1, 2),
                      include.mean = TRUE)

mod_arima111 <- Arima(Serie_Ajustada_train, order = c(1, 1, 1),
                      include.mean = TRUE)

data.frame(
  Modelo = c("ARIMA(0,1,1)", "ARIMA(0,1,2)", "ARIMA(1,1,1)"),
  AIC = c(AIC(mod_arima011), AIC(mod_arima012), AIC(mod_arima111)),
  BIC = c(BIC(mod_arima011), BIC(mod_arima012), BIC(mod_arima111))
)

# -------------- VALIDACIÓN DE SUPUESTOS DEL MODELO --------------------------

validar_modelo <- function(fit, nombre){
  
  cat("\n====================================\n")
  cat(" VALIDACIÓN DEL MODELO:", nombre, "\n")
  cat("====================================\n")
  
  res <- residuals(fit)
  
  # Parámetros detectados automáticamente
  p <- fit$arma[1]
  q <- fit$arma[2]
  fitdf <- p + q
  
  cat("\nParámetros del modelo: p =", p, " | q =", q, "\n")
  cat("fitdf para Ljung–Box =", fitdf, "\n")
  
  # ------------------------------------------------------------------------
  # SUPUESTO 1: ESTACIONARIEDAD (ADF)
  # ------------------------------------------------------------------------
  cat("\n--- ESTACIONARIEDAD DE RESIDUOS (ADF) ---\n")
  adf_res <- adf.test(res)
  cat("p-valor ADF:", adf_res$p.value, "\n")
  cat("Interpretación:",
      ifelse(adf_res$p.value < 0.05,
             "✓ Residuos estacionarios",
             "✗ Residuos NO estacionarios"), "\n")
  
  # ------------------------------------------------------------------------
  # SUPUESTO 2: NORMALIDAD (Shapiro–Wilk)
  # ------------------------------------------------------------------------
  cat("\n--- NORMALIDAD DE RESIDUOS (Shapiro–Wilk) ---\n")
  shap_res <- shapiro.test(res)
  cat("Estadístico W:", shap_res$statistic, "\n")
  cat("p-valor:", shap_res$p.value, "\n")
  cat("Interpretación:",
      ifelse(shap_res$p.value > 0.05,
             "✓ Residuos aproximadamente normales",
             "✗ Residuos NO normales"), "\n")
  
  # ------------------------------------------------------------------------
  # SUPUESTO 3: INDEPENDENCIA (Ljung–Box)
  # ------------------------------------------------------------------------
  cat("\n--- INDEPENDENCIA DE RESIDUOS (Ljung–Box) ---\n")
  ljung_res <- Box.test(res, lag = 20, type = "Ljung-Box", fitdf = fitdf)
  cat("Estadístico X²:", ljung_res$statistic, "\n")
  cat("p-valor:", ljung_res$p.value, "\n")
  cat("Interpretación:",
      ifelse(ljung_res$p.value > 0.05,
             "✓ Residuos independientes (NO autocorrelación)",
             "✗ Residuos NO independientes (hay autocorrelación)"), "\n")
  
  # ------------------------------------------------------------------------
  # Estadísticas adicionales
  # ------------------------------------------------------------------------
  cat("\nMedia de los residuos:", mean(res), "\n")
  
  cat("\n--- Resumen del modelo ---\n")
  print(summary(fit))
  
  # ------------------------------------------------------------------------
  # GRÁFICOS
  # ------------------------------------------------------------------------
  par(mfrow = c(2,2))
  acf(res, main = paste("ACF de residuos -", nombre))
  pacf(res, main = paste("PACF de residuos -", nombre))
  hist(res,
       main = paste("Histograma de residuos -", nombre),
       xlab = "Residuos")
  qqnorm(res, main = paste("QQ-Plot de residuos -", nombre))
  qqline(res, col = "red")
  par(mfrow = c(1,1))
  
  plot(res,
       type = "l",
       main = paste("Residuos en el tiempo -", nombre),
       xlab = "Tiempo",
       ylab = "Residuos")
  
  cat("\n=== FIN VALIDACIÓN:", nombre, "===\n")
}


############################################################
# Validar los 2 modelos candidatos
############################################################

validar_modelo(mod_arima011, "ARIMA(0,1,1)")
validar_modelo(mod_arima012, "ARIMA(0,1,2)")
validar_modelo(mod_arima111, "ARIMA(1,1,1)")


validar_modelo(fit_ma1, "MA(1)")
validar_modelo(fit_ar2, "AR(2)")
validar_modelo(fit_arma11, "ARMA(1,1)")
validar_modelo(fit_arma21, "ARMA(2,1)")


#validar_modelo(fit_arma21, "ARMA(2,1)"


###############################################################################
# Ajustar ARIMA(0,1,1) solo con TRAIN
###############################################################################

modelo_train <- mod_arima011
summary(modelo_train)

# Pronóstico sobre el bloque TEST (solo para validar)
fc_test <- forecast(modelo_train, h = n_test)

# Errores de validación (en la serie estacionaria)
y_test_vec <- as.numeric(y_test)
y_pred_vec <- as.numeric(fc_test$mean)

rmse_test <- sqrt(mean((y_test_vec - y_pred_vec)^2))
mae_test  <- mean(abs(y_test_vec - y_pred_vec))
mape_test <- mean(abs((y_test_vec - y_pred_vec) / y_test_vec)) * 100

cat("\nErrores de validación (sobre serie estacionaria):\n")
cat("RMSE :", rmse_test, "\n")
cat("MAE  :", mae_test,  "\n")
cat("MAPE :", mape_test, "%\n")


###############################################################################
# Reajustar MA(1) con el 100% de la serie estacionaria y_work
###############################################################################

y_full <- y_work  # Serie ya estacionaria (sin estacionalidad y diferenciada)

modelo_final <- Arima(y_full, order = c(0,0,1))
summary(modelo_final)

# Pronóstico final a 6 pasos
fc_6 <- forecast(modelo_final, h = 6)
fc_vals <- as.numeric(fc_6$mean)   # diferencias pronosticadas


###############################################################################
# Invertir la diferencia (d=1)
###############################################################################

ultimo_valor_ajustado <- as.numeric(tail(Serie_Ajustada, 1))

Serie_Ajustada_future <- ultimo_valor_ajustado + cumsum(fc_vals)


###############################################################################
#  Reintroducir estacionalidad determinística
###############################################################################

# Componente estacional original de la descomposición
seasonal_comp <- comp$seasonal

# Estacionalidad promedio por mes
seasonal_means <- tapply(as.numeric(seasonal_comp),
                         cycle(seasonal_comp),
                         mean)

# Identificar los meses futuros
last_month <- cycle(seasonal_comp)[length(seasonal_comp)]
future_months <- sapply(1:6, function(h) ((last_month - 1 + h) %% 12) + 1)

# Estacionalidad futura
seasonal_future <- seasonal_means[future_months]

# Serie final ajustada + estacionalidad
y_future_original <- Serie_Ajustada_future + seasonal_future

###############################################################################
# Gráfico final: histórico vs pronóstico 6 meses
###############################################################################

serie_extendida <- ts(
  c(as.numeric(y_kalman), y_future_original),
  start     = start(y_kalman),
  frequency = frequency(y_kalman)
)

n_hist  <- length(y_kalman)
n_total <- length(serie_extendida)

plot(serie_extendida,
     type = "l", lwd = 2, col = "blue",
     main = "Pronóstico de 6 meses (ARIMA(0,1,1) sobre serie estacionaria)",
     xlab = "Año", ylab = "Nuevas unidades de vivienda privada")

lines(time(serie_extendida)[(n_hist+1):n_total],
      serie_extendida[(n_hist+1):n_total],
      col = "red", lwd = 2)

legend("topleft",
       legend = c("Serie imputada (Kalman)", "Pronóstico 6 meses"),
       col    = c("blue", "red"),
       lwd    = 2)

cat("\nPronóstico de 6 meses en escala original:\n")
print(y_future_original)

###############################################################################
# Gráfico de ZOOM: últimos 2 años + pronóstico 6 meses
###############################################################################

# Vector de tiempos de la serie extendida
t_ext <- time(serie_extendida)

# Si hay al menos 24 meses históricos, usamos últimos 2 años; si no, desde el inicio
if (n_hist > 24) {
  t_ini_zoom <- t_ext[n_hist - 24 + 1]   # ~últimos 24 meses históricos
} else {
  t_ini_zoom <- t_ext[1]
}

# Serie recortada para el zoom (últimos años + pronóstico)
serie_zoom <- window(serie_extendida, start = t_ini_zoom)

plot(serie_zoom,
     type = "l", lwd = 2, col = "blue",
     main = "Pronóstico 6 meses (zoom últimos años)",
     xlab = "Año", ylab = "Nuevas unidades de vivienda privada")

# Añadir en rojo SOLO el tramo pronosticado dentro del zoom
serie_forecast_zoom <- window(serie_extendida,
                              start = t_ext[n_hist + 1])
lines(serie_forecast_zoom, col = "red", lwd = 2)

legend("topleft",
       legend = c("Serie imputada (Kalman)", "Pronóstico 6 meses"),
       col    = c("blue", "red"),
       lwd    = 2)

###############################################################################
# Comprobación de equivalencia MA(1) vs ARIMA(0,1,1)
###############################################################################

dy <- diff(Serie_Ajustada)
fit_ma1 <- Arima(dy, order=c(0,0,1))
summary(fit_ma1)

fit_011 <- Arima(Serie_Ajustada, order=c(0,1,1))
summary(fit_011)

