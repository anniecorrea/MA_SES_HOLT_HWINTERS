install.packages("imputeTS")
install.packages("naniar")
install.packages("heatmaply")

library(readxl)
library(dplyr)
library(ggplot2)
library(forecast)
library(imputeTS)
library(naniar)
library(heatmaply)
library(fpp2)
library(tseries)
library(tidyr)



df <- read_excel("Consumo.xlsx")


str(df)
summary(df$Consumo)

# Cantidad de NA
sum(is.na(df$Consumo))

# Porcentaje de NA
mean(is.na(df$Consumo)) * 100


#Revision de datos nulos
ts_consumo <- ts(df$Consumo, start = c(1970, 1), frequency = 4)
autoplot(ts_consumo) +
  ylab("Cambio Porcentual (%)") +
  xlab("Año")


#Grafico Nulos:
df %>%
  select(Año, Trimestre, Consumo) %>%
  vis_miss() +
  theme(
    axis.text.x = element_text(
      vjust = 0,      # baja las etiquetas
      size  = 10      # reduce tamaño (ajústalo si quieres)
    )
  )


# Crear índice de tiempo correcto (año + trimestre)
df <- df %>%
  mutate(
    Año = as.numeric(Año),
    Trimestre = as.numeric(gsub("Trimestre_", "", Trimestre)),
    tiempo = Año + (Trimestre - 1) / 4
  )



#Pruebas de imputacion:

#Imputacion por medias moviles
df$imp_ma <- na_ma(df$Consumo, k = 4, weighting = "simple")

#Imputacion por Kalman
df$imp_kalman <- na_kalman(df$Consumo, model = "StructTS")

#Comparar
df_long <- df %>%
  select(index, Consumo, imp_ma, imp_kalman) %>%
  pivot_longer(
    cols = -index,
    names_to = "Método",
    values_to = "Valor"
  )

df_long$Método <- factor(
  df_long$Método,
  levels = c("Consumo", "imp_ma", "imp_kalman"),
  labels = c("Original (con NA)", "Media móvil", "Kalman")
)

#Grafico comparando
ggplot(df_long, aes(x = index, y = Valor, color = Método)) +
  geom_line(size = 1) +
  labs(
    title = "Comparación de imputación: Original vs Media Móvil vs Kalman",
    x = "Índice temporal",
    y = "Consumo"
  ) +
  theme_minimal()


#Finalmente se imputó con Kalman
Consumo_kalman <- na_kalman(df$Consumo, model = "StructTS")


Consumo_ts <- ts(
  Consumo_kalman,
  start = c(min(df$Año), 1),
  frequency = 4
)


#DESCOMPOSICION
plot(Consumo_ts)
tsDatos <- ts(Consumo_ts, start = c(1970, 1), frequency = 4)
componentes.ts <- decompose(tsDatos)
plot(componentes.ts)


#Pruebas de estacionariedad
adf.test(Consumo_ts)
Box.test(Consumo_ts)


par(mfrow = c(1, 2))  # dividir en 2 gráficos
acf(Consumo_ts, main = "ACF de la serie Consumo")
pacf(Consumo_ts, main = "PACF de la serie Consumo")
par(mfrow = c(1, 1))  # volver a normal


#Split Train/Test
n <- length(Consumo_ts)
train_size <- round(0.7 * n)

# Convertir la posición a coordenadas (año, trimestre)
train_end_year  <- min(df$Año) + (train_size - 1) %/% 4
train_end_quarter <- 1 + (train_size - 1) %% 4

test_start_year <- min(df$Año) + (train_size) %/% 4
test_start_quarter <- 1 + (train_size) %% 4

# Ventanas train/test correctamente alineadas
train_ts <- window(Consumo_ts,
                   end = c(train_end_year, train_end_quarter))

test_ts <- window(Consumo_ts,
                  start = c(test_start_year, test_start_quarter))



length(train_ts)
length(test_ts)



# Crear un data frame para graficar
df_split <- data.frame(
  tiempo = time(tsDatos),
  valor  = as.numeric(tsDatos),
  tipo   = c(rep("Train", length(train_ts)),
             rep("Test",  length(test_ts)))
)

# Gráfico Split Train/Test
ggplot(df_split, aes(x = tiempo, y = valor, color = tipo)) +
  geom_line(size = 1) +
  geom_vline(xintercept = time(tsDatos)[train_size], 
             linetype = "dashed", color = "black", size = 1) +
  scale_color_manual(values = c("Train" = "steelblue", "Test" = "red")) +
  labs(
    title = "Separación Train/Test en la Serie de Tiempo",
    x = "Tiempo",
    y = "Consumo",
    color = ""
  ) +
  theme_minimal(base_size = 14)



# Prueba de modelos en train
modelo_ar1_train     <- arima(train_ts, order=c(1,0,0))
modelo_ma1_train     <- arima(train_ts, order=c(0,0,1))
modelo_ar2_train     <- arima(train_ts, order=c(2,0,0))
modelo_ma2_train     <- arima(train_ts, order=c(0,0,2))
modelo_arma11_train  <- arima(train_ts, order=c(1,0,1))
modelo_arma12_train  <- arima(train_ts, order=c(1,0,2))
modelo_arma22_train  <- arima(train_ts, order=c(2,0,2))
modelo_arima111_train <- arima(train_ts, order=c(1,1,1))


AIC(
  modelo_ar1_train, modelo_ma1_train, modelo_ar2_train, modelo_ma2_train,
  modelo_arma11_train, modelo_arma22_train, modelo_arma12_train, modelo_arima111_train
)

BIC(
  modelo_ar1_train, modelo_ma1_train, modelo_ar2_train, modelo_ma2_train,
  modelo_arma11_train, modelo_arma22_train, modelo_arma12_train, modelo_arima111_train
)

summary(modelo_arma11_train)


#Supuestos
# 1. Residuos vs tiempo
plot.ts(
  residuals(modelo_arma12_train),
  main = "Residuos del modelo ARIMA(1,0,1)",
  ylab = "Residuo",
  xlab = "Tiempo"
)


# 2. ACF de los residuos
acf(residuals(modelo_arma11_train), main = "ACF de los residuos")

# 3. Test de Ljung-Box
Box.test(residuals(modelo_arma11_train), lag = 10, type = "Ljung-Box")

# 4. Normalidad: histograma
hist(residuals(modelo_arma11_train), breaks = 20, main = "Histograma de los residuos")

# 5. Normalidad: QQ-plot
qqnorm(residuals(modelo_arma11_train))
qqline(residuals(modelo_arma11_train), col = "red")
shapiro.test(residuals(modelo_arma11_train))


# Ajuste en train vs datos de train
df_train_plot <- data.frame(
  tiempo = time(train_ts),
  real   = as.numeric(train_ts),
  fitted = as.numeric(fitted(modelo_arma11_train))
)


ggplot(df_train_plot, aes(x = tiempo)) +
  geom_line(aes(y = real, color = "Real (Train)")) +
  geom_line(aes(y = fitted, color = "Fitted"), linetype = "dashed") +
  scale_color_manual(values = c("Real (Train)" = "black",
                                "Fitted" = "blue")) +
  labs(
    title = "Train: Real vs Fitted (modelo_arma11_train)",
    x = "Tiempo",
    y = "Consumo Porcentual (%)",
    color = ""
  ) +
  theme_minimal()



#Revision de modelo ganador en datos del Test
# 1. Forecast del modelo ganador (ejemplo: modelo_arma11_train)
h <- length(test_ts)
f_test <- forecast(modelo_arma11_train, h = h)

# 2. Crear dataframe para graficar
df_test_plot <- data.frame(
  tiempo     = time(test_ts),
  real       = as.numeric(test_ts),
  pronostico = as.numeric(f_test$mean)
)


# 3. Gráfico estilo ejemplo
ggplot(df_test_plot, aes(x = tiempo)) +
  geom_line(aes(y = real, color = "Real (Test)"), size = 1) +
  geom_line(aes(y = pronostico, color = "Ajustado"), 
            size = 1, linetype = "dashed") +
  scale_color_manual(values = c("Real (Test)" = "red", 
                                "Ajustado" = "blue")) +
  labs(
    title = "Valores Reales vs Ajustados (Test)",
    x = "Tiempo",
    y = "Cambio Porcentual (%)",
    color = ""
  ) +
  theme_minimal(base_size = 14)


# Convertir a ts
test_ts2 <- ts(df_test_plot$real,
               start = start(test_ts),
               frequency = frequency(test_ts))

# 2.ARIMA sobre el test (solo para visualizar)
modelo_test_refit <- Arima(test_ts2, model = modelo_arma11_train)

# 3. Fitted del test (ajuste, no pronóstico)
fitted_test <- fitted(modelo_test_refit)

# 4. Agregar al dataframe
df_test_plot$fitted_refit <- as.numeric(fitted_test)


ggplot(df_test_plot, aes(x = tiempo)) +
  geom_line(aes(y = real, color = "Real (Test)"), size = 1) +
  
  # Pronóstico rolling o directo
  geom_line(aes(y = pronostico, color = "Pronóstico"),
            size = 1, linetype = "dashed") +
  
  # Ajuste modelo en test
  geom_line(aes(y = fitted_refit, color = "Ajuste (fitted)"),
            size = 1, linetype = "dotdash") +
  
  scale_color_manual(values = c(
    "Real (Test)"      = "gray",
    "Pronóstico"       = "red",
    "Ajuste (fitted)"   = "blue"
  )) +
  
  labs(
    title = "Valores Reales vs Pronóstico y Ajuste (Test)",
    x = "Tiempo",
    y = "Cambio Porcentual (%)",
    color = ""
  ) +
  theme_minimal(base_size = 14)

# 1. Forecast del modelo ARMA(1,1)
h <- length(test_ts)
f_test <- forecast(modelo_arma11_train, h = h)

# 2. Valores reales y pronosticados
real <- as.numeric(test_ts)
pred  <- as.numeric(f_test$mean)

# 3. Calcular métricas
MAE  <- mean(abs(real - pred))
RMSE <- sqrt(mean((real - pred)^2))
MAPE <- mean(abs((real - pred) / real)) * 100

# 4. Mostrar métricas
data.frame(
  Modelo = "ARMA(1,1)",
  MAE = round(MAE, 4),
  RMSE = round(RMSE, 4),
  MAPE = round(MAPE, 2)
)






# Pronóstico a 6 meses (2 trimestres)

forecast_6m <- forecast(modelo_arma11, h = 2)

# Ver el resultado numérico
forecast_6m

# Gráfico del pronóstico
autoplot(forecast_6m) +
  ggtitle("Pronóstico a 6 meses con ARIMA(1,0,1)") +
  ylab("Consumo Porcentual (%)") +
  xlab("Tiempo") +
  xlim(2010, NA)      # <-- recorta el gráfico desde 2010 en adelante