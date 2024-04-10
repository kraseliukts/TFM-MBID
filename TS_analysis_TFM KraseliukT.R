###REVISAREMOS LA DIRECCION ACTUAL PARA LEER EL ARCHVIO DE DATOS
getwd()

#Instalamos y Cargamos las librerias necesarias
install.packages("ggplot2")
install.packages("forecast")
install.packages("ggfortify")
install.packages("astsa")
install.packages("TSstudio")
install.packages("vars")
install.packages("MTS")
install.packages("dplyr")
install.packages("lmtest")
install.packages("qqPlot")

library(astsa)
library(forecast)
library(tseries)
library(ggfortify)
library(ggplot2)
library(TSstudio)
library(vars)
library(MTS)  
library(dplyr)
library(lmtest)

### LEAMOS EL ARCHIVO CON LOS DATOS
data_full <- read.csv("resumen_por_anyo_mes.csv")
summary(data_full)
head(data_full)
View(data_full)


### Transformamos la data en 2 series temporales
data_c <- ts(data_full$Casual, start = c(2020,4), frequency = 12)
data_m <- ts(data_full$Member, start = c(2020,4), frequency = 12)

head(data_c)
head(data_m)

#unimos las 2 series
data <- cbind(data_c, data_m)
summary(data)
head(data)
class(data)
View(data)

#Trazamos la serie de tiempo datats
ts_plot(data)

autoplot(data)+
  labs(title = "Serie de tiempo de cantidad viajes por mes",       
       x = "Tiempo: Año - Mes",
       y = "Cantidad de viajes",
       ts.colour = "blue",
       ts.linetype="solid")+
  theme_bw() 


#Realizamos de descomposicion de la serie.
autoplot(decompose(data_c), ts.colour="blue")
autoplot(decompose(data_m), ts.colour="blue")


#KPSS test, para verificar si la serie es estacionaria o no.
kpss.test(data_c)
kpss.test(data_m)

#Calculamos el numero de diferenciaciones regulares y estacionales 
#que se necesita aplicar a la serie.
ndiffs(data_c)
ndiffs(data_m)
nsdiffs(data_c)
nsdiffs(data_m)


#Eliminamos la tendencia y visualizamos la grafica.
dif1.data = diff(data_c)
dif1.data
autoplot(dif1.data, ts.linetype = "solid", ts.colour = "blue")

dif2.data = diff(data_m)
dif2.data
autoplot(dif2.data, ts.linetype = "solid", ts.colour = "blue")


#Análisis de autocorrelación
acf2(dif1.data)
acf2(dif2.data)


#Eliminamos la estacionalidad y visualizamos la grafica.
dif12.data = diff(data_c, lag=12)
autoplot(dif12.data, ts.linetype = "solid", ts.colour = "blue")

dif22.data = diff(data_m, lag=12)
autoplot(dif22.data, ts.linetype = "solid", ts.colour = "blue")


#Análisis de autocorrelación
acf2(dif12.data)
acf2(dif22.data)


###ANALISIS Y ELECCION DEL MEJOR MODELO

#Modelo1, generado despues de analizar los correlogramas
modelo1_1<-arima(data_c,order=c(1,1,1),seasonal=list(order=c(0,1,0),seasonal.periods=12))
modelo1_1
summary(modelo1_1)

modelo1_2<-arima(data_m,order=c(1,1,1),seasonal=list(order=c(1,1,1),seasonal.periods=12))
modelo1_2
summary(modelo1_2)


#Modelo2 propuesto por auto.arima

ajuste1=auto.arima(data_c, stepwise = FALSE, approximation = FALSE)
ajuste1
summary(ajuste1)

ajuste2=auto.arima(data_m, stepwise = FALSE, approximation = FALSE)
ajuste2
summary(ajuste2)



modelo2_1<-arima(data_c,order=c(0,0,3),seasonal=list(order=c(1,1,0),period=12))
modelo2_1
summary(modelo2_1)

modelo2_2<-arima(data_m,order=c(1,0,0),seasonal=list(order=c(1,1,0),period=12))
modelo2_2
summary(modelo2_2)


#Modelo3, los parametros encontrados mediante el bookle

#para data_c
best.order <- c(0, 0, 0)
best.seasonal_order <- c(0, 0, 0)
best.aic <- Inf

for (i in 0:2) {
  for (j in 0:2) {
    for (t in 0:2) {
      for (k in 0:2) {
        fit.aic <- AIC(arima(data_c, order = c(i, 0, j), seasonal=list(order=c(0,0,k))))
        if (fit.aic < best.aic) {
          best.order <- c(i, 0, j)
          best.seasonal_order <- c(0, 0, k)
          best.arima <- arima(data_c, order = best.order, seasonal=best.seasonal_order)
          best.aic <- fit.aic
        }
      }
    }
  }
}
best.order
best.seasonal_order

#para data_m
best.order <- c(0, 0, 0)
best.seasonal_order <- c(0, 0, 0)
best.aic <- Inf

for (i in 0:2) {
  for (j in 0:2) {
    for (t in 0:2) {
      for (k in 0:2) {
        fit.aic <- AIC(arima(data_m, order = c(i, 0, j), seasonal=list(order=c(0,0,k))))
        if (fit.aic < best.aic) {
          best.order <- c(i, 0, j)
          best.seasonal_order <- c(t, 0, k)
          best.arima <- arima(data_m, order = best.order, seasonal=best.seasonal_order)
          best.aic <- fit.aic
        }
      }
    }
  }
}
best.order
best.seasonal_order



modelo3_1<-arima(data_c,order=c(2,1,2),seasonal=list(order=c(0,1,2),period=12))
modelo3_1
summary(modelo3_1)

modelo3_2<-arima(data_m,order=c(2,1,0),seasonal=list(order=c(0,1,2),period=12))
modelo3_2
summary(modelo3_2)



#Se realiza la comparacion del valor AIC de los 3 modelos.

AIC(modelo1_1)
AIC(modelo2_1)
AIC(modelo3_1)

AIC(modelo1_2)
AIC(modelo2_2)
AIC(modelo3_2)

BIC(modelo1_1)
BIC(modelo2_1)
BIC(modelo3_1)

BIC(modelo1_2)
BIC(modelo2_2)
BIC(modelo3_2)


#Despues de realizada la comparacion de los valores AIC de cada modelo, 
#se eligen como mejores modelos los siguiente:
modelo3_1<-arima(data_c,order=c(2,1,2),seasonal=list(order=c(0,1,2),seasonal.periods=12))
modelo1_2<-arima(data_m,order=c(1,1,1),seasonal=list(order=c(1,1,1),seasonal.periods=12))

modelo_c <- modelo3_1
summary(modelo_c)

modelo_m <- modelo1_2
summary(modelo_m)

#Vamos a realizar el análisis de residuos de cada modelo.
par(mar = c(1, 1, 1, 1))

tsdiag(modelo_c)
tsdiag(modelo_m)

#prueba Ljung-Box para verificar que existe el ruido blanco.
Box.test(residuals(modelo_c), type="Ljung-Box")
Box.test(residuals(modelo_m), type="Ljung-Box")

#graficamos los residuos
autoplot(residuals(modelo_c))
autoplot(residuals(modelo_m))

adf.test(residuals(modelo_c))
adf.test(residuals(modelo_m))


### FORECAST DEL MODELO #1

forecast(modelo_c)
autoplot(forecast(modelo_c))+  
  labs(title = "Pronostico de viajes para usuario Casual",       
       x = "Tiempo",
       y = "Cantidad de viajes",
       ts.colour = "blue",
       ts.linetype="solid")+
  theme_bw() 


forecast(modelo_m)
autoplot(forecast(modelo_m))+  
  labs(title = "Serie de tiempo: pronostico de viajes para usuario Member",       
       x = "Tiempo",
       y = "Cantidad de viajes",
       ts.colour = "blue",
       ts.linetype="solid")+
  theme_bw() 

