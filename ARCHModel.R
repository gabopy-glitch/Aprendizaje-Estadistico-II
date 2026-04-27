###################################################
##Modelos de Varianza
###################################################

install.packages("forecast")
install.packages("dynlm")
install.packages("lubridate")
install.packages("urca")
install.packages("quantmod")
install.packages("ggplot2")
install.packages("rugarch")

#Cargamos las librerias
library(forecast)
library(dynlm)
library(lubridate)
library(urca)
library(quantmod)
library(ggplot2)
library(rugarch)

#Modelos ARCH-------------------------------
#Cargamos la data
mdate <- "2023-01-03"

AMZN <- getSymbols("AMZN", from = mdate, auto.assign = FALSE)

retornos <- dailyReturn(AMZN$AMZN.Close, type = "log")

retornos %>% ggtsdisplay(main="")

ret2 <- retornos^2 #Variable proxi para la varinza de la serie

plot(ret2)

ret2 %>% ggtsdisplay(main="")

acf(ret2)

#Test Ljung-Box
Box.test(ret2, lag = 20, type = "Ljung-Box")
#Rechazo H0: no hay autocorrelación en las varianzas

#Estructura
arch1 <- ugarchspec(variance.model = list(model="sGARCH", garchOrder=c(1,0)),
                    mean.model = list(armaOrder=c(0,0)),
                    distribution.model = "norm")

fit_arch1 <- ugarchfit(spec = arch1, data = retornos)
show(fit_arch1)

#El coeficiente de volatilidad pasada se encuentra en alpha1
#En parametros óptimos me dan errores estandar regulares y robustos,
#los 'ultimos son más precisos y por ende ayudan a inferir mejor
#Por ejemplo en este caso el rezago de la volatilidad en los robustos deja de
#ser significativo al 95%

#También se da una vista de muchos estadísticos para tomar una decisión

#ljung-Box
#Prueba de autocorrelación. h0: no hay autocorrelación

#ljung-box errores al cuadrado
#Lo mismo que antes

#ARCH LM test
#Prueba para problemas de autocorrelación que no se detectan con Ljung-Box
#Misma prueba de hipótesis que Ljung

#Sign Bias test
#Un shock afecta de forma simétrica a la media?
#Si no se rechaza la nula de simetría sube o baja de forma agresiva + o -

#Pearson ajustado
#H0: los residuos son normales
#Para este caso no se cumplen y los residuos no son normales

#Modelo GARCH-----------------------------------------------

garch1 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                     mean.model = list(armaOrder = c(0,0)),
                     distribution.model = "norm")
fit_garch1 <- ugarchfit(spec = garch1, data = retornos)
show(fit_garch1)

#beta es nueva y es la persistencia de la volatilidad

vol_arch <- sigma(fit_arch1)
vol_garch <- sigma(fit_garch1)

plot(vol_arch, type="l")
plot(vol_garch, type="l")
