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
