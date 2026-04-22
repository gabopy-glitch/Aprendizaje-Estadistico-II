######################################
##Modelso autoregresivos
######################################

install.packages("forecast")
install.packages("dynlm")
install.packages("lubridate")
install.packages("urca")
install.packages("quantmod")
install.packages("ggplot2")

#Cargamos las librerias
library(forecast)
library(dynlm)
library(lubridate)
library(urca)
library(quantmod)
library(ggplot2)

#Ejemplo de Amazon--------------------------------------------------------

#Cargamos la data
mdate <- "2023-01-01"

AMZN <- getSymbols("AMZN", from = mdate, auto.assign = FALSE)

#Formato ts
y1 = ts(AMZN$AMZN.Close, start = c(2023,1), frequency = 1)

#Analisis gráfico
y1 %>% ggtsdisplay(main="")

#Test ADf Raíz Unitaria
F1 <- ur.df(y1, type = "trend", selectlags = "AIC") #Type = "none", "drift
summary(F1)

#si creo que tiene un intercepto diferente de 0 ponemos drift
#si creo que esta en 0 no se pone nada
#si creo que hay una tendencia entonces trend
#The value of the test statistic is: -3.3627 4.6539 5.7281 
#1. tao: (se tiene que comparar con terminos absolutos), es para ver si es estacionario o no (-3.3627)
#2. phi 2: intercepto es estadisrticamente significativo al 10% (4.6539)
#3. 

#analisis grafico serie diferenciada
y1 %>%  diff() %>% ggtsdisplay(main="")

df2=ur.df(diff(y1),type = "none",selectlags = "AIC") #none #drift
summary(df2)

df <- data.frame(
  y=as.numeric(y1),
  lagl=dplyr::lag(as.numeric(y1),1),
  diff1=c(NA,diff(y1)),
  diff2=c(NA,NA, diff(y1,differences = 2)))

#analizamos difereciaciones con R
ndiffs(y1)#para ver cuantas differenciaciones se necesitan para que se vuelva 
          #estacionaria la serie

nsdiffs(y1)#para ver si hay elementos estacionales

m1 <-arima(y1,order = c(0,1,0))
summary(m1)
#en este caso la serie es de ruido blanco porque no depende de su pasado ni los shocks...
#(por ende no podemos predecir ni entender la media porque no sabemos de que depende)

m2 <-arima(y1,order = c(1,1,1))
summary(m2)

#Test de residuos
#H0:LOs residuos no estan autocorrelacionados
checkresiduals(m1)

#Existen series en los que el ruido blanco (el ruido más simple que existe)
#Nos dificulta el entendimiento de la base por culpa de de su aleatoriedad

#Ejercicio----------------------------------------------------------------
library(readxl)
#Cargamos la data de ejemplo
inflation <- read_xlsx("SCN_Inflacion.xlsx")

#Formato de serie de tiempo para Nacional
ejm <- ts(inflation$NACIONAL, start = c(2005, 1), frequency = 12)

#Analisis gráfico
ejm %>% ggtsdisplay(main="")

#Test de Raiz Unitaria (Con dickey fuller)
F2 <- ur.df(ejm, type = "drift", selectlags = "AIC")
summary(F2)

#Si test-statistcs primer valor absoluto es mayor a los valores de tau2
#Entonces eso nos idica estacionaria en la data

#Como la serie presenta estacionaria no es necesario hacer la diferenciación

#Psita del profe: La serie también es estacionaria por lo que se necesita SARIMA