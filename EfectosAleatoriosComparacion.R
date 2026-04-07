install.packages("wooldridge")
library(wooldridge)
data("airfare")

#Preparamos la base
install.packages("plm")
library(plm)
airfare.p <- pdata.frame(airfare, index = c("id","year"))

#Efectos Aleatorios
mod_re <- plm(fare ~ dist+passen+concen, 
              data = airfare.p, 
              model = "random")
summary(mod_re)

#Calculamos la variabilidad en el termino de error en base a los 
#valores de sus componentes
2856.73/(2856.73+379.39)

#Obtenemos como porcentaje de peso que tiene la variación del error
#en la heterogeneidad de los individuos
#En este caso 88.27% de la variabilidad se da por heterogeneidad de los
#individuos

#Theta también nos ayuda  a ver el peso de la heterogeneidad de
#Nuestros datos

#Para asegurarnos corremos varios modelos para comparar resultados
mod_pool <- plm(fare ~ dist+passen+concen, 
                data = airfare.p, 
                model = "pooling")
summary(mod_pool)

#Tambien hacemos pruebas
#Prueba Breusch-Pagan
#Hipotesis nula: Simga2 de alpha = 0
#Hipotesis1: alpha != 0
plmtest(mod_re, type = "bp")

#obtenemos un alpha prácticamente cero, se rechaza la hipótesis nula
#Por ende la varianza de los individuos es importante y no se 
#usa el modelo pooled

#Efectos fijos (Fixed Efects)
mod_fe <- plm(fare ~ dist+passen+concen, 
              data = airfare.p, 
              model = "within")
summary(mod_fe)

#Al correr una prueba para efectos fijos se tiene
#H0: a_i = a_... Por lo que no tendría sentido usar efectos fijos
#H1: Cambia en un punto y se agrega variabilidad

#Prueba F (H0: todos los interceptos (alpha) son iguales)
pFtest(mod_fe, mod_pool)

#Vemos el p-value ~= 0, por lo que si existe cambio entre alphas
#Que procede?

#Test de Hausman
phtest(mod_fe, mod_re)

#H0: los coeficientes entre fijos y aleatorios son iguales
#H1: son diferentes

#Por resultados vemos como los coeficientes son iguales
#Se rechaza la nula y se usa efectos fijos

#Tiene sentido no correlacionar alpha con las x's?
#Hay vairbales omitidas estructurañes?
#Si estos supuestos tienen sentido se puede defender el uso de 
#EA (RE) por sobre FE aun cuando el test diga lo contrario