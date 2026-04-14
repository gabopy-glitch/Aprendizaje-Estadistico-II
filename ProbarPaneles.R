install.packages("wooldridge")
library(wooldridge)
install.packages("stargazer")
library(stargazer)
data("airfare")
help(airfare)
install.packages("plm")
library(plm)

airfare.p <- pdata.frame(airfare, index = c("id", "year"))

#Primero corremos un modelo pooled como referencia para el resto de modelos
#Modelo pooled
reg_poo <- plm(fare ~ dist+concen+passen, 
               data =airfare.p,
               model = "pooling")
summary(reg_poo)

#Modelo de Efectos fijos usando datos de panel regular
reg_fe <- plm(fare ~ dist+concen+passen,
              data = airfare.p,
              model = "within")
summary(reg_fe)
#R2 en este modelo es realmente un aproximado qe sirve para la comparación 
#con otros modelos

#Vemos con stargazer la comparación entre los modelos
stargazer(reg_poo, reg_fe, type = "text")
#Prueba F para  comparación
pFtest(reg_fe, reg_poo)

#Modelo de efectos aleatorios
reg_re <- plm(fare ~ dist+concen+passen,
              data = airfare.p,
              model = "random")
summary(reg_re)

#Comparación de ambos
stargazer(reg_fe, reg_re, type="text")

#Test de Haussman
phtest(reg_fe, reg_re)
#Se rechaza la hipótesis nula, por lo que mis afectos fijos están correlacionados
#conmis variables y se debe usar efecos fijos.

#Modelo de Efectos fijos usando datos de panel two ways
#Con esto controlamos el tiempo
reg_fe2 <- plm(fare ~ dist+concen+passen+factor(year),
               data = airfare.p,
               model = "within")
summary(reg_fe2)

stargazer(reg_fe, reg_fe2,
          type = "text")

#Test de Multiplicador de Lagrange
#H0: los controles por tiempo son iguales a cero por efecto del tiempo
#thao1 = thao_n = 0
#   Si los controles son por individuo es si estos son iguales entre si
plmtest(reg_fe, effect = "time", type = "bp")

#Realizamos un test de Wald modificado
library(lmtest)
bptest(reg_fe, studentize = F)
#Esto indica problemas de heterocedasticidad
#H0:

#Test de Wooldridge
#H0: NO existe autocorrelación serial
#H1: Existe autocrrelación serial
pbgtest(reg_fe2)

#Tenems un prblema de autocorrelación por lo que los errores estándar están 
#mal calcuados y la significancia no es confiable

#Solución: Errores robustos
coeftest(reg_fe2, vcov. = vcovHC(reg_fe2, cluster = "group"))

#¿Qué hacer en caso de un panel no balanceado?
#Se puede crear una variable dummy para los individuos que estan presentes todo
#el tiempo y todos los que no

#Para ello se puede usar un modelo logit o probit para probar algunas variables
#de interés en relación a ello

#También se puede usar el panel completo o balancear el panel para 
#comparar resultados dependiendo de is los individuos entran o salen del panel