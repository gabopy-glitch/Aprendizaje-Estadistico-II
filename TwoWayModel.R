install.packages("wooldridge")
library(wooldridge)
data("airfare")
install.packages("plm")
library(plm)

airfare.p <- pdata.frame(airfare, index = c("id", "year"))

#Modelo de Efectos Fijos usando el comando regular Regular
reg_fe_d <- lm(fare ~ dist+concen+passen+factor(id), data = airfare)
summary(reg_fe_d)

#Modelo de Efectos fijos usando datos de panel regular
reg_fe <- plm(fare ~ dist+concen+passen,
               data = airfare.p,
               model = "within")
summary(reg_fe)

#Modelo de Efectos fijos usando datos de panel
reg_fe2 <- plm(fare ~ dist+concen+passen+factor(year),
              data = airfare.p,
              model = "within")
summary(reg_fe2)

install.packages("stargazer")
library(stargazer)

stargazer(reg_fe, reg_fe2,
          type = "text")

#Test de Multiplicador de Lagrange
#H0: los controles por tiempo son significativos globalmente
plmtest(reg_fe, effect = "time", type = "bp")
