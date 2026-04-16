#################################################
##Ejercicio datos de panel
################################################

#Librerías necesarias
install.packages("AER")
library(AER)
install.packages("plm")
library(plm)
library(ggplot2)
library(dplyr)
install.packages("stargazer")
library(stargazer)
data("Fatalities")
help("Fatalities")

#Se pasa la data a penl
fatalities.p <- pdata.frame(Fatalities, index = c("state","year"))
pdim(fatalities.p) #Para balancear el dato de panel

#Se crea una variable de mortalidad para la medición
Fatalities$fatal_rate <- Fatalities$fatal/Fatalities$pop *10000

ggplot(Fatalities, aes(x=beertax, y=fatal_rate))+
  geom_point(alpha=0.5)+
  geom_smooth(method = "lm", se = F)+
  theme_minimal()

mod_mco <- lm(fatal_rate ~ beertax, data = Fatalities)
summary(mod_mco)

#Con este gráfico observamos como los efectos estatales crean ruido en el
#resultado de nuestro modelo original
ggplot(Fatalities, aes(x=beertax, y=fatal_rate, colour = factor(state)))+
  geom_point(alpha=0.5)+
  geom_smooth(method = "lm", se = F)+
  theme_minimal()

fatalities2 <- Fatalities %>%
  filter(year %in% c(1982, 1988)) %>%
  arrange(state, year) %>%
  group_by(state) %>%
  mutate(dy=fatal_rate - lag(fatal_rate),
         dx=beertax - lag(beertax)) %>%
  select(state, year, fatal_rate, dy, dx)

mod_mco2 <- lm(dy ~ dx, data = fatalities2)
summary(mod_mco2) 

#Creamos el modelo de efectos fijos para comparación
mod_fe <- plm(fatal_rate ~ beertax, data = fatalities.p,
              model = "within")
summary(mod_fe)

mod_re <- plm(fatal_rate ~ beertax, data = fatalities.p,
              model = "random")
summary(mod_re)

#Comparamos lso tres modelos 
stargazer(mod_mco, mod_fe, mod_re, type = "text")

#Utilizar todas aquellas variables que nos sirvan para explicar, así no se 
#Genera error por omisión (variables explicativas)