#Instalación y carga de paquetes----------------------------
install.packages("wooldridge")
library(wooldridge)
data("wagepan")
help(wagepan)

install.packages("plm")
library(plm)

install.packages("stargazer")
library(stargazer)

#Transformación de la data a panel----------------------------
wp <- pdata.frame(wagepan, index = c("nr", "year"))

#Aplicación de los distintos modelos------------------------
#Empezamos realizando un modelo pooled
mod_pool <- plm(lwage ~ exper+union+married,
                data = wp,
                model = "pooling")
summary(mod_pool)

mod_fe <- plm(lwage ~ exper+union+married,
              data = wp,
              model = "within")
summary(mod_fe)

mod_re <- plm(lwage ~ exper+union+married,
              data = wp,
              model = "random")

#Comparación preliminar de los modelos---------------------
stargazer(mod_pool, mod_fe, mod_re,
          type = "text")

#Pruebas de hipótesis para comparación de modelos---------
#Comparación de FE vs Pooled con prueba F
pFtest(mod_fe,mod_pool)
#Se rechaza la hipotesis nula y se usa FE

#Comparacion de FE vs FE con Hausman
phtest(mod_fe, mod_re)
#Se rechaza la hipótesis nula y se usa RE

#Modelo de Panel Dinámico (Arellano-Bond)--------------------------
#Para este modelo los períodos deben ser pocos (T) y mis individuos extensos (N)
mod_ab <- pgmm(lwage ~ lag(lwage,1)+exper+union+married | lag(lwage, 2:3),
               data = wp,
               effect = "individual",
               model = "twosteps")
summary(mod_ab)
#Siempre se va a tener problemas de autocorrelacion 1 por estructura lógica del 
#modelo
#Lo que se busca es que la Autocorrelación 2 no sea significativa y por ende no 
#haya influencia de más de 2 períodos sobre la dependiente

#La validez del instrumento se ve con el test de Sargan
#En el se prueba que los instrumentos no esten correlacionados con el error
#Si se rechaza la hipótesis nula el artefacto está correlacionado

stargazer(mod_ab, mod_fe, type = "text")

#El test de Wald se usa para verificar la varianza (investigar porque)

