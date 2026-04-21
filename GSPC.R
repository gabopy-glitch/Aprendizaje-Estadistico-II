install.packages("quantmod") 
install.packages("plm") 
install.packages("tswge") 
library(quantmod) 
library(tswge)

getSymbols("^GSPC", src = "yahoo", from = "2000-01-01")

# Convertir a data frame
df <- data.frame(date = index(GSPC), coredata(GSPC))

# Guardar como CSV
write.csv(df, "GSPC_data.csv", row.names = FALSE)

install.packages("openxlsx")
library(openxlsx)

# Convertir a data frame
df <- data.frame(date = index(GSPC), coredata(GSPC))

