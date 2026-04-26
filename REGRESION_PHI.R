
#REGRESIÓN PARA OBTENER PHI 

#CASO 1: 18/03/2026

#1. Instalación de paquetes
#install.packages("stargazer"))
library(stargazer)

# 2. Datos de precios y de gas
precios <- c(14.66, 22.52, 43.96, 46.54, 39.46, 30.93, 30.66, 25.74)
gas_gw  <- c(6.139, 7.541, 10.26, 9.725, 9.027, 9.058, 8.566, 7.581)

#3. Estimación por MCO
modelo_phi <- lm(precios ~  gas_gw)
summary(modelo_phi)

# 4. Generación del código LaTeX

stargazer(modelo_phi, 
          type = "text",     
          title = "Resultados de la Regresión",
          dep.var.labels = "Precio (Pt)",
          covariate.labels = "Gas GW (Dt - Xt)")


stargazer(modelo_phi, 
          type = "latex", 
          header = FALSE,    # Quita mensajes basura de la cabecera
          title = "Estimación del parámetro $\\phi$",
          dep.var.labels = "Precio ($P_t$)",
          covariate.labels = "Gas GW ($D_t - X_t$)")

