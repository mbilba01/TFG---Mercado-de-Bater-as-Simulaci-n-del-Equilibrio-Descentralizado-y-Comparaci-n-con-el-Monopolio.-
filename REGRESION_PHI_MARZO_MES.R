
#1.Carga del fichero de datos extraido de California
library(readxl)
datos <- read.csv("C:/Users/maria/Desktop/QUINTO/0. TFG ECO/TFG REGRESION PHI.csv", sep=";",header=TRUE,dec=",")

#2.Para trabajar con datos panel
#install.packages("plm")
library(plm)

pdata <- pdata.frame(datos,index=c("OPR_DT","OPR_HR"))

#View(pdata) 
#pdim(pdata) Datos balanceados 


pdata$PRECIO <- as.numeric(as.character(pdata$PRECIO))
pdata$GASGW  <- as.numeric(as.character(pdata$GASGW))

#3. Estimación por modelo pool
modelo_pool<-plm(PRECIO ~ GASGW, data=pdata,model="pooling")
summary(modelo_pool)


#4. Estimación por modelo de efectos fijos
modelo_ef<-plm(PRECIO ~ GASGW, data=pdata,model="within")
summary(modelo_ef)

#5. Estimación por modelo de efectos aleatorios
modelo_ea<-plm(PRECIO ~ GASGW, data=pdata,model="random")
summary(modelo_ea)

#Test comparación: efectos fijos y Pool
pFtest(modelo_ef,modelo_pool) #EFECTOS FIJOS PREFERIBLE A POOL

#Test comparación: efectos fijos y aleatorios
phtest(modelo_ef,modelo_ea) #EFECTOS FIJOS PREFERIBLE A EFECTOS ALEATORIOS

#Test comparación: efectos aleatorios y Pool
plmtest(modelo_pool,type="bp") #POOL PREFERIBLE A EFECTOS ALETORIOS

# Seleccionamos efectos fijos

#install.packages("stargazer")
library(stargazer)

stargazer(modelo_ef, 
          type = "text",     
          title = "Resultados de la Regresión",
          dep.var.labels = "Precio (Pt)",
          covariate.labels = "Gas GW (Dt - Xt)")


stargazer(modelo_ef, 
          type = "latex", 
          header = FALSE,   
          title = "Estimación del parámetro $\\phi$",
          dep.var.labels = "Precio ($P_t$)",
          covariate.labels = "Gas GW ($D_t - X_t$)")