#María Belén Bilbao Pavón
#TFG - Mercado de Baterías: Simulación del Equilibrio
#Descentralizado y Comparación con el Monopolio.
#Evaluación de estructuras de mercado en un Problema de coordinación.


# PARTE 1 - PROBLEMA MEANFIELD - BÚSQUEDA DE EQUILIBRIO DESCENTRALIZADO

# 1. Definición de la demanda

gridd = seq(17, 24, by = 0.25)
a=0 
b=25
c=100
tim=length(gridd)

demanda=function(tim,a,b,c){
  y= a*tim^2 + b*tim + c
  resultado = round(pmax(0, y)) #la demanda no puede ser negativa
  return(resultado)
}


valores_demanda=demanda(gridd,a,b,c)
demanda(gridd,a,b,c)

# Datos reales
#valores_demanda=c(24807, 25597, 26206, 26692, 27375, 28200, 28835, 29593, 30139,30745, 31277, 31806, 32004, 32461, 32575, 32947, 33186, 33425, 33783, 33922, 34319, 34364, 34531, 34643, 34778, 34848, 34918,35002, 35077)
# baterias=c(-469,448,1878,5395,8421,9103,11253,11648,11568,11076,10486,9730,8934,8194, 7814, 7018, 6549, 6589, 5944, 4964, 4517, 3659, 2807, 2235, 1719, 1428, 978, 560, 184)

# Gráfica de la demanda
plot(gridd, valores_demanda, type="l",col = "blue", lwd = 2,
     main = "Curva de Demanda Energética",
     xlab = "Hora del día", 
     ylab = "Demanda MW",
     panel.first = grid())

#2. Cáculo de la capacidad de cable L
max_demanda    = max(valores_demanda)
L = max_demanda * 0.33

#3. Definición de Phi - Coste Marginal del Gas

a1=0
a2=2 
#a2=7.81 #Dato real obtenido por regresión
a3=0
phi=function(tim,a1,a2,a3){
  y= a1*tim^2 + a2*tim + a3
  resultado = pmax(0, y)
  return(resultado)
}


#4.Algoritmo: dado S0, phi, D, L y w0 calcula el equilibrio
s_inicial = 500 

w_0=0.45*sum(valores_demanda) 
#w_0=374164.2 #Variación posterior

objetivo_equilibrio = function(s_inicial) {
  # 1. Cálculo de K
  D0 = valores_demanda[1]
  if (s_inicial <= L) {
    K = phi(D0 - s_inicial, a1, a2, a3)
  } else {
    K = (L / s_inicial) * phi(D0 - L, a1, a2, a3)
  }
  
  K = max(1e-6, K)
  
  # 2. Calculo del vector s
  s = numeric(length(valores_demanda))
  for (t in 1:length(valores_demanda)) {
    Dt = valores_demanda[t]
    st_l = Dt - (K - a3) / a2
    
    if (st_l <= L) {
      s[t] = st_l
    } else {
      
      s[t] = (L / K) * phi(Dt-L,a1,a2,a3)
    }
  }
  s = pmax(1e-6, s)
  
  # 3. Devolvemos la diferencia al cuadrado 
  return((sum(s) - w_0)^2)
}

objetivo_equilibrio(s_inicial)


# OPTIMIZACIÓN: Búsqueda de s_inicial en el intervalo [0, w_0] que minimice la funcion
#anterior

optimizacion = optimize(f = objetivo_equilibrio, interval = c(0, w_0))
s_optimo = optimizacion$minimum

#Hallamos la funcion con el valor óptimo para verificar si hay equilibrio
D0 = valores_demanda[1]
if (s_optimo <= L) {
  K = phi(D0 - s_optimo, a1, a2, a3)
} else {
  K = (L / s_optimo) * phi(D0 - L, a1, a2, a3)
}
K = max(1e-6, K)
stim = numeric(length(valores_demanda))
stim[1] = s_optimo

for (t in 2:length(valores_demanda)) {
  Dt = valores_demanda[t]
  st_l = Dt - (K - a3) / a2
  
  if (st_l <= L) {
    stim[t] = st_l
  } else {
    
    stim[t] = (L / K) * phi(Dt-L,a1,a2,a3)
  }
}
stim = pmax(1e-6, stim)
stim

# Verificación
print(paste("S_optimo de equilibrio:", s_optimo))

print(paste("Suma de descargas:", sum(stim)))

print(paste("Diferencia con W0:", sum(stim) - w_0))



#5. Calculo de los sigmas


w = numeric(length(valores_demanda)) 
sigma=numeric(length(valores_demanda))
w[1]=w_0

for(i in 1:tim){
  w[i+1]=pmax(w[i]- stim[i],1e-6)
  
}

for (b in 1:tim){
  if(w[b] > 1e-3){
    sigma[b] = stim[b] / w[b]
  } else {
    sigma[b] = 0 # Si no hay stock, la propensión es 0
  }
}

w 
stim 
sigma


#6. Visualización de la diferencia de sum(stim) y W_0 en función de S_0


s_prueba = seq(1, L, length.out = 100)
errores = numeric(length(s_prueba))

for(i in 1:length(s_prueba)) {
  errores[i] = objetivo_equilibrio(s_prueba[i]) 
}

# Generamos la gráfica
plot(s_prueba, errores, type="l", col="red", lwd=2,
     main="Búsqueda del Equilibrio",
     xlab="S_inicial",
     ylab="Diferencia con W0",
     panel.first = grid())


par(mar=c(5, 6, 4, 2)) 

plot(s_prueba, errores, type="l", col="red", lwd=2,
     main="Búsqueda del Equilibrio",
     xlab=expression(S[inicial]), 
     ylab=expression(sum(S[t], t==0, T-1) - W[0]),
     panel.first = grid())

# Añadimos un punto con el s_optimo encontrado
points(s_optimo, sqrt(objetivo_equilibrio(s_optimo)), col="blue", pch=19, cex=1.5)
abline(h=0, lty=2, col="black") 


# 7. Cálculo de cantidades, precios y Gasto Total
x = pmin(L, stim)   # Cantidad real que pasa por el cable
precios = numeric(tim)
gasto_t = numeric(tim)

for (t in 1:tim) {
  # El precio depende de la demanda neta (Demanda - lo que aporta la batería)
  precios[t] = phi(valores_demanda[t] - x[t], a1, a2, a3)
  gasto_t[t] = precios[t] * x[t]
}

#Gasto Total
total_mercado = sum(gasto_t)
print(paste("Gasto total del mercado:", total_mercado))

#8. Ahorro
#8.1 Ahorro del consumidor

precio_sin_bateria = numeric(tim)
for (t in 1:tim) {
  precio_sin_bateria[t] = phi(valores_demanda[t], a1, a2, a3)
}

# 8.2 El precio  con baterías (precios)

ahorro_periodo = (precio_sin_bateria - precios) * valores_demanda

# 8.3.Ahorro Total
excedente_total = sum(ahorro_periodo)

print(paste("Ahorro del Consumidor:", round(excedente_total, 2)))


#8.4. Gráfica para comparar precios con y sin baterías
y_max = max(precio_sin_bateria) * 1.2 
y_min = min(precios) * 0.9

plot(gridd, precio_sin_bateria, type="o", col="red", pch=4,
     ylim=c(y_min, y_max), 
     main="Efecto de las Baterías en el Precio",
     xlab="Hora", ylab="Precio ($)", panel.first = grid())

lines(gridd, precios, type="o", col="green", pch=19)

legend("topleft", 
       legend=c("Precio sin Batería", "Precio con Batería (Equilibrio)"),
       col=c("red", "green"), 
       pch=c(4, 19), 
       cex=0.7, 
       bty="n")

excedente_MFG = excedente_total


# PARTE 2 - MONOPOLIO Y COMPARACIÓN
#9. Monopolio
precio_sin_bateria = phi(valores_demanda, a1 = 0, a2, a3)
evaluar_K_monopolio = function(K_propuesto) {
  if(K_propuesto < 0) return(1e10)
  s_m = numeric(tim)
  reserva = w_0
  for (t in 1:tim) {
    st_opt = (a2 * valores_demanda[t] + a3 - K_propuesto) / (2 * a2)
    s_m[t] = pmax(0, pmin(L, st_opt, reserva))
    reserva = reserva - s_m[t]
  }
  precios_m = phi(valores_demanda - s_m, a1, a2, a3)
  beneficio = sum(s_m * precios_m)
  return(-beneficio) 
}

opt_K = optimize(f = evaluar_K_monopolio, interval = c(0, max(precio_sin_bateria)))
K_optimo = opt_K$minimum

s_monopolio = numeric(tim)
res = w_0
for (t in 1:tim) {
  st = (a2 * valores_demanda[t] + a3 - K_optimo) / (2 * a2)
  s_monopolio[t] = pmax(0, pmin(L, st, res))
  res = res - s_monopolio[t]
}

print(paste("Ingreso Marginal óptimo encontrado (K):", round(K_optimo, 2)))
print(paste("Suma despachada:", sum(s_monopolio)))
s_monopolio



# 10. Cálculo de cantidades, precios y gasto total
x = pmin(L, s_monopolio)            # Cantidad real que pasa por el cable
precios = numeric(tim)
gasto_t = numeric(tim)

for (t in 1:tim) {
  # El precio depende de la demanda neta (Demanda - lo que aporta la batería)
  precios[t] = phi(valores_demanda[t] - x[t], a1, a2, a3)
  gasto_t[t] = precios[t] * x[t]
}

#Gasto Total
total_mercado = sum(gasto_t)
print(paste("Gasto total del mercado en Monopolio:", total_mercado))

#11. Cuantificación del Ahorro
#11.1 Ahorro del consumidor 

precio_sin_bateria = numeric(tim)
for (t in 1:tim) {
  precio_sin_bateria[t] = phi(valores_demanda[t], a1, a2, a3)
}

# 11.2 El precio  con baterías (precios)

ahorro_periodo = (precio_sin_bateria - precios) * valores_demanda

#11.3. Ahorro Total
excedente_total = sum(ahorro_periodo)

print(paste("Ahorro del Consumidor en Monopolio:", round(excedente_total, 2)))
excedente_MON = excedente_total

#11.4. Gráfica para comparar precios con y sin baterías 
y_max = max(precio_sin_bateria) * 1.2 
y_min = min(precios) * 0.9

plot(gridd, precio_sin_bateria, type="o", col="red", pch=4,
     ylim=c(y_min, y_max), 
     main="Efecto de Baterías en Precio en Monopolio",
     xlab="Hora", ylab="Precio ($)", panel.first = grid())

lines(gridd, precios, type="o", col="green", pch=19)

legend("topleft", 
       legend=c("Precio sin Batería", "Precio con Batería"),
       col=c("red", "green"), 
       pch=c(4, 19), 
       cex=0.7, 
       bty="n")


print(paste("Ahorro Consumidor (MEAN FIELD):", round(excedente_MFG, 2)))
print(paste("Ahorro Consumidor (MONOPOLIO):", round(excedente_MON, 2)))

#12.1 Gráfico de Comparación Global de D_t S_MFE Y S_MON


plot(gridd, valores_demanda, type="l", col="darkgrey", lwd=2, lty=2,
     main = "Demanda vs MFE vs Monopolio",
     xlab = "Hora del día", 
     ylab = "Energía MW",
     ylim = c(0, max(valores_demanda, stim, s_monopolio) * 1.1), 
     panel.first = grid())
lines(gridd, stim, col = "blue", lwd = 2)
lines(gridd, s_monopolio, col = "darkgreen", lwd = 2)
legend("top",                           # La ponemos arriba al centro
       legend = c("Demanda", "MFE", "Monopolio"), 
       col = c("darkgrey", "blue", "darkgreen"), 
       lwd = 2, 
       lty = c(2, 1, 1), 
       horiz = TRUE,                    
       bty = "n",                       
       cex = 0.7,                       
       inset = c(0, -0.05),             
       xpd = TRUE)

#12.2 Porcentaje de la Demanda Satisfecha
stim_truncado <- pmin(stim, L)
porcentaje_demanda_mfe = stim_truncado/valores_demanda
porcentaje_demanda_monopolio=s_monopolio/valores_demanda

plot(gridd, porcentaje_demanda_mfe, type = "l", col = "blue", lwd = 2,
     main = "% Demanda Satisfecha por Baterías",
     xlab = "Hora del día", 
     ylab = "Ratio (Despacho / Demanda)",
     ylim = c(0, max(porcentaje_demanda_mfe, porcentaje_demanda_monopolio) * 1.2),
     panel.first = grid())


lines(gridd, porcentaje_demanda_monopolio, col = "darkgreen", lwd = 2)

legend("topright", 
       legend = c("MFE", "Monopolio"),
       col = c("blue", "darkgreen"), 
       lwd = 2, 
       bty = "n",
       cex = 0.7) 

#13. Tamaño del error
error_absoluto = sum(stim) - w_0
error_relativo = (abs(error_absoluto) / w_0) * 100

cat("Error  absoluto:", error_absoluto, "\n")
cat("Error relativo sobre W0:", round(error_relativo, 6), "%\n")

#14.Comprobación Dispatch vs Cantidad almacenada por periodo
# Si el total despachado es mayor que el total disponible,
#¿en qué período se está generando el desajuste?

stock_inicial_periodo = numeric(tim)
stock_inicial_periodo[1] = w_0  

for (t in 2:tim) {
  stock_inicial_periodo[t] = stock_inicial_periodo[t-1] - stim[t-1]
}


tabla_comprobacion = data.frame(
  Hora = gridd,
  Stock_Disponible = round(stock_inicial_periodo, 4),
  Despacho_Propuesto = round(stim, 4),
  Diferencia = round(stock_inicial_periodo - stim, 4)
)

desajustes = which(tabla_comprobacion$Diferencia < -1e-7)


print(tabla_comprobacion)

if (length(desajustes) > 0) {
  cat("\n¡ATENCIÓN! Se ha detectado desajuste en los siguientes periodos:\n")
  print(tabla_comprobacion[desajustes, ])
} else {
  cat("\nResultado: No hay desajustes. En todos los periodos, el despacho es <= al stock disponible.\n")
}


