basedatos <- read.csv("basedatos.csv")
setwd(dir = "C:/Users/aespe/OneDrive/Documents/UC/2023-2/Cuanti/Taller 1")
setwd("C:/Users/aespe/OneDrive/Documents/UC/2023-2/Cuanti/Taller 1")

basedatos = read.csv("basedatos.csv",header=TRUE, sep=",")
basedatos$X <- NULL

#PREGUNTA 1
# Extraer las variables de la base de datos como vectores.
p <- basedatos$Puntaje # Variable dependiente
t <- basedatos$Tiempo
a <- basedatos$Autopercepcion
g <- basedatos$Genero

# Generar gráfico de dispersión 1: Puntaje (dependiente) vs tiempo de estudio
scatter.smooth(t,p,main="Puntaje vs Tiempo", xlab="Tiempo", ylab="Puntaje")

# Generar gráfico de dispersión 2: Puntaje (dependiente) vs autopercepción
scatter.smooth(a,p,main="Puntaje vs Autopercepcion", xlab="Autopercepcion", ylab="Puntaje")

# Generar gráfico de dispersión 3: Puntaje (dependiente) vs género
scatter.smooth(g,p,main="Puntaje vs Genero",xlab="Genero", ylab="Puntaje")

# Generar consolidados de los tres gráficos
par(mfrow=c(1,1)) # Divide la pantalla en 1 fila y 3 columnas
scatter.smooth(t,p,main="Puntaje vs Tiempo", xlab="Tiempo", ylab="Puntaje")
scatter.smooth(a,p,main="Puntaje vs Autopercepcion", xlab="Autopercepcion", ylab="Puntaje")
scatter.smooth(g,p,main="Puntaje vs Genero",xlab="Genero", ylab="Puntaje")


# PREGUNTA 2
cor(basedatos[,c("Puntaje","Tiempo","Autopercepcion","Genero")])
round(cor(basedatos[,c("Puntaje","Tiempo","Autopercepcion","Genero")]),2)

# Calcular correlaciones por pares para obtener nivel de significancia
cor.test(p,t) # Correlación puntaje - tiempo
cor.test(p,a) #Correlación puntaje - autopercepción
cor.test(p,g) #Correlación puntaje género

# PREGUNTA 3

#Primer modelo
modelo1 <- lm(p ~ t + a + g)
summary(modelo1)

# Segundo modelo
modelo2 <- lm (p ~ t + a)
summary(modelo2)

# Tercer modelo
modelo3 <- lm(p ~ t)
summary(modelo3)

# Cuarto modelo
modelo4 <- lm(p ~ a)
summary(modelo4)

# Quinto modelo
modelo5 <- lm (p ~ g)
summary(modelo5)

# PREGUNTA 4

# Intervalo de 95% de confiabilidad para parámetros en primer modelo
confint(modelo1, conf.level = 0.95)

# Intervalo de 95% de confiabilidad para parámetros en segundo modelo
confint(modelo2, conf.level = 0.95)

# PREGUNTA 5

predictores1 <- predict(modelo2, newdata = data.frame(t = -2, a = 3))
predictores2 <- predict(modelo2, newdata = data.frame(t = -2, a = 7))
aumento <- predictores2 - predictores1

# PREGUNTA 6

# Generar intervalos de predicción para los modelos 3, 4 y 5
yp3_hat <- predict(modelo3, basedatos, interval="prediction")
yp4_hat <- predict(modelo4, basedatos, interval="prediction")
yp5_hat <- predict(modelo5, basedatos, interval="prediction")

# Rescatar la primera columna, con los valores predichos por los modelos
yp3_h <- yp3_hat[,1]
yp4_h <- yp4_hat[,1]
yp5_h <- yp5_hat[,1]

# Establecer el tamaño de la ventana gráfica
window(width=8.5, height=6)

#Generar gráfico para Puntaje ~ Tiempo
plot(t,p,type="p", col="red", main="Predicciones puntuales Modelo 3", xlab="Tiempo", ylab="Puntaje")
lines(t,yp3_h, col="black")
legend(-5.0,270,c("Valores Observador", "Predicciones"), lty=c(2,1), col=c("red", "black"))

#Generar gráfico para Puntaje ~ Autopercepción
plot(a,p,type="p", col="red", main="Predicciones puntuales Modelo 4",xlab="Autopercepcion", ylab="Puntaje")
lines(a,yp4_h,col="black")
legend(-5.0,270,c("Valores Observados","Predicciones"),lty=c(2,1),col=c("red","black"))

#Generar gráfico para Puntaje ~ Género
plot(g,p,type="p", col="red", main="Predicciones puntuales Modelo 5",xlab="Genero", ylab="Puntaje")
lines(g,yp5_h,col="black")
legend(-5.0,270,c("Valores Observados","Predicciones"),lty=c(2,1),col=c("red","black"))

#Calcular la suma cuadrada de los residuos
residuos3 <- (basedatos[,2] - yp3_h)^2
sumr3 <- sum(residuos3)

residuos4 <- (basedatos[,2] - yp4_h)^2
sumr4 <- sum(residuos4)

residuos5 <- (basedatos[,2] - yp5_h)^2
sumr5 <- sum(residuos5)

#Calcular el porcentaje de varianza explicada
y_promedio <- replicate(nrow(basedatos), mean(p)) #Crear un vector con el promedio de los puntajes
var_total <- (basedatos[,2] - y_promedio)^2 #Calcular la varianza total del puntaje
var_total <- sum(var_total) #Calcular la suma de la varianza total

porcentaje3 <- 100*(sumr3/var_total)
porcentaje4 <- 100*(sumr4/var_total)
porcentaje5 <- 100*(sumr5/var_total)
data.frame("% no explicado m3"= porcentaje3,
           "% no explicado m4"= porcentaje4,
           "% no explicado m5"= porcentaje5)
