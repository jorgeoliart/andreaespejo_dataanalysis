basedatos <- read.csv("basedatos.csv")
setwd(dir = "C:/Users/aespe/OneDrive/Documents/UC/2023-2/Cuanti/Taller 1")
setwd("C:/Users/aespe/OneDrive/Documents/UC/2023-2/Cuanti/Taller 1")

basedatos = read.csv("basedatos.csv",header=TRUE, sep=",")
basedatos$X <- NULL

#PREGUNTA #1
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
par(mfrow=c(1,3)) # Divide la pantalla en 1 fila y 3 columnas
scatter.smooth(t,p,main="Puntaje vs Tiempo", xlab="Tiempo", ylab="Puntaje")
scatter.smooth(a,p,main="Puntaje vs Autopercepcion", xlab="Autopercepcion", ylab="Puntaje")
scatter.smooth(g,p,main="Puntaje vs Genero",xlab="Genero", ylab="Puntaje")


# PREGUNTA #2
cor(basedatos[,c("Puntaje","Tiempo","Autopercepcion","Genero")])
round(cor(basedatos[,c("Puntaje","Tiempo","Autopercepcion","Genero")]),2)

# Calcular correlaciones por pares para obtener nivel de significancia
cor.test(p,t) # Correlación puntaje - tiempo
cor.test(p,a) #Correlación puntaje - autopercepción

# PREGUNTA #3

#Primer modelo
modelo1 <- lm(p ~ t + a + g)
summary(modelo1)

# Segundo modelo
modelo2 <- lm(p ~ t)
summary(modelo2)

# Tercer modelo
modelo3 <- lm(p ~ a)
summary(modelo3)

# Cuarto modelo
modelo4 <- lm (p ~ t + a)
summary(modelo4)

# Quinto modelo
modelo5 <- lm (p ~ g)
summary(modelo5)
