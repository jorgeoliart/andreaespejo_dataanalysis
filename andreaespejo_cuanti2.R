set.seed(280851373) #fijar set.seed para fijar la aleatorización de la muestra

#asegurarse de usar las versiones de R y de las librerías instaladas para poder reproducir el código
R.Version()
#versión de R: 4.3.0

#cargar las librerías a ser usadas

library(dplyr) #librería: dplyr, Versión: 1.1.2, permite agrupar y resumir estadísticos
library(ggplot2) #librería ggplot2, permite visualizar las distribuciones de datos
library(car) #librería car, permite usar el test de Levene

iccs2016 <- readRDS("iccs_2016_stu_int.rds") #importar base de datos

#filtrar para crear base de datos de Chile solamente
iccs_chile <- subset(iccs2016, IDCNTRY == 152)

#filtrar para crear base de datos reducida
iccs_Chile_reducida <- iccs_chile[, c("IDSTUD", "S_GENDER", "PV1CIV", "IS3G24G", "IS3G15J", "IS3G16B", "S_POLPART")]

#PREGUNTA 1
#crear muestra de 2500 casos
muestra_n2500 <- dplyr::slice_sample(
  iccs_Chile_reducida,
  n = 2500,
  replace = TRUE
)

#PREGUNTA 2
#obtener estadísticos de tendencia central y dispersión de PV1CIV, según pertenencia a IS3G24G
muestra_medidas_concivico <- muestra_n2500 %>% 
  group_by(IS3G24G) %>%
  summarise(promedio = mean(PV1CIV),
            mediana = median(PV1CIV),
            desvest = sd(PV1CIV),
            rango =  max(PV1CIV) - min(PV1CIV))

#PREGUNTA 3
#comparar medias con prueba ANOVA

muestra_n2500$IS3G24G <- as.factor(muestra_n2500$IS3G24G) # convertir la variable IS3G24G a categórica

#verificar supuestos: normalidad de cada grupo
#crear cuatro submuestras según el valor en PV1C
womenpriority_1 <- muestra_n2500[muestra_n2500$IS3G24G == 1, ]
womenpriority_2 <- muestra_n2500[muestra_n2500$IS3G24G == 2, ]
womenpriority_3 <- muestra_n2500[muestra_n2500$IS3G24G == 3, ]
womenpriority_4 <- muestra_n2500[muestra_n2500$IS3G24G == 4, ]

#correr lo tests de shapiro-wilk para cotejar normalidad de muestras
shapiro_wp1 <- shapiro.test(womenpriority_1$PV1CIV)
shapiro_wp2 <- shapiro.test(womenpriority_2$PV1CIV)
shapiro_wp3 <- shapiro.test(womenpriority_3$PV1CIV)
shapiro_wp4 <- shapiro.test(womenpriority_4$PV1CIV)

#generar gráfico para observar medias y varianzas
boxplot_distribucion <- boxplot(PV1CIV ~ IS3G24G, data = muestra_n2500, ylab = "Conocimiento cívico", xlab = "Actitud hacia la maternidad como el rol prioritario de las mujeres")

#correr test de Levene
resultadoLevene <- leveneTest(muestra_n2500$PV1CIV ~ muestra_n2500$IS3G24G)

#normalizar la distribución variable dependiente
muestra_n2500$PV1CIV_log <- log(muestra_n2500$PV1CIV)

#correr ANOVA para comparar en puntaje, según el valor en IS3G24G
modeloAnova <- aov(PV1CIV_log ~ IS3G24G, data = muestra_n2500) #Ajustar el modelo
tablaAnova <- anova(modeloAnova) #Generar tabla ANOVA

#comparar pares de grupos con t de student

#antes, normalizar cuatro submuestras
womenpriority_1$PV1CIV_log <- log(womenpriority_1$PV1CIV)
womenpriority_2$PV1CIV_log <- log(womenpriority_2$PV1CIV)
womenpriority_3$PV1CIV_log <- log(womenpriority_3$PV1CIV)
womenpriority_4$PV1CIV_log <- log(womenpriority_4$PV1CIV)

#correr pruebas de t de student entre pares de grupos
t_student_1_2 <- t.test(womenpriority_1$PV1CIV_log, womenpriority_2$PV1CIV_log) #t student entre g1 y g2
t_student_1_3 <- t.test(womenpriority_1$PV1CIV_log, womenpriority_3$PV1CIV_log) #t student entre g1 y g3
t_student_1_4 <- t.test(womenpriority_1$PV1CIV_log, womenpriority_4$PV1CIV_log) #t student entre g1 y g4
t_student_2_3 <- t.test(womenpriority_2$PV1CIV_log, womenpriority_3$PV1CIV_log) #t student entre g2 y g3
t_student_2_4 <- t.test(womenpriority_2$PV1CIV_log, womenpriority_4$PV1CIV_log) #t student entre g2 y g4
t_student_3_4 <- t.test(womenpriority_3$PV1CIV_log, womenpriority_4$PV1CIV_log) #t student entre g3 y g4

#PREGUNTA 4

#convertir las variables a ser testeadas a data tipo factor
muestra_n2500$IS3G16B <- as.factor(muestra_n2500$IS3G16B) #convertir la variable de participación ciudadana en la escuela
muestra_n2500$IS3G15J <- as.factor(muestra_n2500$IS3G15J) #convertir la variable de participación ciudadana fuera de la escuela

#correr prueba de chi cuadrado
tabla_chicuadrado <- chisq.test(muestra_n2500$IS3G16B, muestra_n2500$IS3G15J)

#PREGUNTA 5

#convertir las variables a ser correlacionadas a data tipo "numeric"
muestra_n2500$PV1CIV <- as.numeric(muestra_n2500$PV1CIV)
muestra_n2500$S_POLPART <- as.numeric(muestra_n2500$S_POLPART)

#correr la correlación de Pearson
tabla_pearson <- cor.test(muestra_n2500$PV1CIV, muestra_n2500$S_POLPART)

#generar un gráfico de dispersión
plot(muestra_n2500$PV1CIV, muestra_n2500$S_POLPART, main = "Gráfico de dispersión", xlab = "Conocimiento cívico", ylab = "Participación política activa esperada")

#generar una línea de regresión
abline(lm(muestra_n2500$S_POLPART ~ muestra_n2500$PV1CIV), col = "red")
