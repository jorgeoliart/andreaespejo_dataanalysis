set.seed(280851373) #fijar set.seed para fijar la aleatorización de la muestra

#asegurarse de usar las versiones de R y de las librerías instaladas para poder reproducir el código
R.Version()
#versión de R: 4.3.0

#cargar las librerías a ser usadas
library(haven) #cargar la librería que permite importar bases de datos en formato .dta

library(dplyr) #cargar librería: dplyr, Versión: 1.1.2, que permite agrupar y resumir estadísticos

#importar base de datos original.dta
base_original <- read_dta('original.dta')

