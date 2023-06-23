#'Tabla Anova de Diseño Factorial
#'
#'Analiza los datos obtenidos de un diseño factorial, es decir, nos ayuda a
#'analizar los resultados que obtenemos de un Diseño Factorial 2x2, mediante una
#'Tabla Anova, para poder concluir si se rechaza o no la Hipótesis Nula.
#'
#'@param Datos (dataframe) Tabla que contiene los datos a analizar, donde se
#'deberán tener 5 columnas, la primera que indique los niveles del Factor A,
#'la segunda los niveles del Factor B, la tercera los Tratamientos, otra con las
#'Réplicas y la última que contenga las Respuestas.
#'@param a (integer) Número de niveles del Factor A.
#'@param b (integer) Número de niveles del Factor B.
#'@param r (integer) Número de réplicas de cada tratamiento.
#'@param alfa (numeric) Indica la significancia que se tomará para el diseño experimental.
#'@return Devuelve la Tabla Anova que contendrá los grados de libertad, la Suma de
#'Cuadrados, los Cuadrados Medios, los valores de F calculada y de F de tablas.
#'@export
#'
#'@examples
#'\dontrun{
#' ##Limpiar la memoria de R
#' rm(list = ls())
#' #----------------------------------------------------------------------------
#' #EJEMPLO 1
#'
#' ##Se establece la ruta del archivo y se fija
#' ruta <- "C:.../Datos"
#' setwd(ruta)
#'
#' ##Importar la base de datos a un Dataframe
#' datos <- read.csv(file = "Datos.csv")
#' df <- data.frame(datos)
#' print(df)
#'
#' ##Se carga la librería
#' library(DisFac)
#'
#' ##Se ejecuta la función para obtener la Tabla Anova
#' TablaAnovaDF(Datos = "df", a = "2", b = "2", r = "4", alfa = "0.05")
#' }
AnovaDF <- function(Datos, a, b, r, alfa){
##########Se realiza la gráfica de interacciones##########
par(mfrow = c(1,2))
  ##Gráfico de interacciones del Factor A
interaction.plot(x.factor = df$Replica, trace.factor = df$Factor.A, response = df$Respuesta,
                   type = "b", legend = TRUE, xlab = "Réplica", ylab = "Respuesta",
                   col = c("red", "blue"), lty = c(1, 2), pch = c(16, 17))
  ##Gráfica de interacciones del factor B
interaction.plot(x.factor = df$Replica, trace.factor = df$Factor.B, response = df$Respuesta,
                   type = "b", legend = TRUE, xlab = "Réplica", ylab = "Respuesta",
                   col = c("black", "brown"),
                   lty = c(1, 2), pch = c(15, 19))

##########Calculos para realizar la Tabla Anova##########

##Codificar las columnas, como factor
df$Replica <- factor(df$Replica)
df$Tratamiento <- factor(df$Tratamiento)

##Grados de libertad
glA <- a-1
glB <- b-1
glAB <- (a-1)*(b-1)
glE <- (a*b)*(r-1)
glTotal <- (a*b*r)-1

##Suma de Cuadrados
sumaRespuesta <- sum(df$Respuesta)
FC <- (sumaRespuesta^2)/(a*b*r)
sumaTrat <- tapply(df$Respuesta, INDEX = df$Tratamiento, FUN = sum)
SCA <- ((((sumaTrat[1]+sumaTrat[2])^2)+((sumaTrat[3]+sumaTrat[4])^2))/(b*r))-FC
SCB <- ((((sumaTrat[1]+sumaTrat[3])^2)+((sumaTrat[2]+sumaTrat[4])^2))/(a*r))-FC
SCAB <- (((sumaTrat[1]^2)+(sumaTrat[2]^2)+(sumaTrat[3]^2)+(sumaTrat[4]^2))/r)-SCA-SCB-FC
SCTotal <- (sum ((df$Respuesta)^2))-FC
SCE <- SCTotal-SCA-SCB-SCAB

##Cuadrados Medios
CMA <- SCA/glA
CMB <- SCB/glB
CMAB <- SCAB /glAB
CME <- SCE/glE

##F Calculada
fA <- CMA/CME
fB <- CMB/CME
fAB <- CMAB/CME

##F de tablas
FtA <- qf(alfa, glA, glE, lower.tail=FALSE)
FtB <- qf(alfa, glB, glE, lower.tail=FALSE)
FtAB <- qf(alfa, glAB, glE, lower.tail=FALSE)

##########Creación de la Tabla Anova##########
tabla <- data.frame(FuenteVar = c("Factor A", "Factor B", "Interacción AB", "Error", "Total"),
                      GradosLibertad = c(glA, glB, glAB, glE, glTotal),
                      SumaCuadrados = c(SCA, SCB, SCAB, SCE, SCTotal),
                      CuadradosMedios = c(CMA, CMB, CMAB, CME, NA),
                      FCalculada = c(fA, fB, fAB, NA, NA),
                      FTablas = c(FtAB, FtB, FtAB, NA, NA),
                      check.names = FALSE)
  rownames(tabla) <- NULL
  Anova <- format(tabla)
  Anova[is.na(tabla)] <- ""
  return(Anova)
}
