#'Prueba de Diferencia Mínima Significativa
#'
#'Hace una Prueba DMS, para que mediante las medias podamos obtener el mejor tratamiento o combinación de los factores.
#'
#'@param alfa (numeric) Indica la significancia que se tomará para la prube DMS.
#'@param r (numeric) Indica el número de réplicas de cada tratamiento en el experimento.
#'@param glE (numeric) Indica los grados de libertad del Error, que obtendremos de la tabla Anova.
#'@param CME (numeric) Indica el Cuadrado Medio del Error que obtendremos de la Tabla Anova.
#'@return Devuelve dos tablas, la primera que contiene las medias por tratamientos, y la segunda que contendrá las diferencias entre las medias por tratamientos, y señala cuales son mayores respecto al valor del DMS.
#'@export
#'
#'@examples
#'\dontrun{
#' #----------------------------------------------------------------------------
#' #EJEMPLO
#'
#' ##Se ejecuta la función para realizar la Prueba DMS
#' PruebaDms (alfa = "0.05",r = "4", glE = "12", CME = "24.31")
#' }
pDms <- function(alfa, r, glE, CME){
  ##########Calculo de las Medias por Tratamiento##########
  sumaTrat <- tapply(df$Respuesta, INDEX = df$Tratamiento, FUN = sum)
  mediaTrat <- sumaTrat/r

  ##########Obtiene los tratamientos##########
  df$Tratamiento <- factor(df$Tratamiento)
  tratamientos <- levels(df$Tratamiento)

  ##########Calculo de la Diferencia Mínima Significativa##########
  dms <- (qt(1-(alfa/2), glE))*(sqrt((2*CME)/r))
  print(dms)

  ##########Calculo de Diferencias entre Tratamientos##########
  D1 <- abs(mediaTrat[1]-mediaTrat[2])
  D2 <- abs(mediaTrat[1]-mediaTrat[3])
  D3 <- abs(mediaTrat[1]-mediaTrat[4])
  D4 <- abs(mediaTrat[2]-mediaTrat[3])
  D5 <- abs(mediaTrat[2]-mediaTrat[4])
  D6 <- abs(mediaTrat[3]-mediaTrat[4])

  ##########Realiza tabla de Medias por Tratamiento##########
  MediasTratamientos <- data.frame(Tratamiento = c(tratamientos),
                                   TotalTratamiento = c(sumaTrat),
                                   MediaTratamiento = c(mediaTrat))
  print(MediasTratamientos)

  ##########Realiza Tabla de Diferencias entre las medias ##########
  TDiferencias <- data.frame(TRATAMIENTO = c(tratamientos[1], tratamientos[4],tratamientos[3],tratamientos[2]),
                             tratamientos1 = c((paste0(D1,"***")),(paste0(D5,"***")),(paste0(D4,"***")),"0"),
                             tratamientos2 = c(D2, D6, "0", NA),
                             tratamientos3 = c(D3, "0", NA, NA),
                             tratamientos4 = c("0", NA, NA, NA))
  nombres <- c("TRATAMIENTOS", tratamientos[2],tratamientos[3],tratamientos[4],tratamientos[1])
  colnames(TDiferencias) <- nombres
  return(TDiferencias)
}
