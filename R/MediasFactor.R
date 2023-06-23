#'Medias por nivel de cada Factor
#'
#'Hace el cálculo de Medias para cada nivel de cada factor del diseño Factorial.
#'
#'@param f (integer) Indica el factor del cual se pretende calcular las medias, 1 es para el Factor A, y 2 para el Factor B.
#'@param o (integer) Indica el número de observaciones para cada factor.
#'@return Devuelve una lista con las medias por cada nivel del factor.
#'@export
#'
#'@examples
#'\dontrun{
#' #----------------------------------------------------------------------------
#' #EJEMPLO
#' #Si se rechaza la Hipótesis Nula mediante los valores de F calculada y F de tablas obtenidas en la tabla
#' #Anova, se hace un calculo de Medias por nivel del Factor.
#'
#' ##Se ejecuta la función para calcular la Medias por Factor
#' Medias(f = "1", o = "8")
#' }
MediasFactor <- function(f, o){
##########Calculo de medias de los dos niveles del Factor A##########
  if (f == 1) {
    sumaNFactor <- tapply(df$Respuesta, df$Factor.A, FUN = sum)
  } else if (f == 2) {
    sumaNFactor <- tapply(df$Respuesta, df$Factor.B, FUN = sum)
  }
  resultado <- sumaNFactor / o
  return(resultado)
}
