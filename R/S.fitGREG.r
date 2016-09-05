#'Model selection  for the GREG based on CME
#'@description
#' Función que seleciona un conjunto de covariables para el GREG  basado
#' en el cuadrado medio del error (CME), con base en todas las posibles
#' combinanciones de \code{p} variables.

#'@param  x       \code{data.frame} que contiene las variables para el modelo
#'@param  Xk      Vector que contiene los nombres de las variables auxiliares,
#'                estas deben estar contenidas en \code{x}.
#'@param  total   Total de la variable auxiliar
#'@param  prom    Nombre de la columna en \code{x} que indica la variable ha estimar.
#'@param  weight  Factor de expansión de cada observación
#'@param  stratum Indicadora de los dominios en la muestra, por defecto \code{stratum = NULL}
#'@param  p       Númere máximo de combinaciones a realizar con las covariables,
#'                por defecto \code{p = 1}.
#'@param  ...     Argumentos adicional empleado por la función \code{\link{calib}}.
#'@examples
#'data('ResultStudent')
#'attach(ResultStudent)
#'sampling <- data.frame(student[,c('weight','prop','ses')],
#'                       Domains(student[["urbanicity"]]),
#'                       Domains(student[["schooltype"]]))
#'sampling[["sel"]] <- gl(11,31)
#'Xk<-c('Rural', 'Urbana','No.Oficial', 'Oficial', 'ses')
#'
#'S.fitGREG(x = sampling,total = total,Xk = Xk, prom = 'prop',
#'          weight = 'weight', method = 'linear', p = 3)
#'S.fitGREG(x = sampling,total = total,Xk = Xk, prom = "prop",
#'          weight = 'weight',stratum='sel', method = 'linear',p=3)
#'@return
#'Regresa un \code{data.frame} donde se reporta
#'\describe{
#'\item{model}{Lista de modelos resultantes de \eqn{\sum_{i=1}^{p} {{p}\choose{i}}
#'\item{R2}{El  \eqn{R^2} para cada modelo definido.}
#'\item{cme}{El cuadrado medio del error obtenido en la estimación}}
#'@seealso \code{\link{E.GREG}}
#' @export

S.fitGREG <- function(x, Xk, total, prom, weight, p = 1, stratum = NULL, ...) {
  if (length(Xk) == 1) {
    result <- E.GREG(x, total, Xk, prom, stratum, ...)
  } else {
    if (length(Xk) != length(total)) {
      stop("Las longitud de total y Xk no coinciden")
    }
    if (p > length(Xk)) {
      stop("El valor de p es mayor que la longitud  Xk")
    }
    result <- list()
    for (i in 1:p) {
      Cov.xk <- data.frame(model = t(combn(Xk, i)))
      CME <- apply(Cov.xk, 1, function(jj) {
        E.GREG(x, total = total[jj], Xk = jj, prom, weight, stratum, method = 'linear')
      })
      CME <- do.call("rbind", CME)
      if (i > 1) {
        Cov.xk <- apply(Cov.xk, 1, function(x) paste0(x, collapse = "+"))
      }
      result[[i]] <- data.frame(model = Cov.xk, CME)
    }
    result <- data.frame(do.call("rbind", result))
  }

  return(result)

}
