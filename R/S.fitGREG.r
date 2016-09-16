#'Model selection  for the GREG based on CME
#'@description
#' Function which selects a model for the GREG based on the mean squared error (MSE),
#' based on all possible combinations that may be obtained from a set of \code{p} variables.
#'@param  x       \code{data.frame} which contains the model’s variables.
#'@param  Xk      Vector of characters with the names of the co-variables to
#'                be used in the model. These must be contained in the columns of  \code{x}.
#'@param  total   Total of the auxiliary variable.
#'@param  prom    Name of the column in \code{x} which indicates the variable to estimate.
#'@param  weight  Expansion factor of each observation.
#'@param  stratum Indicator of the domains in the sample. By default  \code{stratum = NULL}
#'@param  p       Maximum number of combinations to make with the covariantes. By default \code{p = 1}.
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
#'Returns a \code{data.frame} where report is made.
#'\describe{
#'\item{model}{List of models resulting from \eqn{\sum_{i=1}^{p} {{p}\choose{i}}}}
#'\item{R2}{El  \eqn{R^2} for each model definedo.}
#'\item{mse}{Mean squared error obtained in the estimation}}
#'@seealso \code{\link{E.GREG}}
#' @export

S.fitGREG <- function(x, Xk, total, prom, weight, p = 1, stratum = NULL, ...) {
  if (length(Xk) == 1) {
    result <- E.GREG(x, total, Xk, prom, stratum, ...)
  } else {
    if (length(Xk) != length(total)) {
      stop("The length of total and Xk do not match")
    }
    if (p > length(Xk)) {
      stop("The value of p is greater than the length Xk")
    }
    result <- list()
    for (i in 1:p) {
      Cov.xk <- data.frame(model = t(combn(Xk, i)))
      MSE <- apply(Cov.xk, 1, function(jj) {
        E.GREG(x, total = total[jj], Xk = jj, prom, weight, stratum, method = 'linear')
      })
      MSE <- do.call("rbind", MSE)
      if (i > 1) {
        Cov.xk <- apply(Cov.xk, 1, function(x) paste0(x, collapse = "+"))
      }
      result[[i]] <- data.frame(model = Cov.xk, MSE)
    }
    result <- data.frame(do.call("rbind", result))
  }

  return(result)

}
