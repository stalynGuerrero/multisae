#' Small Area Estimation under a Fay Herriot model.
#'@description
#' La función realiza la estimación del mejor predictor lineal  insesgado (EBLUP)
#' basado en el modelo Fay Herriot.

#'@param x            \code{data.frame} que contiene la información necesaria para realizar las estimaciones
#'@param x.predic     Observaciones a las cuales se realizará el pronóstico
#'@param yhat         Nombre de la columna que contiene los valores estimados de la variable
#'@param Sd.yhat      Nombre de la columna de desviación estándar estimada para  \code{yhat}
#'@param xk           Vector de caractres que contiene los nombres de las covariables
#'@param method       Método de estimación empledo por la función \code{\link{eblupFH}}
#'@examples
#'# Lectura de los datos
#'data('etc2013')
#'# Agregar dummys
#'etc2013[["IND"]] <- ifelse(is.na(etc2013[["hat.prom"]]), 'NO','SI')
#'# Identificar observaciones con datos faltantes
#'etc2013[["IMP"]] <- apply(etc2013[,c('x1','x2','x3','x4')], 1,
#'                          function(x){ifelse(anyNA(x),'IMP','NO_IMP')})
#'table(etc2013[,c("IND","IMP")])
#'# Separar las observaciones
#'sampling <- subset( etc2013,IND == 'SI' & IMP == 'NO_IMP')
#'# Variable de pronóstico
#'predic   <- subset(etc2013,!c(IND == 'SI' & IMP == 'NO_IMP'))
#'# Imputar observaciones en para el prónostico
#'imp <- mice::mice(predic[,c('x1','x2','x3','x4')], method = 'norm.boot')
#'predic[,c('x1','x2','x3','x4')] <- complete(imp)
#'
#'FH <- fitFH(x = sampling, yhat  = "hat.prom", Sd.yhat = "hat.sd",
#'                  xk = c('x1','x2','x3','x4'), x.predic = predic)
#'
#'
#'@seealso  \code{\link{S.fitFH}, \link{eblupFH}}
#'@references
#'\describe{
#'\item{-}{Fay, R.E. and Herriot, R.A. (1979). Estimation of income
#' from small places: An application of James-Stein procedures to
#' census data. Journal of the American Statistical Association 74, 269-277.}
#'\item{-}{Marhuenda, Y., Morales, D. and Pardo, M.C. (2014). Information
#' criteria for Fay-Herriot model selection. Computational Statistics and
#'  Data Analysis 70, 268-280}
#' \item{-}{Rao, J.N.K. (2003). Small Area Estimation. Wiley, London.}}

#' @export
fitFH <- function(x, yhat, Sd.yhat, xk, method = "REML",
                     x.predic = NULL) {

    x[["Intercept"]] <- 1


    formula <- paste0("x$", xk, collapse = " + ")

    model <- paste0("fit <- eblupFH(as.vector(x$", yhat, ")~",
                    formula, ",vardir=x$", Sd.yhat, "^2, method ='", method, "')")

    eval(parse(text = model))
    formula <- paste0("xJK$", xk, collapse = "+")
    CME <- NA
    xk <- c("Intercept",xk)
    for (i in 1:nrow(x)) {
        xJK <- x[-i, ]
        model <- paste0("fitJK <- eblupFH(as.vector(xJK$", yhat, ")~",
                        formula, ",vardir=xJK$", Sd.yhat, "^2, method = '", method, "')")

        eval(parse(text = model))
        CME[i] <- as.vector(as.matrix(x[i, xk]) %*% fitJK$fit$estcoef$beta)
    }

    CME <- (CME - x[[yhat]])^2

    resul <- list(fit        = fit$fit,
                  eblupFH    = fit$eblup[, 1],
                  eblupredic = NULL,
                  CME        = sum(CME)/nrow(x))

    if(!is.null(x.predic)){
      x.predic[["Intercept"]] <- 1
      resul$eblupredic <- as.matrix(x.predic[, xk]) %*% fit$fit$estcoef$beta
                         }

print(fit$fit)
resul <- return(resul)
}

