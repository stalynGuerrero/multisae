#'Model Selection Fay Herriot based on the CME
#'@description
#' Función que seleciona un modelo Fay Herriot  basado
#'
#' en el cuadrado medio del error (CME), con base en todas las posibles
#' combinanciones de \code{p} variables.
#'
#'@param x         \code{data.frame} que contiene las variables para el modelo.
#'@param yhat      Nombre de la columna en \code{x} que indica la variable ha estimar.
#'@param Sd.yhat   Nombre de la columna en \code{x} que indica la desviación estándar
#'                 estimada para \code{yhat}.
#'@param Xk        Vector que contiene los nombres de las variables auxiliares,
#'                 estas deben estar contenidas en \code{x}.
#'@param p         Número máximo de combinaciones a realizar con las covariables,
#'                 por defecto \code{p = 1}.
#'@param y         Opcional, nombre de la columna en \code{x} que tiene los valores
#'                 en la población de la variable estimada.
#'@examples
#'data('etc2013')
#'Xk <- c('x1','x2','x3','x4','x5','x6','x7','x8','x9')
#'Sel.fit <- S.fitFH(etc2013, 'hat.prom', 'hat.sd',Xk, p = 2)
#'Sel.fit <- S.fitFH(etc2013, 'hat.prom', 'hat.sd',Xk, p = 2, y = 'prom')
#'@return
#'Regresa un objeto de clase \code{data.frame}, que tiene las medidas de resumen
#'empleadas para la selección del modelo deseado.
#'\describe{
#'\item{model}{Lista de modelos resultantes de \eqn{\sum_{i=1}^{p} {{p}\choose{i}}
#'\item{loglike}{Medidas de calidad relativas al modelo basado en la función de verosimilitud}
#'\item{cme.Sample}{Cuadrado medio del error obtenido para las observaciones
#'                  sin datos faltantes en las covariables}
#'\item{N.sample}{Número de observaciones sin datos faltantes}
#'\item{cme.predict}{Cuadrado medio del error obtenido para los pronósticos}
#'\item{N.predict}{Número de observaciones con prónostico}
#'\item{cme.predictIMP}{Cuadrado medio del error obtenido para los prónosticos
#'                      con  observaciones faltantes en las covariables}
#'\item{N.predictIMP}{Número de observaciones con datos faltantes}}
#' @references
#'\describe{
#'\item{-}{Fay, R.E. and Herriot, R.A. (1979). Estimation of income
#' from small places: An application of James-Stein procedures to
#' census data. Journal of the American Statistical Association 74, 269-277.}
#'\item{-}{Marhuenda, Y., Morales, D. and Pardo, M.C. (2014). Information
#' criteria for Fay-Herriot model selection. Computational Statistics and
#'  Data Analysis 70, 268-280}
#' \item{-}{Rao, J.N.K. (2003). Small Area Estimation. Wiley, London.}}
#'@export

S.fitFH <- function(x, yhat, Sd.yhat, Xk, p = 1, y = NULL) {
    x[["Intercept"]]  <- 1
    fit <- function(jj, ...) {
        Xkj <- jj

        if (is.null(y)) {
            y <- yhat
            x$predict <- ifelse(!is.na(x[[yhat]]), "estimation", "predict")
        } else {
            x$predict <- apply(x[, c(yhat, Sd.yhat)], 1,
                               function(x) ifelse(!anyNA(x), "estimation", "predict"))
        }

        temp <- x

        if (length(Xkj) == 1) {
          temp$IMP <- ifelse(is.na(temp[[Xkj]]), "imp", "sin.imp")
        } else {
          temp$IMP <- apply(temp[, Xkj], 1,function(x) ifelse(anyNA(x), "imp","sin.imp"))
        }

        sampleTemp <- subset(temp, predict == "estimation" & IMP == "sin.imp")

        xkj <- paste0("sampleTemp$", Xkj, collapse = "+")

        model <- paste0("model <- eblupFH(as.vector(sampleTemp$",
                        yhat, ")~", xkj, ",vardir=sampleTemp$", Sd.yhat,
                        "^2, method = 'REML')")

        eval(parse(text = model))
        N.sample <- nrow(sampleTemp)
        CME <- sum((model$eblup[, 1] - sampleTemp[[y]])^2)/N.sample

        temp2 <- subset(temp, predict == "predict" & IMP == "sin.imp")

        if (nrow(temp2) > 1 & !all(is.na(temp2[[y]]))) {
            temp.prom <- temp2[[y]]
            predict <- as.matrix(temp2[, c("Intercept", Xkj)]) %*% model$fit$estcoef$beta
            N.predict <- nrow(temp2)
            CME.predict <- sqrt(sum((predict[, 1] - temp.prom)^2, na.rm = T)/N.predict)
        } else {
          N.predict <- 0
          CME.predict <- 0
        }

        temp2 <- subset(temp,IMP == "imp")
        temp2 <- temp2[!is.na(temp2[[y]]),]
        if (nrow(temp2) > 5) {
            temp.prom <- temp2[[y]]
            N.predictIMP <- nrow(temp2)

            imput <- tryCatch(mice(temp2[, Xkj], method = "norm.boot",printFlag = FALSE), error = function(e) NULL)

            if (is.null(imput) || anyNA(complete(imput))) {
                CME.predictIMP <- 0
            } else {
                temp2[, Xkj] <- complete(imput)
                predict <- as.matrix(temp2[, c("Intercept", Xkj)]) %*% model$fit$estcoef$beta
                CME.predictIMP <- sqrt(sum((predict[, 1] - temp.prom)^2)/N.predictIMP)
                   }
        } else {
          N.predictIMP  <- 0
          CME.predictIMP <- 0
        }

        return(c(model$fit$goodness[1:4], cme.Sample = sqrt(CME), N.sample = N.sample,
                 cme.predict = CME.predict, N.predict = N.predict,
                 cme.predictIMP = CME.predictIMP, N.predictIMP = N.predictIMP))
    }

    Resultado <- list()
    for (i in 1:p) {
        Cov.xk <- data.frame(fit = t(combn(Xk, i)))
        model <- t(apply(Cov.xk, 1, function(jj) {
            fit(jj, x, yhat, Sd.yhat, y)}))
        if (i > 1) {
            Cov.xk <- apply(Cov.xk, 2, as.character)
            Cov.xk <- apply(Cov.xk, 1, function(x) paste0(x, collapse = "+"))
            Cov.xk <- data.frame(fit = Cov.xk)
        }
        Resultado[[i]] <- cbind(Cov.xk, model)
    }
    Resultado <- do.call("rbind", Resultado)
    print(Resultado[order(Resultado[["loglike"]][1:p]),])
    Resultado <- return(data.frame(Resultado))

}
