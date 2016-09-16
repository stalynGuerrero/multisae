#'Model Selection Fay Herriot based on the CME
#'@description
#' Function that selects a Fay Herriot model based on the mean squared error (MSE),
#' based on all possible combinations of \code{p} variables
#'@param x         \code{data.frame} which contains the model´s variables.
#'@param yhat      Name of the column in \code{x} which indicates the variable to estimate.
#'@param Sd.yhat   Name of the column in \code{x} which indicates the standard deviation
#'                 estimated for \code{yhat}.
#'@param Xk        Vector which contains the names of the auxiliary variables. These must
#'                 be contained in \code{x}.
#'@param p         Maximum number of combinations to make with the covariantes.
#'                 By default \code{p = 1}.
#'@param y         Optional. Name of the column in \code{x} that has the actual values
#'                 of the estimated variable.
#'@examples
#'data('etc2013')
#'Xk <- c('x1','x2','x3','x4','x5','x6','x7','x8','x9')
#'Sel.fit <- S.fitFH(etc2013, 'hat.prom', 'hat.sd',Xk, p = 2)
#'Sel.fit <- S.fitFH(etc2013, 'hat.prom', 'hat.sd',Xk, p = 2, y = 'prom')
#'@return Returns an object of the \code{data.frame} class, which has the summary measures
#' employed for the selection of the desired model.
#'\describe{
#'\item{model}{List of models resulting from \eqn{\sum_{i=1}^{p} {{p}\choose{i}}}}
#'\item{loglike}{Quality measures related to the model based on the likelihood function}
#'\item{mse.Sample}{Mean squared error obtained for observations without missing data in their covariantes.}
#'\item{N.sample}{Number of observations without missing data.}
#'\item{mse.predict}{Mean squared error obtained for the forecasts.}
#'\item{N.predict}{Number of observations with prediction}
#'\item{mse.predictIMP}{Mean square error obtained for predictions with observations missing covariates}
#'\item{N.predictIMP}{Number of observations with missing data}
#'}
#'@references
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
        mse <- sum((model$eblup[, 1] - sampleTemp[[y]])^2)/N.sample

        temp2 <- subset(temp, predict == "predict" & IMP == "sin.imp")

        if (nrow(temp2) > 1 & !all(is.na(temp2[[y]]))) {
            temp.prom <- temp2[[y]]
            predict <- as.matrix(temp2[, c("Intercept", Xkj)]) %*% model$fit$estcoef$beta
            N.predict <- nrow(temp2)
            mse.predict <- sqrt(sum((predict[, 1] - temp.prom)^2, na.rm = T)/N.predict)
        } else {
          N.predict <- 0
          mse.predict <- 0
        }

        temp2 <- subset(temp,IMP == "imp")
        temp2 <- temp2[!is.na(temp2[[y]]),]
        if (nrow(temp2) > 5) {
            temp.prom <- temp2[[y]]
            N.predictIMP <- nrow(temp2)

            imput <- tryCatch(mice(temp2[, Xkj], method = "norm.boot",printFlag = FALSE), error = function(e) NULL)

            if (is.null(imput) || anyNA(complete(imput))) {
                mse.predictIMP <- 0
            } else {
                temp2[, Xkj] <- complete(imput)
                predict <- as.matrix(temp2[, c("Intercept", Xkj)]) %*% model$fit$estcoef$beta
                mse.predictIMP <- sqrt(sum((predict[, 1] - temp.prom)^2)/N.predictIMP)
                   }
        } else {
          N.predictIMP  <- 0
          mse.predictIMP <- 0
        }

        return(c(model$fit$goodness[1:4], mse.Sample = sqrt(mse), N.sample = N.sample,
                 mse.predict = mse.predict, N.predict = N.predict,
                 mse.predictIMP = mse.predictIMP, N.predictIMP = N.predictIMP))
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
