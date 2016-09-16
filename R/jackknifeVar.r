#' Estimated variance for the estimator GREG using the jackknife method
#'@description
#' The function makes the estimation of variances for the Horvitz-Thompson estimator (TH)
#' and the generalized regression estimator (GREG) when plausible values are employed.
#' The estimation of the variance for a domain is given by the equation
#' \deqn{EE(\hat{\eta}) = \sqrt{\frac{1}{5}\sum_{i=1}^{5}Var_m(\hat{\eta}_i)+
#'          \left(1 + \frac{1}{5}\right)\frac{1}{4}\sum_{i=1}^{5}(\hat{\eta}_i -
#'          \hat{\eta})^2}}
#'
#' where \eqn{Var_m(\hat{\eta}_i)} is the variance of the estimator calculated
#' from the observations and the sampling design. The second term of the equation
#' is estimated by using the jackknife method. In the case of the generalized
#' regression estimator, it is necessary to adjust a lineal model, where
#' the plausible value \eqn{\hat{\eta}_{ij}} included as the response variable, as follows:
#' \deqn{\hat{\eta}_{ij}=\beta X_i+e_{ij}}
#' The GREG variance is estimated for each doman with \eqn{X_i} as auxiliary information,
#' employing the model’s estimated residuals using the expression
#'
#' \deqn{\widehat{Var}(\hat{\eta}_{GREG})=\sum\sum_s\frac{\Delta_{jl}}{\pi_{jl}}g_{js}
#'       \frac{\hat{e}_{js}}{\pi_j}g_{ls}\frac{\hat{e}_{ls}}{\pi_l}}
#'donde
#'\describe{
#'\item{-}{\eqn{g_{js} = 1+\left( \bar{X}-\hat{\bar{X}}\right)^t \left(\sum_m \frac{w_j}{\sigma^2_l}x_jx_j^t\right)^{-1}x_j}}
#'\item{-}{\eqn{\hat{e}_{ij} = \hat{\eta}_{ij} - \hat{\beta} X_i}}
#'\item{-}{\eqn{w_j = \frac{1}{\pi_j}}}
#' }
#'@param x       \code{data.frame} which contains the information necessary to make the estimations
#'@param yhat    Name of the column which contains the estimated values of the variable
#'@param weight  Expansion factor of each observation
#'@param total   Total for the auxiliary variable
#'@param Xk      Vector which contains the names of the covariantes.
#'@param stratum Indicator of the domains in the sample, by default  \code{stratum = NULL}
#'@param ...     Additional arguments employed by the \link{calib} function.

#' @references
#'\describe{
#'\item{-}{Shao, J., & Tu, D. (2012). The jackknife and bootstrap. Springer Science & Business Media.}
#'\item{-}{Rubin, D. B. (2004). Multiple imputation for nonresponse in surveys (Vol. 81). John Wiley & Sons.}
#'\item{-}{Von Davier, M., Gonzalez, E., & Mislevy, R. (2009). What are plausible values and why are
#'        they useful. IERI monograph series, 2, 9-36.}
#'}
#'
#'@return Returns a \code{\link{data.frame}} with the results obtained for the Horvitz-Thompson
#' estimator and the GREG as well as their corresponding variances for the indicated groups and subgroups
#'@examples
#'data('ResultStudent')
#'attach(ResultStudent)
#' sampling <- ResultStudent[["student"]]
#' #Add dummys
#' sampling <- data.frame(sampling[,c("schooltype",'weight','prop','ses')],
#'                       Domains(sampling[["urbanicity"]]),
#'                       Domains(sampling[["schooltype"]]))
#'
#' # Covariantes
#' Xk <- c('Rural', 'Urbana','No.Oficial', 'Oficial', 'ses')
#'
#'jackknifeVar(x=sampling,yhat = "prop",weight = 'weight',
#'             Xk=c("ses", "Rural","Urbana"),
#'             total = total, method='linear')
#'
#'jackknifeVar(sampling,weight = 'weight',yhat = "prop",
#'          Xk = 'ses',total = total, method='linear')

#'jackknifeVar(sampling,yhat = "prop",weight = 'weight',
#'            Xk = 'ses',total = total, method='linear',
#'            stratum = 'schooltype')

#'@seealso  \code{\link{calib}, \link{E.GREG}}
#' @export


jackknifeVar <- function(x, yhat, weight, total, Xk, stratum = NULL,  ...) {


  JK <- function(x, weight, yhat) {
    n <- nrow(x)
    if (n < 6) {
      warning("n es menor que 6 \n")
      return(data.frame(prom = NA, sd = NA, sd.jk = NA, SD = NA, cve = NA))
    }

    theta2 <- paste0("theta2<-data.frame(", paste0("theta", 1:length(yhat),
                                           sep = "=NA", collapse = ","), ")")
    eval(parse(text = theta2))

    if (length(yhat) == 1) {
      bar.HT <- sum(x[[weight]] * x[, yhat])/sum(x[[weight]])
      sd.bar.HT <- 0
      for (i in 1:n) {
      num <-  sum(x[-i, weight] * x[-i, yhat], na.rm = T)
      den <-  sum(x[-i, weight], na.rm = T)
      theta2[i, ] <- num/den
      }

    } else {
      bar.HT <- colSums((x[[weight]] * x[, yhat])/sum(x[[weight]]))
      sd.bar.HT <- sd(bar.HT)
      bar.HT <- mean(bar.HT)
      for (i in 1:n) {
        num <- colSums(x[-i, weight] * x[-i, yhat], na.rm = T)
        den <- sum(x[-i, weight], na.rm = T)
        theta2[i, ] <- mean(num / den)
      }
    }

    a <- (n - 1)/n
    diff2 <- a * (theta2 - bar.HT)^2
    sd.HT = sqrt(mean(colSums(diff2)))

    data.frame(cbind(prom = mean(bar.HT),
                     sd1 = sd.bar.HT,
                     sd2 = sd.HT,
                     SD = sqrt(sd.HT^2 + 1.2 * (sd.bar.HT^2)),
                     cve = sqrt(sd.HT^2 +1.2 * (sd.bar.HT^2))/mean(bar.HT)))
  }

    if (anyNA(x[[weight]])) {
        stop(paste0("Algunos pesos son 'NA'"))
    }

    if (anyNA(x[, Xk])) {
      warning("Las covariables (Xk) tienen valores NA´s, estos fueron imputados")
       imp <- mice(x[, Xk], meth = "norm.boot",printFlag = FALSE)
       x[, Xk] <- complete(imp)[, Xk]
    }

    gkl <- calib(cbind(x[, Xk]), d = x[[weight]], total = as.numeric(total[Xk]), ...)
    x[["pesos.greg"]] <- x[[weight]] * gkl

    if (length(yhat) == 1) {
        bar.HT <- (x[[weight]] * x[, yhat])/sum(x[[weight]])
        bar.greg <- (x[["pesos.greg"]] * x[, yhat])/sum(x[["pesos.greg"]])
    } else {
        bar.HT <- rowMeans((x[[weight]] * x[, yhat])/sum(x[[weight]]))
        bar.greg <- rowMeans((x[["pesos.greg"]] * x[, yhat])/sum(x[["pesos.greg"]]))
    }

    # Calculando los errores del modelo de regresión
    for (i in 1:length(yhat)) {
        texto <- paste0("x$eepv", i, "=residuals(lm(", yhat[i],
                        "~.-1,data=x[,c(yhat[", i,
                        "],Xk)],weights = x[['weight']]))")
        eval(parse(text = texto))
    }

    ee.yhat <- colnames(x)[grepl("eepv", colnames(x))]

    resulDomGreg <- data.frame(PROM = sum(bar.greg), Sd = sd(bar.greg),
                               SD.JK = JK(x, yhat = ee.yhat, weight = "pesos.greg")[,"SD"],
                               cve = sd(bar.greg)/sum(bar.greg))

    resulDomHT <- data.frame(PROM = sum(bar.HT), Sd = sd(bar.HT),
                             SD.JK = JK(x, yhat = yhat, weight = weight)[,"SD"],
                             cve = sd(bar.HT)/sum(bar.HT))

    # Seleccionando las IE pertenecen a la ETC
    if (is.null(stratum)) {

        salida <- list(resulHT = resulDomHT, resulGreg = resulDomGreg)


    } else {
        id_IE <- factor(as.character(x[[stratum]]))
        # Estimar la Varianza Jackknife en los dominios dentro de la estratos
        sd.HT <- do.call("rbind",
                         as.list(by(id_IE, data = x,
                                    function(x) JK(x, yhat = yhat, weight = weight)[,c("prom", "sd1", "sd2", "SD")])))

        PROM.IE <- do.call("c",
                           as.list(by(id_IE, data = x,
                                      function(x) JK(x, yhat = yhat, weight = "pesos.greg")[,"prom"])))

        sd.GREG <- do.call("rbind",
                           as.list(by(id_IE, data = x,
                                      function(x) JK(x, yhat = ee.yhat, weight = "pesos.greg")[,c("sd1", "sd2", "SD")])))
        sd.GREG[["prom"]] <- PROM.IE

        sd.jk <- cbind(HT = sd.HT, GREG = sd.GREG[,names(sd.HT)])
        salida <- list(resulDom = data.frame(sd.jk), resulDomHT = resulDomHT, resulDomGreg = resulDomGreg)
    }

    return(salida)
}
