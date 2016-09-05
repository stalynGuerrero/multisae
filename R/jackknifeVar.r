#' Estimated variance for the estimator GREG using the jackknife method
#'@description
#' La función realiza la estimación de las varianzas para el estimador
#' Horvitz-Thompson (TH)  y el estimador de regresión generalizada (GREG)
#' cuando se emplean los valores plausibles. La estimación de la varianza estimada
#' para un dominio viene dada por la  ecuación
#' \deqn{EE(\hat{\eta}) = \sqrt{\frac{1}{5}\sum_{i=1}^{5}Var_m(\hat{\eta}_i)+
#'          \left(1 + \frac{1}{5}\right)\frac{1}{4}\sum_{i=1}^{5}(\hat{\eta}_i -
#'          \hat{\eta})^2}}
#'
#' donde \eqn{Var_m(\hat{\eta}_i)} es la varianza del estimador calculada a partir
#' de las observaciones y el  diseño muestral.
#' El segundo término de la ecuación es estimada utilizando el método jackknife.
#' En el caso del  estimador de regresión generalizada, es necesario ajustar un modelo lineal,
#' donde el valor plausible \eqn{\hat{\eta}_{ij}} es incluido como la variable respuesta, así:
#' \deqn{\hat{\eta}_{ij}=\beta X_i+e_{ij}}
#' con \eqn{X_i} es la información auxiliar, empleando los residuales estimados del modelo
#' se estima la varianza del GREG para cada dominio, utilizando la expresión
#' \deqn{\widehat{Var}(\hat{\eta}_{GREG})=\sum\sum_s\frac{\Delta_{jl}}{\pi_{jl}}g_{js}
#'       \frac{\hat{e}_{js}}{\pi_j}g_{ls}\frac{\hat{e}_{ls}}{\pi_l}}
#'donde
#'\describe{
#'\item{-}{\eqn{g_{js} = 1+\left( \bar{X}-\hat{\bar{X}}\right)^t \left(\sum_m \frac{w_j}{\sigma^2_l}x_jx_j^t\right)^{-1}x_j}}
#'\item{-}{\eqn{\hat{e}_{ij} = \hat{\eta}_{ij} - \hat{\beta} X_i}}
#'\item{-}{\eqn{w_j = \frac{1}{\pi_j}}}
#' }
#'@param x       \code{data.frame} que contiene la información necesaria para realizar las estimaciones
#'@param yhat    Nombre de la columna que contiene los valores estimados de la variable
#'@param weight  Factor de expansión de cada observación
#'@param total   Total de la variable auxiliar
#'@param Xk      Vector que contiene los nombres de las covariables.
#'@param stratum Indicadora de los dominios en la muestra, por defecto \code{stratum = NULL}
#'@param ...     Argumentos adicionales empleados por la función \link{calib}

#' @references
#'\describe{
#'\item{-}{Shao, J., & Tu, D. (2012). The jackknife and bootstrap. Springer Science & Business Media.}
#'\item{-}{Rubin, D. B. (2004). Multiple imputation for nonresponse in surveys (Vol. 81). John Wiley & Sons.}
#'\item{-}{Von Davier, M., Gonzalez, E., & Mislevy, R. (2009). What are plausible values and why are
#'        they useful. IERI monograph series, 2, 9-36.}
#'}
#'
#'@return Retorna un \code{\link{data.frame}} con los resultados obtenidos para el estimador de Horvitz-Thompson  y
#' el GREG, así como sus respectivas varianzas para los grupos y subgrupos.
#'@examples
#'data('ResultStudent')
#'attach(ResultStudent)
#' sampling <- ResultStudent[["student"]]
#' #Crear variables dummys
#' sampling <- data.frame(sampling[,c("schooltype",'weight','prop','ses')],
#'                       Domains(sampling[["urbanicity"]]),
#'                       Domains(sampling[["schooltype"]]))
#'
#' # Covariables
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

#'@seealso  \code{\link{calib}}
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
