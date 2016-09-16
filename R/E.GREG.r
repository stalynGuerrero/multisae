#' Generalized regression estimator (GREG)
#'@description
#'The function calculates the mean squared error (MSE) for the set of covariables used in the GREG.
#'@param   x          \code{data.frame} which contains the information necessary to make the estimations.
#'@param   Xk         Vector which contains the names of the auxiliary variables.
#'@param   total      Total of the auxiliary variable.
#'@param   prom       Name of the variable(s) to be estimated.
#'@param   weight     Expansion factor of each observation.
#'@param   stratum    Indicator of the domains in the sample. By default \code{stratum = NULL}.
#'@param  ...         Additional arguments for the \code{\link{calib}} function.
#'@examples
#' #Reading base
#' data('ResultStudent')
#' require(TeachingSampling)
#' #Selecting data
#' sampling <- ResultStudent[["student"]]
#' #Creating dummy variables
#' sampling <- data.frame(sampling[,c('weight','prop','ses')],
#'                       Domains(sampling[["urbanicity"]]),
#'                       Domains(sampling[["schooltype"]]))
#'#Total variable dummy
#' total <- ResultStudent[["total"]]
#'
#' Xk <- c('Rural', 'Urbana','No.Oficial', 'Oficial', 'ses')
#'
#' E.GREG(x = sampling,total = total, Xk = Xk,
#'               prom = 'prop',weight = 'weight',method='linear')
#'@return
#'Retorna un \code{data.frame}
#'\describe{
#'\item{R2}{El  \eqn{R^2} for the model  defined}
#'\item{mse}{The mean squared error obtained in the estimation}
#'}
#'@seealso \code{\link{S.fitFH}}, \code{\link{calib}}
#'@references
#'\describe{
#'\item{-}{Särdnal, C. E., Swensson, B., & Wretman, J. H. (1992).
#'         Model assisted survey sampling.}
#'\item{-}{Rubin, D. B. (2004). Multiple imputation for
#'         nonresponse in surveys (Vol. 81). John Wiley & Sons.}
#'\item{-}{Von Davier, M., Gonzalez, E., & Mislevy, R. (2009).
#'         What are plausible values and why are they useful. IERI
#'         monograph series, 2, 9-36.}
#'}
#'
#' @export
E.GREG <- function(x, Xk, total, prom, weight, stratum = NULL, ...) {
    if (is.null(stratum)) {
        x$stratum <-  1
        stratum <-  "stratum"
    }
    gkl <- calib(cbind(x[, Xk]), d = x[[weight]], total = as.numeric(total), ...)

    x$pesos.greg <- x[[weight]] * gkl

    Resul <- paste0("Resul<-data.frame(", paste0("R2_", 1:length(prom), sep = "=NA", collapse = ","), ")")

    eval(parse(text = Resul))

    for (i in 1:length(prom)) {
        texto <- paste0("Resul$R2_", i, "=summary(lm(", prom[i],
                        "~.-1,data=x[,c(prom[",i, "],Xk)],weights = x$",
                       weight, "))$r.squared")
        eval(parse(text = texto))
        texto <- paste0("x$PROM.GREG", i, "<-x[['pesos.greg']]*x$", prom[i])
        eval(parse(text = texto))
        if (i == 1) {
            texto <- paste("PROM.IE <- x %>% group_by(", stratum,
                           ") %>% summarise_each(funs(mean(.,na.rm = T)),matches('",
                           prom[i],"'))", sep = "")
            eval(parse(text = texto))
        } else {
            texto <- paste("PROM.IE2<-x%>%group_by(", stratum,
                           ")%>%summarise_each(funs(mean(.,na.rm = T)),matches('",
                           prom[i],"'))", sep = "")
            eval(parse(text = texto))
            PROM.IE <- merge(PROM.IE, PROM.IE2, by = stratum)
        }
    }
    texto <- paste0("PROM.GREG <- x %>% group_by(", stratum,
                    ") %>% summarise_each(funs(sum(., na.rm = T)/sum(pesos.greg,na.rm = T)), matches('PROM.GREG'))")
    eval(parse(text = texto))
    for (i in 1:length(prom)) {
        xk <- abs(PROM.IE[, 1 + i] - PROM.GREG[, 1 + i])^2
        n <- length(xk)
        texto <- paste("Resul$mse", i, "<-sum(xk,na.rm = T)/n", sep = "")
        eval(parse(text = texto))
    }
    Resul
}
