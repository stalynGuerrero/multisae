#' Students' results in saber 3, 5, and 9 exam in 2015
#' @description
#' Proporción de respuestas correctas en matemáticas, obtenidas por los estudiantes
#' de grado 7 que presentaron el examen  saber 3, 5, y 9 en el 2015.
#' @format La \code{list}  tiene dos \code{data.frame} con la sigueinte información
#'\describe{
#' \item{student}{\code{data.frame} tiene 341 filas  y 5 columnas}
#'      \itemize{
#'         \item{\code{urbanicity}: }{Identifica si el estudiante estudia en un
#'                                    colegio del sector oficial o no oficial.}
#'         \item{\code{schooltype}: }{Identifica a los estudiantes que se encuentran
#'                                    matriculados en colegios pertenecientes al sector oficial o no oficial}
#'         \item{\code{weight}: }{Factor de expansión resultante del diseño muestral}
#'         \item{\code{prop}: }{Proporción de respuestas correctas}
#'         \item{\code{ses}: }{Índice socioeconómico estimado para el estudiante}
#'              }
#' \item{total}{\code{data.frame} tiene los totales por modalidad de las variables
#' en el 2013.}
#'      \itemize{
#'         \item{\code{Rural}: }{Número de estudiantes en la zona rural de la entidad territorial certificada}
#'         \item{\code{Urbana}: }{Número de estudiantes en la zona urbana de la entidad territorial certificada}
#'         \item{\code{No.Oficial}: }{Número de estudiantes matricualdos en los colegios
#'                                    no oficial en la entidad territorial certificada}
#'         \item{\code{Oficial}: }{Número de estudiantes matricualdos en los colegios
#'                                 oficiales en la entidad territorial certificada}
#'         \item{\code{ses}: }{Sumatroria del índice socioeconómico estimado para los entudiantes
#'                             en la entidad territorial certificada}
#'              }
#'}
#'@references
#' Instituto Colombiano para la Evaluación de la Educación ICFES, (2016).
#'\url{http://www.icfes.gov.co/}
#' @source
#'\href{http://www.icfes.gov.co/item/1861-investigador-el-icfes-actualizo-ftp-para-facilitar-acceso-a-sus-bases-de-datos}{ftp://ftp.icfes.gov.co/}
"ResultStudent"
