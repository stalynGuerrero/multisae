#' Students' results in saber 3, 5, and 9 exam in 2015
#' @description
#' Ratio of correct answers in mathematics obtained by the students of 7th grade who sat the
#' Saber 3, 5, and 9 tests in 2015
#' @format The \code{\link{list}} has two \code{data.frame} with the following information
#'\describe{
#' \item{student}{\code{data.frame} has 341 rows and 5 columns.}
#'      \itemize{
#'         \item{\code{urbanicity}: }{Identifies whether the student studies in a school
#'                                    from the official or non-official sector.}
#'         \item{\code{schooltype}: }{Identifies the students enrolled in schools belonging
#'                                    to the official or non-official sector}
#'         \item{\code{weight}: }{Expansion factor resulting from the sampling design}
#'         \item{\code{prop}: }{Ration of correct answers}
#'         \item{\code{ses}: }{Estimated socioeconomic index for the student}
#'              }
#' \item{total}{\code{data.frame} has the totals per modality of the variables in the year 2013.}
#'      \itemize{
#'         \item{\code{Rural}: }{Number of students in the rural area of the certified territorial entity.}
#'         \item{\code{Urbana}: }{Number of students in the urban area of the certified territorial entity.}
#'         \item{\code{No.Oficial}: }{Number of students enrolled in non-official schools of the
#'                                    certfied territorial entity. }
#'         \item{\code{Oficial}: }{Number of students enrolled in official schools of the
#'          certified territorial entity.}
#'         \item{\code{ses}: }{Estimated sum of socioeconomic index for students
#'                             in the certified territorial entity}
#'              }
#'}
#'@references
#' Instituto Colombiano para la Evaluación de la Educación ICFES, (2016).
#'\url{http://www.icfes.gov.co/}
#' @source
#'\href{http://www.icfes.gov.co/item/1861-investigador-el-icfes-actualizo-ftp-para-facilitar-acceso-a-sus-bases-de-datos}{ftp://ftp.icfes.gov.co/}
"ResultStudent"
