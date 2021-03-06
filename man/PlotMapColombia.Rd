% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PlotMapColombia.r
\name{PlotMapColombia}
\alias{PlotMapColombia}
\title{Plot map of Colombia}
\usage{
PlotMapColombia(x, x.graph, id.nivagre, nivagre, n = 10, divipola, ShapesCol)
}
\arguments{
\item{x}{\code{data.frame}}

\item{x.graph}{Name of the variable contained in  \code{x} to be graphed on the map.}

\item{id.nivagre}{Variable which contains the identification codes as per the desired disaggregation}

\item{nivagre}{Desired municipal (‘mpio’), departmental (‘dpto’), or certified territorial entity ('ETC')
desired aggregation levels.}

\item{n}{Number of categories into which the variable \code{x.graph} is divided.}

\item{divipola}{\code{data.frame} which lists the territorial división in Colombia.}

\item{ShapesCol}{Shapes of the political-administrative division of Colombia}
}
\value{
The map of Colombia as a \code{ggplot} object.
}
\description{
Function which plots the map of Colombia and the aggregated results per level desired.
}
\examples{
data('cal.muni')
data(divipola)
data(ShapesCol)
PlotMapColombia(x = cal.muni[,c('mpio','prom')], x.graph = 'prom',
               id.nivagre = 'mpio', nivagre = 'mpio',
               divipola = divipola, ShapesCol = ShapesCol)

dpto.prom <- aggregate(cal.muni[,'prom'], list(dpto = cal.muni[['dpto']]), mean)

PlotMapColombia(x = dpto.prom, x.graph = 'x', id.nivagre = 'dpto',nivagre = 'dpto',
               divipola = divipola, ShapesCol = ShapesCol)

etc.prom <-aggregate(cal.muni[,'prom'],list(etc = cal.muni[['codETC']]), mean)

mapCol <- PlotMapColombia(x = etc.prom,x.graph = 'x',id.nivagre = 'etc',
                         nivagre = 'ETC',divipola = divipola,
                         ShapesCol = ShapesCol)

subSanAndres <- mapCol + xlim(c(165298, 171000)) + ylim(c(1884070, 1897000)) +
               guides(fill = "none") + labs(title = "")

subCatalina <- mapCol + xlim(c(205000, 210389)) + ylim(c(1975000, 1984871)) +
guides(fill = "none") + labs(title = "")

require(grid)
grid.newpage()
print(mapCol)
vp <- viewport(width = 0.1, height = 0.2, x = 0.02, y = 0.75, just = c("left", "bottom"))
print(subSanAndres, vp = vp)

vp <- viewport(width = 0.1, height = 0.2, x = 0.15, y = 0.75, just = c("left", "bottom"))
print(subCatalina, vp = vp)

}
\seealso{
\code{\link{divipola}, \link{etc2013}, \link{ggmap}, \link{ggplot2}, \link{ShapesCol}}
}

