#' Plot map of Colombia
#'@description Function which plots the map of Colombia and the aggregated results per level desired.
#'@param x            \code{data.frame}
#'@param x.graph      Name of the variable contained in  \code{x} to be graphed on the map.
#'@param id.nivagre   Variable which contains the identification codes as per the desired disaggregation
#'@param nivagre      Desired municipal (‘mpio’), departmental (‘dpto’), or certified territorial entity ('ETC')
#'                    desired aggregation levels.
#'@param n            Number of categories into which the variable \code{x.graph} is divided.
#'@param divipola     \code{data.frame} which lists the territorial división in Colombia.
#'@param ShapesCol    Shapes of the political-administrative division of Colombia
#'
#'@examples
#'data('cal.muni')
#'data(divipola)
#'data(ShapesCol)
#'PlotMapColombia(x = cal.muni[,c('mpio','prom')], x.graph = 'prom',
#'                id.nivagre = 'mpio', nivagre = 'mpio',
#'                divipola = divipola, ShapesCol = ShapesCol)
#'
#'dpto.prom <- aggregate(cal.muni[,'prom'], list(dpto = cal.muni[['dpto']]), mean)
#'
#'PlotMapColombia(x = dpto.prom, x.graph = 'x', id.nivagre = 'dpto',nivagre = 'dpto',
#'                divipola = divipola, ShapesCol = ShapesCol)
#'
#'etc.prom <-aggregate(cal.muni[,'prom'],list(etc = cal.muni[['codETC']]), mean)
#'
#'mapCol <- PlotMapColombia(x = etc.prom,x.graph = 'x',id.nivagre = 'etc',
#'                          nivagre = 'ETC',divipola = divipola,
#'                          ShapesCol = ShapesCol)
#'
#'subSanAndres <- mapCol + xlim(c(165298, 171000)) + ylim(c(1884070, 1897000)) +
#'                guides(fill = "none") + labs(title = "")
#'
#'subCatalina <- mapCol + xlim(c(205000, 210389)) + ylim(c(1975000, 1984871)) +
#'guides(fill = "none") + labs(title = "")
#'
#'require(grid)
#'grid.newpage()
#'print(mapCol)
#'vp <- viewport(width = 0.1, height = 0.2, x = 0.02, y = 0.75, just = c("left", "bottom"))
#'print(subSanAndres, vp = vp)
#'
#'vp <- viewport(width = 0.1, height = 0.2, x = 0.15, y = 0.75, just = c("left", "bottom"))
#'print(subCatalina, vp = vp)
#'
#'@return The map of Colombia as a \code{ggplot} object.
#'@seealso \code{\link{divipola}, \link{etc2013}, \link{ggmap}, \link{ggplot2}, \link{ShapesCol}}
#'@export
PlotMapColombia <- function(x, x.graph, id.nivagre, nivagre, n = 10,
                            divipola, ShapesCol) {

    if (nivagre %in% c("mpio", "dpto", "ETC")) {
        x[, "pro"] <- x[, x.graph]

        if (nivagre == "mpio") {
            x[, "mpio"] <- x[, id.nivagre]
            shapeET2 <- merge(divipola, x, by = "mpio", all.x = T)
        }
        if (nivagre == "dpto") {
            x[, "dpto"] <- x[, id.nivagre]
            shapeET2 <- merge(divipola, x, by = "dpto", all.x = T)
        }
        if (nivagre == "ETC") {
            x[, "codETC"] <- x[, id.nivagre]
            shapeET2 <- merge(divipola, x, by = "codETC", all.x = T)
        }

        MunicipiosET <- unique(shapeET2[, c("id.espacia", "tip.etc", "pro")])

        ################################ Municipios ###################################

        ohsCol <- ShapesCol$Municipios
        ohsCol@data[, "id.espacia"] <- ohsCol@data[, "MPIOS"]
        ohsColI <- fortify(ohsCol)
        idMuni <- data.frame(id = as.vector(rownames(ohsCol@data)),
                             ohsCol@data[, c("DPTO", "MPIO", "id.espacia")])
        ohsColI[, "ord"] <- 1:nrow(ohsColI)
        shapeMpio <- merge(ohsColI, idMuni, by = "id")
        shapeMpio2 <- merge(shapeMpio, MunicipiosET, by = "id.espacia")
        shapeMpio22 <- subset(shapeMpio2, tip.etc == "Municipal")
        shapeMpio2 <- shapeMpio2[order(shapeMpio2[, "ord"]), ]

        ################################ Departamentos ################################

        ohsCol <- ShapesCol$Departamento
        ohsCol@data[, "id.espacia"] <- ohsCol@data[, "DPTO"]
        ohsColI <- fortify(ohsCol)
        ################### intervalos SAE ET ################
        if (nivagre == "ETC") {
            mapCol2 <- ggplot() + geom_polygon(data = shapeMpio2, aes(x = long, y = lat, group = group, fill = pro)) +
                scale_fill_distiller(palette = 2, breaks = pretty_breaks(n = n), na.value = "red", direction = 1) +
                geom_polygon(data = ohsColI, aes(x = long, y = lat, group = group, fill = pro), fill = NA, color = "black",
                  size = 0.5) + geom_polygon(data = shapeMpio22, aes(x = long, y = lat, group = group, fill = pro),
                fill = NA, color = "blue", size = 0.2) + labs(x = "", y = "", title = "", fill = "") + theme_nothing(legend = TRUE) +
                guides(fill = guide_legend(reverse = TRUE))
            mapCol2
        } else {
        ###############################################################################
            if (nivagre == "dpto") {
                mapCol2 <- ggplot() + geom_polygon(data = shapeMpio2,
                                                   aes(x = long, y = lat, group = group, fill = pro))
            }

            if (nivagre == "mpio") {
                  mapCol2 <- ggplot() +
                  geom_polygon(data = shapeMpio2, aes(x = long, y = lat, group = group, fill = pro),
                  color = "black", size = 0.2)
            }

            mapCol2 <- mapCol2 + scale_fill_distiller(palette = 2, breaks = pretty_breaks(n = n),
                                                      na.value = "red", direction = 1) +
            geom_polygon(data = ohsColI, aes(x = long, y = lat, group = group), fill = NA,
                           color = "black", size = 0.5) +
              labs(x = "", y = "", title = "", fill = "") +
              theme_nothing(legend = TRUE) + guides(fill = guide_legend(reverse = TRUE))
            return(mapCol2)
        }
        ###############################################################################

    } else {
        print("Valor no valido para el nivel de agregados \n")
    }
}
