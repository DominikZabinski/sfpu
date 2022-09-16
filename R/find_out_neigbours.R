#' @title Finding out adjacent polygons
#'
#' @description Functions finds out polygons which are adjacent to each other (using sf::st_intersects)
#'
#' @param poly sf object with polygons
#' @param kol column name
#' @param idName name of column with id
#' @param neighName name of column with neighbours id
#'
#' @return data.table with columns containing original id of polygons and ids of adjacent polygons. Names of columns are based on argument \code{idName} and \code{neighname} respectively
#' @export
#'
#' @importFrom data.table data.table rbindlist setnames
#' @importFrom sf st_intersects
#'
#' @examples
#' find_out_neigbours(poly = shapes_wojewodztwa, kol = "JPT_KOD_JE")
find_out_neigbours <- function(poly, kol, idName = "co", neighName = "s")
{
    # check classes
    check_class( object = poly,       needed = "sf" )
    check_class( object = kol,        needed = "character" )

    # check if 'kol' is in 'poly'
    check_value(object = kol, needed = list("value" = names(data.table(poly))))

    # check other clasess
    check_class( object = idName,     needed = "character" )
    check_class( object = neighName,  needed = "character" )

    vecUniqueIdsPolys <- unique(data.table(poly)[[kol]])

    res <- rbindlist(
        l = lapply(
            X = vecUniqueIdsPolys,
            FUN = function(polyId)
            {
                whichRowIsIt <- which(data.table(poly)[[kol]] == polyId)
                vecNeighs <- poly[[kol]][which(st_intersects(poly,  poly[whichRowIsIt, ], sparse = F) == TRUE)]
                # according to st_intersects, polygon A is adjacent to itself
                vecNeighs <- vecNeighs[vecNeighs != polyId]
                data.table(co = polyId, s = vecNeighs)
            }
        )
    )

    setnames(res, names(res), c(idName, neighName))

    return(res)
}
