#' Finding out neighbouring polygons
#'
#' @param poly sf object with polygons
#' @param kol column name
#' @param idName name of column with id
#' @param neighName name of column with neighbours id
#'
#' @return
#' @export
#'
#' @examples
#' find_out_neigbours(poly = shapes_wojewodztwa, kol = "JPT_KOD_JE")
#'
#' @importFrom data.table data.table rbindlist setnames
#' @importFrom sf st_intersects
find_out_neigbours <- function(poly, kol, idName = "co", neighName = "s")
{
    # check classes
    check_class( object = poly,       needed = "sf" )
    check_class( object = kol,        needed = "character" )
    check_class( object = idName,     needed = "character" )
    check_class( object = neighName,  needed = "character" )

    pp <- unique(data.table(poly)[[kol]])

    res <- rbindlist(
        l = lapply(
            X = pp,
            FUN = function(i)
            {
                ktory <- which(data.table(poly)[[kol]] == i)
                sasiedzi <- poly$JPT_KOD_JE[which(st_intersects(poly,  poly[ktory, ], sparse = F) == TRUE)]
                data.table(co = i, s = sasiedzi)
            }
        )
    )

    setnames(res, names(res), c(idName, neighName))

    return(res[co != s][])
}
