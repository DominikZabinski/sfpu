#' Merging polygons
#'
#' @param orPol oryginalny shape
#' @param uklad data.table o kolumnie 'id' (ktora ma odpowiadac 'JPT_KOD_JE') i klaster czyli id klastra
#' @param clusterName column name in uklad that corresponds to cluster id
#' @param polysId column name in uklad that corresponds to polygon id
#' @param geometryName column name with geometry
#'
#' @return sf object?
#' @export
#' @details zamysl jest taki, ze dla jednego klastra jest jedno id klastra. dla jednego id klastra jest wiele id powiatow. dla jednego id klastra
#' @examples
#' merge_polygons(shapes_wojewodztwa, data.table(id = c('02', '16'), klaster = "1"))
#' @importFrom sf st_as_sf st_make_valid st_union st_sf
#' @importFrom magrittr '%>%'
#' @importFrom data.table .N
merge_polygons <- function(orPol, uklad, clusterName = "klaster", polysId = "id", geometryName = "g")
{
    # checking classes of arguments
    check_class( object = orPol,        classNeeded = "sf"         )
    check_class( object = uklad,        classNeeded = "data.table" )
    check_class( object = clusterName,  classNeeded = "character"  )
    check_class( object = polysId,      classNeeded = "character"  )
    check_class( object = geometryName, classNeeded = "character"  )

    # checking if 'clusterName' and 'polysId' are in 'uklad'
    if ( !clusterName %in% names(uklad) ) stop(sprintf("'%s' not in 'uklad'", clusterName))
    if ( !polysId     %in% names(uklad) ) stop(sprintf("'%s' not in 'uklad'", polysId))

    # checking if any of polysId is in orPol
    if ( !any( uklad[[polysId]] %in% orPol$JPT_KOD_JE) ) stop("None of the polys id values is in orPol")

    # checking if all polysId are assginde to only one cluster
    uklad <- subset( x = uklad, select = c(clusterName, polysId) ) %>% unique()
    checkPolys <- uklad[,list(.N), by = polysId][N > 1]
    if ( nrow( checkPolys ) > 0 ) stop(sprintf("Some polysId are in more than one cluster (e.g. '%s')", checkPolys[[polysId]][1]))

    l = lapply(
        X = sort(unique(uklad[[clusterName]])),
        FUN = function(i)
        {
            idiki <- uklad[which(uklad[[clusterName]] == i)][[polysId]]
            aa <- which(orPol$JPT_KOD_JE %in% idiki)
            kk <- orPol[aa, ] %>%
                st_as_sf() %>%
                st_make_valid() %>%
                st_union()
            kk_d <- st_sf(clusterName = i, g = kk)
            return(kk_d)
        }
    )

    res <- do.call(what = rbind, args = l)
    names(res) <- c(clusterName, geometryName)
    return(res)
}

# to do: if a cluster has only one polygon, there is no need to merge
