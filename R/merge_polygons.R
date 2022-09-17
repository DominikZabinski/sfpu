#' @title Merging polygons
#'
#' @param orPol shapefile with needed geometry
#' @param uklad data.table with a column with polygons 'id' (which has to correspond to 'JPT_KOD_JE' in shapefiles) and cluster id
#' @param clusterName column name in \code{uklad} that corresponds to cluster id
#' @param polysId column name in \code{uklad} that corresponds to polygon id
#' @param geometryName column name with geometry
#'
#' @details for each cluster there is a one cluster id. for each cluster id there are >=1 id of a polygon
#' to do: if a cluster has only one polygon, there is no need to merge
#'
#' @return sf object with columns containing cluster name (provided via \code{clusterName}) and new geometry of a cluster (with a name provided via \code{geometryName})
#'
#' @export
#'
#' @importFrom sf st_as_sf st_make_valid st_union st_sf
#' @importFrom data.table .N
#'
#' @examples
#' res <- merge_polygons(shapes_wojewodztwa, data.table(id = c('02', '16'), klaster = "1"))
#' ggplot2::ggplot(res) + ggplot2::geom_sf()
merge_polygons <- function(orPol, uklad, clusterName = "klaster", polysId = "id", geometryName = "g")
{
    # checking classes of arguments
    check_class( object = orPol,        needed = "sf"         )
    check_class( object = uklad,        needed = "data.table" )
    check_class( object = clusterName,  needed = "character"  )
    check_class( object = polysId,      needed = "character"  )
    check_class( object = geometryName, needed = "character"  )

    # checking if 'clusterName' and 'polysId' are in 'uklad'
    check_value(object = clusterName, needed = list("value" = names(uklad)))
    check_value(object = polysId, needed = list("value" = names(uklad)))

    # checking if any of polysId is in orPol
    check_value(object = uklad[[polysId]], needed = list("value" = orPol$JPT_KOD_JE))

    # checking if all polysId are assigned to only one cluster
    uklad <- subset( x = uklad, select = c(clusterName, polysId) ) %>% unique()
    checkPolys <- uklad[,list(.N), by = polysId]
    checkPolys <- checkPolys[checkPolys$N > 1]
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
