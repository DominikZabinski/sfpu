#' Returns data.table with centroids of polygon
#'
#' @param shp shapefile object
#'
#' @return data.table with columns 'id', 'x' and 'y'
#' @export
#' @importFrom sf st_centroid
#' @importFrom data.table rbindlist data.table
#'
#' @examples
#' shp_centroids(sfpu::shapes_wojewodztwa)
shp_centroids <- function(shp)
{
    # using suppressWarnings() because, for now, do not know how to suppress this warning from st_centroid:
    #   st_centroid assumes attributes are constant over geometries of x
    suppressWarnings(expr = {this_cents <- st_centroid(shp)})
    res <- rbindlist(
        l = lapply(
            X = 1:nrow(this_cents),
            FUN = function(i)
            {
                g <- unlist(this_cents$geometry[i])
                data.table(id = this_cents$JPT_KOD_JE[i], x = g[1], y = g[2])
            }
        )
    )
    return(res)
}
