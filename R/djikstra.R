#' Creates data.table of distances between centroids of polygons
#'
#' @param lvl lvl (one of 'wojewodztwa', 'powiaty' or 'gminy')
#' @param neighs data.table containing pairs ids of polygons (default: adjacent polygons of level \code{lvl})
#'
#' @return data.table with columns 'ID_TER', 'ID_NEIGH' and 'odl' (euclidean distance between centroids of polygons in 'ID_TER' and 'ID_NEIGH')
#' @export
#'
#' @details \code{neighs} must have columns 'ID_TER' and 'ID_NEIGH'
#'
#' @import data.table
#'
#' @examples
#' table_dists("wojewodztwa")
table_dists <- function(lvl, neighs = sfpu::neigh_list[[lvl]])
{
    # simple checks
    check_class(object = lvl, needed = "character")
    check_value(object = lvl, needed = list("value" = c("wojewodztwa", "powiaty", "gminy")))

    # determine needed shape
    this_shape <- get(x = paste0("shapes_", lvl))

    # calculate centroids of shapes
    this_cents <- shp_centroids(this_shape)

    # add coordinates of centroids to polygons in both 'ID_TER' and 'ID_NEIGH' column
    this_odl <- merge(x = neighs, y = this_cents, by.x = "ID_TER", by.y = "id")
    this_odl <- merge(x = this_odl, y = this_cents, by.x = "ID_NEIGH", by.y = "id")

    # calculate simple euclidean distance
    this_odl$odl <- sqrt((this_odl$x.x - this_odl$x.y)^2 + (this_odl$y.x - this_odl$y.y)^2)
    return(subset(this_odl, select = c("ID_TER", "ID_NEIGH", "odl")))
}

#' Calculates shortest paths from start to every other point in given graph
#'
#' @param start id
#' @param g data.table with columns 'ID_TER', 'ID_NEIGH' and 'odl'
#'
#' @return data.table with columns: 'a' id of polys, 'odl' (dist from start in meters) and 'b' (ids of polygon in exact order that constructs shortest path from \code{start} to \code{a}, excluding starting point, in the form of id1 - id2 - ... - idN)
#'
#' @description \code{g} might be a product of table_dists
#'
#' @details Calculating shortest path is based on Djikstra algorithm
#'
#' @import data.table
#'
#' @export
#'
#' @examples
#' short_paths(start = '02', g = table_dists('wojewodztwa'))
short_paths <- function(start, g)
{
    # simple checks
    check_class(object = g,        needed = "data.table")
    check_value(object = names(g), needed = list("value" = c('ID_TER', 'ID_NEIGH', 'odl')))

    oo <- unique(g$ID_TER)

    check_value(object = start, needed = list("value" = oo))

    vecs <- vector(mode = "list", length = length(oo))
    names(vecs) <- oo
    for (i in oo) vecs[[i]] <- list(odl = Inf, p = c())
    vecs[[start]]$odl <- 0
    path <- c()
    while (length(path) < length(vecs))
    {
        jj <- setdiff(names(vecs), path)
        od <- unlist(lapply(X = jj, FUN = function(x) vecs[[x]]$odl))
        ff <- min(od)
        tt <- jj[which(od == ff)]
        path <- c(path, tt)
        ttNeig <- g[g$ID_TER == tt]
        ttNeig <- ttNeig[!ttNeig$ID_NEIGH %in% path]
        for (ii in ttNeig$ID_NEIGH)
        {
            curP <- vecs[[ii]]$odl
            newP <- ff + ttNeig[ttNeig$ID_NEIGH == ii]$odl
            if (newP < curP)
            {
                vecs[[ii]]$odl <- newP
                vecs[[ii]]$p <- c(vecs[[tt]]$p, ii)
            }
        }
    }

    res <- rbindlist(
        l = lapply(
            X = names(vecs),
            FUN = function(i)
            {
                data.table(a = i, odl = vecs[[i]]$odl, b = paste0(vecs[[i]]$p, collapse = " - "))
            }
        )
    )

    return(res)
}

#' Visualizes shortest path
#'
#' @param path a result of \code{sfpu::short_paths}
#' @param end an id of ending polygon
#'
#' @return ggplot2 object with visualization of a path
#'
#' @export
#'
#' @import ggplot2 data.table
#'
#' @examples
#' shortest_paths_from_12_void <- short_paths(start = '12', g = table_dists('wojewodztwa'))
#' vis_paths(path = shortest_paths_from_12_void, end = "32")
vis_paths <- function(path, end)
{
    # couple of simple checks
    check_class(object = path, needed = "data.table")
    check_value(object = names(path), needed = list("value" = c("odl", "a", "b")))
    check_class(object = end , needed = "character" )

    # find out starting polygon (this is a polygon which distance from itself is equal 0)
    startingId <- path[path$odl == 0]$a

    # determine shapes that are needed
    needed_shapes <- switch(
        EXPR = as.character(nchar(startingId)),
        "2" = sfpu::shapes_wojewodztwa,
        "4" = sfpu::shapes_powiaty,
        "7" = sfpu::shapes_gminy
    )

    needed_centroids <- shp_centroids(needed_shapes)

    # check for 'end' argument. it needs to be id of polygon of the same level as start
    check_value(object = end, needed = list("value" = needed_centroids$id))

    # construct a path in form of idStart - id1 - ... - idN - idEnd
    whole_path <- paste0(startingId, " - ", path[path$a == end]$b)

    # retrieve ids of polygon from constructed path
    whole_path_ids <- unlist(strsplit(x = whole_path, split = " - "))

    # construct a path in form of data.table with coordinates of starting and ending points for each segment in path
    whole_path_segment <- rbindlist(
        l = lapply(
            X = 1:(length(whole_path_ids) - 1),
            FUN = function(i)
            {
                l1 = needed_centroids[needed_centroids$id == whole_path_ids[i]]
                l2 = needed_centroids[needed_centroids$id == whole_path_ids[i + 1]]
                data.table(xs = l1$x, ys = l1$y, xe = l2$x, ye = l2$y)
            }
        )
    )

    whole_path_segment[, id := 1:nrow(whole_path_segment)]

    ggplot() +
        geom_sf(data = needed_shapes, fill = 'white', color = 'black') +
        geom_segment(data = whole_path_segment, mapping = aes(x = xs, y = ys, xend = xe, yend = ye, group = id), color = "red") +
        geom_point(data = needed_centroids[id %in% c(startingId, end)], mapping = aes(x = x, y = y), size = 4, color = "orange") +
        labs(
            title = sprintf(
                "From %s (%s) to %s (%s) is %s km",
                needed_shapes[needed_shapes$JPT_KOD_JE == startingId, ]$JPT_NAZWA_, startingId,
                needed_shapes[needed_shapes$JPT_KOD_JE == end, ]$JPT_NAZWA_, end,
                round(path[path$a == end]$odl / 1e3, 0)
            )
        )
}
