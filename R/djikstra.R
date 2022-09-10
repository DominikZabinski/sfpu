#' Creates data.table of distances between neighbouring centroids
#'
#' @param lvl lvl (wojewodztwa, powiaty, gminy)
#' @param neighs data.table of neighbours
#'
#' @return
#' @export
#'
#' @examples
#' table_dists("wojewodztwa")
table_dists <- function(lvl, neighs = sfpu::neigh_list[[lvl]])
{
    this_shape <- get(x = paste0("shapes_", lvl))
    this_cents <- shp_centroids(this_shape)
    this_odl <- merge(x = neighs, y = this_cents, by.x = "ID_TER", by.y = "id")
    this_odl <- merge(x = this_odl, y = this_cents, by.x = "ID_NEIGH", by.y = "id")
    this_odl[, odl := sqrt((x.x - x.y)^2 + (y.x - y.y)^2)]
    return(this_odl[,.(ID_TER, ID_NEIGH, odl)][])
}

#' Calculates shortest paths from start to every othe point in given graph
#'
#' @param start id
#' @param g data.table with ID_TER, ID_NEIGH and odl
#'
#' @return data.table with columns: 'a' id of polys, 'odl' (dist from start) and 'b' (path from start to end, excludin starting point)
#' @description g might be a product of table_dists
#' @export
#'
#' @examples
#' short_paths(start = '02', g = table_dists('wojewodztwa'))
short_paths <- function(start, g)
{
    path <- c()
    oo <- unique(g$ID_TER)
    vecs <- vector(mode = "list", length = length(oo))
    names(vecs) <- oo
    for (i in oo) vecs[[i]] <- list(odl = Inf, p = c())
    vecs[[start]]$odl <- 0
    while (length(path) < length(vecs))
    {
        jj <- setdiff(names(vecs), path)
        od <- unlist(lapply(X = jj, FUN = function(x) vecs[[x]]$odl))
        ff <- min(od)
        tt <- jj[which(od == ff)]
        path <- c(path, tt)
        ttNeig <- g[ID_TER == tt][!ID_NEIGH %in% path]
        for (ii in ttNeig$ID_NEIGH)
        {
            curP <- vecs[[ii]]$odl
            newP <- ff + ttNeig[ID_NEIGH == ii]$odl
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
#' @param path reuslt of short_paths()
#' @param end id of ending polygon
#'
#' @return ggplot2 object with visualization of path
#' @export
#' @import ggplot2
#' @examples
#' oj <- short_paths(start = '12', g = table_dists('wojewodztwa'))
#' vis_paths(path = oj, end = "32")
vis_paths <- function(path, end)
{
    tt <- copy(path)
    s <- tt[odl == 0]$a
    ss <- sfpu::shapes_wojewodztwa
    s2 <- shp_centroids(sfpu::shapes_wojewodztwa)
    if (nchar(end) == 4)
    {
        ss <- sfpu::shapes_powiaty
        s2 <- shp_centroids(sfpu::shapes_powiaty)
    }
    ende <- paste0(s, " - ", tt[a == end]$b)
    endeT <- unlist(strsplit(x = ende, split = " - "))
    gg <- rbindlist(
        l = lapply(
            X = 1:(length(endeT) - 1),
            FUN = function(i)
            {
                l1 = s2[id == endeT[i]]
                l2 = s2[id == endeT[i + 1]]
                data.table(xs = l1$x, ys = l1$y, xe = l2$x, ye = l2$y)
            }
        )
    )
    gg[, id := 1:nrow(gg)]

    ggplot() +
        geom_sf(data = ss, fill = 'white', color = 'black') +
        # geom_point(data = s2, aes(x = x, y = y)) +
        geom_segment(data = gg, aes(x = xs, y = ys, xend = xe, yend = ye, group = id), color = "red") +
        geom_point(data = s2[id %in% c(s, end)], aes(x = x, y = y), size = 4, color = "orange")

}
