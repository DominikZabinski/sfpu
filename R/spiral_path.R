#' Creates spiral segment
#'
#' @param data
#' @param lO density
#'
#' @return
#' @export
#'
#' @examples
#' jj <- same_arc_length_points(100, dlugoscPozadana = 6)
#' jj[, ww := runif(n = nrow(jj), min = -1, max = 1)]
#' jj[, ww := cumsum(ww)]
#' jj2 <- spiral_path(data = jj, lO = 1000, k = "ww")
#' ggplot() + geom_path(data=jj2, aes(x = x, y = y, color = w), size = 2) +geom_point(data = jj, aes(x=x, y=y, color = ww), size = 4)
spiral_path <- function(data, lO = 1000, k = "w")
{
    check_value(object = "t" %in% names(data), needed = list("value" = T))
    dataPlotDens2 <- data.table(t = seq(from = min(data$t), to = max(data$t), length.out = lO))
    dataPlotDens2[, x := t * cos(t)]
    dataPlotDens2[, y := t * sin(t)]
    if (!is.null(k))
    {
        check_value(object = k %in% names(data), needed = list("value" = T))
        wart <- unlist(
            lapply(
                X = 1:nrow(dataPlotDens2),
                FUN = function(i)
                {
                    tt <- dataPlotDens2[i]$t
                    tett <- c(data$t[max(which(data$t <= tt))], data$t[min(which(data$t >= tt))])
                    mean(data[t %in% tett][[k]], na.rm = T)
                }
            )
        )
        dataPlotDens2$w <- wart
    }

    return(dataPlotDens2)
}
