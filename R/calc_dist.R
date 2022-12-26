#' Calculate Euclidean distance between two points
#'
#' @param p1
#' @param p2
#'
#' @return
#' @export
#'
#' @examples
#' calc_dist(c(1, 2), c(4, 5))
calc_dist <- function(p1, p2)
{
    check_class(object = p1, needed = "numeric")
    check_class(object = p2, needed = "numeric")
    check_value(object = length(p1) == length(p2), needed = list(value = T))
    sqrt( sum( (p1 - p2) ^ 2) )
}
