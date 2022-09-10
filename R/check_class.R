#' @title Functions to check things
#'
#' @name check_
#'
#' @param object object to test
#' @param needed vector of class that are searched for in an object
#'
#' @return
#' If passes, function do not return anything; if not they return an error
#' @details
#' \code{check_class} checking for specific class. In that case \code{needed} is a vector of class that are searched for in an object
#' \code{check_values} checking for specific values. In that case \code{needed} is a named list. Name of list indicates method of comparison (values, regex, range)
#' @export
#'
#' @examples
#' check_class("abba", "character")
#' check_class(c(1:3), "integer")
#' check_value(c(1:5), c(-6:6))
check_class <- function(object, needed)
{
    classes <- class(object)
    if (!any(classes %in% needed))
        stop(sprintf("Needed: %s, actual: %s", paste0(needed, collapse = ", "), paste0(classes, collapse = ", ")))
    return(invisible(NULL))
}


#' @rdname check_
#' @export
check_value <- function(object, needed)
{
    check_class(object = needed, needed = "list")
    methodCheck <- names(needed)
    if (methodCheck == "value") if (!all(object) %in% needed[[1]]) stop("Some of the values are outside needed range")
    if (methodCheck == "regex") if (any(regexpr(pattern = needed[[1]], text = object) < 0)) stop("Some of the values are outside needed regex")
    if (methodCheck == "range") if (any(object < min(needed[[1]])) | any(object > max(needed[[1]]))) stop("Some of the values are outside needed range")
    return(invisible(NULL))
}
