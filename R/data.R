#' A sf object of simplified voidships
#'
#' @format A sf data.frame object with columns such as
#' \describe{
#'   \item{JPT_KOD_JE}{NUTS2 code '02', '32'}
#'   \item{JPT_NAZWA_}{name of NUTS2}
#'   \item{Shape_Area}{area of shape}
#'   \item{geometry}{geometry of shape}
#' }
#' @source \url{https://gis-support.pl/baza-wiedzy-2/dane-do-pobrania/granice-administracyjne/}
"shapes_wojewodztwa"

#' A sf object of simplified voidships
#'
#' @format A sf data.frame object with columns such as
#' \describe{
#'   \item{JPT_KOD_JE}{NUTS2 code '2602', '3205'}
#'   \item{JPT_NAZWA_}{name of NUTS2}
#'   \item{Shape_Area}{area of shape}
#'   \item{geometry}{geometry of shape}
#' }
#' @source \url{https://gis-support.pl/baza-wiedzy-2/dane-do-pobrania/granice-administracyjne/}
"shapes_powiaty"

#' A sf object of simplified counties
#'
#' @format A sf data.frame object with columns such as
#' \describe{
#'   \item{JPT_KOD_JE}{NUTS2 code '260204', '2602063'}
#'   \item{JPT_NAZWA_}{name of NUTS2}
#'   \item{Shape_Area}{area of shape}
#'   \item{geometry}{geometry of shape}
#' }
#' @source \url{https://gis-support.pl/baza-wiedzy-2/dane-do-pobrania/granice-administracyjne/}
"shapes_gminy"

#' A sf object of simplified counties
#'
#' @format A list of data.tables. Each data.table
#' \describe{
#'   \item{ID_TER}{NUTS code of territory}
#'   \item{ID_NEIGH}{NUTS of its neighbour}
#' }
#' @source \url{https://gis-support.pl/baza-wiedzy-2/dane-do-pobrania/granice-administracyjne/}
"neigh_list"
