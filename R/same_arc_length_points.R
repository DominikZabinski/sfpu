#' Creates set of points with same distance between them, on spiral
#'
#' @param noPoints number of points needed
#' @param katStartowy
#' @param dlugoscPozadana
#' @param krokKatowy
#'
#' @return
#' @export
#'
#' @examples
#' jj <- same_arc_length_points(100, dlugoscPozadana = 6)
#' ggplot(data = jj)+geom_point(aes(x=x, y=y))
same_arc_length_points <- function(noPoints, katStartowy = 1, dlugoscPozadana = 12, krokKatowy = .0125)
{
    check_class(object = noPoints, needed = "numeric")
    check_value(object = noPoints, needed = list("range" = c(0, Inf)))
    liczbaDocelowaPunktow <- noPoints - 1
    ostatniPunkt <- c(katStartowy * cos(katStartowy), katStartowy * sin(katStartowy))
    zbiorPunktow <- data.table(x = ostatniPunkt[1], y = ostatniPunkt[2], t = katStartowy, odl = NA)
    odleglosciBiezacegoOdcinka <- c()
    biezacyKat <- katStartowy
    while (nrow(zbiorPunktow) < liczbaDocelowaPunktow + 1)
    {
        while (sum(odleglosciBiezacegoOdcinka) < dlugoscPozadana)
        {
            biezacyKat <- biezacyKat + krokKatowy / biezacyKat
            dodanyPunkt <- c(biezacyKat * cos(biezacyKat), biezacyKat * sin(biezacyKat))
            odleglosciBiezacegoOdcinka <- c(odleglosciBiezacegoOdcinka, calc_dist(ostatniPunkt, dodanyPunkt))
            ostatniPunkt <- dodanyPunkt
        }
        zbiorPunktow <- rbind(zbiorPunktow, data.table(x = ostatniPunkt[1], y = ostatniPunkt[2], t = biezacyKat, odl = sum(odleglosciBiezacegoOdcinka)))
        odleglosciBiezacegoOdcinka <- c()
    }

    return(zbiorPunktow[,.(x, y, t)])
}
