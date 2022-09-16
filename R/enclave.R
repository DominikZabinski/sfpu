#' Set of functions for adjusting some of polygons for being surrounded completely ('enclave') by other polygon
#' @name enclave_
#' @return each function serves different purpose and returnes different object:
#' \itemize{
#'  \item{enclave_lst()}{returns named list. Each element is a vector of ids of polygons: one is an enclave, other is polygon surrounding it. Name of element is an id of that set}
#'  \item{enclave_dtl()}{returns data.table with columns 'JPT_KOD_JE' and 'id'. 'id' contains an id of set of polygons, and 'JPT_KOD_JE' its actual ids}
#'  \item{enclave_vec()}{returns a vector that switches original ids of polygons. If an id is listed in enclave_dtl()$JPT_KOD_JE, it gets corresponding enclave_dtl()$id}
#' }
#'
#' @export
#'
#' @examples
#' enclave_lst()
#' enclave_dtl()
#' enclave_vec(vecPolyId = c("2065", "1411"))
enclave_lst <- function()
{
    list(
        "0206_0261" = c("0206", "0261"), # jeleniogórski + Jelenia Góra
        "0209_0262" = c("0209", "0262"), # legnicki + Legnica
        "0223_0264" = c("0223", "0264"), # wrocławski + Wrocław
        "0221_0265" = c("0221", "0265"), # wałbrzyski + Wałbrzych

        "0403_0461" = c("0403", "0461"), # bydgoski + Bydgoszcz
        "0415_0463" = c("0415", "0463"), # toruński + Toruń
        "0418_0464" = c("0418", "0464"), # włocławski + Włocławek

        "0601_0661" = c("0601", "0661"), # bialski + Biała Podlaska
        "0603_0662" = c("0603", "0662"), # chełmski + Chełm
        "0609_0663" = c("0609", "0663"), # lubelski + Lublin
        "0620_0664" = c("0620", "0664"), # zamojski + Zamość

        "0801_0861" = c("0801", "0861"), # gorzowski + Gorzów Wielkoposlki
        "0809_0862" = c("0809", "0862"), # zielonogórski + Zielona Góra

        "1015_1063" = c("1015", "1063"), # skierniewicki + Skierniewice
        "1010_1062" = c("1010", "1062"), # piotrkowski + Piotrków Trybunalski

        "1206_1261" = c("1206", "1261"), # krakowski + Kraków
        "1216_1263" = c("1216", "1263"), # tarnowski + Tarnów
        "1210_1262" = c("1210", "1262"), # nowosądecki + Nowy Sącz

        "1426_1464" = c("1426", "1464"), # siedlecki + Siedlce
        "1419_1462" = c("1419", "1462"), # płocki + Płock
        "1425_1463" = c("1463", "1425"), # radomski+Radom
        "1415_1461" = c("1461", "1415"), # ostrołęcki + Ostrołęka

        "1813_1862" = c("1813", "1862"), # przemyski + Przemyśl
        "1820_1864" = c("1820", "1864"), # tarnobrzeski + Tarnobrzeg (niekoniecznie obwarzenk)
        "1816_1863" = c("1816", "1863"), # rzeszowski + Rzeszow
        "1807_1861" = c("1807", "1861"), # krośnieński + Krosno

        "2002_2061" = c("2002", "2061"), # białostocki + Białystok
        "2012_2063" = c("2012", "2063"), # suwalski + Suwałki
        "2007_2062" = c("2007", "2062"), # łomżyński + Łomża

        "2212_2263" = c("2212", "2263"), # słupski + Słupsk

        "2412_2473" = c("2412", "2473"), # rybnicki + Rybnik
        "2404_2464" = c("2404", "2464"), # częstochowski + Częstochowa

        "2604_2661" = c("2604", "2661"), # kielecki + Kielce

        "2804_2861" = c("2804", "2861"), # elbląski + Elbląg
        "2814_2862" = c("2814", "2862"), # olsztyński + Olsztyn

        "3021_3064" = c("3021", "3064"), # poznański + Poznań
        "3013_3063" = c("3013", "3063"), # leszczyński + Leszno
        "3010_3062" = c("3010", "3062"), # koniński + Konin
        "3007_3061" = c("3007", "3061"), # kaliski + Kalisz (nie do konca obwarzanek)
        "3209_3261" = c("3209", "3261")  # koszaliński + Koszalin
    )
}

#' @rdname enclave_
#' @export
#' @param lstOb list returned by enclave_lst()
enclave_dtl <- function(lstOb = enclave_lst())
{
    rbindlist(
        l = lapply(
            X = names(lstOb),
            FUN = function(i)
            {
                data.table(JPT_KOD_JE = lstOb[[i]], id = i)
            }
        )
    )
}

#' @rdname enclave_
#' @param vecPolyId vector of polygons id
#' @export
enclave_vec <- function(vecPolyId)
{
    obw <- enclave_lst()
    for (i in names(obw))
    {
        vecPolyId[vecPolyId %in% obw[[i]]] <- i
    }
    return(vecPolyId)
}
