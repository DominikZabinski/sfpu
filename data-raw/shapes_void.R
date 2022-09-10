# [PL] Przygotowanie i uproszczenie shapefili
# [EN] Preparing and simplifying shapefiles
# biblioteki/libraries ----
library(sfpu)
library(stringr)
library(rmapshaper)

# przygotowanie danych / prepare data ----
# pliki pochodza z https://gis-support.pl/baza-wiedzy-2/dane-do-pobrania/granice-administracyjne/; stan na 28.07.2022
# files come from https://gis-support.pl/baza-wiedzy-2/dane-do-pobrania/granice-administracyjne/; urls are as of 28.07.2022
base_url <- "https://www.gis-support.pl/downloads/"
urls <- list(
    wojewodztwa = "Wojewodztwa.zip",
    powiaty = "Powiaty.zip",
    gminy = "Gminy.zip"
)

# uproszczenie wielokatow na podstawie https://datascience.blog.wzb.eu/2021/03/15/simplifying-geospatial-features-in-r-with-sf-and-rmapshaper/
# simplifying polygons based on https://datascience.blog.wzb.eu/2021/03/15/simplifying-geospatial-features-in-r-with-sf-and-rmapshaper/
tols <- list(
    wojewodztwa = .005,
    powiaty = .01,
    gminy = .025
)

neigh_list <- list()

for (i in names(urls))
{
    download.file(url = str_glue(base_url, urls[[i]]), destfile = str_glue("data-raw/", urls[[i]]))

    where_to_unzip <- str_glue("data-raw/", str_replace(string = urls[[i]], pattern = ".zip", replacement = ""))
    unzip(zipfile = str_glue("data-raw/", urls[[i]]), exdir = where_to_unzip)

    shp_this <- st_read(dsn = str_glue(where_to_unzip, "/", list.files(where_to_unzip, pattern = "shp")))
    # determine negibours
    neigh_list[[i]] <- sfpu::find_out_neigbours(poly = shp_this, kol = "JPT_KOD_JE", idName = "ID_TER", neighName = "ID_NEIGH")

    # simplify shapefile
    simp_this <- ms_simplify(input = shp_this, keep = tols[[i]], keep_shapes = T)
    # check result
    # ggplot() + geom_sf(data = simp_this, aes(fill = as.numeric(JPT_KOD_JE)), size = .1, color = "yellow") + theme_void() + theme(legend.position = "none")
    # compare sizes
    # round(c(object.size(shp_this), object.size(simp_this)) / 1024)
    assign(x = str_glue("shapes_", i), value = simp_this)
}

names(neigh_list) <- names(urls)

# save objects in good format
usethis::use_data(shapes_wojewodztwa, overwrite = TRUE)
usethis::use_data(shapes_powiaty, overwrite = TRUE)
usethis::use_data(shapes_gminy, overwrite = TRUE)
usethis::use_data(neigh_list, overwrite = TRUE)
