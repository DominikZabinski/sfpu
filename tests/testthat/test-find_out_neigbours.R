test_that("testing finding out neighbours", {
    expect_error(object = find_out_neigbours(poly = mtcars), regexp = "Needed: sf")
    expect_error(object = find_out_neigbours(shapes_wojewodztwa, kol = "id"), regexp = "Some of the values")
    res <- find_out_neigbours(poly = shapes_wojewodztwa, kol = "JPT_KOD_JE")
    expect_equal(object = class(res), c("data.table", "data.frame"))
    expect_equal(object = nrow(res), 68)
})

# devtools::test_file()
