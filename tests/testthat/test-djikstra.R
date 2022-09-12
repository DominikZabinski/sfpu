test_that("testing 'table_dists'", {
    expect_error(object = table_dists(lvl = 1)      , regexp = "Needed: character, actual: numeric")
    expect_error(object = table_dists(lvl = "wrong"), regexp = "values"                            )

    dist_wojewodztwa <- table_dists(lvl = "wojewodztwa")
    expect_equal(object = class(dist_wojewodztwa), expected = c("data.table", "data.frame")     )
    expect_equal(object = nrow(dist_wojewodztwa) , expected = nrow(sfpu::neigh_list$wojewodztwa))

    dist_powiaty <- table_dists(lvl = "powiaty")
    expect_equal(object = class(dist_powiaty), expected = c("data.table", "data.frame") )
    expect_equal(object = nrow(dist_powiaty) , expected = nrow(sfpu::neigh_list$powiaty))

    dist_gminy <- table_dists(lvl = "gminy")
    expect_equal(object = class(dist_gminy), expected = c("data.table", "data.frame"))
    expect_equal(object = nrow(dist_gminy) , expected = nrow(sfpu::neigh_list$gminy) )
})

test_that("testing 'short_paths'", {
    expect_error(object = short_paths(g = 1)                 , regexp = "Needed: data.table, actual: numeric")
    expect_error(object = short_paths(g = data.table(mtcars)), regexp = "Some of the values"                 )

    dist_wojewodztwa <- table_dists(lvl = "wojewodztwa")
    expect_error(object = short_paths(g = dist_wojewodztwa, start = 1), regexp = "Some of the values")
    res_paths <- short_paths(g = dist_wojewodztwa, start = '02')
    expect_equal(object = class(res_paths), expected = c("data.table", "data.frame"))
    expect_equal(object = nrow(res_paths), expected = nrow(sfpu::shapes_wojewodztwa))
    expect_equal(object = res_paths[a == '02']$odl, expected = 0)
})

test_that("testing 'vis_paths'", {
    expect_error(object = vis_paths(path = "1"), regexp = "Needed: data.table, actual: character")
    expect_error(object = vis_paths(path = data.table(mtcars)), regexp = "Some of the values")
    res_paths <- short_paths(g = table_dists(lvl = "wojewodztwa"), start = '02')
    expect_error(object = vis_paths(path = res_paths, end = 2), regexp = "Needed: character, actual: numeric")
    expect_error(object = vis_paths(path = res_paths, end = "2"), regexp = "Some of the values are outside needed range")
    res_vis_paths <- vis_paths(path = res_paths, end = "16")
    expect_equal(object = class(res_vis_paths), expected = c("gg", "ggplot"))
})

# devtools::test_file()
