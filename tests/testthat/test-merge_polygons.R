test_that("testing merging polygons", {
    library(data.table)
    expect_error( object = merge_polygons(orPol = "what"), regexp = "Needed: sf" )
    expect_error( object = merge_polygons(orPol = sfpu::shapes_wojewodztwa, uklad = "what"), regexp = "Needed: data.table" )
    properPol = sfpu::shapes_wojewodztwa
    expect_error( object = merge_polygons(orPol = properPol, uklad = data.table(id = "24", kl = 1)) )
    expect_error( object = merge_polygons(orPol = properPol, uklad = data.table(ida = "24", kl = 1), clusterName = "kl") )
    expect_error( object = merge_polygons(orPol = properPol, uklad = data.table(ida = "24", kl = 1:2), clusterName = "kl", polysId = "ida") , regexp = "24")
})

# devtools::test_file()
