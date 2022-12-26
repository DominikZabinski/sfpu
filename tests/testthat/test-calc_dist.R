test_that("calc_dist works", {
    expect_error(object = calc_dist("a", 1))
    expect_error(object = calc_dist(1.4, "b"))
    expect_error(object = calc_dist(c(1.4, 3, 2), c(1)))
    expect_equal(object = calc_dist(1, 3), expected = 2)
    expect_equal(object = calc_dist(c(0, 0), c(1, 1)), expected = sqrt(2))
})

# devtools::test_file()
