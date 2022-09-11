test_that("checking classes with check_class", {
    expect_invisible(call = check_class(object = "abba", needed = "character"))
    expect_error(object = check_class(object = c(1:3), needed = "list"))
})

# devtools::test_file()
