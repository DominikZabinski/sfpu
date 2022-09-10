test_that("checking classes with check_class", {
    expect_invisible(check_class(object = "abba", classNeeded = "character"))
    expect_error(object = check_class(object = c(1:3), classNeeded = "list"))
})

# devtools::test_file()
