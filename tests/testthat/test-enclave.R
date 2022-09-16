test_that("testing enclave_lst", {
    expect_equal(object = class(enclave_lst()), expected = "list")
})

test_that("testing enclave_lst", {
    expect_equal(object = class(enclave_dtl()), expected = c("data.table", "data.frame"))
})

test_that("testing enclave_lst", {
    expect_identical(enclave_vec(1), "1")
    expect_identical(enclave_vec(c("1", "3261", "15")), c(1, "3209_3261", "15"))
})

# devtools::test_file()
