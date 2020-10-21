testing
test_that("llr output has correct length", {
    expect_equal(length(llr(x, y, z, omega = 1), length(z)))
})
