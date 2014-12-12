test_that("Maybe Functions As Expected", {
    pattern <- 'abcdefg'
    expect_that(grep((Regularity() %>% Maybe('letters'))$regex, pattern), testthat::equals(1))
    expect_that(grep((Regularity() %>% Maybe('a'))$regex, pattern), testthat::equals(1))
    expect_that(grep((Regularity() %>% Maybe('abcd'))$regex, pattern), testthat::equals(1))
    expect_that(grep((Regularity() %>% Maybe('digits'))$regex, pattern), testthat::equals(1))
    
    expect_that(grepl((Regularity() %>% Maybe('digits'))$regex, pattern), testthat::is_true())
    expect_that(grepl((Regularity() %>% Maybe('letters'))$regex, pattern), testthat::is_true())
    expect_that(grepl((Regularity() %>% Maybe('lowercase'))$regex, pattern), testthat::is_true())
    expect_that(grepl((Regularity() %>% Maybe('uppercase'))$regex, pattern), testthat::is_true())
    expect_that(grepl((Regularity() %>% Maybe('space'))$regex, pattern), testthat::is_true())
    expect_that(grepl((Regularity() %>% Maybe('whitespace'))$regex, pattern), testthat::is_true())
    expect_that(grepl((Regularity() %>% Maybe('tab'))$regex, pattern), testthat::is_true())
    expect_that(grepl((Regularity() %>% Maybe('alphanumeric'))$regex, pattern), testthat::is_true())
    expect_that(grepl((Regularity() %>% Maybe('bviebvihefbvi'))$regex, pattern), testthat::is_true())
})