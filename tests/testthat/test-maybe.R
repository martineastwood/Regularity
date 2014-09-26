test_that("Maybe Functions As Expected", {
    pattern <- 'abcdefg'
    expect_that(grep((regr() %>% Maybe('letters'))$regex, pattern), testthat::equals(1))
    expect_that(grep((regr() %>% Maybe('a'))$regex, pattern), testthat::equals(1))
    expect_that(grep((regr() %>% Maybe('abcd'))$regex, pattern), testthat::equals(1))
    expect_that(grep((regr() %>% Maybe('digits'))$regex, pattern), testthat::equals(1))
    
    expect_that(grepl((regr() %>% Maybe('digits'))$regex, pattern), testthat::is_true())
    expect_that(grepl((regr() %>% Maybe('letters'))$regex, pattern), testthat::is_true())
    expect_that(grepl((regr() %>% Maybe('lowercase'))$regex, pattern), testthat::is_true())
    expect_that(grepl((regr() %>% Maybe('uppercase'))$regex, pattern), testthat::is_true())
    expect_that(grepl((regr() %>% Maybe('space'))$regex, pattern), testthat::is_true())
    expect_that(grepl((regr() %>% Maybe('whitespace'))$regex, pattern), testthat::is_true())
    expect_that(grepl((regr() %>% Maybe('tab'))$regex, pattern), testthat::is_true())
    expect_that(grepl((regr() %>% Maybe('alphanumeric'))$regex, pattern), testthat::is_true())
    expect_that(grepl((regr() %>% Maybe('bviebvihefbvi'))$regex, pattern), testthat::is_true())
})