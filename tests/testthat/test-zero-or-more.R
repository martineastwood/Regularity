test_that("ZeroOrMore Functions As Expected", {
    pattern <- 'abcdefg'
    expect_that(grep((regr() %>% ZeroOrMore('letters'))$regex, pattern), testthat::equals(1))
    expect_that(grep((regr() %>% ZeroOrMore('a'))$regex, pattern), testthat::equals(1))
    expect_that(grep((regr() %>% ZeroOrMore('abcd'))$regex, pattern), testthat::equals(1))
    expect_that(grep((regr() %>% ZeroOrMore('digits'))$regex, pattern), testthat::equals(1))
    
    expect_that(grepl((regr() %>% ZeroOrMore('digits'))$regex, pattern), testthat::is_true())
    expect_that(grepl((regr() %>% ZeroOrMore('letters'))$regex, pattern), testthat::is_true())
    expect_that(grepl((regr() %>% ZeroOrMore('lowercase'))$regex, pattern), testthat::is_true())
    expect_that(grepl((regr() %>% ZeroOrMore('uppercase'))$regex, pattern), testthat::is_true())
    expect_that(grepl((regr() %>% ZeroOrMore('space'))$regex, pattern), testthat::is_true())
    expect_that(grepl((regr() %>% ZeroOrMore('whitespace'))$regex, pattern), testthat::is_true())
    expect_that(grepl((regr() %>% ZeroOrMore('tab'))$regex, pattern), testthat::is_true())
    expect_that(grepl((regr() %>% ZeroOrMore('alphanumeric'))$regex, pattern), testthat::is_true())
    expect_that(grepl((regr() %>% ZeroOrMore('bviebvihefbvi'))$regex, pattern), testthat::is_true())
})
