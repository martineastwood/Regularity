test_that("ZeroOrMore Functions As Expected", {
    pattern <- 'abcdefg'
    expect_that(grep((Regularity() %>% ZeroOrMore('letters'))$regex, pattern), testthat::equals(1))
    expect_that(grep((Regularity() %>% ZeroOrMore('a'))$regex, pattern), testthat::equals(1))
    expect_that(grep((Regularity() %>% ZeroOrMore('abcd'))$regex, pattern), testthat::equals(1))
    expect_that(grep((Regularity() %>% ZeroOrMore('digits'))$regex, pattern), testthat::equals(1))
    
    expect_that(grepl((Regularity() %>% ZeroOrMore('digits'))$regex, pattern), testthat::is_true())
    expect_that(grepl((Regularity() %>% ZeroOrMore('letters'))$regex, pattern), testthat::is_true())
    expect_that(grepl((Regularity() %>% ZeroOrMore('lowercase'))$regex, pattern), testthat::is_true())
    expect_that(grepl((Regularity() %>% ZeroOrMore('uppercase'))$regex, pattern), testthat::is_true())
    expect_that(grepl((Regularity() %>% ZeroOrMore('space'))$regex, pattern), testthat::is_true())
    expect_that(grepl((Regularity() %>% ZeroOrMore('whitespace'))$regex, pattern), testthat::is_true())
    expect_that(grepl((Regularity() %>% ZeroOrMore('tab'))$regex, pattern), testthat::is_true())
    expect_that(grepl((Regularity() %>% ZeroOrMore('alphanumeric'))$regex, pattern), testthat::is_true())
    expect_that(grepl((Regularity() %>% ZeroOrMore('bviebvihefbvi'))$regex, pattern), testthat::is_true())
})
