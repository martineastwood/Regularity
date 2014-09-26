test_that("OneOrMore Functions Correctly", {
    pattern <- 'abcdefg* '
    expect_that(grep((regr() %>% OneOrMore('letter'))$regex, pattern), testthat::equals(1))
    expect_that(grep((regr() %>% OneOrMore('a'))$regex, pattern), testthat::equals(1))
    expect_that(grep((regr() %>% OneOrMore('abcdefg'))$regex, pattern), testthat::equals(1))
    
    expect_that(grepl((regr() %>% OneOrMore('digit'))$regex, pattern), testthat::is_false())
    expect_that(grepl((regr() %>% OneOrMore('<'))$regex, pattern), testthat::is_false())
    expect_that(grepl((regr() %>% OneOrMore('*'))$regex, pattern), testthat::is_true())
    
    expect_that(grepl((regr() %>% OneOrMore('abcde'))$regex, pattern), testthat::is_true())
    expect_that(grepl((regr() %>% OneOrMore('abcdek'))$regex, pattern), testthat::is_false())
    expect_that(grepl((regr() %>% OneOrMore('whitespace'))$regex, pattern), testthat::is_true())
    expect_that(grepl((regr() %>% OneOrMore('space'))$regex, pattern), testthat::is_true())
    expect_that(grepl((regr() %>% OneOrMore('tab'))$regex, pattern), testthat::is_false())
    expect_that(grepl((regr() %>% OneOrMore('\t'))$regex, pattern), testthat::is_false())
})
