test_that("OneOrMore Functions Correctly", {
    pattern <- 'abcdefg* '
    expect_that(grep((Regularity() %>% OneOrMore('letter'))$regex, pattern), testthat::equals(1))
    expect_that(grep((Regularity() %>% OneOrMore('a'))$regex, pattern), testthat::equals(1))
    expect_that(grep((Regularity() %>% OneOrMore('abcdefg'))$regex, pattern), testthat::equals(1))
    
    expect_that(grepl((Regularity() %>% OneOrMore('digit'))$regex, pattern), testthat::is_false())
    expect_that(grepl((Regularity() %>% OneOrMore('<'))$regex, pattern), testthat::is_false())
    expect_that(grepl((Regularity() %>% OneOrMore('*'))$regex, pattern), testthat::is_true())
    
    expect_that(grepl((Regularity() %>% OneOrMore('abcde'))$regex, pattern), testthat::is_true())
    expect_that(grepl((Regularity() %>% OneOrMore('abcdek'))$regex, pattern), testthat::is_false())
    expect_that(grepl((Regularity() %>% OneOrMore('whitespace'))$regex, pattern), testthat::is_true())
    expect_that(grepl((Regularity() %>% OneOrMore('space'))$regex, pattern), testthat::is_true())
    expect_that(grepl((Regularity() %>% OneOrMore('tab'))$regex, pattern), testthat::is_false())
    expect_that(grepl((Regularity() %>% OneOrMore('\t'))$regex, pattern), testthat::is_false())
})
