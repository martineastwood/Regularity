test_that("EndWith Functions Correctly", {
    pattern <- 'abcdefg'
    expect_that(grep((Regularity() %>% EndWith('letter'))$regex, pattern), testthat::equals(1))
    expect_that(grep((Regularity() %>% EndWith('fg'))$regex, pattern), testthat::equals(1))
    expect_that(grep((Regularity() %>% EndWith(7, 'letters'))$regex, pattern), testthat::equals(1))
    
    expect_that(grepl((Regularity() %>% EndWith('digit'))$regex, pattern), testthat::is_false())
    expect_that(grepl((Regularity() %>% EndWith(5, 'digits'))$regex, pattern), testthat::is_false())
    expect_that(grepl((Regularity() %>% EndWith(5, 'whitespace'))$regex, pattern), testthat::is_false())
    expect_that(grepl((Regularity() %>% EndWith(5, 'tab'))$regex, pattern), testthat::is_false())
    expect_that(grepl((Regularity() %>% EndWith(5, 'space'))$regex, pattern), testthat::is_false())
    expect_that(grepl((Regularity() %>% EndWith(5, 'uppercase'))$regex, pattern), testthat::is_false())
    expect_that(grepl((Regularity() %>% EndWith('uppercase'))$regex, pattern), testthat::is_false())
    
    expect_that(grepl((Regularity() %>% EndWith(5, 'alphanumeric'))$regex, pattern), testthat::is_true())
    expect_that(grepl((Regularity() %>% EndWith(5, 'lowercases'))$regex, pattern), testthat::is_true())
    expect_that(grepl((Regularity() %>% EndWith('lowercase'))$regex, pattern), testthat::is_true())
    
    pattern <- 'abcdefg '
    expect_that(grep((Regularity() %>% EndWith('space'))$regex, pattern), testthat::equals(1))
    expect_that(grep((Regularity() %>% EndWith('whitespace'))$regex, pattern), testthat::equals(1))
    
    pattern <- 'abcdefg\t'
    expect_that(grep((Regularity() %>% EndWith('tab'))$regex, pattern), testthat::equals(1))
    expect_that(grep((Regularity() %>% EndWith('\t'))$regex, pattern), testthat::equals(1))
})
