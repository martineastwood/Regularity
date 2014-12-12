test_that("StartWith Functions Correctly", {
    pattern <- 'abcdefg'
    expect_that(grep((Regularity() %>% StartWith('letter'))$regex, pattern), testthat::equals(1))
    expect_that(grep((Regularity() %>% StartWith('a'))$regex, pattern), testthat::equals(1))
    expect_that(grep((Regularity() %>% StartWith(7, 'letters'))$regex, pattern), testthat::equals(1))
    
    expect_that(grepl((Regularity() %>% StartWith('digit'))$regex, pattern), testthat::is_false())
    expect_that(grepl((Regularity() %>% StartWith(5, 'digits'))$regex, pattern), testthat::is_false())
    expect_that(grepl((Regularity() %>% StartWith(5, 'whitespace'))$regex, pattern), testthat::is_false())
    expect_that(grepl((Regularity() %>% StartWith(5, 'tab'))$regex, pattern), testthat::is_false())
    expect_that(grepl((Regularity() %>% StartWith(5, 'space'))$regex, pattern), testthat::is_false())
    expect_that(grepl((Regularity() %>% StartWith(5, 'uppercase'))$regex, pattern), testthat::is_false())
    expect_that(grepl((Regularity() %>% StartWith('uppercase'))$regex, pattern), testthat::is_false())
    
    expect_that(grepl((Regularity() %>% StartWith(5, 'alphanumeric'))$regex, pattern), testthat::is_true())
    expect_that(grepl((Regularity() %>% StartWith(5, 'lowercases'))$regex, pattern), testthat::is_true())
    expect_that(grepl((Regularity() %>% StartWith('lowercase'))$regex, pattern), testthat::is_true())
})




