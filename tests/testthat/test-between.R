test_that("Between Functions As Expected", {
    pattern <- 'abbbbbcddddefghhh<<*&^'
    expect_that(grep((regr() %>% Between(c(1, 5), 'b'))$regex, pattern), testthat::equals(1))
    expect_that(grep((regr() %>% Between(c(0, 1), 'a'))$regex, pattern), testthat::equals(1))
    expect_that(grep((regr() %>% Between(c(2, 4), 'd'))$regex, pattern), testthat::equals(1))
    
    expect_that(grepl((regr() %>% Between(c(0, 10), 'digits'))$regex, pattern), testthat::is_true())
    expect_that(grepl((regr() %>% Between(c(1, 10), 'digits'))$regex, pattern), testthat::is_false())   
    expect_that(grepl((regr() %>% Between(c(0, 10), 'alphanumeric'))$regex, pattern), testthat::is_true())
    expect_that(grepl((regr() %>% Between(c(1, 15), 'uppercase'))$regex, pattern), testthat::is_false())
    expect_that(grepl((regr() %>% Between(c(0, 3), 'uppercase'))$regex, pattern), testthat::is_true())
    
    expect_that(grepl((regr() %>% Between(c(6, 8), 'b'))$regex, pattern), testthat::is_false())
    expect_that(grepl((regr() %>% Between(c(1, 5), 'a'))$regex, pattern), testthat::is_true())
    expect_that(grepl((regr() %>% Between(c(0, 5), '<'))$regex, pattern), testthat::is_true())
    expect_that(grepl((regr() %>% Between(c(0, 5), '&'))$regex, pattern), testthat::is_true())
    expect_that(grepl((regr() %>% Between(c(1, 5), '/'))$regex, pattern), testthat::is_false())
    })