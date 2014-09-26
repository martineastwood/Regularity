test_that("One Of Functions As Expected", {
    pattern <- 'abcdefg'
    expect_that(grep((regr() %>% OneOf(c('a', 'b', 'c')))$regex, pattern), testthat::equals(1))
    expect_that(grep((regr() %>% OneOf(c('letters', 'digits', 'tab')))$regex, pattern), testthat::equals(1))
    expect_that(grep((regr() %>% OneOf(c('letters', 'xxx', 'tab')))$regex, pattern), testthat::equals(1))
    
    expect_that(grepl((regr() %>% OneOf(c('letters', 'xxx', 'tab')))$regex, pattern), testthat::is_true())
    expect_that(grepl((regr() %>% OneOf(c('x', 'j', 'f')))$regex, pattern), testthat::is_true())
    expect_that(grepl((regr() %>% OneOf(c('x', 'j', 'y')))$regex, pattern), testthat::is_false())
    expect_that(grepl((regr() %>% OneOf(c(0, 1, 2, 3)))$regex, pattern), testthat::is_false())
    expect_that(grepl((regr() %>% OneOf(c('uppercase', 'uppercase')))$regex, pattern), testthat::is_false())
    expect_that(grepl((regr() %>% OneOf(c('uppercase', 'digits')))$regex, pattern), testthat::is_false())
    expect_that(grepl((regr() %>% OneOf(c('uppercase', 'lowercase')))$regex, pattern), testthat::is_true())
    expect_that(grepl((regr() %>% OneOf(c('uppercase', 'letters')))$regex, pattern), testthat::is_true())
    expect_that(grepl((regr() %>% OneOf(c('a', 'bb')))$regex, pattern), testthat::is_true())
    expect_that(grepl((regr() %>% OneOf(c('aa', 'bb')))$regex, pattern), testthat::is_false())
})
