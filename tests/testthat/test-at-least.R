test_that("At Least Functions As Expected", {
    pattern <- 'abbbbbcddddefghhh<<*&^'
    expect_that(grep((regr() %>% AtLeast(5, 'b'))$regex, pattern), testthat::equals(1))
    expect_that(grep((regr() %>% AtLeast(0, 'a'))$regex, pattern), testthat::equals(1))
    expect_that(grep((regr() %>% AtLeast(2, '<'))$regex, pattern), testthat::equals(1))
    
    expect_that(grepl((regr() %>% AtLeast(5, 'digits'))$regex, pattern), testthat::is_false())
    expect_that(grepl((regr() %>% AtLeast(0, 'digits'))$regex, pattern), testthat::is_true())   
    expect_that(grepl((regr() %>% AtLeast(10, 'alphanumeric'))$regex, pattern), testthat::is_true())
    expect_that(grepl((regr() %>% AtLeast(100, 'uppercase'))$regex, pattern), testthat::is_false())
    expect_that(grepl((regr() %>% AtLeast(100, 'lowercase'))$regex, pattern), testthat::is_false())
    
    expect_that(grepl((regr() %>% AtLeast(15, 'b'))$regex, pattern), testthat::is_false())
    expect_that(grepl((regr() %>% AtLeast(1, 'a'))$regex, pattern), testthat::is_true())
    expect_that(grepl((regr() %>% AtLeast(2, '<'))$regex, pattern), testthat::is_true())
    expect_that(grepl((regr() %>% AtLeast(20, '<'))$regex, pattern), testthat::is_false())
    expect_that(grepl((regr() %>% AtLeast(5, '&'))$regex, pattern), testthat::is_false())
    expect_that(grepl((regr() %>% AtLeast(1, '/'))$regex, pattern), testthat::is_false())
})