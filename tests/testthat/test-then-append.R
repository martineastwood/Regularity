test_that("Then and Append Functions As Expected", {
    pattern <- 'abcdefg'
    
    expect_that(grep((Regularity() %>% StartWith('letter') %>% Then('letter'))$regex, pattern), testthat::equals(1))
    expect_that(grep((Regularity() %>% StartWith('letter') %>% Then('alphanumeric'))$regex, pattern), testthat::equals(1))
    expect_that(grep((Regularity() %>% StartWith('letter') %>% Then(3, 'letters'))$regex, pattern), testthat::equals(1))
    expect_that(grep((Regularity() %>% StartWith('letter') %>% Then(4, 'alphanumerics'))$regex, pattern), testthat::equals(1))
    
    expect_that(grep((Regularity() %>% StartWith('letter') %>% Append('letter'))$regex, pattern), testthat::equals(1))
    expect_that(grep((Regularity() %>% StartWith('letter') %>% Append('alphanumeric'))$regex, pattern), testthat::equals(1))
    expect_that(grep((Regularity() %>% StartWith('letter') %>% Append(3, 'letters'))$regex, pattern), testthat::equals(1))
    expect_that(grep((Regularity() %>% StartWith('letter') %>% Append(4, 'alphanumerics'))$regex, pattern), testthat::equals(1))
    expect_that(grep((Regularity() %>% StartWith('letter') %>% Append(4, 'lowercase'))$regex, pattern), testthat::equals(1))
    
    expect_that(grepl((Regularity() %>% StartWith('letter') %>% Append('letter'))$regex, pattern), testthat::is_true())
    expect_that(grepl((Regularity() %>% StartWith('letter') %>% Append('alphanumeric'))$regex, pattern), testthat::is_true())
    expect_that(grepl((Regularity() %>% StartWith('letter') %>% Append(3, 'letters'))$regex, pattern), testthat::is_true())
    expect_that(grepl((Regularity() %>% StartWith('letter') %>% Append(4, 'alphanumerics'))$regex, pattern), testthat::is_true())
    expect_that(grepl((Regularity() %>% StartWith('letter') %>% Append(4, 'lowercase'))$regex, pattern), testthat::is_true())

    expect_that(grepl((Regularity() %>% StartWith('letter') %>% Then('digit'))$regex, pattern), testthat::is_false())
    expect_that(grepl((Regularity() %>% StartWith('letter') %>% Then(3, 'digits'))$regex, pattern), testthat::is_false())
    expect_that(grepl((Regularity() %>% StartWith('letter') %>% Then('space'))$regex, pattern), testthat::is_false())
    expect_that(grepl((Regularity() %>% StartWith('letter') %>% Then('whitespace'))$regex, pattern), testthat::is_false())
    expect_that(grepl((Regularity() %>% StartWith('letter') %>% Then('tab'))$regex, pattern), testthat::is_false())
    expect_that(grepl((Regularity() %>% StartWith('letter') %>% Then('uppercase'))$regex, pattern), testthat::is_false())
    
    expect_that(grepl((Regularity() %>% StartWith('letter') %>% Append('digit'))$regex, pattern), testthat::is_false())
    expect_that(grepl((Regularity() %>% StartWith('letter') %>% Append(3, 'digits'))$regex, pattern), testthat::is_false())
    expect_that(grepl((Regularity() %>% StartWith('letter') %>% Append('space'))$regex, pattern), testthat::is_false())
    expect_that(grepl((Regularity() %>% StartWith('letter') %>% Append('whitespace'))$regex, pattern), testthat::is_false())
    expect_that(grepl((Regularity() %>% StartWith('letter') %>% Append('tab'))$regex, pattern), testthat::is_false())
    expect_that(grepl((Regularity() %>% StartWith('letter') %>% Append('uppercase'))$regex, pattern), testthat::is_false())
})