test_that("Checking ebase_years", {
  
  result <- EBASE::ebase_years(dat, interval = 900, Z = 1.85, n.chains = NULL, doave = F, 
                         n.iter = 50, quiet = F) 
  
  expect_s3_class(result, 'data.frame')
  
})

test_that("Checking ebase_years failure", {
  
  result <- EBASE::ebase_years(dat, interval = 900, Z = 1.85, n.chains = NULL, 
                               n.iter = 50, maxtry = 1, bprior = c(0, 0)) 
  
  expect_null(result)
  
})

test_that("Checking ebase_years incorrect Z", {
  
  expect_error(EBASE::ebase_years(dat, interval = 900, Z = c(1.85, 1.85), n.chains = NULL, 
                               n.iter = 50), 'Supplied value for Z has length 2, should be 1 or 288')
  
})