test_that("Checking ebase_prep names", {
  result <- ebase_prep(dat, H = 1.85, interval = 900) %>% 
    names
  
  expect_equal(result, c("Date", "DateTimeStamp", "isinterp", "DO_obs", "DO_sat", "H", "Temp", "Sal", 
                         "PAR", "WSpd", "sc", "grp"))
})

test_that("Checking ebase_prep incorrect input", {
  
  expect_error(ebase_prep(exdat[,-5], interval = 900, H = 1.85))
               
})

test_that("Checking ebase_prep incorrect H input", {
  
  expect_error(ebase_prep(exdat, interval = 900, H = c(1, 1.85)))
  
})

test_that("Checking ebase_prep all groups", {
  
  result <- ebase_prep(dat, H = 1.85, interval = 900, ndays = 2) %>% 
    pull(grp) %>% 
    unique %>% 
    length
  
  expect_equal(result, 1)
  
})

test_that("Checking ebase_prep interpolation", {
  
  set.seed(222)
  dat2 <- dat %>% 
    slice_sample(prop = 0.9) %>% 
    arrange(DateTimeStamp)
  expect_message(expect_message(expect_equal(
    ebase_prep(dat2, H = 1.85, interval = 900, ndays = 1, interp = T) %>% nrow(), 192
    )))
  
})

test_that("Checking ebase_prep error for different time step and no interp", {
  
  set.seed(222)
  dat2 <- dat %>% 
    slice_sample(prop = 0.9) %>% 
    arrange(DateTimeStamp)
  expect_error(
    ebase_prep(dat2, H = 1.85, interval = 900, ndays = 1, interp = F) %>% nrow()
    )
})

