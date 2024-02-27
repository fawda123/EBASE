test_that("Checking ebase_prep names", {
  result <- ebase_prep(dat, Z = 1.85, interval = 900) %>% 
    names
  
  expect_equal(result, c("Date", "DateTimeStamp", "isinterp", "DO_obs", "DO_sat", "Z", "Temp", "Sal", 
                         "PAR", "WSpd", "sc", "grp"))
})

test_that("Checking ebase_prep incorrect input", {
  
  expect_error(ebase_prep(exdat[,-5], interval = 900, Z = 1.85))
               
})

test_that("Checking ebase_prep incorrect Z input", {
  
  expect_error(ebase_prep(exdat, interval = 900, Z = c(1, 1.85)))
  
})

test_that("Checking ebase_prep all groups", {
  
  result <- ebase_prep(dat, Z = 1.85, interval = 900, ndays = 3) %>% 
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
  
  expect_warning(
    expect_warning(
      expect_warning(
        expect_equal(ebase_prep(dat2, Z = 1.85, interval = 900, ndays = 1) %>% nrow(), 96)
        )
      )
  )
  
})

test_that("Checking strip of dangling start or end dates", {
  
  toadd <-tibble(
      dts = range(dat$DateTimeStamp)
    ) %>%
    mutate(
      DateTimeStamp = case_when(
        dts == min(dts) ~ list(seq.POSIXt(from = min(dts) - (60 * 60), to = min(dts), by = 900)), 
        dts == max(dts) ~ list(seq.POSIXt(from = max(dts), to = max(dts) + (60 * 60), by = 900))
      )
    ) %>% 
    unnest('DateTimeStamp') %>% 
    select(-dts)

  dat2 <- toadd %>% 
    full_join(dat, by = 'DateTimeStamp') %>% 
    arrange(DateTimeStamp)
  
  expect_warning(
    expect_warning(
      expect_equal(ebase_prep(dat2, Z = 1.85, interval = 900) %>% nrow(), 288)
      )
  )
  
})

