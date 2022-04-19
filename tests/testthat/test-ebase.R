test_that("Checking ebase names", {
  
  # get four days of data
  dat <- exdat %>%
    filter(month(exdat$DateTimeStamp) == 6 & day(exdat$DateTimeStamp) %in% 1:2)

  result <- ebase(dat, interval = 900, H = 1.85, progress = TRUE, n.chains = 2, 
                  model_file = file.path(system.file(package="EBASE"), "ebase_model.txt")) %>% 
    names
  
  expect_equal(result, c("Date", "DO_obs", "DO_mod", "DateTimeStamp", "dDO", "a", "b", 
                         "Pg_vol", "Rt_vol", "D"))
  
})
