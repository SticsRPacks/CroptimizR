obs_list <- list(sit1=data.frame(Date=as.POSIXct(c("2009-11-30","2009-12-10")),var1=c(1.1,1.5),var2=c(NA,2.1)),
              sit2=data.frame(Date=as.POSIXct(c("2009-11-30","2009-12-5")),var1=c(1.3,2)),
              sit3=data.frame(Date=as.POSIXct(c("2010-10-03","2010-10-04")),var1=c(1.,1.1),var2=c(2.0,2.1)))

# Check if nothing is captured if sim and obs lists are identical
res <- CroptimizR:::make_obsSim_consistent(obs_list, obs_list)
test_that("obs and sim are identical", {
  expect_identical(res$obs_list,obs_list)
  expect_identical(res$sim_list,obs_list)
})


# Check if it captures that there are non consistent types for some sim and obs variables
sim_list <- obs_list
sim_list[[1]]$var1 <- as.character(sim_list[[1]]$var1)
sim_list[[3]]$var2 <- as.character(sim_list[[3]]$var2)
test_that("Capture non-consistent variable types", {
  expect_error(CroptimizR:::make_obsSim_consistent(sim_list, obs_list),"different types")
})

# Check if it captures that some Date columns are of non expected type
sim_list <- obs_list
sim_list[[1]]$Date <- as.character(sim_list[[1]]$Date)
sim_list[[3]]$Date <- as.character(sim_list[[3]]$Date)
test_that("Capture unexpected type for Date columns", {
  expect_error(CroptimizR:::make_obsSim_consistent(sim_list, obs_list),"incorrect format")
})

# Check if it corrects non consistent types for sim and obs Dates
sim_list <- obs_list
sim_list[[1]]$Date <- as.Date(sim_list[[1]]$Date)
sim_list[[3]]$Date <- as.Date(sim_list[[3]]$Date)
res <- CroptimizR:::make_obsSim_consistent(sim_list, obs_list)
test_that("Check correction of dates types", {
  expect_true(lubridate::is.POSIXct(res$sim_list[[1]]$Date))
  expect_true(lubridate::is.POSIXct(res$sim_list[[3]]$Date))
})
