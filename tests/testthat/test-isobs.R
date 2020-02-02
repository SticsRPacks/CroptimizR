# Correct format
obs_list1 <- list(sit1=data.frame(Date=as.POSIXct(c("2009-11-30","2009-12-10")),var1=c(1.1,1.5),var2=c(NA,2.1)),
                 sit2=data.frame(Date=as.POSIXct(c("2009-11-30","2009-12-5")),var1=c(1.3,2)))
# Missing Date column
obs_list2 <- list(sit1=data.frame(var1=c(1.1,1.5),var2=c(NA,2.1)),
                 sit2=data.frame(Date=as.POSIXct(c("2009-11-30","2009-12-5")),var1=c(1.3,2)))
# Bad Date format
obs_list3 <- list(sit1=data.frame(Date=c("2009-11-30","2009-12-10"),var1=c(1.1,1.5),var2=c(NA,2.1)),
                 sit2=data.frame(Date=c("2009-11-30","2009-12-5"),var1=c(1.3,2)))
test_that("is.obs", {
  expect_true(CroptimizR:::is.obs(obs_list1))
  expect_false(CroptimizR:::is.obs(obs_list2))
#  expect_false(CroptimizR:::is.obs(obs_list3))
})
