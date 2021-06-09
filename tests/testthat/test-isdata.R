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
  expect_true(eval(parse(text = "CroptimizR:::is.obs(obs_list1)")))
  expect_false(suppressWarnings(eval(parse(text = "CroptimizR:::is.obs(obs_list2)"))))
  expect_false(suppressWarnings(eval(parse(text = "CroptimizR:::is.obs(obs_list3)"))))
})


# Correct format
sim_list1 <- vector("list",2)
sim_list1 <- list(sit1=data.frame(Date=as.POSIXct(c("2009-11-30","2009-12-10")),var1=c(1.1,1.5),var2=c(NA,2.1)),
                      sit2=data.frame(Date=as.POSIXct(c("2009-11-30","2009-12-5")),var1=c(1.3,2)))
# Missing Date column
sim_list2 <- vector("list",2)
sim_list2 <- list(sit1=data.frame(Date=as.POSIXct(c("2009-11-30","2009-12-10")),var1=c(1.1,1.5),var2=c(NA,2.1)),
                      sit2=data.frame(var1=c(1.3,2)))
# Bad Date format
sim_list3 <- vector("list",1)
sim_list3 <- list(sit1=data.frame(Date=c("2009-11-30","2009-12-10"),var1=c(1.1,1.5),var2=c(NA,2.1)),
                  sit2=data.frame(Date=c("2009-11-30","2009-12-5"),var1=c(1.3,2)))

test_that("is.sim", {
  expect_true(CroptimizR:::is.sim(sim_list1))
  expect_false(suppressWarnings(eval(parse(text = "CroptimizR:::is.sim(sim_list2)"))))
  expect_false(suppressWarnings(eval(parse(text = "CroptimizR:::is.sim(sim_list3)"))))
})


# Correct format
data_list1 <- list(sit1=data.frame(Date=as.POSIXct(c("2009-11-30","2009-12-10")),var1=c(1.1,1.5),var2=c(NA,2.1)),
                  sit2=data.frame(Date=as.POSIXct(c("2009-11-30","2009-12-5")),var1=c(1.3,2)))
# Missing Date column
data_list2 <- list(sit1=data.frame(var1=c(1.1,1.5),var2=c(NA,2.1)),
                  sit2=data.frame(Date=as.POSIXct(c("2009-11-30","2009-12-5")),var1=c(1.3,2)))
# Bad Date format
data_list3 <- list(sit1=data.frame(Date=c("2009-11-30","2009-12-10"),var1=c(1.1,1.5),var2=c(NA,2.1)),
                  sit2=data.frame(Date=c("2009-11-30","2009-12-5"),var1=c(1.3,2)))
# Relicates in Date column
data_list4 <- list(sit1=data.frame(Date=as.POSIXct(c("2009-11-30","2009-12-10","2009-12-10")),var1=c(1.1,1.5,2.3),var2=c(NA,2.1,NA)),
                   sit2=data.frame(Date=as.POSIXct(c("2009-11-30","2009-12-5")),var1=c(1.3,2)))
test_that("is.data", {
  expect_true(CroptimizR:::is.data(data_list1))
  expect_false(suppressWarnings(eval(parse(text = "CroptimizR:::is.data(data_list2)"))))
  expect_false(suppressWarnings(eval(parse(text = "CroptimizR:::is.data(data_list3)"))))
  expect_false(suppressWarnings(eval(parse(text = "CroptimizR:::is.data(data_list4)"))))
})
