context("Test the get_params_* functions")


# Test get_param_bounds
prior_simple <- list(
  lb = c(dlaimax = 0.0005, durvieF = 50),
  ub = c(dlaimax = 0.0025, durvieF = 400)
)
prior_spec_var <- list()
prior_spec_var$dlaimax <- list(
  sit_list = list(c(
    "bou99t3", "bou00t3", "bou99t1", "bou00t1",
    "bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+"
  )),
  lb = 0.0005, ub = 0.0025
)
prior_spec_var$durvieF <- list(
  sit_list = list(
    c("bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+"),
    c("bou99t3", "bou00t3", "bou99t1", "bou00t1")
  ),
  lb = c(50, 100), ub = c(400, 500)
)
test_that("get_params_bounds", {
  expect_equal(eval(parse(text = "CroptimizR:::get_params_bounds(prior_simple)")),
               list(lb=c(dlaimax=5e-04, durvieF=5e+01),
                    ub=c(dlaimax=2.5e-03, durvieF=4.0e+02)))
  expect_equal(eval(parse(text = "CroptimizR:::get_params_bounds(prior_spec_var)")),
               list(lb=c(dlaimax=5e-04, durvieF1=5e+01, durvieF2=1e+02),
                    ub=c(dlaimax=2.5e-03, durvieF1=4.0e+02, durvieF2=5.0e+02)))
})



# Test get_init_values
prior_1=list(init_values=c(dlaimax=0.001, durvieF=200),
             lb=c(dlaimax=0.0001, durvieF=50),
             ub=c(dlaimax=0.01, durvieF=400))
prior_2=list(init_values=data.frame(dlaimax=c(0.001,0.002), durvieF=c(50,200)),
             lb=c(dlaimax=0.0001, durvieF=50),
             ub=c(dlaimax=0.01, durvieF=400))
prior_3=list()
prior_3$dlaimax=list(sit_list=list(c("bou99t3", "bou00t3", "bou99t1", "bou00t1",
                                               "bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+")),
                               init_values=c(0.001,0.002),lb=0.0001,ub=0.1)
prior_3$durvieF=list(sit_list=list(c("bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+"),
                                             c("bou99t3", "bou00t3", "bou99t1", "bou00t1")),
                               init_values=data.frame(c(200,300),c(250,350)),lb=50,ub=400)
test_that("get_init_values", {
  expect_equal(eval(parse(text = "CroptimizR:::get_init_values(prior_1)")),
               data.frame(dlaimax=1e-03, durvieF=2e+02))
  expect_equal(eval(parse(text = "CroptimizR:::get_init_values(prior_2)")),
               data.frame(dlaimax=c(0.001,0.002), durvieF=c(50,200)))
  expect_equal(eval(parse(text = "CroptimizR:::get_init_values(prior_3)")),
               data.frame(dlaimax=c(0.001, 0.002), durvieF1=c(200, 300), durvieF2=c(250, 350)))
})



# Test get_params_names
prior_1 <- list(
       lb = c(dlaimax = 0.0005, durvieF = 50),
       ub = c(dlaimax = 0.0025, durvieF = 400)
  )
prior_2 <- list()
prior_2$dlaimax <- list(
       sit_list = list(c("bou99t3", "bou00t3", "bou99t1", "bou00t1",
                   "bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+")),
       lb = 0.0005, ub = 0.0025
   )
prior_2$durvieF <- list(
       sit_list = list(
             c("bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+"),
             c("bou99t3", "bou00t3", "bou99t1", "bou00t1")
         ),
       lb = c(50, 50), ub = c(400, 400)
   )
prior_3 <- list()
prior_3$durvieF <- list(
  sit_list = list(
    c("bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+"),
    c("bou99t3", "bou00t3", "bou99t1", "bou00t1")
  ),
  lb = c(50, 50), ub = c(400, 400)
)
prior_3$dlaimax <- list(
  sit_list = list(c("bou99t3", "bou00t3", "bou99t1", "bou00t1",
                    "bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+")),
  lb = 0.0005, ub = 0.0025
)
test_that("get_params_names", {
  expect_equal(eval(parse(text = "CroptimizR:::get_params_names(prior_1)")),
               c("dlaimax", "durvieF"))
  expect_equal(eval(parse(text = "CroptimizR:::get_params_names(prior_2)")),
               c("dlaimax", "durvieF1", "durvieF2"))
  expect_equal(eval(parse(text = "CroptimizR:::get_params_names(prior_2, short_list=TRUE)")),
               c("dlaimax", "durvieF"))
  expect_equal(eval(parse(text = "CroptimizR:::get_params_names(prior_3)")),
               c("durvieF1", "durvieF2", "dlaimax"))
  expect_equal(eval(parse(text = "CroptimizR:::get_params_names(prior_3, short_list=TRUE)")),
               c("durvieF", "dlaimax"))
})



# Test get_params_per_sit
sg <- list(
  p1 = list(sit_list = list(c("sit1", "sit2", "sit3"), c("sit4", "sit5", "sit6"))),
  p2 = list(sit_list = list(c("sit1", "sit2", "sit3", "sit4", "sit5", "sit6"))),
  p3 = list(sit_list = list(c("sit1", "sit2", "sit3"), c("sit4", "sit5", "sit6")))
)
vec <- c(1, 2, 3, 4, 5)
names(vec) <- eval(parse(text = "CroptimizR:::get_params_names(sg)"))
test_that("get_params_per_sit", {
  expect_equal(eval(parse(text = "CroptimizR:::get_params_per_sit(sg, \"sit2\", vec)")),c(p1=1,p2=3,p3=4))
  expect_equal(eval(parse(text = "CroptimizR:::get_params_per_sit(sg, \"sit4\", vec)")),c(p1=2,p2=3,p3=5))
})
