params <-
list(cores_nb = 2L)

## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(eval = FALSE)

## -----------------------------------------------------------------------------
#  #' @title Running situation(s) from txt input files stored in one directory
#  #' per `situation`, simulated results are returned in a list
#  #'
#  #' @description This function uses the model directly through a system call, can
#  #' force model input parameters with values given in arguments.
#  #'
#  #' @param param_values named vector containing the value(s) and names of the
#  #' parameters to force (optional)
#  #'
#  #' @param site_var_dates_mask List of situations, variables and dates for which
#  #' simulated values should be returned. Typically a list containing the
#  #' observations to which simulations should be compared
#  #' (i.e. a list of variables/dates per situation)
#  #'
#  #' @param prior_information Prior information on the parameters to estimate.
#  #' For the moment only uniform distribution are allowed.
#  #' Either a list containing (named) vectors of upper and lower
#  #' bounds (\code{ub} and \code{lb}), or a named list containing for each
#  #' parameter the list of situations per group (\code{sit_list})
#  #' and the vector of upper and lower bounds (one value per group) (\code{ub} and \code{lb})
#  #'
#  #' @param model_options List containing any information needed by the model.
#  #' For example: the path of the model executable file,
#  #' the path of the directory containing input data
#  #'
#  #' @return A list containing simulated values (\code{sim_list}) and a flag
#  #' (\code{flag_allsim}) indicating if all required situations, variables and
#  #' dates were simulated.
#  
#  model_wrapper <- function( param_values=NULL, site_var_dates_mask=NULL,
#                             prior_information=NULL, model_options ) {
#    ...
#  
#  }
#  

## -----------------------------------------------------------------------------
#  model_wrapper_options <- function(model_path,
#                                    data_dir, ... ) {
#  
#    # options list template
#    options <- list()
#    # model executable
#    options$model_path <- character(0)
#    # input data
#    options$data_dir <- character(0)
#    # parallel calculation switch
#    options$parallel <- FALSE
#    # setting cores number
#    options$cores <- NA
#    # duration time display switch
#    options$time_display <- FALSE
#    # warning display switch
#    options$warning_display <- TRUE
#  
#    # Getting the options list template content
#    # when running model_wrapper_options()
#    if (! nargs()) return(options)
#  
#    # Fixing mandatory fields values
#    options$model_path <- model_path
#    options$data_dir <- data_dir
#  
#    # Fixing optional fields:
#    # if names in given list (...) correspond
#    # to exact field names in options list
#    list_names <- names (options)
#    add_args <- list(...)
#  
#    for (n in names(add_args)) {
#      if ( n %in% list_names) {
#        options[[n]] <- add_args[[n]]
#      }
#    }
#  
#    return(options)
#  }

## ----warning=FALSE------------------------------------------------------------
#  library(SticsRFiles)
#  path <- system.file(file.path("extdata","obs","V9.0"), package = "SticsRFiles")
#  obs_list <- read_obs(path)
#  lapply(obs_list, function(x) head(x,4))
#  

## -----------------------------------------------------------------------------
#  library(CroptimizR)
#  sg=list(p1=list(sit_list=list(c("sit1","sit2","sit3"),c("sit4","sit5","sit6"))),
#          p2=list(sit_list=list(c("sit1","sit2","sit3","sit4","sit5","sit6"))))
#  
#  vec=c(1,2,3)
#  
#  names(vec) <- c("p2","p1","p1")
#  
#  params_sit2 <- get_params_per_sit(sg,"sit2",vec)
#  
#  params_sit2
#  

## -----------------------------------------------------------------------------
#  # Code of an example of foreach loop algo
#  library("doParallel")
#  print(params$cores_nb)
#  
#  test_parallel <- function(cores_nb = 1,
#                            pa = FALSE,
#                            max_it = 5) {
#  
#    # Launching the cluster
#    cl <- makeCluster(cores_nb)
#    registerDoParallel(cl)
#  
#    # List preallocation
#    out_pa <- vector(mode = "list", max_it)
#  
#    # Parallel loop
#    out <- foreach(i = 1:max_it) %dopar% {
#      if (pa) {
#        out_pa[[i]] <- i
#      } else {
#        return(i)
#      }
#    }
#  
#    # Stopping the cluster
#    stopCluster(cl)
#  
#    if (pa) {
#      return(out_pa)
#    } else {
#      return(out)
#    }
#  
#  
#  }
#  
#  out <- test_parallel(params$cores_nb)
#  
#  out_pa <- test_parallel(params$cores_nb, TRUE)
#  
#  out
#  
#  out_pa
#  

