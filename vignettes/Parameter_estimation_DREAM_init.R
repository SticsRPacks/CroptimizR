
# Download the example USMs:
data_dir= normalizePath(tempdir(), winslash = "/", mustWork = FALSE)
data_dir_zip= normalizePath(file.path(data_dir,"master.zip"), winslash = "/", mustWork = FALSE)
download.file("https://github.com/SticsRPacks/data/archive/master.zip", data_dir_zip)
unzip(data_dir_zip, exdir = data_dir)
unlink(data_dir_zip)
data_dir= file.path(normalizePath(list.dirs(data_dir)[2], winslash = "/"),"study_case_1","V9.0")
# NB: all examples are now in data_dir
# Define the path to the local version of JavaStics 
javastics_path=file.path(path_to_JavaStics,"JavaSTICS-1.41-stics-9.0")
stics_path=file.path(javastics_path,"bin/stics_modulo.exe")

#' 
#' 
#' ## Read and select the corresponding observations
#' 
#' This part is not shown here, it is the same as this of the [specific and varietal parameters estimation vignette](https://sticsrpacks.github.io/CroptimizR/articles/Parameter_estimation_Specific_and_Varietal.html).
#' 
## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------
# Read observation files
obs_list=get_obs(file.path(data_dir,"XmlFiles"))
obs_list=filter_obs(obs_list, var_names=c("lai_n"),include=TRUE)

#' 
#' 
#' ## Set options for the DREAM method
#' 
#' The main basic options are set here. The reader can refer to `? DREAMzs` (see doc for input argument `settings`) for documentation on more advanced ones. Note that `startValue` can also be used to provide initial values for the Markov Chains (in that case, it must be a matrix having as many lines as desired number of Markov Chains and a number of columns equivalent to the number of estimated parameters, ordered in the same way as `param_info$lb` and `param_info$ub`).
#' 
## ----message=FALSE, warning=FALSE--------------------------------------
optim_options=list() 
optim_options$iterations <- 10000 # Total number of iterations 
                                  # (=> optim_options$iterations/optim_options$startValue 
                                  # iterations per chain)
optim_options$startValue <- 3 # Number of markov chains
optim_options$path_results <- data_dir # path where to store the results (graph and Rdata)
optim_options$ranseed <- 1234 # seed for random numbers
