
# Download the example USMs:
data_dir= normalizePath(tempdir(), winslash = "/", mustWork = FALSE)
data_dir_zip= normalizePath(file.path(data_dir,"master.zip"), winslash = "/", mustWork = FALSE)
download.file("https://github.com/SticsRPacks/data/archive/master.zip", data_dir_zip)
unzip(data_dir_zip, exdir = data_dir)
unlink(data_dir_zip)
data_dir= file.path(normalizePath(list.dirs(data_dir)[2], winslash = "/"),"study_case_1","V9.0")
# NB: all examples are now in data_dir
# Define the path to the local version of JavaStics
javastics_path=file.path(getwd(),"JavaSTICS-1.41-stics-9.0")
stics_path=file.path(javastics_path,"bin/stics_modulo.exe")

#'
#' ## Read and select the corresponding observations
#'
#' In this example, observed LAI are used for all USMs for which there is an observation file in `file.path(data_dir,"XmlFiles")` folder.
#'
## ----message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Read observation files
obs_list=get_obs(file.path(data_dir,"XmlFiles"))
obs_list=filter_obs(obs_list, var_names=c("lai_n"),include=TRUE)

#'
#' ## Set information on the parameters to estimate
#'
#' **`param_info` allows handling specific / varietal parameters** (dlaimax vs durvieF parameters in this example): dlaimax is defined to take the same value for all situations, whereas durvieF is defined in such a way that it may take one value for situations `c("bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+")`, that correspond to a given variety, and another for situations `c("bou99t3", "bou00t3", "bou99t1", "bou00t1")`, that correspond to another variety, `sit_list` being in this case a list of size 2 (see code below).
#' Please note that bounds can take different values for the different groups of situations (lb and ub are vectors of size 2 for durvieF).
#'
## ----message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------
param_info=list()
param_info$dlaimax=list(sit_list=list(c("bou99t3", "bou00t3", "bou99t1", "bou00t1",
                                        "bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+")),
                        lb=0.0005,ub=0.0025)
param_info$durvieF=list(sit_list=list(c("bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+"),
                                      c("bou99t3", "bou00t3", "bou99t1", "bou00t1")),
                        lb=c(50,100),ub=c(400,450))

#'
#' ## Set options for the parameter estimation method
#'
## ----message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------
optim_options=list()
optim_options$nb_rep <- 7 # Number of repetitions of the minimization
                          # (each time starting with different initial
                          # values for the estimated parameters)
optim_options$maxeval <- 1000 # Maximum number of evaluations of the
                            # minimized criteria
optim_options$xtol_rel <- 1e-04 # Tolerance criterion between two iterations
                                # (threshold for the relative difference of
                                # parameter values between the 2 previous
                                # iterations)
optim_options$path_results <- data_dir # path where to store the results (graph and Rdata)
optim_options$ranseed <- 1234 # random seed

