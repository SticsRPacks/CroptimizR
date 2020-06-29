#' @title main function for parameter estimation
#'
#' @param obs_list List of observed values to use for parameter estimation
#' A `named list` (names = situations names) of data.frame containing
#' one column named Date with the dates (Date or POSIXct format) of the different observations
#' and one column per observed variables with either the measured values or NA, if
#' the variable is not observed at the given date.
#' @param crit_function Function implementing the criterion to optimize
#' (optional, see default value in the function signature). See
#' [here](https://sticsrpacks.github.io/CroptimizR/reference/ls_criterion.html)
#' for more details about the list of proposed criterion.
#' @param model_function Crop Model wrapper function to use.
#' @param model_options List of options for the Crop Model wrapper (see help of
#' the Crop Model wrapper function used).
#' @param optim_method Name of the parameter estimation method to use (optional,
#' see default value in the function signature). For the moment, can be "simplex"
#' or "dreamzs". See [here](https://sticsrpacks.github.io/CroptimizR/articles/Available_parameter_estimation_algorithms.html)
#' for a brief description and references on the available methods.
#' @param optim_options List of options of the parameter estimation method.
#' `path_results` The path where to store the results (optional, default=getwd())
#' Click on the links to see the specific options for the [simplex](https://sticsrpacks.github.io/CroptimizR/articles/Parameter_estimation_simple_case.html)
#' and [DreamZS](https://sticsrpacks.github.io/CroptimizR/articles/Parameter_estimation_DREAM.html) methods.
#' @param param_info Information on the parameters to estimate.
#' Either
#' a list containing:
#'    - (named) vectors of upper and lower bounds (`ub` and `lb`) (-Inf and Inf can be used),
#'    - `init_values`, A data.frame containing initial
#' values to test for the parameters (optional, if not provided, or if less values
#' than number of repetitions of the minimization are provided), the, or part
#' of the, initial values will be randomly generated using LHS sampling within
#' parameter bounds).
#'
#' or
#' a named list containing for each parameter the list of situations per group
#' (`sit_list`), the vector of upper and lower bounds (one value per group)
#' (`ub` and `lb`) and the list of initial values per group
#' `init_values` (data.frame, one column per group, optional).
#' (see [here](https://sticsrpacks.github.io/CroptimizR/articles/Parameter_estimation_Specific_and_Varietal.html) for an example)
#' @param transform_obs Function for transforming observations (optional)
#' @param transform_sim Function for transforming simulations (optional)
#'
#' @return prints, graphs and a list containing the results of the parameter estimation,
#' which content depends on the method used, all that saved in the defined in
#' `optim_options.path_results`
#'
#' @seealso For more detail and examples, see the different vignettes in
#' [CroptimizR website](https://sticsrpacks.github.io/CroptimizR/)
#'
#' @export
#'

temp_data_path <- system.file(file.path("extdata", "Bonsai_bio", "Tmoy_France_INRA_STATION_33550003 - R_mJ_m2.csv"),
                              package="CroptimizR")


p<- c(K=0.65497,Eimax=0.971669,Eb=2.726781,tzero=204.5381,Ti=1560.870155,deltaTs=678.088316,B=0.002107,LAImax=10.197236,C=0.035339)
p1<- c(K=0.65497,Eimax=0.971669,Eb=2.726781,tzero=204.5381,Ti=1560.870155,deltaTs=678.088316,B=0,LAImax=10.197236,C=0)
p2<- c(K=0.65497,Eimax=0.971669,Eb=2.726781,tzero=204.5381,Ti=1560.870155,deltaTs=678.088316,B=-500,LAImax=10.197236,C=500)
p3<- c(K=0.65497,Eimax=0.971669,Eb=2.726781,tzero=204.5381,Ti=1560.870155,deltaTs=678.088316,B=0.002107,LAImax=0,C=0.035339)
x <- rbind(p,p1,p2,p3)

p_true <- array( c(p,p1,p2,p3),
                 dim=c(1,9,4),
                 dimnames=list(NULL,c("K", "Eimax","Eb","tzero","Ti",
                                      "deltaTs","B","LAImax","C")
                               ,c("21106002_2013_130260","68028003_2013_100300",
                                  "33550003_2013_100360","84007004_2016_050260")))

# Install and load the needed libraries
if(!require("SticsRPacks")){
  devtools::install_github("SticsRPacks/SticsRPacks")
  library("SticsRPacks")
}





# Set the model options (see '? stics_wrapper_options' for details)
#model_options=stics_wrapper_options(stics_path,stics_inputs_path,parallel=FALSE)
model_options2<-list()
model_options2$path<-temp_data_path
model_options2$begin_end<-c("130260","100300","100360","050260","140270")

# Run the model on all situations found in stics_inputs_path
sim_before_optim=CroptimizR:::bonsai_bio_wrapper(model_options2,p_true)


t_obs<-c(140,150,160,170,180,190,200,210,220,230,240,250,260,270)
wrapper<-CroptimizR:::bonsai_bio_wrapper

obs_list<-CroptimizR:::create_synth_obs(wrapper, model_options2,t_obs,p_true)
obs_list




sit_name=c("21106002_2013_130260","68028003_2013_100300","33550003_2013_100360","84007004_2016_050260")  # can be a vector of situation names if you want to consider several, e.g. c("bo96iN+","bou00t1")
lai_name=c("LAI","biom","LAI_obs","biom_obs")    # can be a vector of variable names if you want to consider several, e.g. c("lai_n","masec_n")


obs_list


optim_options=list()
optim_options$nb_rep <- 7 # Number of repetitions of the minimization
# (each time starting with different initial
# values for the estimated parameters)
optim_options$maxeval <- 500 # Maximum number of evaluations of the
# minimized criteria
optim_options$xtol_rel <- 1e-03 # Tolerance criterion between two iterations
# (threshold for the relative difference of
# parameter values between the 2 previous
# iterations)
optim_options$path_results <-temp_data_path  # path where to store the results (graph and Rdata)
optim_options$ranseed <- 1234 # set random seed so that each execution give the same results
# If you want randomization, don't set it.

# 2 parameters here: dlaimax and durvieF, of bounds [0.0005,0.0025] and [50,400].

optim_results=estim_param(obs_list=as.data.frame(obs_list),
                          model_function=wrapper,
                          model_options=model_options2,
                          optim_options=optim_options,
                          param_info=param_info)

