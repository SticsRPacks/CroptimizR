context("Test the test_wrapper function")
param_names<-c("Ti","deltaTs","B","LAImax","C","Eb","Eimax","K")    # set the name of one or several model input parameters in a vector
param_lb<-c(500,400,0.0011,3,0.01,0.9,0.9,0.6)       # set the lower bounds of these parameters in a vector (no Inf or -Inf ...)
param_ub<-c(2500,1200,0.003,12,0.1,2.8,0.99,0.8)       # set the upper bounds of these parameters in a vector (no Inf or -Inf ...)
var_name<-c("LAI","biomas")       # give the name of an output variable sensitive to this (or these) parameter(s)
situation_name<-c("2013_Sit_204") # give the name of the situation to simulate
temp_data_path <- file.path("extdata", "Bonsai_bio")
model_options<-list()
model_options$path<-temp_data_path
model_options$begin<-1
model_options$end<-232 # give the model options
wrapper<- bonsai_bio_wrapper       # give the name of your wrapper

#Execute the call of the wrapper with two sets of parameter and evaluate the sum of the difference on each variable
param_values_min <- setNames(param_lb, param_names)
param_values_max <- setNames(param_ub, param_names)
sim_min       <- wrapper(param_values = param_values_min, model_options = model_options,
                         sit_names=situation_name)
sim_max       <- wrapper(param_values = param_values_max, model_options = model_options,
                         sit_names=situation_name)

print(paste("Sum of differences, variable",var_name,", situation",situation_name," = ",
            sum(abs(sim_max$sim_list[[situation_name]][,var_name] -
                      sim_min$sim_list[[situation_name]][,var_name]),na.rm=TRUE)))
# Should give a value different from 0.
